{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A mostly-complete test selection and execution program for
-- running HUnit-Plus tests.  The only thing missing are the actual
-- test suites, which are provided as parameters to 'createMain'.
--
-- Given a set of test suites, module can be used to create a test
-- execution program as follows:
--
-- >module Main(main) where
-- >
-- >import Test.HUnitPlus.Main
-- >import MyProgram.Tests(testsuites)
-- >
-- >main :: IO ()
-- >main = createMain testsuites
--
-- Where @testsuites@ is a list of 'TestSuite's.
-- 
-- The resulting program, when executed with no arguments will execute
-- all test suites and write a summary to @stdout@.  Additionally, the
-- test program has a number of options that control reporting and
-- test execution.
--
-- A summary of the options follows:
--
-- * @-c /mode/, --consolemode=/mode/@: Set the behavior of console
--   reporting to /mode/.  Can be 'quiet', 'terminal', 'text', and
--   'verbose'.  Default is 'terminal'.
--
-- * @-t [/file/], --txtreport[=/file/]@: Write a text report to
--   /file/ (if specified; if not, the default is 'report.txt').
--   Formatting of the report is the same as the 'verbose' terminal
--   mode.
--
-- * @-x [/file/], --xmlreport[=/file/]@: Write a JUnit-style XML
--   report to /file/ (if specified; if not, the default is 'report.xml').
--
-- * @-l /file/, --testlist=/file/@: Read a testlist from /file/.  The
--   file must contain a number of filters, one per line.  Empty lines
--   or lines beginning with '#' are ignored.  Multiple files may be
--   specified.  The filters from all files are combined, and added to
--   any filters specified on the command line.
--
-- Any additional arguments are assumed to be filters, which specify a
-- set of tests to be run.  For more information on the format of
-- filters, see the 'Filter' module.  If no filters are
-- given either on the command line or in testlist files, then all
-- tests will be run.
module Test.HUnitPlus.Main(
       Opts(..),
       ConsoleMode(..),
       opts,
       createMain,
       topLevel
       ) where

import Control.Exception
import Data.ByteString.Lazy(hPut)
import Data.Either
import Data.Map(Map)
import System.Console.CmdArgs hiding (Quiet)
import System.Exit
import System.IO
import Test.HUnitPlus.Base hiding (name)
import Test.HUnitPlus.Execution
import Test.HUnitPlus.Filter
import Test.HUnitPlus.Reporting hiding (Node)
import Test.HUnitPlus.Text
import Test.HUnitPlus.XML
import Text.XML.Expat.Format
import Text.XML.Expat.Tree(Node)

-- | Console mode options.
data ConsoleMode =
  -- | Do not generate any console output.
    Quiet
  -- | Report test counts interactively during execution, updating the
  -- number of tests run, skipped, failed, and errored as they
  -- execute.
  | Terminal
  -- | Report a summary of tests run, skipped, failed, and errored
  -- after execution.
  | Text
  -- | Report extra information as tests execute.
  | Verbose
  deriving (Typeable, Data, Show)

-- | Command-line options for generated programs.
data Opts =
  Opts {
    -- | A file to which to write a JUnit-style XML report.  The list
    -- must contain a single value, or be empty, or else the test
    -- program will report bad options.  If the list is empty, no XML
    -- report will be generated.
    xmlreport :: [String],
    -- | Filters in string format, specifying which tests should be
    -- run.  If no filters are given, then all tests will be run.  For
    -- information on the string format, see "Test.HUnitPlus.Filter".
    filters :: [String],
    -- | A file to which to write a plain-text report.  The list must
    -- contain a single value, or be empty, or else the test program
    -- will report bad options.  If the list is empty, no report will
    -- be generated.
    txtreport :: [String],
    -- | The behavior of the console output.
    consmode :: [ConsoleMode],
    -- | Files from which to read testlists.  Multiple files may be
    -- specified.  The contents will be parsed and added to the list
    -- of filters specified on the command line.
    testlist :: [String]
  }
  deriving (Typeable, Show, Data)

-- | Command-line options for the "System.Console.CmdArgs" module.
opts :: Opts
opts =
  Opts {
    testlist = []
      &= explicit
      &= typFile
      &= name "l"
      &= name "testlist"
      &= help "Read test filters from FILE",
    xmlreport = []
      &= typFile
      &= help "Output an XML report, with an optional filename for the report (default is \"report.xml\")"
      &= opt "report.xml",
    txtreport = []
      &= typFile
      &= help "Output a plain text report, with an optional filename for the report (default is \"report.txt\")"
      &= opt "report.txt",
    consmode = []
      &= explicit
      &= name "c"
      &= name "consolemode"
      &= help "Specify console output behavior.  MODE is one of: \"quiet\", \"terminal\", \"text\", \"verbose\" (Default is \"terminal\")"
      &= typ "MODE",
    filters = []
      &= args
      &= typ "FILTERS"
  } &= summary "HUnit-Plus Standard Test Runner, v1.0.0"
    &= program "runtests"
    &= noAtExpand
    &= details ["FILTERS specifies one or more test filters, which select " ++
                "which tests will be run.  If no filters are provided, all " ++
                "tests will be selected.  If multiple filters are " ++
                "specified, tests that match any of the filters will be " ++
                "selected.  The format for a filter is " ++
                "[SUITE::][PATH][@TAGS].  All components are optional.",
                "",
                "SUITE specifies the suite in which selected tests are " ++
                "found.  If no suite is specified, then the filter will be " ++
                "applied to all tests.",
                "",
                "PATH is a path of the form [NAME.]*NAME.  All tests whose " ++
                "paths start with the path will be selected.  If no path is " ++
                "specified, then all tests matching the rest of the filter " ++
                "will be selected",
                "",
                "TAGS is a comma separated list of tags.  All tests with " ++
                "any of the given tags will be selected.  If no tags are " ++
                "specified, then all tests matching the rest of the filter " ++
                "will be selected"
               ]

-- | Read and parse a single test list file
parseTestLists :: Opts
               -- ^ The command line options
               -> IO (Either [String] [Filter])
parseTestLists Opts { testlist = filenames, filters = cmdfilters } =
  let
    cmdresults = map (either (Left . (: [])) (Right . (: [])) .
                      parseFilter "command line") cmdfilters
  in do
    fileresults <- mapM parseFilterFile filenames
    case partitionEithers (fileresults ++ cmdresults) of
      ([], allfilters) -> return (Right (concat allfilters))
      (errs, _) -> return (Left (concat errs))

-- | Translate an @IOError@ into an error message
interpretException :: String
                   -- ^ Prefix to attach to error messages
                   -> IOError
                   -- ^ Exception to interpret
                   -> String
interpretException prefix e = prefix ++ show e

-- | Get the file for reporting XML data
withReportHandles :: Opts
                   -- ^ The command line options
                   -> (Maybe Handle -> Maybe Handle -> IO a)
                   -- ^ A monad parameterized by the xml report handle
                   -- and the text report handle.
                   -> IO (Either [String] a)
withReportHandles Opts { xmlreport = [], txtreport = [] } cmd =
  cmd Nothing Nothing >>= return . Right
withReportHandles Opts { xmlreport = [ xmlfile ], txtreport = [] } cmd =
  let
    runcmd xmlhandle =
      do
        res <- try (cmd (Just xmlhandle) Nothing)
        case res of
          Left e ->
            return (Left [interpretException "Error generating report file: " e])
          Right res' -> return (Right res')
  in do
    res <- try (withFile xmlfile WriteMode runcmd)
    case res of
      Left e ->
        return (Left [interpretException "Error opening XML report file: " e])
      Right res' -> return res'
withReportHandles Opts { xmlreport = [], txtreport = [ txtfile ] } cmd =
  let
    runcmd txthandle =
      do
        res <- try (cmd Nothing (Just txthandle))
        case res of
          Left e ->
            return (Left [interpretException "Error generating report file: " e])
          Right res' -> return (Right res')
  in do
    res <- try (withFile txtfile WriteMode runcmd)
    case res of
      Left e ->
        return (Left [interpretException "Error opening text report file: " e])
      Right res' -> return res'
withReportHandles Opts { xmlreport = [ xmlfile ], txtreport = [ txtfile ] } cmd =
  let
    runWithXML xmlhandle =
      let
        runcmd txthandle =
          do
            res <- try (cmd (Just xmlhandle) (Just txthandle))
            case res of
              Left e ->
                return (Left [interpretException
                                "Error generating report file: " e])
              Right res' -> return (Right res')
      in do
        res <- try (withFile txtfile WriteMode runcmd)
        case res of
          Left e ->
            return (Left [interpretException
                            "Error opening text report file: " e])
          Right res' -> return res'
  in do
    res <- try (withFile xmlfile WriteMode runWithXML)
    case res of
      Left e ->
        return (Left [interpretException "Error opening XML report file: " e])
      Right res' -> return res'
withReportHandles Opts { xmlreport = _ : _ : _, txtreport = _ : _ : _ } _ =
  return (Left ["Cannot specify multiple files for XML reports",
                "Cannot specify multiple files for text reports"])
withReportHandles Opts { xmlreport = _ : _ : _ } _ =
  return (Left ["Cannot specify multiple files for XML reports"])
withReportHandles Opts { txtreport = _ : _ : _ } _ =
  return (Left ["Cannot specify multiple files for text reports"])

-- | Create a standard test execution program from a set of test
-- suites.  The resulting @main@ will process command line options as
-- described, execute the appropirate tests, and exit with success if
-- all tests passed, and fail otherwise.
createMain :: [TestSuite] -> IO ()
createMain suites =
  do
    cmdopts <- cmdArgs opts
    res <- topLevel suites cmdopts
    case res of
      Left errs ->
        do
          mapM_ (putStr . (++ "\n")) errs
          exitFailure
      Right False -> exitFailure
      Right True -> exitSuccess

-- | Top-level function for executing test suites. 'createMain' is
-- simply a wrapper around this function.  This function allows users
-- to supply their own options, and to decide what to do with the
-- result of test execution.
topLevel :: [TestSuite] -> Opts -> IO (Either [String] Bool)
topLevel suites cmdopts @ Opts { consmode = cmodeopt } =
  let
    cmode = case cmodeopt of
      [] -> Right Terminal
      [ cmode' ] -> Right cmode'
      _ -> Left "Cannot specify multiple terminal output options"

    suitenames = map suiteName suites
  in do
    testlistres <- parseTestLists cmdopts
    case (testlistres, cmode) of
      (Left terrs, Left merrs) -> return (Left (merrs : terrs))
      (Left terrs, _) -> return (Left terrs)
      (_, Left merrs) -> return (Left [merrs])
      (Right testlists, Right mode) ->
        let
          normfilters = suiteSelectors suitenames testlists
        in
          withReportHandles cmdopts (executeTests suites normfilters mode)

executeTests :: [TestSuite]
             -- ^ The test suites to run
             -> Map String Selector
             -- ^ The filters to use
             -> ConsoleMode
             -- ^ The mode to use for console output
             -> Maybe Handle
             -- ^ The @Handle@ for XML reporting
             -> Maybe Handle
             -- ^ The @Handle@ for text reporting
             -> IO Bool
executeTests suites testlists cmode xmlhandle txthandle =
  let
    textTerminalReporter = textReporter (putTextToHandle stdout) False
    verboseTerminalReporter = textReporter (putTextToHandle stdout) True

    writeXML :: Handle -> [[Node String String]] -> IO ()
    writeXML outhandle [[tree]] = hPut outhandle (format tree)
    writeXML _ _ =
      error "Internal error in XML reporting: extra nodes on node stack"

    -- Unfortunately, the polymorphic typing of reporters mandates
    -- doing things this way...
    runTests :: ConsoleMode -> Maybe Handle -> Maybe Handle -> IO Counts
    runTests Quiet Nothing Nothing =
      let
        quietReporter = defaultReporter { reporterStart = return () }
      in do
        (out, ()) <- performTestSuites quietReporter testlists suites
        return out
    runTests Terminal Nothing Nothing =
      do
        (out, _) <- performTestSuites terminalReporter testlists suites
        return out
    runTests Text Nothing Nothing =
      do
        (out, _) <- performTestSuites textTerminalReporter testlists suites
        return out
    runTests Verbose Nothing Nothing =
      do
        (out, _) <- performTestSuites verboseTerminalReporter testlists suites
        return out
    runTests Quiet (Just xmlhandle') Nothing =
      do
        (out, tree) <- performTestSuites xmlReporter testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Terminal (Just xmlhandle') Nothing =
      let
        rep = combinedReporter xmlReporter terminalReporter
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Text (Just xmlhandle') Nothing =
      let
        rep = combinedReporter xmlReporter textTerminalReporter
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Verbose (Just xmlhandle') Nothing =
      let
        rep = combinedReporter xmlReporter verboseTerminalReporter
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Quiet Nothing (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
      in do
        (out, _) <- performTestSuites txtrep testlists suites
        return out
    runTests Terminal Nothing (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        rep = combinedReporter terminalReporter txtrep
      in do
        (out, _) <- performTestSuites rep testlists suites
        return out
    runTests Text Nothing (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        rep = combinedReporter textTerminalReporter txtrep
      in do
        (out, _) <- performTestSuites rep testlists suites
        return out
    runTests Verbose Nothing (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        rep = combinedReporter verboseTerminalReporter txtrep
      in do
        (out, _) <- performTestSuites rep testlists suites
        return out
    runTests Quiet (Just xmlhandle') (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        rep = combinedReporter xmlReporter txtrep
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Terminal (Just xmlhandle') (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        termrep = combinedReporter terminalReporter txtrep
        rep = combinedReporter xmlReporter termrep
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Text (Just xmlhandle') (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        termrep = combinedReporter textTerminalReporter txtrep
        rep = combinedReporter xmlReporter termrep
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
    runTests Verbose (Just xmlhandle') (Just texthandle') =
      let
        txtrep = textReporter (putTextToHandle texthandle') True
        termrep = combinedReporter verboseTerminalReporter txtrep
        rep = combinedReporter xmlReporter termrep
      in do
        (out, (tree, _)) <- performTestSuites rep testlists suites
        writeXML xmlhandle' tree
        return out
  in do
    res <- runTests cmode xmlhandle txthandle
    case res of
      Counts { cErrors = 0, cFailures = 0 } -> return True
      _ -> return False
