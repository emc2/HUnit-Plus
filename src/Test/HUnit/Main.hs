{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A module containing a mostly-complete program for executing
-- tests.  The only thing missing are the actual test suites, which
-- are provided as parameters to the [@main@].  This module is
-- indended to be used to create a top-level [@Main@] module.
--
-- The [@main@] herein obtains and parses the command-line options,
-- then executes tests accordingly.
module Test.HUnit.Main(
       createMain
       ) where

import Control.Exception
import Data.ByteString.Lazy(hPut)
import Data.Either
import Data.Map(Map)
import System.Console.CmdArgs hiding (Quiet)
import System.Exit
import System.IO
import Test.HUnit.Base hiding (name)
import Test.HUnit.Execution
import Test.HUnit.Filter
import Test.HUnit.Reporting hiding (Node)
import Test.HUnit.Text
import Test.HUnit.XML
import Text.XML.Expat.Format
import Text.XML.Expat.Tree(Node)

data ConsoleMode = Quiet | Terminal | Text | Verbose
  deriving (Typeable, Data, Show)

data Opts =
  Opts {
    xmlreport :: ![String],
    filters :: ![String],
    txtreport :: ![String],
    consmode :: ![ConsoleMode],
    testlist :: ![String]
  }
  deriving (Typeable, Show, Data)

opts :: Opts
opts =
  Opts {
    testlist = []
      &= explicit
      &= name "l"
      &= name "testlist"
      &= help "Read test filters from FILE"
      &= typFile,
    xmlreport = []
      &= help "Output an XML report, with an optional filename for the report (default is \"report.xml\")"
      &= opt "report.xml"
      &= typFile,
    txtreport = []
      &= help "Output a plain text report, with an optional filename for the report (default is \"report.txt\")"
      &= opt "report.txt"
      &= typFile,
    consmode = []
      &= explicit
      &= name "c"
      &= name "consolemode"
      &= help "Specify console output behavior.  MODE is one of: \"quiet\", \"terminal\", \"text\", \"verbose\" (Default is \"terminal\")"
      &= typ "MODE",
    filters = []
      &= args
      &= typ "FILTERS"
  } &= summary "HUnit-Plus Standard Test Runner"
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

-- | Create a standard test execution program from a set of test suites.
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
      do
       (out, _) <- performTestSuites defaultReporter testlists suites
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
