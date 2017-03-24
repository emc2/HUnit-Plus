{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Text-based reporting functionality for reporting either as text,
-- or to the terminal.  This module is an adaptation of code from the
-- original HUnit library.
--
-- Note that the test execution function in this module are included
-- for (a measure of) compatibility with HUnit, but are deprecated in
-- favor of the function in the "Test.HUnitPlus.Main" module.
module Test.HUnitPlus.Text(
       -- * Utilities
       PutText(..),
       putTextToHandle,
       putTextToShowS,
       showCounts,
       -- * Text Reporting
       textReporter,
       runTestText,
       runSuiteText,
       runSuitesText,
       -- * Terminal reporting
       terminalReporter,
       runTestTT,
       runSuiteTT,
       runSuitesTT
       ) where

import Distribution.TestSuite
import Test.HUnitPlus.Base
import Test.HUnitPlus.Execution
import Test.HUnitPlus.Filter
import Test.HUnitPlus.Reporting

import Control.Monad (when)
import System.IO (Handle, stderr)
import Text.Printf(printf)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict

-- | The text-based reporters ('textReporter' and 'terminalReporter')
-- construct strings and pass them to the function embodied in a
-- 'PutText'.  This function handles the string in one of several
-- ways.  Two schemes are defined here.  'putTextToHandle' writes
-- report lines to a given handle.  'putTextToShowS' accumulates lines
-- for return as a whole.
--
-- The 'PutText' function is also passed, and returns, an arbitrary state
-- value (called 'st' here).  The initial state value is given in the
-- 'PutText'; the final value is returned by 'runTestText'.

data PutText st = PutText (Strict.Text -> st -> IO st) st

-- | Writes persistent lines to the given handle.
putTextToHandle :: Handle -> PutText ()
putTextToHandle handle = PutText (\line () -> Strict.hPutStr handle line) ()

-- | Accumulates lines for return by 'runTestText'.  The
-- accumulated lines are represented by a @'ShowS' ('String' ->
-- 'String')@ function whose first argument is the string to be
-- appended to the accumulated report lines.
putTextToShowS :: PutText (Strict.Text -> Strict.Text)
putTextToShowS =
  PutText (\line func -> return (\rest -> func (Strict.concat [line, rest]))) id

-- | Create a 'Reporter' that outputs a textual report for
-- non-terminal output.
textReporter :: PutText us
             -- ^ The method for outputting text.
             -> Bool
             -- ^ Whether or not to output verbose text.
             -> Reporter us
textReporter (PutText put initUs) verbose =
  let
    reportProblem prefix msg ss us =
      let
        path = showQualName ss
        line = Strict.concat ["### ", prefix, path, ": ", msg, "\n"]
      in
        put line us

    reportOutput prefix msg ss us =
      let
        path = showQualName ss
        line = Strict.concat ["### ", prefix, path, ": ", msg, "\n"]
      in
        if verbose then put line us else return us

    reportStartSuite ss us =
      let
        line = Strict.concat ["Test suite ", stName ss, " starting\n"]
      in
        if verbose then put line us else return us

    reportEndSuite time ss us =
      let
        timestr = printf "%.6f" time
        line = Strict.concat ["Test suite", stName ss, " completed in ",
                              Strict.pack timestr, " sec\n"]
      in
        if verbose then put line us else return us

    reportStartCase ss us =
      let
        path = showQualName ss
        line = if Strict.null path then "Test case starting\n"
               else Strict.concat ["Test case ", path, " starting\n"]
      in
        if verbose then put line us else return us

    reportEndCase time ss us =
      let
        path = showQualName ss
        timestr = printf "%.6f" time
        line = if Strict.null path
                 then Strict.concat ["Test completed in ",
                                     Strict.pack timestr, " sec\n"]
                 else Strict.concat ["Test ", path, " completed in ",
                                     Strict.pack timestr, " sec\n"]
      in
        if verbose then put line us else return us

    reportEnd time counts us =
      let
        countstr = Strict.concat [showCounts counts, "\n"]
        timestr = printf "%.6f" time
        timeline = Strict.concat ["Tests completed in ",
                                  Strict.pack timestr, " sec\n"]
      in do
        if verbose
          then do
            us' <- put timeline us
            put countstr us'
          else put countstr us
  in
    defaultReporter {
      reporterStart = return initUs,
      reporterEnd = reportEnd,
      reporterStartSuite = reportStartSuite,
      reporterEndSuite = reportEndSuite,
      reporterStartCase = reportStartCase,
      reporterEndCase = reportEndCase,
      reporterSystemOut = reportOutput "STDOUT from ",
      reporterSystemErr = reportOutput "STDERR from ",
      reporterError = reportProblem "Error in ",
      reporterFailure = reportProblem "Failure in "
    }

-- | Execute a test, processing text output according to the given
-- reporting scheme.  The reporting scheme's state is threaded through
-- calls to the reporting scheme's function and finally returned,
-- along with final count values.  The text is output in non-terminal
-- mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runTestText :: PutText us
            -- ^ A function which accumulates output.
            -> Bool
            -- ^ Whether or not to run the test in verbose mode.
            -> Test
            -- ^ The test to run
            -> IO (Counts, us)
runTestText puttext @ (PutText put us0) verbose t =
  let
    initState = State { stCounts = zeroCounts, stName = "",
                        stPath = [], stOptions = HashMap.empty,
                        stOptionDescs = [] }

    reporter = textReporter puttext verbose
  in do
    (ss1, us1) <- ((performTest $! reporter) allSelector $! initState) us0 t
    us2 <- put (Strict.concat [showCounts (stCounts ss1), "\n"]) us1
    return (stCounts ss1, us2)

-- | Execute a test suite, processing text output according to the
-- given reporting scheme.  The reporting scheme's state is threaded
-- through calls to the reporting scheme's function and finally
-- returned, along with final count values.  The text is output in
-- non-terminal mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runSuiteText :: PutText us
             -- ^ A function which accumulates output.
             -> Bool
             -- ^ Whether or not to run the tests in verbose mode.
             -> TestSuite
             -- ^ The test suite to run.
             -> IO (Counts, us)
runSuiteText puttext @ (PutText put us0) verbose
             suite @ TestSuite { suiteName = sname } =
  let
    selectorMap = HashMap.singleton sname allSelector
    reporter = textReporter puttext verbose
  in do
    (counts, us1) <- ((performTestSuite $! reporter) $!selectorMap) us0 suite
    us2 <- put (Strict.concat [showCounts counts, "\n"]) us1
    return (counts, us2)

-- | Execute the given test suites, processing text output according
-- to the given reporting scheme.  The reporting scheme's state is
-- threaded through calls to the reporting scheme's function and
-- finally returned, along with final count values.  The text is
-- output in non-terminal mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runSuitesText :: PutText us
              -- ^ A function which accumulates output
              -> Bool
              -- ^ Whether or not to run the test in verbose mode.
              -> [TestSuite]
              -- ^ The test to run
              -> IO (Counts, us)
runSuitesText puttext @ (PutText put _) verbose suites =
  let
    suiteNames = map suiteName suites
    selectorMap = foldl (\suitemap sname ->
                          HashMap.insert sname allSelector suitemap)
                        HashMap.empty suiteNames
    reporter = textReporter puttext verbose
  in do
    (counts, us1) <- ((performTestSuites $! reporter) $! selectorMap) suites
    us2 <- put (Strict.concat [showCounts counts, "\n"]) us1
    return (counts, us2)

-- | Converts test execution counts to a string.
showCounts :: Counts -> Strict.Text
showCounts Counts { cCases = cases, cTried = tried,
                    cErrors = errors, cFailures = failures,
                    cAsserts = asserts, cSkipped = skipped } =
  Strict.concat ["Cases: ", Strict.pack (show cases), "  Tried: ",
                 Strict.pack (show tried), "  Errors: ",
                 Strict.pack (show errors), "  Failures: ",
                 Strict.pack (show failures), "  Assertions: ",
                 Strict.pack (show asserts), "  Skipped: ",
                 Strict.pack (show skipped)]

-- | Terminal output function, used by the run*TT function and
-- terminal reporters.
termPut :: Strict.Text -> Bool -> Int -> IO Int
termPut line pers (-1) =
  do
    when pers (Strict.hPutStrLn stderr line)
    return (-1)
termPut line True cnt =
  do
    Strict.hPutStrLn stderr (Strict.concat [erase cnt, line])
    return 0
termPut line False _ =
  do
    Strict.hPutStr stderr (Strict.concat ["\r", line])
    return (Strict.length line)

-- The "erasing" strategy with a single '\r' relies on the fact that the
-- lengths of successive summary lines are monotonically nondecreasing.
erase :: Int -> Strict.Text
erase cnt =
  if cnt == 0 then "" else Strict.concat ["\r", Strict.replicate cnt " ", "\r"]

-- | A reporter that outputs lines indicating progress to the
-- terminal.  Reporting is made to standard error, and progress
-- reports are included.
terminalReporter :: Reporter Int
terminalReporter =
  let
    reportProblem prefix msg ss us =
      let
        line = Strict.concat ["### ", prefix, path, "\n", msg]
        path = showQualName ss
      in
        termPut line True us
  in
    defaultReporter {
      reporterStart = return 0,
      reporterEnd = \_ _ _ -> do Strict.hPutStr stderr "\n"; return 0,
      reporterEndCase = \_ ss us -> termPut (showCounts (stCounts ss)) False us,
      reporterError = reportProblem "Error in:   ",
      reporterFailure = reportProblem "Failure in: "
    }

-- | Execute a test, processing text output according to the given
-- reporting scheme.  The reporting scheme's state is threaded through
-- calls to the reporting scheme's function and finally returned,
-- along with final count values.  The text is output in terminal
-- mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runTestTT :: Test -> IO Counts
runTestTT t =
  let
    initState = State { stCounts = zeroCounts, stName = "",
                        stPath = [], stOptions = HashMap.empty,
                        stOptionDescs = [] }
  in do
    (ss1, us1) <- (performTest terminalReporter allSelector $! initState) 0 t
    0 <- termPut (showCounts (stCounts ss1)) True us1
    return (stCounts ss1)

-- | Execute a test suite, processing text output according to the
-- given reporting scheme.  The reporting scheme's state is threaded
-- through calls to the reporting scheme's function and finally
-- returned, along with final count values.  The text is output in
-- terminal mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runSuiteTT :: TestSuite -> IO Counts
runSuiteTT suite @ TestSuite { suiteName = sname } =
  let
    selectorMap = HashMap.singleton sname allSelector
  in do
    (counts, us) <- (performTestSuite terminalReporter $! selectorMap) 0 suite
    0 <- termPut (Strict.concat [showCounts counts, "\n"]) True us
    return counts

-- | Execute the given test suites, processing text output according
-- to the given reporting scheme.  The reporting scheme's state is
-- threaded through calls to the reporting scheme's function and
-- finally returned, along with final count values.  The text is
-- output in terminal mode.
--
-- This function is deprecated.  The preferred way to run tests is to
-- use the functions in "Test.HUnitPlus.Main".
runSuitesTT :: [TestSuite] -> IO Counts
runSuitesTT suites =
  let
    suiteNames = map suiteName suites
    selectorMap = foldl (\suitemap sname ->
                          HashMap.insert sname allSelector suitemap)
                        HashMap.empty suiteNames
  in do
    (counts, us) <- (performTestSuites terminalReporter $! selectorMap) suites
    0 <- termPut (Strict.concat [showCounts counts, "\n"]) True us
    return counts
