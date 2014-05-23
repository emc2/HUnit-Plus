{-# OPTIONS_GHC -Wall -Werror #-}

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
import System.IO (Handle, stderr, hPutStr, hPutStrLn)
import Text.Printf(printf)

import qualified Data.Map as Map


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

data PutText st = PutText (String -> st -> IO st) st

-- | Writes persistent lines to the given handle.
putTextToHandle :: Handle -> PutText ()
putTextToHandle handle = PutText (\line () -> hPutStr handle line) ()

-- | Accumulates lines for return by 'runTestText'.  The
-- accumulated lines are represented by a @'ShowS' ('String' ->
-- 'String')@ function whose first argument is the string to be
-- appended to the accumulated report lines.
putTextToShowS :: PutText ShowS
putTextToShowS =
  PutText (\line func -> return (\rest -> func (line ++ rest))) id

-- | Create a 'Reporter' that outputs a textual report for
-- non-terminal output.
textReporter :: PutText us
             -- ^ The method for outputting text.
             -> Bool
             -- ^ Whether or not to output verbose text.
             -> Reporter us
textReporter (PutText put initUs) verbose =
  let
    reportProblem p0 p1 msg ss us =
      let
        kind = if null path then p0 else p1
        path = showPath (stPath ss)
        line = "### " ++ kind ++ path ++ ": " ++ msg ++ "\n"
      in
        put line us

    reportOutput p0 p1 msg ss us =
      let
        kind = if null path then p0 else p1
        path = showPath (stPath ss)
        line = "### " ++ kind ++ path ++ ": " ++ msg ++ "\n"
      in
        if verbose then put line us else return us

    reportStartSuite ss us =
      let
        line = "Test suite " ++ stName ss ++ " starting\n"
      in
        if verbose then put line us else return us

    reportEndSuite time ss us =
      let
        timestr = printf "%.6f" time
        line = "Test suite" ++ stName ss ++ " completed in " ++
               timestr ++ " sec\n"
      in
        if verbose then put line us else return us

    reportStartCase ss us =
      let
        path = showPath (stPath ss)
        line = if null path then "Test case starting\n"
               else "Test case " ++ path ++ " starting\n"
      in
        if verbose then put line us else return us

    reportEndCase time ss us =
      let
        path = showPath (stPath ss)
        timestr = printf "%.6f" time
        line = if null path then "Test completed in " ++ timestr ++ " sec\n"
               else "Test " ++ path ++ " completed in " ++ timestr ++ " sec\n"
      in
        if verbose then put line us else return us

    reportEnd time counts us =
      let
        countstr = showCounts counts ++ "\n"
        timestr = printf "%.6f" time
        timeline = "Tests completed in " ++ timestr ++ " sec\n"
      in do
        if verbose
          then do
            us' <- put timeline us
            put countstr us'
          else
            put countstr us
  in
    defaultReporter {
      reporterStart = return initUs,
      reporterEnd = reportEnd,
      reporterStartSuite = reportStartSuite,
      reporterEndSuite = reportEndSuite,
      reporterStartCase = reportStartCase,
      reporterEndCase = reportEndCase,
      reporterSystemOut = reportOutput "STDOUT " "STDOUT from ",
      reporterSystemErr = reportOutput "STDERR" "STDERR from ",
      reporterError = reportProblem "Error " "Error in ",
      reporterFailure = reportProblem "Failure" "Failure in "
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
                        stPath = [], stOptions = Map.empty,
                        stOptionDescs = [] }

    reporter = textReporter puttext verbose
  in do
    (ss1, us1) <- ((performTest $! reporter) allSelector $! initState) us0 t
    us2 <- put (showCounts (stCounts ss1) ++ "\n") us1
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
    selectorMap = Map.singleton sname allSelector
    reporter = textReporter puttext verbose
  in do
    (counts, us1) <- ((performTestSuite $! reporter) $!selectorMap) us0 suite
    us2 <- put (showCounts counts ++ "\n") us1
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
                          Map.insert sname allSelector suitemap)
                        Map.empty suiteNames
    reporter = textReporter puttext verbose
  in do
    (counts, us1) <- ((performTestSuites $! reporter) $! selectorMap) suites
    us2 <- put (showCounts counts ++ "\n") us1
    return (counts, us2)

-- | Converts test execution counts to a string.
showCounts :: Counts -> String
showCounts Counts { cCases = cases, cTried = tried,
                    cErrors = errors, cFailures = failures,
                    cAsserts = asserts, cSkipped = skipped } =
  "Cases: " ++ show cases ++ "  Tried: " ++ show tried ++
  "  Errors: " ++ show errors ++ "  Failures: " ++ show failures ++
  "  Assertions: " ++ show asserts ++ "  Skipped: " ++ show skipped

-- | Terminal output function, used by the run*TT function and
-- terminal reporters.
termPut :: String -> Bool -> Int -> IO Int
termPut line pers (-1) = do when pers (hPutStrLn stderr line); return (-1)
termPut line True  cnt = do hPutStrLn stderr (erase cnt ++ line); return 0
termPut line False _   = do hPutStr stderr ('\r' : line); return (length line)

-- The "erasing" strategy with a single '\r' relies on the fact that the
-- lengths of successive summary lines are monotonically nondecreasing.
erase :: Int -> String
erase cnt = if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"

-- | A reporter that outputs lines indicating progress to the
-- terminal.  Reporting is made to standard error, and progress
-- reports are included.
terminalReporter :: Reporter Int
terminalReporter =
  let
    reportProblem p0 p1 msg ss us =
      let
        line = "### " ++ kind ++ path ++ '\n' : msg
        path = showPath (stPath ss)
        kind = if null path then p0 else p1
      in
        termPut line True us
  in
    defaultReporter {
      reporterStart = return 0,
      reporterEnd = (\_ _ _ -> do hPutStr stderr "\n"; return 0),
      reporterEndCase =
        (\_ ss us -> termPut (showCounts (stCounts ss)) False us),
      reporterError = reportProblem "Error:" "Error in:   ",
      reporterFailure = reportProblem "Failure:" "Failure in: "
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
                        stPath = [], stOptions = Map.empty,
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
    selectorMap = Map.singleton sname allSelector
  in do
    (counts, us) <- (performTestSuite terminalReporter $! selectorMap) 0 suite
    0 <- termPut (showCounts counts ++ "\n") True us
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
                          Map.insert sname allSelector suitemap)
                        Map.empty suiteNames
  in do
    (counts, us) <- (performTestSuites terminalReporter $! selectorMap) suites
    0 <- termPut (showCounts counts ++ "\n") True us
    return counts
