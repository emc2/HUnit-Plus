{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Reporting functionality for HUnit-Plus.  Test reporting is now
-- defined using a set of events.  A 'Reporter' contains handlers for
-- these events, which have access to and update a 'Reporter'-defined
-- state value.  The handlers in a 'Reporter' are called at
-- appropriate points during text execution.
--
-- This module also contains a basic 'defaultReporter' that simply
-- passes the state value through unchanged.  It also defines
-- 'combinedReporter', which facilitates \"gluing\" two 'Reporter's
-- together.
module Test.HUnitPlus.Reporting(
       Node(..),
       State(..),
       Counts(..),
       Reporter(..),
       Path,
       zeroCounts,
       showPath,
       showQualName,
       defaultReporter,
       combinedReporter
       ) where

import Data.List
import Data.Map(Map)
import Distribution.TestSuite

-- | A record that holds the results of tests that have been performed
-- up until this point.
data Counts =
  Counts {
    -- | Number of total cases.
    cCases :: !Word,
    -- | Number of cases tried.
    cTried :: !Word,
    -- | Number of cases that failed with an error.
    cErrors :: !Word,
    -- | Number of cases that failed.
    cFailures :: !Word,
    -- | Number of cases that were skipped.
    cSkipped :: !Word,
    -- | Total number of assertions checked.
    cAsserts :: !Word,
    -- | Number of assertions checked by the last test case.
    cCaseAsserts :: !Word
  }
  deriving (Eq, Show, Read)

-- | Keeps track of the remaining tests and the results of the performed tests.
-- As each test is performed, the path is removed and the counts are
-- updated as appropriate.
data State =
  State {
    -- | The name of the case or suite currently being run.
    stName :: !String,
    -- | The path to the test case currently being run.
    stPath :: !Path,
    -- | The current test statistics.
    stCounts :: !Counts,
    -- | The current option values.
    stOptions :: !(Map String String),
    -- | The current option descriptions we know about.
    stOptionDescs :: ![OptionDescr]
  }
  deriving (Eq, Show, Read)

-- | Uniquely describes the location of a test within a test hierarchy.
-- Node order is from test case to root.
type Path = [Node]

-- | Composed into 'Path's.
data Node = Label String
  deriving (Eq, Show, Read)

-- | Report generator.  This record type contains a number of
-- functions that are called at various points throughout a test run.
data Reporter us = Reporter {
    -- | Called at the beginning of a test run.
    reporterStart :: IO us,
    -- | Called at the end of a test run.
    reporterEnd :: Double
                --  The total time it took to run the tests
                -> Counts
                --  The counts from running the tests
                -> us
                --  The user state for this test reporter
                -> IO us,
    -- | Called at the start of a test suite run.
    reporterStartSuite :: State
                       --  Options given to the test suite
                       -> us
                       --  The user state for this test reporter
                       -> IO us,
    -- | Called at the end of a test suite run.
    reporterEndSuite :: Double
                     --  The total time it tgook to run the test suite
                     -> State
                     --  The counts from running the tests
                     -> us
                     --  The user state for this test reporter
                     -> IO us,
    -- | Called at the start of a test case run.
    reporterStartCase :: State
                      --  The HUnit internal state
                      -> us
                      --  The user state for this test reporter
                      -> IO us,
    -- | Called to report progress of a test case run.
    reporterCaseProgress :: String
                         --  A progress message
                         -> State
                         --  The HUnit internal state
                         -> us
                         --  The user state for this test reporter
                         -> IO us,
    -- | Called at the end of a test case run.
    reporterEndCase :: Double
                    --  The total time it took to run the test suite
                    -> State
                    --  The HUnit internal state
                    -> us
                    --  The user state for this test reporter
                    -> IO us,
    -- | Called when skipping a test case.
    reporterSkipCase :: State
                     --  The HUnit internal state
                     -> us
                     --  The user state for this test reporter
                     -> IO us,
    -- | Called to report output printed to the system output stream.
    reporterSystemOut :: String
                      --  The content printed to system out
                      -> State
                      --  The HUnit internal state
                      -> us
                      --  The user state for this test reporter
                      -> IO us,
    -- | Called to report output printed to the system error stream.
    reporterSystemErr :: String
                      --  The content printed to system out
                      -> State
                      --  The HUnit internal state
                      -> us
                      --  The user state for this test reporter
                      -> IO us,
    -- | Called when a test fails.
    reporterFailure :: String
                    --  A message relating to the error
                    -> State
                    --  The HUnit internal state
                    -> us
                    --  The user state for this test reporter
                    -> IO us,
    -- | Called when a test reports an error.
    reporterError :: String
                  --  A message relating to the error
                  -> State
                  --  The HUnit internal state
                  -> us
                  --  The user state for this test reporter
                  -> IO us
  }

-- | A 'Counts' with all zero counts.
zeroCounts :: Counts
zeroCounts = Counts { cCases = 0, cTried = 0, cErrors = 0, cCaseAsserts = 0,
                      cFailures = 0, cAsserts = 0, cSkipped = 0 }

-- | A reporter containing default actions, which are to do nothing
-- and return the user state unmodified.
defaultReporter :: Reporter a
defaultReporter = Reporter {
    reporterStart = fail "Must define a reporterStart value",
    reporterEnd = \_ _ us -> return us,
    reporterStartSuite = \_ us -> return us,
    reporterEndSuite = \_ _ us -> return us,
    reporterStartCase = \_ us -> return us,
    reporterCaseProgress = \_ _ us -> return us,
    reporterEndCase = \_ _ us -> return us,
    reporterSkipCase = \_ us -> return us,
    reporterSystemOut = \_ _ us -> return us,
    reporterSystemErr = \_ _ us -> return us,
    reporterFailure = \_ _ us -> return us,
    reporterError = \_ _ us -> return us
  }

-- | Converts a test case path to a string, separating adjacent elements by
--   a dot (\'.\'). An element of the path is quoted (as with 'show') when
--   there is potential ambiguity.
showPath :: Path -> String
showPath [] = ""
showPath nodes =
  let
    showNode (Label label) = safe label (show label)
    safe s ss = if '.' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s
  in
    intercalate "." (reverse (map showNode nodes))

-- | Gewerate a string showing the entire qualified name from the
-- reporting state.
showQualName :: State -> String
showQualName st =
  case stPath st of
    [] -> stName st
    nodes ->
      let
        showNode (Label label) = safe label (show label)
        safe s ss = if '.' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s
      in
        intercalate "." (reverse (map showNode nodes)) ++ "." ++ stName st


-- | Combines two 'Reporter's into a single reporter that calls both.
combinedReporter :: Reporter us1 -> Reporter us2 -> Reporter (us1, us2)
combinedReporter Reporter { reporterStart = reportStart1,
                            reporterEnd = reportEnd1,
                            reporterStartSuite = reportStartSuite1,
                            reporterEndSuite = reportEndSuite1,
                            reporterStartCase = reportStartCase1,
                            reporterCaseProgress = reportCaseProgress1,
                            reporterEndCase = reportEndCase1,
                            reporterSkipCase = reportSkipCase1,
                            reporterSystemOut = reportSystemOut1,
                            reporterSystemErr = reportSystemErr1,
                            reporterFailure = reportFailure1,
                            reporterError = reportError1
                          }
                 Reporter { reporterStart = reportStart2,
                            reporterEnd = reportEnd2,
                            reporterStartSuite = reportStartSuite2,
                            reporterEndSuite = reportEndSuite2,
                            reporterStartCase = reportStartCase2,
                            reporterCaseProgress = reportCaseProgress2,
                            reporterEndCase = reportEndCase2,
                            reporterSkipCase = reportSkipCase2,
                            reporterSystemOut = reportSystemOut2,
                            reporterSystemErr = reportSystemErr2,
                            reporterFailure = reportFailure2,
                            reporterError = reportError2
                          } =
  let
    reportStart =
      do
        us1 <- reportStart1
        us2 <- reportStart2
        return $! (us1, us2)

    reportEnd time counts (us1, us2) =
      do
        us1' <- reportEnd1 time counts us1
        us2' <- reportEnd2 time counts us2
        return $! (us1', us2')

    reportStartSuite ss (us1, us2) =
      do
        us1' <- reportStartSuite1 ss us1
        us2' <- reportStartSuite2 ss us2
        return $! (us1', us2')

    reportEndSuite time ss (us1, us2) =
      do
        us1' <- reportEndSuite1 time ss us1
        us2' <- reportEndSuite2 time ss us2
        return $! (us1', us2')

    reportStartCase ss (us1, us2) =
      do
        us1' <- reportStartCase1 ss us1
        us2' <- reportStartCase2 ss us2
        return $! (us1', us2')

    reportCaseProgress msg ss (us1, us2) =
      do
        us1' <- reportCaseProgress1 msg ss us1
        us2' <- reportCaseProgress2 msg ss us2
        return $! (us1', us2')

    reportEndCase time ss (us1, us2) =
      do
        us1' <- reportEndCase1 time ss us1
        us2' <- reportEndCase2 time ss us2
        return $! (us1', us2')

    reportSkipCase ss (us1, us2) =
      do
        us1' <- reportSkipCase1 ss us1
        us2' <- reportSkipCase2 ss us2
        return $! (us1', us2')

    reportSystemOut msg ss (us1, us2) =
      do
        us1' <- reportSystemOut1 msg ss us1
        us2' <- reportSystemOut2 msg ss us2
        return $! (us1', us2')

    reportSystemErr msg ss (us1, us2) =
      do
        us1' <- reportSystemErr1 msg ss us1
        us2' <- reportSystemErr2 msg ss us2
        return $! (us1', us2')

    reportFailure msg ss (us1, us2) =
      do
        us1' <- reportFailure1 msg ss us1
        us2' <- reportFailure2 msg ss us2
        return $! (us1', us2')

    reportError msg ss (us1, us2) =
      do
        us1' <- reportError1 msg ss us1
        us2' <- reportError2 msg ss us2
        return $! (us1', us2')
  in
    Reporter {
      reporterStart = reportStart,
      reporterEnd = reportEnd,
      reporterStartSuite = reportStartSuite,
      reporterEndSuite = reportEndSuite,
      reporterStartCase = reportStartCase,
      reporterCaseProgress = reportCaseProgress,
      reporterEndCase = reportEndCase,
      reporterSkipCase = reportSkipCase,
      reporterSystemOut = reportSystemOut,
      reporterSystemErr = reportSystemErr,
      reporterFailure = reportFailure,
      reporterError = reportError
    }
