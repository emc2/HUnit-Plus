{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module Test.HUnit.Execution(
       performTestCase,
       performTest,
       performTestSuite,
       performTestSuites
       ) where

import Control.Monad (unless, foldM)
import Distribution.TestSuite
import System.TimeIt
import Test.HUnit.Base
import Test.HUnit.Reporting

performTestCase :: Reporter us
                -- ^ Report generator for the test run
                -> State
                -- ^ HUnit internal state
                -> us
                -- ^ State for the report generator
                -> TestInstance
                -- ^ The test to be executed
                -> IO (State, us)
performTestCase Reporter { reporterStartCase = reportStartCase,
                           reporterCaseProgress = reportCaseProgress,
                           reporterEndCase = reportEndCase,
                           reporterError = reportError,
                           reporterFailure = reportFailure }
                ss @ State { stCounts = c @ Counts { tried = n },
                             stName = oldname } us
                TestInstance { run = runTest, name = testName } =
  let
    -- Add the name to the state we use to run the tests
    ssWithName = ss { stName = testName }

    finishTestCase us' action =
      do
        progress <- action
        case progress of
          Progress msg nextAction ->
            do
              usNext <- reportCaseProgress msg ss us'
              finishTestCase usNext nextAction
          Finished res -> return (res, us')
  in do
    -- Call the reporter's start case function
    usStarted <- reportStartCase ssWithName us
    -- Actually run the test
    (time, (r, usFinished)) <- timeItT (finishTestCase usStarted runTest)
    -- Eventually, will need to report stdout and stderr activity
    case r of
      -- If the test succeeded, just report end of case
      Pass ->
        let
          -- Counts to use in event of success
          ssSuccess = ssWithName { stCounts = c { tried = n + 1 } }
        in do
          usEnded <- reportEndCase time ssSuccess usFinished
          return (ssSuccess { stName = oldname }, usEnded)
      -- If there was a failure, report it, then report end of case
      Fail msg ->
        let
          -- Counts to use in event of a failure
          ssFail = ssWithName { stCounts = c { tried = n + 1,
                                               failures = failures c + 1 } }
        in do
          usFail <- reportFailure msg ssFail usFinished
          usEnded <- reportEndCase time ssFail usFail
          return (ssFail { stName = oldname }, usEnded)
      -- If there was an error, report it, then report end of case
      Error msg ->
        let
          -- Counts to use in event of an error
          ssError = ssWithName { stCounts = c { tried = n + 1,
                                                errors = errors c + 1 } }
        in do
          usError <- reportError msg ssError usFinished
          usEnded <- reportEndCase time ssError usError
          return (ssError { stName = oldname }, usEnded)

-- | Performs a test run with the specified report generators.
--
-- This handles the actual running of the tests.  Most developers will want
-- to use @HUnit.Text.runTestTT@ instead.  A developer could use this function
-- to execute tests via another IO system, such as a GUI, or to output the
-- results in a different manner (e.g., upload XML-formatted results to a
-- webservice).
--
-- Note that the counts in a start report do not include the test case
-- being started, whereas the counts in a problem report do include the
-- test case just finished.  The principle is that the counts are sampled
-- only between test case executions.  As a result, the number of test
-- case successes always equals the difference of test cases tried and
-- the sum of test case errors and failures.
performTest :: Reporter us
            -- ^ Report generator for the test run
            -> State
            -- ^ Initial counts for tests
            -> us
            -- ^ State for the report generator
            -> Test
            -- ^ The test to be executed
            -> IO (State, us)
performTest rep initState initialUs initialTest =
  let
    -- The recursive worker function that actually runs all the tests
    performTest' ss us Group { groupName = gname, groupTests = testlist } =
      let
        -- Update the path for running the group's tests
        oldpath = stPath ss
        ssWithPath = ss { stPath = (Label gname) : oldpath }

        foldfun (ss', us') t = performTest' ss' us' t
      in do
        -- Run the tests with the updated path
        (ssAfter, usAfter) <- foldM foldfun (ssWithPath, us) testlist
        -- Return the state, reset to the old path
        return (ssAfter { stPath = oldpath }, usAfter)
    performTest' ss us (Test testinstance) =
      -- For an individual test, just run the test case
      performTestCase rep ss us testinstance
    performTest' ss us (ExtraOptions _ inner) =
      -- For now, options aren't being handled.  This will need to do
      -- more when they are.
      performTest' ss us inner
  in do
    (ss', us') <- performTest' initState initialUs initialTest
    unless (null (stPath ss')) $ error "performTest: Final path is nonnull"
    return (ss', us')

performTestSuite :: Reporter us
                 -- ^ Report generator to use for running the test suite
                 -> us
                 -- ^ State for the report generator
                 -> TestSuite
                 -- ^ Test suite to be run
                 -> IO (Counts, us)
performTestSuite rep @ Reporter { reporterStartSuite = reportStartSuite,
                                  reporterEndSuite = reportEndSuite }
                 initialUs
                 TestSuite { suiteName = sname, suiteTests = testlist,
                             suiteOptions = suiteOpts } =
  let
    -- XXX fold in calculation of numbers of test cases into the
    -- traversal of the test cases themselves
    initCounts = Counts { cases = fromIntegral (sum (map testCaseCount testlist)),
                          tried = 0, errors = 0, failures = 0 }
    initState = State { stCounts = initCounts, stName = sname,
                        stPath = [], stOptions = suiteOpts }

    foldfun (c, us) test = performTest rep c us test
  in do
    startedUs <- reportStartSuite initState initialUs
    (time, (finishedState, finishedUs)) <-
      timeItT (foldM foldfun (initState, startedUs) testlist)
    endedUs <- reportEndSuite time finishedState finishedUs
    return (stCounts finishedState, endedUs)

performTestSuites :: Reporter us
                  -- ^ Report generator to use for running the test suite
                  -> [TestSuite]
                  -- ^ Test suite to be run
                  -> IO (Counts, us)
performTestSuites rep @ Reporter { reporterStart = reportStart,
                                   reporterEnd = reportEnd } suites =
  let
    initialCounts = Counts { cases = 0, tried = 0, errors = 0, failures = 0 }

    combineCounts Counts { cases = cases1, tried = tried1,
                           errors = errors1, failures = failures1 }
                  Counts { cases = cases2, tried = tried2,
                           errors = errors2, failures = failures2 } =
      Counts { cases = cases1 + cases2, tried = tried1 + tried2,
               errors = errors1 + errors2, failures = failures1 + failures2 }

    foldfun (accumCounts, accumUs) suite =
      do
        (suiteCounts, suiteUs) <- performTestSuite rep accumUs suite
        return (combineCounts accumCounts suiteCounts, suiteUs)
  in do
    initialUs <- reportStart
    (time, (finishedCounts, finishedUs)) <-
      timeItT (foldM foldfun (initialCounts, initialUs) suites)
    endedUs <- reportEnd time finishedCounts finishedUs
    return (finishedCounts, endedUs)
