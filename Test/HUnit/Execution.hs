{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

module Test.HUnit.Execution(
       performTestCase,
       performTest,
       performTestSuite,
       performTestSuites
       ) where

import Control.Monad (unless, foldM)
import Control.Applicative
import Distribution.TestSuite
import Data.Map(Map)
import Data.Set(Set)
import Prelude hiding (elem)
import System.TimeIt
import Test.HUnit.Base
import Test.HUnit.Filter
import Test.HUnit.Reporting

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Perform an individual test case.
performTestCase :: Reporter us
                -- ^ Report generator for the test run
                -> State
                -- ^ HUnit internal state
                -> us
                -- ^ State for the report generator
                -> TestInstance
                -- ^ The test to be executed
                -> IO (State, us)
performTestCase rep @ Reporter { reporterStartCase = reportStartCase,
                                 reporterCaseProgress = reportCaseProgress,
                                 reporterEndCase = reportEndCase }
                ss @ State { stCounts = c @ Counts { cTried = tried },
                             stName = oldname } us
                TestInstance { run = runTest, name = testname } =
  let
    -- Add the name to the state we use to run the tests
    ssWithName = ss { stName = testname, stCounts = c { cTried = tried + 1 } }

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
    (time, (res, usFinished)) <- timeItT (finishTestCase usStarted runTest)
    -- Report the results
    (ssFinal, usFinal) <- reportTestInfo res rep ssWithName usFinished 
    -- Eventually, will need to report stdout and stderr activity
    usEnded <- reportEndCase time ssFinal usFinal
    return (ssFinal { stName = oldname }, usEnded)

skipTestCase :: Reporter us
             -- ^ Report generator for the test run
             -> State
             -- ^ HUnit internal state
             -> us
             -- ^ State for the report generator
             -> TestInstance
             -- ^ The test to be executed
             -> IO (State, us)
skipTestCase Reporter { reporterSkipCase = reportSkipCase }
             ss @ State { stCounts = c @ Counts { cSkipped = skipped,
                                                  cCases = cases },
                          stName = oldname } us
             TestInstance { name = testname } =
  let
    ss' = ss { stCounts = c { cSkipped = skipped + 1, cCases = cases + 1 },
               stName = testname }
  in do
    us' <- reportSkipCase ss' us
    return (ss' { stName = oldname }, us')

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
            -> Selector
            -- ^ The selector to apply to all tests in the suite
            -> State
            -- ^ Initial counts for tests
            -> us
            -- ^ State for the report generator
            -> Test
            -- ^ The test to be executed
            -> IO (State, us)
performTest rep initSelector initState initialUs initialTest =
  let
    -- The recursive worker function that actually runs all the tests
    --
    -- We need to actually run through all the tests, so we can record
    -- the ones we skip.  So we keep a Maybe Selector, where Nothing
    -- represents tests that aren't actually going to be executed.
    --
    -- We also have to keep a set of tags by which we're filtering.
    -- The empty tag set means we don't actually filter at all.
    performTest' selector ss us Group { groupTests = testlist,
                                        groupName = gname } =
      let
        selector' = updateSelector gname selector
        -- Update the path for running the group's tests
        oldpath = stPath ss
        ssWithPath = ss { stPath = Label gname : oldpath }

        foldfun (ss', us') t = performTest' selector' ss' us' t
      in do
        -- Run the tests with the updated path
        (ssAfter, usAfter) <- foldM foldfun (ssWithPath, us) testlist
        -- Return the state, reset to the old path
        return (ssAfter { stPath = oldpath }, usAfter)
    performTest' selector ss us (Test t @ TestInstance { name = testname,
                                                         tags = testtags }) =
      -- Update the selector and take an action based on its value
      case updateSelector testname selector of
        -- If we see an All selector, check to see if the test has any
        -- of the tags we're running (or if the tag set is empty), and
        -- run it, or else skip it.
        Just (tagset, selector') | selector' == allSelector ->
            if tagset == Set.empty ||
               any (\tag -> Set.member tag tagset) testtags
              then performTestCase rep ss us t
              else skipTestCase rep ss us t
        -- Otherwise, we skip the case
        _ -> skipTestCase rep ss us t
    performTest' selector ss us (ExtraOptions _ inner) =
      -- For now, options aren't being handled.  This will need to do
      -- more when they are.
      performTest' selector ss us inner
  in do
    (ss', us') <-
      performTest' (Just (Set.empty, initSelector))
                   initState initialUs initialTest
    unless (null (stPath ss')) $ error "performTest: Final path is nonnull"
    return (ss', us')

-- | Given a name representing the current group or test name and a
-- selection state, return a new selection state to use in recursive
-- calls.
updateSelector :: String -> Maybe (Set String, Selector) ->
                  Maybe (Set String, Selector)
-- For All, we are going to run all tests that match the tag set
updateSelector _ res @ (Just (_, selector)) | selector == allSelector = res
-- For Union, pick the first inner that produces a result other than Nothing.
updateSelector elem (Just (tagset, Union { unionInners = inners })) =
  let
    mapfun inner = updateSelector elem (Just (tagset, inner))
  in
    foldr (<|>) Nothing (map mapfun (Set.elems inners))
-- For Unions, pick the first one that matches
updateSelector elem (Just (tagset, Path { pathElem = elem',
                                          pathInner = inner }))
  -- For Paths, if the path element matches the name we have, then
  -- import all tags in the inner selector and return it.
  | elem == elem' = Just (tagset, inner)
  -- Otherwise, we don't match the path, so return Nothing
  | otherwise = Nothing
-- For tags, just union them with the tag set
updateSelector elem (Just (tagset, Tags { tagsNames = newtags,
                                          tagsInner = inner })) =
  updateSelector elem (Just (Set.union tagset newtags, inner))
-- For Nothing, we're already skipping all the tests
updateSelector _ Nothing = Nothing

performTestSuite :: Reporter us
                 -- ^ Report generator to use for running the test suite
                 -> Map String Selector
                 -- ^ The selector to apply to all tests in the suite
                 -> us
                 -- ^ State for the report generator
                 -> TestSuite
                 -- ^ Test suite to be run
                 -> IO (Counts, us)
performTestSuite rep @ Reporter { reporterStartSuite = reportStartSuite,
                                  reporterEndSuite = reportEndSuite }
                 filters initialUs
                 TestSuite { suiteName = sname, suiteTests = testlist,
                             suiteOptions = suiteOpts } =
  case Map.lookup sname filters of
    Just selector ->
      let
        initState = State { stCounts = zeroCounts, stName = sname,
                            stPath = [], stOptions = Map.fromList suiteOpts }

        foldfun (c, us) testcase = performTest rep selector c us testcase
      in do
        startedUs <- reportStartSuite initState initialUs
        (time, (finishedState, finishedUs)) <-
          timeItT (foldM foldfun (initState, startedUs) testlist)
        endedUs <- reportEndSuite time finishedState finishedUs
        return (stCounts finishedState, endedUs)
    _ ->
      return (Counts { cCases = 0, cTried = 0, cErrors = 0, cFailures = 0,
                       cAsserts = 0, cSkipped = 0 }, initialUs)

performTestSuites :: Reporter us
                  -- ^ Report generator to use for running the test suite
                  -> Map String Selector
                  -- ^ The processed filter to use
                  -> [TestSuite]
                  -- ^ Test suite to be run
                  -> IO (Counts, us)
performTestSuites rep @ Reporter { reporterStart = reportStart,
                                   reporterEnd = reportEnd }
                  filters suites =
  let
    initialCounts = Counts { cCases = 0, cTried = 0, cErrors = 0,
                             cFailures = 0, cAsserts = 0, cSkipped = 0 }

    combineCounts Counts { cCases = cases1, cTried = tried1,
                           cErrors = errors1, cFailures = failures1,
                           cAsserts = asserts1, cSkipped = skipped1 }
                  Counts { cCases = cases2, cTried = tried2,
                           cErrors = errors2, cFailures = failures2,
                           cAsserts = asserts2, cSkipped = skipped2 } =
      Counts { cCases = cases1 + cases2, cTried = tried1 + tried2,
               cErrors = errors1 + errors2, cFailures = failures1 + failures2,
               cAsserts = asserts1 + asserts2, cSkipped = skipped1 + skipped2 }

    foldfun (accumCounts, accumUs) suite =
      do
        (suiteCounts, suiteUs) <- performTestSuite rep filters accumUs suite
        return (combineCounts accumCounts suiteCounts, suiteUs)
  in do
    initialUs <- reportStart
    (time, (finishedCounts, finishedUs)) <-
      timeItT (foldM foldfun (initialCounts, initialUs) suites)
    endedUs <- reportEnd time finishedCounts finishedUs
    return (finishedCounts, endedUs)
