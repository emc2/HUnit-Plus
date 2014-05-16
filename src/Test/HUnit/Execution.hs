{-# OPTIONS_GHC -Wall -Werror #-}

module Test.HUnit.Execution(
       performTestCase,
       performTest,
       performTestSuite,
       performTestSuites
       ) where

import Control.Monad (unless, foldM)
import Distribution.TestSuite
import Data.Map(Map)
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
                                  reporterError = reportError,
                                  reporterEndCase = reportEndCase }
                ss @ State { stCounts = c @ Counts { cTried = tried,
                                                      cCases = cases },
                              stName = oldname, stOptions = optmap,
                              stOptionDescs = descs } initialUs
                initTi @ TestInstance { name = testname,
                                        options = testdescs,
                                        setOption = setopt } =
  let
    -- First, apply all the options
    alldescs = testdescs ++ descs
    -- Add the name to the state we use to run the tests

    -- Update the state before running
    ssWithName = ss { stName = testname, stCounts = c { cTried = tried + 1,
                                                        cCases = cases + 1 } }

    -- Fold function for applying options
    applyOptions (us, ti) OptionDescr { optionName = optname,
                                        optionDefault = def } =
      let
        setresult :: Either String TestInstance
        setresult =
          case Map.lookup optname optmap of
            Just optval -> setopt optname optval
            Nothing -> case def of
              Just optval -> setopt optname optval
              Nothing -> Right ti
      in case setresult of
        Left errmsg ->
          do
            newUs <- reportError errmsg ssWithName us
            return $! (newUs, ti)
        Right newTi -> return $! (us, newTi)
  in do
    -- Get all the rest of the information from the resulting test instance
    (usOpts, TestInstance { run = runTest }) <-
      foldM applyOptions (initialUs, initTi) alldescs
    -- Call the reporter's start case function
    usStarted <- reportStartCase ssWithName usOpts
    -- Actually run the test
    (time, ssFinal, usFinal) <- executeTest rep ssWithName usStarted runTest
    -- Call the reporters end case function
    usEnded <- reportEndCase time ssFinal usFinal
    -- Restore the old name before returning
    return $ (ssFinal { stName = oldname }, usEnded)

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
    return $! (ss' { stName = oldname }, us')

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
    performTest' Selector { selectorInners = inners, selectorTags = currtags }
                 ss us Group { groupTests = testlist, groupName = gname } =
      let
        -- Build the new selector
        selector' =
          -- Try looking up the group in the inners
          case Map.lookup gname inners of
            -- If we don't find anything, we can only keep executing
            -- if our tag state allows it.
            Nothing -> Selector { selectorInners = Map.empty,
                                  selectorTags = currtags }
            -- Otherwise, combine the inner's tag state with ours and
            -- carry on.
            Just inner @ Selector { selectorTags = innertags } ->
              inner { selectorTags = combineTags currtags innertags }

        -- Update the path for running the group's tests
        oldpath = stPath ss
        ssWithPath = ss { stPath = Label gname : oldpath }

        foldfun (ss', us') t = performTest' selector' ss' us' t
      in do
        -- Run the tests with the updated path
        (ssAfter, usAfter) <- foldM foldfun (ssWithPath, us) testlist
        -- Return the state, reset to the old path
        return $! (ssAfter { stPath = oldpath }, usAfter)
    performTest' Selector { selectorInners = inners, selectorTags = currtags }
                 ss us (Test t @ TestInstance { name = testname,
                                                tags = testtags }) =
      let
        -- Get the final tag state
        finaltags =
          -- Try looking up the group in the inners
          case Map.lookup testname inners of
            -- If we don't find anything, we can only keep executing
            -- if our tag state allows it.
            Nothing -> currtags
            -- Otherwise, combine the inner's tag state with ours and
            -- carry on.
            Just Selector { selectorTags = innertags } ->
              combineTags currtags innertags
        -- Decide if we can execute the test
        canExecute =
          case finaltags of
            Nothing -> False
            Just set
              | set == Set.empty -> True
              | otherwise -> any (\tag -> Set.member tag set) testtags
      in
        if canExecute
          then performTestCase rep ss us t
          else skipTestCase rep ss us t
    performTest' selector ss @ State { stOptionDescs = descs }
                 us (ExtraOptions newopts inner) =
      performTest' selector ss { stOptionDescs = descs ++ newopts } us inner
  in do
    (ss', us') <- performTest' initSelector initState initialUs initialTest
    unless (null (stPath ss')) $ error "performTest: Final path is nonnull"
    return $! (ss', us')

-- | Given a name representing the current group or test name and a
-- selection state, return a new selection state to use in recursive
-- calls.

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
                            stPath = [], stOptions = Map.fromList suiteOpts,
                            stOptionDescs = [] }

        foldfun (c, us) testcase = performTest rep selector c us testcase
      in do
        startedUs <- reportStartSuite initState initialUs
        (time, (finishedState, finishedUs)) <-
          timeItT (foldM foldfun (initState, startedUs) testlist)
        endedUs <- reportEndSuite time finishedState finishedUs
        return $! (stCounts finishedState, endedUs)
    _ ->
      return $! (Counts { cCases = 0, cTried = 0, cErrors = 0, cFailures = 0,
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
        return $! (combineCounts accumCounts suiteCounts, suiteUs)
  in do
    initialUs <- reportStart
    (time, (finishedCounts, finishedUs)) <-
      timeItT (foldM foldfun (initialCounts, initialUs) suites)
    endedUs <- reportEnd time finishedCounts finishedUs
    return $! (finishedCounts, endedUs)
