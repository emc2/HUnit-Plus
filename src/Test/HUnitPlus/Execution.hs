{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for executing test cases, test paths, and test suites.
-- These functions are provided for the sake of convenience and
-- testing; however, the preferred way of using HUnit-Plus is to use
-- the "Test.HUnitPlus.Main#createMain" to create a test program
-- directly from a list of test suites.
module Test.HUnitPlus.Execution(
       performTestCase,
       performTest,
       performTestSuite,
       performTestSuites
       ) where

import Control.Monad (unless, foldM)
import Distribution.TestSuite
import Data.HashMap.Strict(HashMap)
import Data.Time
import Data.Version
import Network.HostName
import Prelude hiding (elem)
import System.Info
import System.TimeIt
import Test.HUnitPlus.Base
import Test.HUnitPlus.Filter
import Test.HUnitPlus.Reporting

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Strict

-- | Execute an individual test case.
performTestCase :: Reporter us
                -- ^ Report generator for the test run.
                -> State
                -- ^ HUnit-Plus internal state.
                -> us
                -- ^ State for the report generator.
                -> TestInstance
                -- ^ The test to be executed.
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
    ssWithName = ss { stName = Strict.pack testname,
                      stCounts = c { cTried = tried + 1, cCases = cases + 1 } }

    -- Fold function for applying options
    applyOptions (us, ti) OptionDescr { optionName = optname } =
      let
        setresult :: Either String TestInstance
        setresult =
          case HashMap.lookup (Strict.pack optname) optmap of
            Just optval -> setopt optname (Strict.unpack optval)
            Nothing -> Right ti
      in case setresult of
        Left errmsg ->
          do
            newUs <- reportError (Strict.pack errmsg) ssWithName us
            return (newUs, ti)
        Right newTi -> return (us, newTi)
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
    return (ssFinal { stName = oldname }, usEnded)

-- | Log a skipped test case.
skipTestCase :: Reporter us
             -- ^ Report generator for the test run.
             -> State
             -- ^ HUnit-Plus internal state.
             -> us
             -- ^ State for the report generator.
             -> TestInstance
             -- ^ The test to be executed.
             -> IO (State, us)
skipTestCase Reporter { reporterSkipCase = reportSkipCase }
             ss @ State { stCounts = c @ Counts { cSkipped = skipped,
                                                  cCases = cases },
                          stName = oldname } us
             TestInstance { name = testname } =
  let
    ss' = ss { stCounts = c { cSkipped = skipped + 1, cCases = cases + 1 },
               stName = Strict.pack testname }
  in do
    us' <- reportSkipCase ss' us
    return (ss' { stName = oldname }, us')

-- | Execute a given test (which may be a group), with the specified
-- selector and report generators.  Only tests which match the
-- selector will be executed.  The rest will be logged as skipped.
performTest :: Reporter us
            -- ^ Report generator for the test run.
            -> Selector
            -- ^ The selector to apply to all tests in the suite.
            -> State
            -- ^ HUnit-Plus internal state.
            -> us
            -- ^ State for the report generator.
            -> Test
            -- ^ The test to be executed.
            -> IO (State, us)
performTest rep initSelector initialState initialUs initialTest =
  let
    -- The recursive worker function that actually runs all the tests
    --
    -- We need to actually run through all the tests, so we can record
    -- the ones we skip.  So we keep a Maybe Selector, where Nothing
    -- represents tests that aren't actually going to be executed.
    --
    -- We also have to keep a set of tags by which we're filtering.
    -- The empty tag set means we don't actually filter at all.
    performTest' s @ Selector { selectorInners = inners,
                                selectorTags = currtags }
                 ss us Group { groupTests = testlist, groupName = gname } =
      let
        -- Build the new selector
        selector' =
          -- Try looking up the group in the inners
          case HashMap.lookup (Strict.pack gname) inners of
            -- If we don't find anything, we can only keep executing
            -- if our tag state allows it.
            Nothing -> s { selectorInners = HashMap.empty,
                           selectorTags = currtags }
            -- Otherwise, combine the inner's tag state with ours and
            -- carry on.
            Just inner @ Selector { selectorTags = innertags } ->
              inner { selectorTags = combineTags currtags innertags }

        -- Update the path for running the group's tests
        oldpath = stPath ss
        ssWithPath = ss { stPath = Label (Strict.pack gname) : oldpath }

        foldfun (ss', us') = performTest' selector' ss' us'
      in do
        -- Run the tests with the updated path
        (ssAfter, usAfter) <- foldM foldfun (ssWithPath, us) testlist
        -- Return the state, reset to the old path
        return (ssAfter { stPath = oldpath }, usAfter)
    performTest' Selector { selectorInners = inners, selectorTags = currtags }
                 ss us (Test t @ TestInstance { name = testname,
                                                tags = testtags }) =
      let
        -- Get the final tag state
        finaltags =
          -- Try looking up the group in the inners
          case HashMap.lookup (Strict.pack testname) inners of
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
              | HashSet.null set -> True
              | otherwise -> any (\tag -> HashSet.member tag set)
                                 (map Strict.pack testtags)
      in
        if canExecute
          then performTestCase rep ss us t
          else skipTestCase rep ss us t
    performTest' selector ss @ State { stOptionDescs = descs }
                 us (ExtraOptions newopts inner) =
      performTest' selector ss { stOptionDescs = descs ++ newopts } us inner
  in do
    (ss', us') <- performTest' initSelector initialState initialUs initialTest
    unless (null (stPath ss')) $ error "performTest: Final path is nonnull"
    return (ss', us')

-- | Decide whether to execute a test suite based on a map from suite
-- names to selectors.  If the map contains a selector for the test
-- suite, execute all tests matching the selector, and log the rest as
-- skipped.  If the map does not contain a selector, do not execute
-- the suite, and do /not/ log its tests as skipped.
performTestSuiteInstance :: Reporter us
                         -- ^ Report generator to use for running the
                         -- test suite.
                         -> OptionMap
                         -- ^ The options for this instance.
                         -> Selector
                         -- ^ The selector to use.
                         -> State
                         -- ^ HUnit-Plus internal state.
                         -> us
                         -- ^ State for the report generator.
                         -> TestSuite
                         -- ^ Test suite to be run.
                         -> IO (State, us)
performTestSuiteInstance rep @ Reporter { reporterStartSuite = reportStartSuite,
                                          reporterEndSuite = reportEndSuite }
                         instopts selector
                         st @ State { stOptions = stopts } initialUs
                         TestSuite { suiteName = sname, suiteTests = testlist,
                                     suiteOptions = suiteOpts } =
  let
    makestate timestamp =
      let
        timestr = formatTime defaultTimeLocale "%c" timestamp
        withtime = HashMap.insert "timestamp" (Strict.pack timestr) stopts
        -- Don't allow people to override the system information
        withInstOpts = HashMap.union withtime instopts
        unioned = HashMap.union withInstOpts (HashMap.fromList suiteOpts)
      in
        return st { stOptions = unioned, stName = sname }

    foldfun (c, us) = performTest rep selector c us
  in do
    timestamp <- getCurrentTime
    state <- makestate timestamp
    startedUs <- reportStartSuite state initialUs
    (time, (finishedState @ State { stCounts = counts }, finishedUs)) <-
      timeItT (foldM foldfun (state, startedUs) testlist)
    endedUs <- reportEndSuite time finishedState finishedUs
    return (finishedState { stCounts = counts { cCaseAsserts = 0 } },
            endedUs)

-- | Decide whether to execute a test suite based on a map from suite
-- names to selectors.  If the map contains a selector for the test
-- suite, execute all tests matching the selector, and log the rest as
-- skipped.  If the map does not contain a selector, do not execute
-- the suite, and do /not/ log its tests as skipped.
performTestSuiteInternal :: Reporter us
                         -- ^ Report generator to use for running the
                         -- test suite.
                         -> HashMap Strict.Text (HashMap OptionMap Selector)
                         -- ^ The map containing selectors for each suite.
                         -> State
                         -- ^ HUnit-Plus internal state.
                         -> us
                         -- ^ State for the report generator.
                         -> TestSuite
                         -- ^ Test suite to be run.
                         -> IO (State, us)
performTestSuiteInternal rep filters initialSs initialUs
                         suite @ TestSuite { suiteName = sname } =
  case HashMap.lookup sname filters of
    Just optmap ->
      let
        foldfun (ss, us) (opts, selector) =
          performTestSuiteInstance rep opts selector ss us suite
      in
        foldM foldfun (initialSs, initialUs) (HashMap.toList optmap)
    _ -> return (initialSs, initialUs)

initState :: State
initState = State { stCounts = zeroCounts, stName = "",
                    stPath = [], stOptions = HashMap.empty,
                    stOptionDescs = [] }

performTestSuite :: Reporter us
                 -- ^ Report generator to use for running the test suite.
                 -> HashMap Strict.Text (HashMap OptionMap Selector)
                 -- ^ The map containing selectors for each suite.
                 -> us
                 -- ^ State for the report generator.
                 -> TestSuite
                 -- ^ Test suite to be run.
                 -> IO (Counts, us)
performTestSuite rep filters us suite =
  do
    (State { stCounts = out }, _) <-
      performTestSuiteInternal rep filters initState us suite
    return (out, us)

-- | Top-level function for a test run.  Given a set of suites and a
-- map from suite names to selectors, execute all suites that have
-- selectors.  For any test suite, only the tests specified by its
-- selector will be executed; the rest will be logged as skipped.
-- Suites that do not have a selector will be omitted entirely, and
-- their tests will /not/ be logged as skipped.
performTestSuites :: Reporter us
                  -- ^ Report generator to use for running the test suite.
                  -> HashMap Strict.Text (HashMap OptionMap Selector)
                  -- ^ The processed filter to use.
                  -> [TestSuite]
                  -- ^ Test suite to be run.
                  -> IO (Counts, us)
performTestSuites rep @ Reporter { reporterStart = reportStart,
                                   reporterEnd = reportEnd }
                  filters suites =
  let
    foldfun (accumState, accumUs) =
      performTestSuiteInternal rep filters accumState accumUs

    startstate =
      do
        hostname <- getHostName
        return initState {
                 stOptions = HashMap.fromList
                               [("hostname", Strict.pack hostname),
                                ("os", Strict.pack os),
                                ("arch", Strict.pack arch),
                                ("compiler-name",
                                 Strict.pack compilerName),
                                ("compiler-version",
                                 Strict.pack (showVersion compilerVersion))]
               }
  in do
    initialUs <- reportStart
    st <- startstate
    (time, (State { stCounts = finishedCounts }, finishedUs)) <-
      timeItT (foldM foldfun (st, initialUs) suites)
    endedUs <- reportEnd time finishedCounts finishedUs
    return (finishedCounts, endedUs)
