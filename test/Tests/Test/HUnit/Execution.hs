module Tests.Test.HUnit.Execution where

import Data.List
import Data.Map(Map)
import Data.Maybe
import Debug.Trace
import Distribution.TestSuite
import Test.HUnit.Base
import Test.HUnit.Execution
import Test.HUnit.Filter
import Test.HUnit.Reporting

import qualified Data.Set as Set
import qualified Data.Map as Map

data ReportEvent =
    EndEvent Counts
  | StartSuiteEvent State
  | EndSuiteEvent State
  | StartCaseEvent State
  | EndCaseEvent State
  | SkipEvent State
  | ProgressEvent String State
  | FailureEvent String State
  | ErrorEvent String State
  | SystemErrEvent String State
  | SystemOutEvent String State
    deriving (Eq, Show)

fullLoggingReporter :: Reporter [ReportEvent]
fullLoggingReporter = defaultReporter {
    reporterStart = return [],
    reporterEnd =
      \_ ss events -> return $ (EndEvent ss :events),
    reporterStartSuite =
      \ss events -> return $ (StartSuiteEvent ss : events),
    reporterEndSuite =
      \_ ss events -> return $ (EndSuiteEvent ss : events),
    reporterStartCase =
      \ss events -> return $ (StartCaseEvent ss : events),
    reporterEndCase =
      \_ ss events -> return $ (EndCaseEvent ss : events),
    reporterSkipCase =
      \ss events -> return $ (SkipEvent ss : events),
    reporterCaseProgress =
      \msg ss events -> return $ (ProgressEvent msg ss : events),
    reporterFailure =
      \msg ss events -> return $ (FailureEvent msg ss : events),
    reporterError =
      \msg ss events -> return $ (ErrorEvent msg ss : events),
    reporterSystemErr =
      \msg ss events -> return $ (SystemErrEvent msg ss : events),
    reporterSystemOut =
      \msg ss events -> return $ (SystemOutEvent msg ss : events)
  }

makeTagName False False = "no_tag"
makeTagName True False = "tag1"
makeTagName False True = "tag2"
makeTagName True True = "tag12"

makeTagSet (False, False) = Set.empty
makeTagSet (True, False) = Set.singleton "tag1"
makeTagSet (False, True) = Set.singleton "tag2"
makeTagSet (True, True) = Set.fromList ["tag1", "tag2"]

makeResName Pass = "pass"
makeResName (Fail _) = "fail"
makeResName (Error _) = "error"

makeAssert Pass = assertSuccess
makeAssert (Fail msg) = assertFailure msg
makeAssert (Error msg) = abortError msg

updateCounts Pass c = c
updateCounts (Fail _) c @ Counts { cFailures = fails } =
  c { cFailures = fails + 1 }
updateCounts (Error _) c @ Counts { cErrors = errors } =
  c { cErrors = errors + 1 }

makeName :: (Bool, Bool, Result) -> String
makeName (tag1, tag2, res) =
  makeTagName tag1 tag2 ++ "_" ++ makeResName res

makeTest :: String -> (Bool, Bool, Result) -> Test
makeTest prefix tdata @ (tag1, tag2, res) =
  let
    inittags = if tag1 then ["tag1"] else []
    tags = if tag2 then "tag2" : inittags else inittags

    runTest = return (Finished res)

    testInstance = TestInstance { name = prefix ++ makeName tdata, tags = tags,
                                  setOption = (\_ _ -> Right testInstance),
                                  options = [], run = runTest }
  in
    Test testInstance

makeTestData :: String -> ([Test], [ReportEvent], State) ->
                Either (Bool, Bool, Result) (Bool, Bool, Result) ->
                ([Test], [ReportEvent], State)
makeTestData prefix
             (tests, events,
              ss @ State { stName = oldname,
                           stCounts = counts @ Counts { cCases = cases,
                                                        cTried = tried } })
             (Right tdata @ (tag1, tag2, res)) =
  let
    startedCounts = counts { cCases = cases + 1, cTried = tried + 1 }
    finishedCounts = updateCounts res startedCounts
    ssWithName = ss { stName = prefix ++ makeName tdata }
    ssStarted = ssWithName { stCounts = startedCounts }
    ssFinished = ssWithName { stCounts = finishedCounts }
    -- Remember, the order is reversed for these, because we reverse
    -- the events list in the end.
    newevents =
      case res of
        Pass -> EndCaseEvent ssFinished : StartCaseEvent ssStarted : events
        Fail msg -> EndCaseEvent ssFinished : FailureEvent msg ssStarted :
                    StartCaseEvent ssStarted : events
        Error msg -> EndCaseEvent ssFinished : ErrorEvent msg ssStarted :
                     StartCaseEvent ssStarted : events
  in
    (makeTest prefix tdata : tests, newevents,
     ssFinished { stName = oldname })
makeTestData prefix
             (tests, events, ss @ State { stCounts =
                                             c @ Counts { cSkipped = skipped,
                                                          cCases = cases },
                                          stName = oldname })
             (Left tdata) =
  let
    newcounts = c { cCases = cases + 1, cSkipped = skipped + 1 }
    newstate = ss { stCounts = newcounts, stName = prefix ++ makeName tdata }
  in
    (makeTest prefix tdata : tests, SkipEvent newstate : events,
     newstate { stName = oldname })

resultVals :: [Result]
resultVals = [Pass, Fail "Fail Message", Error "Error Message"]

tagVals :: [Bool]
tagVals = [True, False]

testData :: [(Bool, Bool, Result)]
testData = foldl (\accum tag1 ->
                   foldl (\accum tag2 ->
                           foldl (\accum res -> (tag1, tag2, res) : accum)
                                 accum resultVals)
                         accum tagVals)
                 [] tagVals

tag1Filter tdata @ (True, _, _) = Right tdata
tag1Filter tdata = Left tdata

tag2Filter tdata @ (_, True, _) = Right tdata
tag2Filter tdata = Left tdata

tag12Filter tdata @ (True, _, _) = Right tdata
tag12Filter tdata @ (_, True, _) = Right tdata
tag12Filter tdata = Left tdata

data ModFilter = All | WithTags (Bool, Bool) | None deriving Show

getTests :: ModFilter -> [Either (Bool, Bool, Result) (Bool, Bool, Result)]
getTests All = map Right testData
getTests (WithTags (True, False)) = map tag1Filter testData
getTests (WithTags (False, True)) = map tag2Filter testData
getTests (WithTags (True, True)) = map tag12Filter testData
getTests None = map Left testData

-- Generate a list of all mod filters we can use for a sub-module, and
-- the selectors we need for them
getSuperSet :: (Selector -> Selector) -> ModFilter -> [(ModFilter, Selector)]
-- If we're already running all tests, there's nothing else we can do
getSuperSet wrapinner All =
  [(All, wrapinner (allSelector { selectorTags = Nothing }))]
-- If we're running tests with both tags, we can do that, or we can
-- run all tests in the submodule.
getSuperSet wrapinner (WithTags (True, True)) =
  [(WithTags (True, True),
    wrapinner (allSelector { selectorTags = Nothing })),
   (All, wrapinner allSelector)]
-- If we're running tests with one of the tags, we can do that, or we
-- can run with both tags, or we can run all tests.
getSuperSet wrapinner (WithTags (False, True)) =
  [(WithTags (False, True),
    wrapinner (allSelector { selectorTags = Nothing })),
   (WithTags (True, True),
    wrapinner (allSelector { selectorTags =
                                   Just $! Set.fromList ["tag1", "tag2" ] })),
   (All, wrapinner allSelector) ]
getSuperSet wrapinner (WithTags (True, False)) =
  [(WithTags (True, False),
    wrapinner (allSelector { selectorTags = Nothing })),
   (WithTags (True, True),
    wrapinner (allSelector { selectorTags =
                                   Just $! Set.fromList ["tag1", "tag2" ] })),
   (All, wrapinner allSelector) ]
-- If we're not running any tests, we can do anything
getSuperSet wrapinner None =
  [(None, wrapinner (allSelector { selectorTags = Nothing })),
   (WithTags (True, False),
    wrapinner (allSelector { selectorTags = Just $! Set.singleton "tag1" })),
   (WithTags (False, True),
    wrapinner (allSelector { selectorTags = Just $! Set.singleton "tag2" })),
   (WithTags (True, True),
    wrapinner (allSelector { selectorTags =
                                   Just $! Set.fromList ["tag1", "tag2" ] })),
   (All, wrapinner allSelector) ]

-- Make the tests for a group, with a starting modfilter
makeLeafGroup :: String -> (Selector -> Selector) -> ModFilter ->
                 ([Test], [ReportEvent], State, [Selector]) ->
                 [([Test], [ReportEvent], State, [Selector])]
makeLeafGroup gname wrapinner mfilter initialTests =
  let
    mapfun :: ([Test], [ReportEvent], State, [Selector]) ->
              (ModFilter, Selector) -> ([Test], [ReportEvent], State, [Selector])
    mapfun (tests, events, ss @ State { stPath = oldpath }, selectors)
           (mfilter, selector) =
      let
        ssWithPath = ss { stPath = Label gname : oldpath }
        (grouptests, events', ss') = foldl (makeTestData (gname ++ "_"))
                                           ([], events, ssWithPath)
                                           (getTests mfilter)
        tests' = Group { groupName = gname, groupTests = reverse grouptests,
                         concurrently = True } : tests
      in
        (tests', events', ss' { stPath = oldpath }, selector : selectors)
  in
    map (mapfun initialTests) (getSuperSet wrapinner mfilter)

makeOuterGroup :: ModFilter -> ([Test], [ReportEvent], State, [Selector]) ->
                  [([Test], [ReportEvent], State, [Selector])]
makeOuterGroup mfilter initialTests =
  let
    mapfun :: ([Test], [ReportEvent], State, [Selector]) ->
              (ModFilter, Selector) ->
              [([Test], [ReportEvent], State, [Selector])]
    mapfun (tests, events, ss @ State { stPath = oldpath }, selectors)
           (mfilter, selector) =
      let
        ssWithPath = ss { stPath = Label "Outer" : oldpath }

        mapfun :: ([Test], [ReportEvent], State, [Selector]) ->
                  ([Test], [ReportEvent], State, [Selector])
        mapfun (innergroup : tests, events, ss, selectors) =
          let
            (grouptests, events', ss') = foldl (makeTestData "Outer_")
                                               ([innergroup], events, ss)
                                               (getTests mfilter)

            tests' = Group { groupName = "Outer",
                             groupTests = reverse grouptests,
                             concurrently = True } : tests
          in
           (tests', events', ss' { stPath = oldpath }, selector : selectors)

        wrapInnerPath inner =
          Selector {
            selectorInners =
               Map.singleton "Outer" Selector {
                                       selectorInners =
                                          Map.singleton "Inner" inner,
                                       selectorTags = Nothing
                                     },
            selectorTags = Nothing
          }

        withInner :: [([Test], [ReportEvent], State, [Selector])]
        withInner = makeLeafGroup "Inner" wrapInnerPath mfilter
                                  (tests, events, ssWithPath, selectors)
      in
        map mapfun withInner

    wrapOuterPath inner =
      Selector { selectorInners = Map.singleton "Outer" inner,
                 selectorTags = Nothing }
  in
    concatMap (mapfun initialTests) (getSuperSet wrapOuterPath mfilter)

modfilters = [ All, WithTags (True, False), WithTags (False, True),
               WithTags (True, True), None ]

genFilter :: String -> [(TestSuite, [ReportEvent], Map String Selector, Counts)]
genFilter sname =
  let
    -- Take a root ModFilter and an initial (suite list, event list,
    -- selectors).  We generate a stock suite, derive a selector from
    -- the root ModFilter, and produce a list of possible (suite list,
    -- event list, selectors)'s, one for each possibility.
    suiteTestInst :: ModFilter -> [(TestSuite, [ReportEvent],
                                    Map String Selector, Counts)]
    suiteTestInst mfilter =
      let
        -- Initial state for a filter
        initState = State { stCounts = zeroCounts, stName = sname,
                            stPath = [], stOptions = Map.empty,
                            stOptionDescs = [] }

        -- The selectors for the root set
        rootSelectors :: [Selector]
        rootSelectors =
          case mfilter of
            All -> [allSelector]
            WithTags tags ->
              [allSelector { selectorTags = Just $! makeTagSet tags }]
            None -> []

        -- Result after executing the root tests.
        (rootTests, rootEvents, rootState) =
          foldl (makeTestData "") ([], [StartSuiteEvent initState], initState)
                (getTests mfilter)

        wrapOtherPath inner =
          Selector { selectorInners = Map.singleton "Other" inner,
                     selectorTags = Nothing }

        -- Results after executing tests in the Other module
        withOther :: [([Test], [ReportEvent], State, [Selector])]
        withOther = makeLeafGroup "Other" wrapOtherPath mfilter
                                  (rootTests, rootEvents,
                                   rootState, rootSelectors)

        finalData = concatMap (makeOuterGroup mfilter) withOther

        -- Wrap up a test list, end state, and selector list into a
        -- test suite and a filter.  Also add the EndSuite event to
        -- the events list.
        buildSuite :: ([Test], [ReportEvent], State, [Selector]) ->
                      (TestSuite, [ReportEvent], Map String Selector, Counts)
        buildSuite (tests, _, _, []) =
          let
            suite =
              TestSuite { suiteName = sname, suiteTests = reverse tests,
                          suiteConcurrently = True, suiteOptions = [] }
          in
            (suite, [], Map.empty, zeroCounts)
        buildSuite (tests, events, state @ State { stCounts = counts },
                    selectors) =
          let
            -- Build the test suite out of the name and test list, add
            -- it to the list of suites.
            suite =
              TestSuite { suiteName = sname, suiteTests = reverse tests,
                          suiteConcurrently = True, suiteOptions = [] }

            -- Add an end suite event
            eventsWithEnd = EndSuiteEvent state : events

            -- Add an entry for this suite to the selector map
            selectormap :: Map String Selector
            selectormap =
              case selectors of
                [one] -> Map.singleton sname one
                _ -> Map.singleton sname (foldl1 combineSelectors selectors)
          in
            (suite, reverse eventsWithEnd, selectormap, counts)
      in
        map buildSuite finalData
  in
    -- Create test data for this suite with all possible modfilters,
    -- and add it to the existing list of test instances.
    concatMap suiteTestInst modfilters

suite1Data :: [(TestSuite, [ReportEvent], Map String Selector, Counts)]
suite1Data = genFilter "Suite1"

suite2Data :: [(TestSuite, [ReportEvent], Map String Selector, Counts)]
suite2Data = genFilter "Suite2"

combineSuites :: (TestSuite, [ReportEvent], Map String Selector, Counts) ->
                 (TestSuite, [ReportEvent], Map String Selector, Counts) ->
                 ([TestSuite], [ReportEvent], Map String Selector)
combineSuites (suite1, events1, selectormap1, Counts { cAsserts = asserts1,
                                                       cCases = cases1,
                                                       cErrors = errors1,
                                                       cFailures = failures1,
                                                       cSkipped = skipped1,
                                                       cTried = tried1 })
              (suite2, events2, selectormap2, Counts { cAsserts = asserts2,
                                                       cCases = cases2,
                                                       cErrors = errors2,
                                                       cFailures = failures2,
                                                       cSkipped = skipped2,
                                                       cTried = tried2 }) =
  let
    counts = Counts { cAsserts = asserts1 + asserts2,
                      cErrors = errors1 + errors2,
                      cCases = cases1 + cases2,
                      cFailures = failures1 + failures2,
                      cSkipped = skipped1 + skipped2,
                      cTried = tried1 + tried2 }
    suites = [suite1, suite2]
    events = events1 ++ events2 ++ [EndEvent counts]
    selectormap = Map.union selectormap1 selectormap2
  in
    (suites, events, selectormap)


suiteData :: [([TestSuite], [ReportEvent], Map String Selector)]
suiteData = foldl (\accum suite1 ->
                    foldl (\accum suite2 ->
                            (combineSuites suite1 suite2) : accum)
                          accum suite2Data)
                  [] suite1Data

makeExecutionTest :: ([TestSuite], [ReportEvent], Map String Selector) ->
                     (Int, [Test]) -> (Int, [Test])
makeExecutionTest (suites, expected, selectors) (index, tests) =
  let
    format events = intercalate "\n" (map show events)

    selectorStrs =
      intercalate "\n" (map (\(suite, selector) -> "[" ++ suite ++ "]" ++
                                                   show selector)
                            (Map.assocs selectors))

    check expected @ (e : expecteds) actual @ (a : actuals)
      | e == a = check expecteds actuals
      | otherwise =
        return (Finished (Fail ("Selectors\n" ++ selectorStrs ++
                                "\nExpected\n************************\n" ++
                                show e ++
                                "\nbut got\n************************\n" ++
                                show a)))
    check [] [] = return (Finished Pass)
    check expected [] =
      return (Finished (Fail ("Missing output:\n" ++ format expected)))
    check [] actual =
      return (Finished (Fail ("Extra output:\n" ++ format actual)))

    runTest =
      do
        (_, actual) <- performTestSuites fullLoggingReporter selectors suites
        check expected (reverse actual)

    testInstance = TestInstance { name = "execution_test_" ++ show index,
                                  tags = [], options = [], run = runTest,
                                  setOption = (\_ _ -> Right testInstance) }
  in
    (index + 1, Test testInstance : tests)

tests :: Test
tests = testGroup "Execution" (snd (foldr makeExecutionTest (0, []) suiteData))
