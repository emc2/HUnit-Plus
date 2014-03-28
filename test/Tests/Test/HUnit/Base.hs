module Tests.Test.HUnit.Base where

import Data.List
import Distribution.TestSuite(Test(..),
                              TestInstance(..),
                              Result(Pass, Fail),
                              Progress(Finished),
                              testGroup)
import Test.HUnit.Base
import Test.HUnit.Reporting

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Tests.Test.HUnit.ReporterUtils as Utils

type ReportEvent = Utils.ReportEvent

loggingReporter = Utils.loggingReporter

makeTestCase :: (Test, String, [String], Counts, Result, [ReportEvent]) -> Test
makeTestCase (Test TestInstance { name = actualName,
                                  tags = actualTags,
                                  run = runInnerTest },
              expectedName, expectedTags, expectedCounts,
              expectedResult, expectedEvents) =
  let
    initState = State { stCounts = zeroCounts, stName = "", stPath = [],
                        stOptions = Map.empty, stOptionDescs = [] }

    genResult actualResult actualCounts actualEvents =
      let
        nameErr =
          if actualName /= expectedName
            then ["Expected name \"" ++ expectedName ++
                  "\" but got \"" ++ actualName ++ "\""]
            else []
        tagsErr =
          if Set.fromList expectedTags /= Set.fromList actualTags
            then ("Expected tags " ++ show expectedTags ++
                  " but got " ++ show actualTags) : nameErr
            else nameErr
        resultErr =
          if expectedResult /= actualResult
            then ("Expected result " ++ show expectedResult ++
                  " but got " ++ show actualResult) : tagsErr
            else tagsErr
        countsErr =
          if expectedCounts /= actualCounts
            then ("Expected counts " ++ show expectedCounts ++
                  " but got " ++ show actualCounts) : resultErr
            else resultErr
        eventsErr =
          if expectedEvents /= actualEvents
            then ("Expected reporting events " ++ show expectedEvents ++
                  " but got " ++ show actualEvents) : countsErr
            else countsErr
      in case eventsErr of
        [] -> Finished Pass
        _ -> Finished (Fail (intercalate "\n" countsErr))

    runRealTest =
      do
        Finished actualResult <- runInnerTest
        (State { stCounts = actualCounts }, actualEvents) <-
          reportTestInfo actualResult loggingReporter initState []
        return (genResult actualResult actualCounts actualEvents)

    testInstance = TestInstance { name = actualName, tags = [], options = [],
                                  setOption = (\_ _ -> Right testInstance),
                                  run = runRealTest }
  in
    Test testInstance

emptyTest :: IO Progress
emptyTest = return (Finished Pass)

testCases :: [(Test, String, [String], Counts, Result, [ReportEvent])]
testCases = [
    ("emptyHUnitTest" ~: emptyTest, "emptyHUnitTest", [],
     zeroCounts { cCases = 1 }, Pass, [])
  ]

tests :: Test
tests = testGroup "Base" (map makeTestCase testCases)
