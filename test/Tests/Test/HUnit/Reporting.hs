module Tests.Test.HUnit.Reporting(tests) where

import Data.List
import Distribution.TestSuite(Test(..),
                              TestInstance(..),
                              Progress(Finished),
                              Result(Pass, Fail),
                              testGroup)
import Test.HUnit.Reporting
import Tests.Test.HUnit.ReporterUtils(ReportEvent(..))

import qualified Tests.Test.HUnit.ReporterUtils as Utils

type ReporterState = [ReportEvent]
type CombinedReporterState = (ReporterState, ReporterState)
type ReporterOp = Utils.ReporterOp ReporterState
type CombinedReporterOp = Utils.ReporterOp CombinedReporterState
type DefaultReporterOp = Utils.ReporterOp Int

loggingReporter = Utils.loggingReporter

combinedLoggingReporter :: Reporter CombinedReporterState
combinedLoggingReporter = combinedReporter loggingReporter loggingReporter

reportSystemErr = Utils.reportSystemErr combinedLoggingReporter
reportSystemOut = Utils.reportSystemOut combinedLoggingReporter
reportFailure = Utils.reportFailure combinedLoggingReporter
reportError = Utils.reportError combinedLoggingReporter
reportSkip = Utils.reportSkip combinedLoggingReporter
reportProgress = Utils.reportProgress combinedLoggingReporter
reportStartCase = Utils.reportStartCase combinedLoggingReporter
reportEndCase = Utils.reportEndCase combinedLoggingReporter
runReporterTest = Utils.runReporterTest combinedLoggingReporter
reportStartSuite = Utils.reportStartSuite combinedLoggingReporter
reportEndSuite = Utils.reportEndSuite combinedLoggingReporter
reportEnd = Utils.reportEnd combinedLoggingReporter

reporterActions :: [(String, CombinedReporterOp, ReporterState)]
reporterActions = [
    ("systemErr", reportSystemErr "Error Message", [SystemErr "Error Message"]),
    ("systemOut", reportSystemOut "Output Message",
     [SystemOut "Output Message"]),
    ("failure", reportFailure "Failure Message", [Failure "Failure Message"]),
    ("error", reportError "Error Message", [Error "Error Message"]),
    ("progress", reportProgress "Progress Message",
     [Progress "Progress Message"]),
    ("skip", reportSkip, [Skip]),
    ("startCase", reportStartCase, [StartCase]),
    ("endCase", reportEndCase 1.0, [EndCase 1.0]),
    ("startSuite", reportStartSuite, [StartSuite]),
    ("endSuite", reportEndSuite 2.0, [EndSuite 2.0]),
    ("end", reportEnd 3.0, [End 3.0])
  ]

defaultReporterCases :: [(String, State -> Int -> IO Int, Int)]
defaultReporterCases = [
    ("systemErr", (reporterSystemErr defaultReporter) "Error Message", 1),
    ("systemOut", (reporterSystemOut defaultReporter) "Output Message", 2),
    ("failure", (reporterFailure defaultReporter) "Failure Message", 3),
    ("error", (reporterError defaultReporter) "Error Message", 4),
    ("progress", (reporterCaseProgress defaultReporter) "Progress Message", 5),
    ("skip", reporterSkipCase defaultReporter, 6),
    ("startCase", reporterStartCase defaultReporter, 7),
    ("endCase", (reporterEndCase defaultReporter) 1.0, 8),
    ("startSuite", reporterStartSuite defaultReporter, 9),
    ("endSuite", (reporterEndSuite defaultReporter) 2.0, 10),
    ("end", (reporterEnd defaultReporter) 3.0 . stCounts, 11)
  ]

reporterCases :: [[(String, CombinedReporterOp, ReporterState)]]
reporterCases =
  map (: []) reporterActions ++
  foldr (\a accum ->
          foldr (\b accum -> [a, b] : accum)
                accum reporterActions)
        [] reporterActions

genCombinedReporterTest :: [(String, CombinedReporterOp, ReporterState)] -> Test
genCombinedReporterTest testactions =
  let
    name = intercalate "_" (map (\(a, _, _) -> a) testactions)
    ops = map (\(_, a, _) -> a) testactions
    log = concat (map (\(_, _, a) -> a) testactions)
    expected = (log, log)

    out = TestInstance { name = "combinedReporter_" ++ name,
                         tags = [], options = [],
                         setOption = (\_ _ -> Right out),
                         run = runReporterTest ops expected show >>=
                               return . Finished }
  in
    Test out

genDefaultReporterTest :: (String, State -> Int -> IO Int, Int) -> Test
genDefaultReporterTest (name, reporterAction, reporterState) =
  let
    runTest =
      do
        result <- reporterAction Utils.initState reporterState
        if result == reporterState
          then return (Finished Pass)
          else return (Finished (Fail "defaultReporter altered state!"))

    out = TestInstance { name = "defaultReporter_" ++ name,
                         tags = [], options = [], run = runTest,
                         setOption = (\_ _ -> Right out) }
  in
    Test out


tests :: Test
tests = testGroup "Reporting" (map genCombinedReporterTest reporterCases ++
                               map genDefaultReporterTest defaultReporterCases)
