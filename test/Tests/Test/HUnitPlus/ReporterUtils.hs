module Tests.Test.HUnitPlus.ReporterUtils where

import Control.Monad
import Data.Word
import Distribution.TestSuite(Result(Pass, Fail))
import Test.HUnitPlus.Reporting

import qualified Data.Map as Map

data ReportEvent =
    End Double
  | StartSuite
  | EndSuite Double
  | StartCase
  | EndCase Double
  | Skip
  | Progress String
  | Failure String
  | Error String
  | SystemErr String
  | SystemOut String
    deriving (Eq, Show)

type ReporterOp us = (State, us) -> IO (State, us)

loggingReporter :: Reporter [ReportEvent]
loggingReporter = defaultReporter {
    reporterStart = return [],
    reporterEnd = (\time _ events -> return (events ++ [End time])),
    reporterStartSuite = (\_ events -> return (events ++ [StartSuite])),
    reporterEndSuite = (\time _ events -> return (events ++ [EndSuite time])),
    reporterStartCase = (\_ events -> return (events ++ [StartCase])),
    reporterEndCase = (\time _ events -> return (events ++ [EndCase time])),
    reporterSkipCase = (\_ events -> return (events ++ [Skip])),
    reporterCaseProgress = (\msg _ events -> return (events ++ [Progress msg])),
    reporterFailure = (\msg _ events -> return (events ++ [Failure msg])),
    reporterError = (\msg _ events -> return (events ++ [Error msg])),
    reporterSystemErr = (\msg _ events -> return (events ++ [SystemErr msg])),
    reporterSystemOut = (\msg _ events -> return (events ++ [SystemOut msg]))
  }

initState :: State
initState = State { stName = "", stPath = [], stCounts = zeroCounts,
                    stOptions = Map.empty, stOptionDescs = [] }

setName :: String -> ReporterOp us
setName name (s @ State { stName = _ }, repstate) =
  return (s { stName = name }, repstate)

setOpt :: String -> String -> ReporterOp us
setOpt key value (s @ State { stOptions = opts }, repstate) =
  return (s { stOptions = Map.insert key value opts }, repstate)

pushPath :: String -> ReporterOp us
pushPath name (s @ State { stPath = path }, repstate) =
  return (s { stPath = Label name : path }, repstate)

popPath :: ReporterOp us
popPath (s @ State { stPath = _ : path }, repstate) =
  return (s { stPath = path }, repstate)

addOption :: String -> String -> ReporterOp us
addOption key value (s @ State { stOptions = opts }, repstate) =
  return (s { stOptions = Map.insert key value opts }, repstate)

countAsserts :: Word -> ReporterOp us
countAsserts count (s @ State { stCounts = c @ Counts { cAsserts = n } },
                    repstate) =
  return (s { stCounts = c { cAsserts = n + count } }, repstate)

countTried :: Word -> ReporterOp us
countTried count (s @ State { stCounts = c @ Counts { cCases = cases,
                                                      cTried = tried } },
                  repstate) =
  return (s { stCounts = c { cCases = cases + count,
                             cTried = tried + count } },
          repstate)

countSkipped :: Word -> ReporterOp us
countSkipped count (s @ State { stCounts = c @ Counts { cSkipped = skipped,
                                                        cCases = cases } },
                  repstate) =
  return (s { stCounts = c { cSkipped = skipped + count,
                             cCases = cases + count } },
          repstate)

countErrors :: Word -> ReporterOp us
countErrors count (s @ State { stCounts = c @ Counts { cErrors = errors } },
                   repstate) =
  return (s { stCounts = c { cErrors = errors + count } }, repstate)

countFailed :: Word -> ReporterOp us
countFailed count (s @ State { stCounts = c @ Counts { cFailures = failed } },
                   repstate) =
  return (s { stCounts = c { cFailures = failed + count } }, repstate)

reportProgress :: Reporter us -> String -> ReporterOp us
reportProgress reporter msg (state, repstate) =
  do
    repstate' <- (reporterCaseProgress reporter) msg state repstate
    return (state, repstate')

reportSystemErr :: Reporter us -> String -> ReporterOp us
reportSystemErr reporter msg (state, repstate) =
  do
    repstate' <- (reporterSystemErr reporter) msg state repstate
    return (state, repstate')

reportSystemOut :: Reporter us -> String -> ReporterOp us
reportSystemOut reporter msg (state, repstate) =
  do
    repstate' <- (reporterSystemOut reporter) msg state repstate
    return (state, repstate')

reportFailure :: Reporter us -> String -> ReporterOp us
reportFailure reporter msg (state, repstate) =
  do
    repstate' <- (reporterFailure reporter) msg state repstate
    return (state, repstate')

reportError :: Reporter us -> String -> ReporterOp us
reportError reporter msg (state, repstate) =
  do
    repstate' <- (reporterError reporter) msg state repstate
    return (state, repstate')

reportSkip :: Reporter us -> ReporterOp us
reportSkip reporter (state, repstate) =
  do
    repstate' <- (reporterSkipCase reporter) state repstate
    return (state, repstate')

reportStartCase :: Reporter us -> ReporterOp us
reportStartCase reporter (state, repstate) =
  do
    repstate' <- (reporterStartCase reporter) state repstate
    return (state, repstate')

reportEndCase :: Reporter us -> Double -> ReporterOp us
reportEndCase reporter time (state, repstate) =
  do
    repstate' <- (reporterEndCase reporter) time state repstate
    return (state, repstate')

reportStartSuite :: Reporter us -> ReporterOp us
reportStartSuite reporter (state, repstate) =
  do
    repstate' <- (reporterStartSuite reporter) state repstate
    return (state, repstate')

reportEndSuite :: Reporter us -> Double -> ReporterOp us
reportEndSuite reporter time (state, repstate) =
  do
    repstate' <- (reporterEndSuite reporter) time state repstate
    return (state, repstate')

reportEnd :: Reporter us -> Double -> ReporterOp us
reportEnd reporter time (state @ State { stCounts = counts }, repstate) =
  do
    repstate' <- (reporterEnd reporter) time counts repstate
    return (state, repstate')

runReporterTest :: Eq us => Reporter us -> [ReporterOp us] -> us ->
                   (us -> String) -> IO Result
runReporterTest reporter tests expected format =
  do
    initrepstate <- reporterStart reporter
    (_, actual) <- foldM (\state op -> op state) (initState, initrepstate) tests
    if actual == expected
      then return Pass
      else return (Fail ("Expected " ++ format expected ++
                        "\nbut got " ++ format actual))
