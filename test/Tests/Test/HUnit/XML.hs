module Tests.Test.HUnit.XML where

import Control.Monad
import Data.Word
import Distribution.TestSuite
import Test.HUnit.XML
import Test.HUnit.Reporting hiding (Node)
import Text.XML.Expat.Format
import Text.XML.Expat.Tree

import qualified Data.Map as Map

type ReporterState = [[Node String String]]
type ReporterOp = (State, ReporterState) -> IO (State, ReporterState)

initState :: State
initState = State { stName = "", stPath = [], stCounts = zeroCounts,
                    stOptions = Map.empty, stOptionDescs = [] }

setName :: String -> ReporterOp
setName name (s @ State { stName = _ }, repstate) =
  return (s { stName = name }, repstate)

pushPath :: String -> ReporterOp
pushPath name (s @ State { stPath = path }, repstate) =
  return (s { stPath = Label name : path }, repstate)

popPath :: ReporterOp
popPath (s @ State { stPath = _ : path }, repstate) =
  return (s { stPath = path }, repstate)

addOption :: String -> String -> ReporterOp
addOption key value (s @ State { stOptions = opts }, repstate) =
  return (s { stOptions = Map.insert key value opts }, repstate)

countAsserts :: Word -> ReporterOp
countAsserts count (s @ State { stCounts = c @ Counts { cAsserts = n } },
                    repstate) =
  return (s { stCounts = c { cAsserts = n + count } }, repstate)

countTried :: Word -> ReporterOp
countTried count (s @ State { stCounts = c @ Counts { cCases = cases,
                                                      cTried = tried } },
                  repstate) =
  return (s { stCounts = c { cCases = cases + count,
                             cTried = tried + count } },
          repstate)

countSkips :: Word -> ReporterOp
countSkips count (s @ State { stCounts = c @ Counts { cSkipped = skipped,
                                                      cCases = cases } },
                  repstate) =
  return (s { stCounts = c { cSkipped = skipped + count,
                             cCases = cases + count } },
          repstate)

countError :: Word -> ReporterOp
countError count (s @ State { stCounts = c @ Counts { cErrors = errors } },
                  repstate) =
  return (s { stCounts = c { cErrors = errors + count } }, repstate)

countFail :: Word -> ReporterOp
countFail count (s @ State { stCounts = c @ Counts { cFailures = failed } },
                 repstate) =
  return (s { stCounts = c { cFailures = failed + count } }, repstate)

reportSkip :: ReporterOp
reportSkip (state, repstate) =
  do
    repstate' <- (reporterSkipCase xmlReporter) state repstate
    return (state, repstate')

runReporterTest :: [ReporterOp] -> Node String String -> IO Result
runReporterTest tests expected =
  do
    initrepstate <- reporterStart xmlReporter
    (_, res) <- foldM (\state op -> op state) (initState, initrepstate) tests
    case res of
      [[actual]]
        | actual == expected -> return Pass
        | otherwise -> return (Fail ("Expected " ++ show (formatNode expected) ++
                                     " but got " ++ show (formatNode actual)))
      _ -> return (Fail ("Ending node stack had more than one item:\n" ++
                         show res))

runReporterTests :: [([ReporterOp], Node String String)] ->
                    IO Progress
runReporterTests tests =
  let
    combineResults Pass Pass = Pass
    combineResults (Error msg1) (Error msg2) = Error (msg1 ++ msg2)
    combineResults (Error msg1) (Fail msg2) = Error (msg1 ++ msg2)
    combineResults (Fail msg1) (Error msg2) = Error (msg1 ++ msg2)
    combineResults (Error msg) _ = Error msg
    combineResults _ (Error msg) = Error msg
    combineResults (Fail msg) _ = Fail msg
    combineResults _ (Fail msg) = Fail msg

    foldfun prevres (test, expected) =
      do
        res <- runReporterTest test expected
        return (combineResults res prevres)
  in do
    res <- foldM foldfun Pass tests
    return (Finished res)

reporterTestCases :: [([ReporterOp], Node String String)]
reporterTestCases = [([reportSkip], Element { eName = "skipped",
                                              eAttributes = [],
                                              eChildren = [] })]

reporterTest =
  TestInstance { name = "Reporter", options = [],
                 setOption = (\_ _ -> Right reporterTest),
                 tags = [], run = runReporterTests reporterTestCases }

tests :: Test
tests = testGroup "XML" [ Test reporterTest ]
