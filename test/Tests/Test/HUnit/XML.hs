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

reportSystemErr :: String -> ReporterOp
reportSystemErr msg (state, repstate) =
  do
    repstate' <- (reporterSystemErr xmlReporter) msg state repstate
    return (state, repstate')

reportSystemOut :: String -> ReporterOp
reportSystemOut msg (state, repstate) =
  do
    repstate' <- (reporterSystemOut xmlReporter) msg state repstate
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

reporterTestCases :: [(String, [ReporterOp], Node String String)]
reporterTestCases =
  [("xmlReporter_skip", [setName "Test", pushPath "Path", reportSkip],
    Element { eName = "testcase",
              eChildren = [Element { eName = "skipped",
                                     eAttributes = [],
                                     eChildren = [] }],
              eAttributes = [("name", "Test"),
                             ("classname", "Path")] }),
   ("xmlReporter_systemErr", [reportSystemErr "Error Message Content"],
    Element { eName = "system-err", eChildren = [Text "Error Message Content"],
              eAttributes = [] }),
   ("xmlReporter_systemOut", [reportSystemErr "Message Content"],
    Element { eName = "system-out", eChildren = [Text "Message Content"],
              eAttributes = [] })
  ]

genReporterTest :: (String, [ReporterOp], Node String String) -> Test
genReporterTest (name, op, expected) =
  let
    out = TestInstance { name = name, tags = [], options = [],
                         setOption = (\_ _ -> Right out),
                         run = runReporterTest op expected >>=
                               return . Finished }
  in
    Test out

tests :: Test
tests = testGroup "XML" (map genReporterTest reporterTestCases)
