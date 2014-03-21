module Tests.Test.HUnit.XML where

import Control.Monad
import Data.Word
import Distribution.TestSuite
import Network.HostName
import System.IO.Unsafe
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

countSkipped :: Word -> ReporterOp
countSkipped count (s @ State { stCounts = c @ Counts { cSkipped = skipped,
                                                        cCases = cases } },
                  repstate) =
  return (s { stCounts = c { cSkipped = skipped + count,
                             cCases = cases + count } },
          repstate)

countErrors :: Word -> ReporterOp
countErrors count (s @ State { stCounts = c @ Counts { cErrors = errors } },
                   repstate) =
  return (s { stCounts = c { cErrors = errors + count } }, repstate)

countFailed :: Word -> ReporterOp
countFailed count (s @ State { stCounts = c @ Counts { cFailures = failed } },
                   repstate) =
  return (s { stCounts = c { cFailures = failed + count } }, repstate)

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

reportFailure :: String -> ReporterOp
reportFailure msg (state, repstate) =
  do
    repstate' <- (reporterFailure xmlReporter) msg state repstate
    return (state, repstate')

reportError :: String -> ReporterOp
reportError msg (state, repstate) =
  do
    repstate' <- (reporterError xmlReporter) msg state repstate
    return (state, repstate')

reportSkip :: ReporterOp
reportSkip (state, repstate) =
  do
    repstate' <- (reporterSkipCase xmlReporter) state repstate
    return (state, repstate')

reportStartCase :: ReporterOp
reportStartCase (state, repstate) =
  do
    repstate' <- (reporterStartCase xmlReporter) state repstate
    return (state, repstate')

reportEndCase :: Double -> ReporterOp
reportEndCase time (state, repstate) =
  do
    repstate' <- (reporterEndCase xmlReporter) time state repstate
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

reportStartSuite :: ReporterOp
reportStartSuite (state, repstate) =
  do
    repstate' <- (reporterStartSuite xmlReporter) state repstate
    return (state, repstate')

reportEndSuite :: Double -> ReporterOp
reportEndSuite time (state, repstate) =
  let
    removeTimestamp ((e @ Element { eAttributes = attrs } : rest) : stack) =
      (e { eAttributes = filter ((/= "timestamp") . fst) attrs } : rest) : stack
    removeTimestamp out = out
  in do
    repstate' <- (reporterEndSuite xmlReporter) time state repstate
    return (state, removeTimestamp repstate')

reporterTestCases :: [(String, [ReporterOp], Node String String)]
reporterTestCases =
  [("xmlReporter_systemErr", [reportSystemErr "Error Message Content"],
    Element { eName = "system-err", eChildren = [Text "Error Message Content"],
              eAttributes = [] }),
   ("xmlReporter_systemOut", [reportSystemOut "Message Content"],
    Element { eName = "system-out", eChildren = [Text "Message Content"],
              eAttributes = [] }),
   ("xmlReporter_failure", [reportFailure "Failure Message"],
    Element { eName = "failure", eChildren = [],
              eAttributes = [("message", "Failure Message")] }),
   ("xmlReporter_error", [reportError "Error Message"],
    Element { eName = "error", eChildren = [],
              eAttributes = [("message", "Error Message")] }),
   ("xmlReporter_skip", [setName "Test", pushPath "Path", reportSkip],
    Element { eName = "testcase",
              eChildren = [Element { eName = "skipped",
                                     eAttributes = [],
                                     eChildren = [] }],
              eAttributes = [("name", "Test"),
                             ("classname", "Path")] }),
   ("xmlReporter_empty_case",
    [setName "Test", pushPath "Path", pushPath "Inner", reportStartCase,
     countAsserts 3, reportEndCase pi],
    Element { eName = "testcase", eChildren = [],
              eAttributes = [("name", "Test"),
                             ("classname", "Path.Inner"),
                             ("assertions", show 3),
                             ("time", show pi)] }),
   ("xmlReporter_output_case",
    [setName "Test", pushPath "Path", pushPath "Inner",
     reportStartCase, countAsserts 3,
     reportSystemErr "Error Message Content", reportSystemOut "Message Content",
     reportEndCase pi],
    Element { eName = "testcase",
              eChildren = [Element { eName = "system-err",
                                     eChildren = [Text "Error Message Content"],
                                     eAttributes = [] },
                           Element { eName = "system-out",
                                     eChildren = [Text "Message Content"],
                                     eAttributes = [] }],
              eAttributes = [("name", "Test"),
                             ("classname", "Path.Inner"),
                             ("assertions", show 3),
                             ("time", show pi)] }),
   ("xmlReporter_failing_case",
    [setName "Test", pushPath "Path", pushPath "Inner",
     reportStartCase, countAsserts 3,
     reportSystemErr "Error Message Content", reportSystemOut "Message Content",
     reportFailure "Failure Message", reportEndCase pi],
    Element { eName = "testcase",
              eChildren = [Element { eName = "system-err",
                                     eChildren = [Text "Error Message Content"],
                                     eAttributes = [] },
                           Element { eName = "system-out",
                                     eChildren = [Text "Message Content"],
                                     eAttributes = [] },
                           Element { eName = "failure", eChildren = [],
                                     eAttributes = [("message",
                                                     "Failure Message")] }],
              eAttributes = [("name", "Test"),
                             ("classname", "Path.Inner"),
                             ("assertions", show 3),
                             ("time", show pi)] }),
   ("xmlReporter_error_case",
    [setName "Test", pushPath "Path", pushPath "Inner",
     reportStartCase, countAsserts 3,
     reportSystemErr "Error Message Content", reportSystemOut "Message Content",
     reportError "Error Message", reportEndCase pi],
    Element { eName = "testcase",
              eChildren = [Element { eName = "system-err",
                                     eChildren = [Text "Error Message Content"],
                                     eAttributes = [] },
                           Element { eName = "system-out",
                                     eChildren = [Text "Message Content"],
                                     eAttributes = [] },
                           Element { eName = "error", eChildren = [],
                                     eAttributes = [("message",
                                                     "Error Message")] }],
              eAttributes = [("name", "Test"),
                             ("classname", "Path.Inner"),
                             ("assertions", show 3),
                             ("time", show pi)] }),
   ("xmlReporter_empty_suite",
    [setName "Test", pushPath "Path", reportStartSuite,
     countTried 4, countSkipped 3, countErrors 2, countFailed 1,
     reportEndSuite pi],
    Element { eName = "testsuite", eChildren = [],
              eAttributes = [("name", "Test"),
                             ("hostname", hostname),
                             ("time", show pi),
                             ("tests", show 10),
                             ("failures", show 1),
                             ("errors", show 2),
                             ("skipped", show 3)] })
   ]

{-# NOINLINE hostname #-}
hostname :: String
hostname = unsafePerformIO $ getHostName

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
