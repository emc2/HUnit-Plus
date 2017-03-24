{-# LANGUAGE OverloadedStrings #-}

module Tests.Test.HUnitPlus.XML(tests) where

import Distribution.TestSuite
import Network.HostName
import System.IO.Unsafe
import Test.HUnitPlus.XML
import Text.XML.Expat.Format
import Text.XML.Expat.Tree

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as Strict
import qualified Tests.Test.HUnitPlus.ReporterUtils as Utils

type ReporterState = [[Node Strict.Text Strict.Text]]
type ReporterOp = Utils.ReporterOp ReporterState

countAsserts = Utils.countAsserts
countTried = Utils.countTried
countSkipped = Utils.countSkipped
countErrors = Utils.countErrors
countFailed = Utils.countFailed
setOpt = Utils.setOpt
setName = Utils.setName
pushPath = Utils.pushPath
popPath = Utils.popPath
reportSystemErr = Utils.reportSystemErr xmlReporter
reportSystemOut = Utils.reportSystemOut xmlReporter
reportFailure = Utils.reportFailure xmlReporter
reportError = Utils.reportError xmlReporter
reportSkip = Utils.reportSkip xmlReporter
reportStartCase = Utils.reportStartCase xmlReporter
reportEndCase = Utils.reportEndCase xmlReporter
runReporterTest = Utils.runReporterTest xmlReporter
reportStartSuite = Utils.reportStartSuite xmlReporter
reportEnd = Utils.reportEnd xmlReporter

reportEndSuite :: Double -> ReporterOp
reportEndSuite time state =
  let
    removeTimestamp ((e @ Element { eAttributes = attrs } : rest) : stack) =
      (e { eAttributes = filter ((/= "timestamp") . fst) attrs } : rest) : stack
    removeTimestamp out = out
  in do
    (state, repstate) <- Utils.reportEndSuite xmlReporter time state
    return (state, removeTimestamp repstate)

reporterTestCases :: [(String, [ReporterOp], Node Strict.Text Strict.Text)]
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
                             ("assertions", Strict.pack (show 3)),
                             ("time", Strict.pack (show pi))] }),
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
                             ("assertions", Strict.pack (show 3)),
                             ("time", Strict.pack (show pi))] }),
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
                             ("assertions", Strict.pack (show 3)),
                             ("time", Strict.pack (show pi))] }),
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
                             ("assertions", Strict.pack (show 3)),
                             ("time", Strict.pack (show pi))] }),
   ("xmlReporter_empty_suite",
    [setName "Test", pushPath "Path", reportStartSuite,
     countTried 4, countSkipped 3, countErrors 2, countFailed 1,
     reportEndSuite pi],
    Element { eName = "testsuite", eChildren = [],
              eAttributes = [("name", "Test"),
                             ("hostname", Strict.pack hostname),
                             ("time", Strict.pack (show pi)),
                             ("tests", Strict.pack (show 7)),
                             ("failures", Strict.pack (show 1)),
                             ("errors", Strict.pack (show 2)),
                             ("skipped", Strict.pack (show 3))] }),
   ("xmlReporter_options_suite",
    [setName "Test", pushPath "Path", reportStartSuite,
     setOpt "option1" "value1", setOpt "option2" "value2",
     countTried 4, countSkipped 3, countErrors 2, countFailed 1,
     reportEndSuite pi],
    Element { eName = "testsuite",
              eChildren =
                [Element { eName = "properties", eAttributes = [],
                           eChildren =
                             [Element { eName = "property", eChildren = [],
                                        eAttributes = [("name", "option1"),
                                                       ("value", "value1")] },
                              Element { eName = "property", eChildren = [],
                                        eAttributes = [("name", "option2"),
                                                       ("value", "value2")] }
                             ] }],
              eAttributes = [("name", "Test"),
                             ("hostname", Strict.pack hostname),
                             ("time", Strict.pack (show pi)),
                             ("tests", Strict.pack (show 7)),
                             ("failures", Strict.pack (show 1)),
                             ("errors", Strict.pack (show 2)),
                             ("skipped", Strict.pack (show 3))] }),
   ("xmlReporter_content_suite",
    [setName "Test", pushPath "Path", reportStartSuite,
     countTried 4, setName "Pass", reportStartCase, countAsserts 3,
     reportSystemErr "Error Message Content", reportSystemOut "Message Content",
     reportEndCase (pi - 1),
     setName "Skip", pushPath "Inner", reportSkip, countSkipped 1, popPath,
     setName "Fail", countErrors 1, reportStartCase, countAsserts 4,
     reportFailure "Failure Message", reportEndCase (pi / 2),
     setName "Error", countFailed 1, reportStartCase, countAsserts 2,
     reportError "Error Message", reportEndCase pi,
     reportSystemErr "Suite Error Message", reportSystemOut "Suite Message",
     setName "Test", reportEndSuite (pi * pi)],
    Element { eName = "testsuite",
              eAttributes = [("name", "Test"),
                             ("hostname", Strict.pack hostname),
                             ("time", Strict.pack (show (pi * pi))),
                             ("tests", Strict.pack (show 5)),
                             ("failures", Strict.pack (show 1)),
                             ("errors", Strict.pack (show 1)),
                             ("skipped", Strict.pack (show 1))],
              eChildren =
                [Element { eName = "testcase",
                           eChildren =
                             [Element { eName = "system-err",
                                        eChildren =
                                          [Text "Error Message Content"],
                                        eAttributes = [] },
                              Element { eName = "system-out",
                                        eChildren = [Text "Message Content"],
                                        eAttributes = [] }],
                           eAttributes = [("name", "Pass"),
                                          ("classname", "Path"),
                                          ("assertions", Strict.pack (show 3)),
                                          ("time", Strict.pack (show (pi - 1)))] },
                 Element { eName = "testcase",
                           eChildren = [Element { eName = "skipped",
                                                  eAttributes = [],
                                                  eChildren = [] }],
                           eAttributes = [("name", "Skip"),
                                          ("classname", "Path.Inner")] },
                 Element { eName = "testcase",
                           eChildren =
                             [Element { eName = "failure", eChildren = [],
                                        eAttributes = [("message",
                                                        "Failure Message")] }],
                           eAttributes = [("name", "Fail"),
                                          ("classname", "Path"),
                                          ("assertions", Strict.pack (show 4)),
                                          ("time", Strict.pack (show (pi / 2)))] },
                 Element { eName = "testcase",
                           eChildren =
                             [Element { eName = "error", eChildren = [],
                                        eAttributes = [("message",
                                                        "Error Message")] }],
                           eAttributes = [("name", "Error"),
                                          ("classname", "Path"),
                                          ("assertions", Strict.pack (show 2)),
                                          ("time", Strict.pack (show pi))] },
                 Element { eName = "system-err",
                           eChildren = [Text "Suite Error Message"],
                           eAttributes = [] },
                 Element { eName = "system-out",
                           eChildren = [Text "Suite Message"],
                           eAttributes = [] }
                 ] }),
   ("xmlReporter_empty_suites", [reportEnd pi],
    Element { eName = "testsuites", eChildren = [],
              eAttributes = [("time", Strict.pack (show pi))] }),
   ("xmlReporter_content_suites",
    [setName "Empty", reportStartSuite,
     countTried 5, countSkipped 4, countErrors 2, countFailed 1,
     reportEndSuite pi,
     setName "Test", pushPath "Path", reportStartSuite,
     setOpt "option1" "value1", setOpt "option2" "value2",
     countTried 4, setName "Pass", reportStartCase, countAsserts 3,
     reportSystemErr "Error Message Content", reportSystemOut "Message Content",
     reportEndCase (pi - 1),
     setName "Skip", pushPath "Inner", reportSkip, countSkipped 1, popPath,
     setName "Fail", countErrors 1, reportStartCase, countAsserts 4,
     reportFailure "Failure Message", reportEndCase (pi / 2),
     setName "Error", countFailed 1, reportStartCase, countAsserts 2,
     reportError "Error Message", reportEndCase pi,
     reportSystemErr "Suite Error Message", reportSystemOut "Suite Message",
     setName "Test", reportEndSuite (pi * pi), reportEnd (sqrt pi)],
    Element {
      eName = "testsuites", eAttributes = [("time", Strict.pack (show (sqrt pi)))],
      eChildren =
        [Element {
            eName = "testsuite", eChildren = [],
            eAttributes = [("name", "Empty"),
                           ("hostname", Strict.pack hostname),
                           ("time", Strict.pack (show pi)),
                           ("tests", Strict.pack (show 9)),
                           ("failures", Strict.pack (show 1)),
                           ("errors", Strict.pack (show 2)),
                           ("skipped", Strict.pack (show 4))] },
         Element {
           eName = "testsuite",
           eAttributes = [("name", "Test"),
                          ("hostname", Strict.pack hostname),
                          ("time", Strict.pack (show (pi * pi))),
                          ("tests", Strict.pack (show 14)),
                          ("failures", Strict.pack (show 2)),
                          ("errors", Strict.pack (show 3)),
                          ("skipped", Strict.pack (show 5))],
           eChildren =
             [Element { eName = "properties", eAttributes = [],
                           eChildren =
                             [Element { eName = "property", eChildren = [],
                                        eAttributes = [("name", "option1"),
                                                       ("value", "value1")] },
                              Element { eName = "property", eChildren = [],
                                        eAttributes = [("name", "option2"),
                                                       ("value", "value2")] }
                             ] },
              Element {
                 eName = "testcase",
                 eChildren =
                   [Element { eName = "system-err",
                              eChildren =
                                [Text "Error Message Content"],
                              eAttributes = [] },
                    Element { eName = "system-out",
                              eChildren = [Text "Message Content"],
                              eAttributes = [] }],
                 eAttributes = [("name", "Pass"),
                                ("classname", "Path"),
                                ("assertions", Strict.pack (show 3)),
                                ("time", Strict.pack (show (pi - 1)))] },
              Element { eName = "testcase",
                        eChildren = [Element { eName = "skipped",
                                               eAttributes = [],
                                               eChildren = [] }],
                        eAttributes = [("name", "Skip"),
                                       ("classname", "Path.Inner")] },
              Element { eName = "testcase",
                        eChildren =
                          [Element { eName = "failure", eChildren = [],
                                     eAttributes = [("message",
                                                     "Failure Message")] }],
                        eAttributes = [("name", "Fail"),
                                       ("classname", "Path"),
                                       ("assertions", Strict.pack (show 4)),
                                       ("time", Strict.pack (show (pi / 2)))] },
              Element { eName = "testcase",
                        eChildren =
                          [Element { eName = "error", eChildren = [],
                                     eAttributes = [("message",
                                                     "Error Message")] }],
                        eAttributes = [("name", "Error"),
                                       ("classname", "Path"),
                                       ("assertions", Strict.pack (show 2)),
                                       ("time", Strict.pack (show pi))] },
              Element { eName = "system-err",
                        eChildren = [Text "Suite Error Message"],
                        eAttributes = [] },
              Element { eName = "system-out",
                        eChildren = [Text "Suite Message"],
                        eAttributes = [] }
             ] }
        ] })
   ]

{-# NOINLINE hostname #-}
hostname :: String
hostname = unsafePerformIO $ getHostName

genReporterTest :: (String, [ReporterOp], Node Strict.Text Strict.Text) -> Test
genReporterTest (name, op, expected) =
  let
    format :: [[Node Strict.Text Strict.Text]] -> String
    format = concat . map (concat . map (BC.unpack . formatNode . indent 2))

    out = TestInstance { name = name, tags = [], options = [],
                         setOption = (\_ _ -> Right out),
                         run = runReporterTest op [[expected]] format >>=
                               return . Finished }
  in
    Test out

tests :: Test
tests = testGroup "XML" (map genReporterTest reporterTestCases)
