module Tests.Test.HUnit.Main where

import Distribution.TestSuite
import System.Directory
import Test.HUnit.Main

makeFailTest :: (String, IO (), IO (), Opts) -> Test
makeFailTest (name, setup, cleanup, opts) =
  let
    runTest =
      do
        setup
        out <- topLevel [] opts
        cleanup
        case out of
          Left _ -> return (Finished Pass)
          Right _ -> return (Finished (Fail "Expected test to fail"))

    testInstance = TestInstance { name = name, tags = [], options = [],
                                  setOption = (\_ _ -> return testInstance),
                                  run = runTest }
  in
    Test testInstance

makeTestDir :: IO ()
makeTestDir = createDirectory "TestDir"

delTestDir :: IO ()
delTestDir = removeDirectory "TestDir"

failTests = [
    ("multiple_console_mode", return (), return (),
     opts { consmode = [Quiet, Terminal] }),
    ("multiple_xml_report", return (), return (),
     opts { xmlreport = ["report1.xml", "report2.xml"] }),
    ("multiple_txt_report", return (), return (),
     opts { txtreport = ["report1.txt", "report2.txt"] }),
    ("multiple_xml_txt_report", return (), return (),
     opts { xmlreport = ["report1.xml", "report2.xml"],
            txtreport = ["report1.txt", "report2.txt"] }),
    ("nonexistent_xml_report", makeTestDir, delTestDir,
     opts { xmlreport = ["TestDir/nonexistent/report.xml"] }),
    ("nonexistent_txt_report", makeTestDir, delTestDir,
     opts { txtreport = ["TestDir/nonexistent/report.txt"] }),
    ("nonexistent_txt_xml_report", makeTestDir, delTestDir,
     opts { txtreport = ["TestDir/nonexistent/report.txt"],
            xmlreport = ["TestDir/nonexistent/report.xml"] })
  ]

tests :: Test
tests = testGroup "Main" (map makeFailTest failTests)
