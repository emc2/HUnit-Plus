module Tests.Test.HUnit.Main where

import Data.List
import Distribution.TestSuite
import System.Directory
import Test.HUnit.Main
import Test.HUnit.Base

makeMainTest :: (String, IO (), IO (), Bool, [TestSuite], Opts) -> Test
makeMainTest (name, setup, cleanup, shouldPass, suites, opts) =
  let
    runTest =
      do
        setup
        out <- topLevel suites opts
        cleanup
        case out of
          Left msgs ->
            if not shouldPass
              then return (Finished Pass)
              else return (Finished (Fail ("Expected test to pass, " ++
                                           "but failed with" ++
                                           intercalate "\n" msgs)))
          Right _ ->
            if shouldPass
              then return (Finished Pass)
              else return (Finished (Fail "Expected test to fail"))
    testInstance = TestInstance { name = name, tags = [], options = [],
                                  setOption = (\_ _ -> return testInstance),
                                  run = runTest }
  in
    Test testInstance

makeTestDir :: IO ()
makeTestDir = createDirectory "TestDir"

delTestDir :: IO ()
delTestDir = removeDirectory "TestDir"

delXMLReport :: IO ()
delXMLReport = removeFile "TestDir/report.xml" >> delTestDir

delTxtReport :: IO ()
delTxtReport = removeFile "TestDir/report.txt" >> delTestDir

delTxtXMLReport :: IO ()
delTxtXMLReport = removeFile "TestDir/report.xml" >>
                  removeFile "TestDir/report.txt" >>
                  delTestDir

quietOpts = opts { consmode = [Quiet] }

filterOpts = [ (False, False), (True, False), (False, True), (True, True) ]

suites = [ TestSuite { suiteName = "Suite1", suiteConcurrently = False,
                       suiteOptions = [], suiteTests = suiteTestList },
           TestSuite { suiteName = "Suite2", suiteConcurrently = False,
                       suiteOptions = [], suiteTests = suiteTestList } ]

suiteCombos =
  foldl (\accum filters -> (map (\suite -> (suite, filters)) suites) : accum)
        [] filterOpts

makeFilter suitename (False, False) = []
makeFilter suitename (True, False) = ["[" ++ suitename ++ "]Pass"]
makeFilter suitename (False, True) = ["[" ++ suitename ++ "]Fail"]
makeFilter suitename (True, True) = ["[" ++ suitename ++ "]Pass",
                                     "[" ++ suitename ++ "]Fail"]

makeFilters suitedata =
  foldl (\accum (TestSuite { suiteName = suitename }, filters) ->
          makeFilter suitename filters ++ accum) [] suitedata

shouldPass suitedata = not (all (\(_, (a, b)) -> not a && not b) suitedata) &&
                       not (any (\(_, (_, fail)) -> fail) suitedata)

suiteTestList = [ "Pass" ~: assertSuccess, "Fail" ~: assertFailure "Fail" ]

makeName suitedata =
  intercalate "__"
    (foldl (\accum (TestSuite { suiteName = name }, (pass, fail)) ->
             (name ++ "_" ++ show pass ++ "_" ++ show fail) : accum)
           [] suitedata)

makeCmdOptTest suitedata =
  ("cmdopt___" ++ makeName suitedata, return (), return (), shouldPass suitedata,
   map fst suitedata, quietOpts { filters = makeFilters suitedata })

makeTestlistTest suitedata =
  let
    createFilterFile =
      do
        createDirectory "TestDir"
        writeFile "TestDir/testlist" (intercalate "\n" (makeFilters suitedata))

    delFilterFile = removeFile "TestDir/testlist" >> delTestDir
  in   
    ("testlist___" ++ makeName suitedata, createFilterFile, delFilterFile,
     shouldPass suitedata, map fst suitedata,
     quietOpts { testlist = ["testlist"] })

mainTests = [
    ("multiple_console_mode", return (), return (), False, [],
     opts { consmode = [Quiet, Terminal] }),
    ("multiple_xml_report", return (), return (), False, [],
     opts { xmlreport = ["report1.xml", "report2.xml"] }),
    ("multiple_txt_report", return (), return (), False, [],
     opts { txtreport = ["report1.txt", "report2.txt"] }),
    ("multiple_xml_txt_report", return (), return (), False, [],
     opts { xmlreport = ["report1.xml", "report2.xml"],
            txtreport = ["report1.txt", "report2.txt"] }),
    ("nonexistent_xml_report", makeTestDir, delTestDir, False, [],
     opts { xmlreport = ["TestDir/nonexistent/report.xml"] }),
    ("nonexistent_txt_report", makeTestDir, delTestDir, False, [],
     opts { txtreport = ["TestDir/nonexistent/report.txt"] }),
    ("nonexistent_txt_xml_report", makeTestDir, delTestDir, False, [],
     opts { txtreport = ["TestDir/nonexistent/report.txt"],
            xmlreport = ["TestDir/nonexistent/report.xml"] }),
    ("run_quiet_no_xml_no_txt", return (), return (), True, [], quietOpts),
    ("run_terminal_no_xml_no_txt", return (), return (), True, [],
     opts { consmode = [Terminal] }),
    ("run_text_no_xml_no_txt", return (), return (), True, [],
     opts { consmode = [Text] }),
    ("run_verbose_no_xml_no_txt", return (), return (), True, [],
     opts { consmode = [Verbose] }),
    ("run_quiet_xml_no_txt", makeTestDir, delXMLReport, True, [],
     opts { consmode = [Quiet], xmlreport = ["TestDir/report.xml"] }),
    ("run_terminal_xml_no_txt", makeTestDir, delXMLReport, True, [],
     opts { consmode = [Terminal], xmlreport = ["TestDir/report.xml"] }),
    ("run_text_xml_no_txt", makeTestDir, delXMLReport, True, [],
     opts { consmode = [Text], xmlreport = ["TestDir/report.xml"] }),
    ("run_verbose_xml_no_txt", makeTestDir, delXMLReport, True, [],
     opts { consmode = [Verbose], xmlreport = ["TestDir/report.xml"] }),
    ("run_quiet_no_xml_txt", makeTestDir, delTxtReport, True, [],
     opts { consmode = [Quiet], txtreport = ["TestDir/report.txt"] }),
    ("run_terminal_no_xml_txt", makeTestDir, delTxtReport, True, [],
     opts { consmode = [Terminal], txtreport = ["TestDir/report.txt"] }),
    ("run_text_no_xml_txt", makeTestDir, delTxtReport, True, [],
     opts { consmode = [Text], txtreport = ["TestDir/report.txt"] }),
    ("run_verbose_no_xml_txt", makeTestDir, delTxtReport, True, [],
     opts { consmode = [Verbose], txtreport = ["TestDir/report.txt"] }),
    ("run_quiet_xml_no_txt", makeTestDir, delTxtXMLReport, True, [],
     opts { consmode = [Quiet], xmlreport = ["TestDir/report.xml"],
            txtreport = ["TestDir/report.txt"] }),
    ("run_terminal_xml_no_txt", makeTestDir, delTxtXMLReport, True, [],
     opts { consmode = [Terminal], xmlreport = ["TestDir/report.xml"],
            txtreport = ["TestDir/report.txt"] }),
    ("run_text_xml_no_txt", makeTestDir, delTxtXMLReport, True, [],
     opts { consmode = [Text], xmlreport = ["TestDir/report.xml"],
            txtreport = ["TestDir/report.txt"] }),
    ("run_verbose_xml_no_txt", makeTestDir, delTxtXMLReport, True, [],
     opts { consmode = [Verbose], xmlreport = ["TestDir/report.xml"],
            txtreport = ["TestDir/report.txt"] })
  ] ++
  map makeCmdOptTest suiteCombos ++
  map makeTestlistTest suiteCombos

tests :: Test
tests = testGroup "Main" (map makeMainTest mainTests)
