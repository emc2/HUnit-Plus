module Tests.Test.HUnitPlus.Base(tests) where

import Data.List
import Debug.Trace
import Distribution.TestSuite(Test(..),
                              TestInstance(..),
                              Result(..),
                              Progress(..),
                              testGroup)
import Test.HUnitPlus.Base
import Test.HUnitPlus.Reporting

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Tests.Test.HUnitPlus.ReporterUtils as Utils

type ReportEvent = Utils.ReportEvent

loggingReporter = Utils.loggingReporter

makeTestCase :: (Test, String, [String], Counts, [ReportEvent]) -> Test
makeTestCase (Test TestInstance { name = actualName,
                                  tags = actualTags,
                                  run = runInnerTest },
              expectedName, expectedTags, expectedCounts, expectedEvents) =
  let
    initState = State { stCounts = zeroCounts, stName = "", stPath = [],
                        stOptions = Map.empty, stOptionDescs = [] }

    genResult actualCounts actualEvents =
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
        countsErr =
          if expectedCounts /= actualCounts
            then ("Expected counts " ++ show expectedCounts ++
                  " but got " ++ show actualCounts) : tagsErr
            else tagsErr
        eventsErr =
          if expectedEvents /= actualEvents
            then ("Expected reporting events " ++ show expectedEvents ++
                  " but got " ++ show actualEvents) : countsErr
            else countsErr
      in case eventsErr of
        [] -> Finished Pass
        _ -> Finished (Fail (intercalate "\n" eventsErr))

    runRealTest =
      do
        (_, State { stCounts = actualCounts }, actualEvents) <-
          executeTest loggingReporter initState [] runInnerTest
        return (genResult actualCounts actualEvents)

    testInstance = TestInstance { name = actualName, tags = [], options = [],
                                  setOption = (\_ _ -> Right testInstance),
                                  run = runRealTest }
  in
    Test testInstance

oneFail :: Counts
oneFail = zeroCounts { cFailures = 1 }

oneError :: Counts
oneError = zeroCounts { cErrors = 1 }

oneAssert :: Counts
oneAssert = zeroCounts { cAsserts = 1 }

twoFails :: Counts
twoFails = zeroCounts { cFailures = 2 }

twoErrors :: Counts
twoErrors = zeroCounts { cErrors = 2 }

twoAsserts :: Counts
twoAsserts = zeroCounts { cAsserts = 2 }

oneFailOneAssert :: Counts
oneFailOneAssert = zeroCounts { cFailures = 1, cAsserts = 1 }

twoFailsOneAssert :: Counts
twoFailsOneAssert = zeroCounts { cFailures = 2, cAsserts = 1 }

oneFailOneAssertOneError :: Counts
oneFailOneAssertOneError =
  zeroCounts { cFailures = 1, cErrors = 1, cAsserts = 1 }

oneErrorOneAssert :: Counts
oneErrorOneAssert = zeroCounts { cErrors = 1, cAsserts = 1 }

oneFailTwoAsserts :: Counts
oneFailTwoAsserts = zeroCounts { cFailures = 1, cAsserts = 2 }

twoFailsTwoAsserts :: Counts
twoFailsTwoAsserts = zeroCounts { cFailures = 2, cAsserts = 2 }

twoErrorsTwoAsserts :: Counts
twoErrorsTwoAsserts = zeroCounts { cErrors = 2, cAsserts = 2 }

externalTestPass :: Test
externalTestPass =
  let
    runTest = return (Finished Pass)

    testInstance =  TestInstance { name = "externalTestPass", tags = [],
                                   options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

externalTestEventuallyPass :: Test
externalTestEventuallyPass =
  let
    runTest = return (eventually Pass)

    testInstance =  TestInstance { name = "externalTestEventuallyPass",
                                   tags = [], options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

externalTestFail :: Test
externalTestFail =
  let
    runTest = return (Finished (Fail "External Test Failed"))

    testInstance =  TestInstance { name = "externalTestFail", tags = [],
                                   options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

externalTestEventuallyFail :: Test
externalTestEventuallyFail =
  let
    runTest = return (eventually (Fail "External Test Failed"))

    testInstance =  TestInstance { name = "externalTestEventuallyFail",
                                   tags = [], options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

externalTestError :: Test
externalTestError =
  let
    runTest = return (Finished (Error "External Test Error"))

    testInstance =  TestInstance { name = "externalTestError", tags = [],
                                   options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

externalTestEventuallyError :: Test
externalTestEventuallyError =
  let
    runTest = return (eventually (Error "External Test Error"))

    testInstance =  TestInstance { name = "externalTestEventuallyError",
                                   tags = [], options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

eventually :: Result -> Progress
eventually result =
  Progress "."
           (return (Progress ".."
                             (return (Progress "..."
                                               (return (Finished result))))))

externalTestException :: Test
externalTestException =
  let
    runTest = error "Exception Message" >> return (Finished Pass)

    testInstance =  TestInstance { name = "externalTestException",
                                   tags = [], options = [], run = runTest,
                                   setOption = (\_ _ -> return testInstance) }
  in
    Test testInstance

testCases :: [(Test, String, [String], Counts, [ReportEvent])]
testCases = [
    -- Test low-level functions
    ("logAssert" ~: logAssert, "logAssert", [], oneAssert, []),
    ("logFailure" ~: logFailure "Fail Message", "logFailure", [],
     oneFail, [Utils.Failure "Fail Message"]),
    ("logError" ~: logError "Error Message", "logError", [],
     oneError, [Utils.Error "Error Message"]),
    ("logAssert_twice" ~: do logAssert; logAssert,
     "logAssert_twice", [], twoAsserts, []),
    ("logFailure_twice" ~: do logFailure "Fail Message\n"
                              logFailure "Fail Message 2\n",
     "logFailure_twice", [], oneFail,
     [Utils.Failure "Fail Message\n", Utils.Failure "Fail Message 2\n"]),
    ("logError_twice" ~: do logError "Error Message\n"
                            logError "Error Message 2\n",
     "logError_twice", [], oneError,
     [Utils.Error "Error Message\n", Utils.Error "Error Message 2\n"]),
    -- Test assertion functions
    ("assertSuccess" ~: assertSuccess, "assertSuccess", [], oneAssert, []),
    ("assertSuccess_twice" ~: do assertSuccess; assertSuccess,
     "assertSuccess_twice", [], twoAsserts, []),
    ("assertFailure" ~: assertFailure "Fail Message", "assertFailure",
     [], oneFailOneAssert, [Utils.Failure "Fail Message"]),
    ("assertFailure_twice" ~: do assertFailure "Fail Message\n"
                                 assertFailure "Fail Message 2\n",
     "assertFailure_twice", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message\n", Utils.Failure "Fail Message 2\n"]),
    ("assertSuccess_assertFailure" ~: do assertSuccess
                                         assertFailure "Fail Message",
     "assertSuccess_assertFailure", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("assertFailure_assertSuccess" ~: do assertFailure "Fail Message"
                                         assertSuccess,
     "assertFailure_assertSuccess", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("abortError" ~: abortError "Error Message", "abortError", [], oneError,
     [Utils.Error "Error Message"]),
    ("abortFailure" ~: abortFailure "Failure Message", "abortFailure",
     [], oneFail, [Utils.Failure "Failure Message"]),
    ("assertFailure_abortError" ~: do assertFailure "Failure Message"
                                      abortError "Error Message",
     "assertFailure_abortError", [], oneFailOneAssertOneError,
     [Utils.Failure "Failure Message", Utils.Error "Error Message"]),
    ("assertFailure_abortFailure" ~: do assertFailure "Failure Message"
                                        abortFailure "Failure Message 2",
     "assertFailure_abortFailure", [], oneFailOneAssert,
     [Utils.Failure "Failure Message", Utils.Failure "Failure Message 2"]),
    ("abortError_assertFailure" ~: do abortError "Error Message"
                                      assertFailure "Failure Message",
     "abortError_assertFailure", [], oneError, [Utils.Error "Error Message"]),
    ("abortFailure_assertFailure" ~: do abortFailure "Failure Message"
                                        assertFailure "Failure Message 2",
     "abortFailure_assertFailure", [], oneFail,
     [Utils.Failure "Failure Message"]),
    ("assertBool_True" ~: assertBool "Fail Message" True,
     "assertBool_True", [], oneAssert, []),
    ("assertBool_False" ~: assertBool "Fail Message" False, "assertBool_False",
     [], oneFailOneAssert, [Utils.Failure "Fail Message"]),
    ("assertString_empty" ~: assertString "",
     "assertString_empty", [], oneAssert, []),
    ("assertString_nonempty" ~: assertString "non-empty",
     "assertString_nonempty", [], oneFailOneAssert, [Utils.Failure "non-empty"]),
    ("assertEqual_eq" ~: assertEqual "Prefix" 3 3,
     "assertEqual_eq", [], oneAssert, []),
    ("assertEqual_neq" ~: assertEqual "Prefix" 3 4,
     "assertEqual_neq", [], oneFailOneAssert,
     [Utils.Failure "Prefix\nexpected: 3\nbut got: 4"]),
    -- Assertable instances
    ("assert_Unit" ~: assert (), "assert_Unit", [], zeroCounts, []),
    ("assert_True" ~: assert True, "assert_True", [], oneAssert, []),
    ("assert_False" ~: assert False, "assert_False",
     [], oneFailOneAssert, [Utils.Failure ""]),
    ("assertWithMsg_True" ~: assertWithMsg "Message" True,
     "assertWithMsg_True", [], oneAssert, []),
    ("assertWithMsg_False" ~: assertWithMsg "Message" False,
     "assertWithMsg_False", [], oneFailOneAssert, [Utils.Failure "Message"]),
    ("assert_String_empty" ~: assert "",
     "assert_String_empty", [], oneAssert, []),
    ("assert_String_nonempty" ~: assert "non-empty", "assert_String_nonempty",
     [], oneFailOneAssert, [Utils.Failure "non-empty"]),
    ("assertWithMsg_String_empty" ~: assertWithMsg "Prefix: " "",
     "assertWithMsg_String_empty", [], oneAssert, []),
    ("assertWithMsg_String_nonempty" ~: assertWithMsg "Prefix: " "non-empty",
     "assertWithMsg_String_nonempty", [], oneFailOneAssert,
     [Utils.Failure "Prefix: non-empty"]),
    ("assert_list_assertSuccess_twice" ~: assert [assertSuccess, assertSuccess],
     "assert_list_assertSuccess_twice", [], twoAsserts, []),
    ("assert_list_assertFailure_twice" ~:
       assert [assertFailure "Fail Message\n", assertFailure "Fail Message 2\n"],
     "assert_list_assertFailure_twice", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message\n", Utils.Failure "Fail Message 2\n"]),
    ("assert_list_assertSuccess_assertFailure" ~:
       assert [assertSuccess, assertFailure "Fail Message"],
     "assert_list_assertSuccess_assertFailure", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("assert_list_assertFailure_assertSuccess" ~:
       assert [assertFailure "Fail Message", assertSuccess],
     "assert_list_assertFailure_assertSuccess", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("assertWithMsg_list_assertSuccess_twice" ~:
       assertWithMsg "Prefix: " [assertSuccess, assertSuccess],
     "assertWithMsg_list_assertSuccess_twice", [], twoAsserts, []),
    ("assertWithMsg_list_assertFailure_twice" ~:
       assertWithMsg "Prefix: " [assertFailure "Fail Message\n",
                                 assertFailure "Fail Message 2\n"],
     "assertWithMsg_list_assertFailure_twice", [], oneFailTwoAsserts,
     [Utils.Failure "Prefix: Fail Message\n",
      Utils.Failure "Prefix: Fail Message 2\n"]),
    ("assertWithMsg_list_assertSuccess_assertFailure" ~:
       assertWithMsg "Prefix: " [assertSuccess, assertFailure "Fail Message"],
     "assertWithMsg_list_assertSuccess_assertFailure", [], oneFailTwoAsserts,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_list_assertFailure_assertSuccess" ~:
       assertWithMsg "Prefix: " [assertFailure "Fail Message", assertSuccess],
     "assertWithMsg_list_assertFailure_assertSuccess", [], oneFailTwoAsserts,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assert_Pass" ~: assert Pass, "assert_Pass", [], oneAssert, []),
    ("assert_Fail" ~: assert (Fail "Fail Message"), "assert_Fail", [],
     oneFailOneAssert, [Utils.Failure "Fail Message"]),
    ("assert_Error" ~: assert (Error "Error Message"), "assert_Error", [],
     oneError, [Utils.Error "Error Message"]),
    ("assertWithMsg_Pass" ~: assertWithMsg "Prefix: " Pass,
     "assertWithMsg_Pass", [], oneAssert, []),
    ("assertWithMsg_Fail" ~: assertWithMsg "Prefix: " (Fail "Fail Message"),
     "assertWithMsg_Fail", [], oneFailOneAssert,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_Error" ~:
       assertWithMsg "Prefix: " (Error "Error Message"),
     "assertWithMsg_Error", [], oneError, [Utils.Error "Prefix: Error Message"]),
    ("assert_eventually_Pass" ~: assert (eventually Pass),
     "assert_eventually_Pass", [], oneAssert, []),
    ("assert_eventually_Fail" ~: assert (eventually (Fail "Fail Message")),
     "assert_eventually_Fail", [], oneFailOneAssert,
     [Utils.Failure "Fail Message"]),
    ("assert_eventually_Error" ~: assert (eventually (Error "Error Message")),
     "assert_eventually_Error", [], oneError, [Utils.Error "Error Message"]),
    ("assertWithMsg_eventually_Pass" ~: assert (eventually Pass),
     "assertWithMsg_eventually_Pass", [], oneAssert, []),
    ("assertWithMsg_eventually_Fail" ~:
       assertWithMsg "Prefix: " (eventually (Fail "Fail Message")),
     "assertWithMsg_eventually_Fail", [], oneFailOneAssert,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_eventually_Error" ~:
       assertWithMsg "Prefix: " (eventually (Error "Error Message")),
     "assertWithMsg_eventually_Error", [], oneError,
     [Utils.Error "Prefix: Error Message"]),
    -- Monadized assertable instances
    ("assert_return_Unit" ~: assert (return () :: IO ()),
     "assert_return_Unit", [], zeroCounts, []),
    ("assert_return_True" ~: assert (return True :: IO Bool),
     "assert_return_True", [], oneAssert, []),
    ("assert_return_False" ~: assert (return False :: IO Bool),
     "assert_return_False", [], oneFailOneAssert, [Utils.Failure ""]),
    ("assertWithMsg_return_True" ~:
       assertWithMsg "Message" (return True :: IO Bool),
     "assertWithMsg_return_True", [], oneAssert, []),
    ("assertWithMsg_return_False" ~:
       assertWithMsg "Message" (return False :: IO Bool),
     "assertWithMsg_return_False", [], oneFailOneAssert,
     [Utils.Failure "Message"]),
    ("assert_return_String_empty" ~: assert (return "" :: IO String),
     "assert_return_String_empty", [], oneAssert, []),
    ("assert_return_String_nonempty" ~: assert (return "non-empty" :: IO String),
     "assert_return_String_nonempty", [], oneFailOneAssert,
     [Utils.Failure "non-empty"]),
    ("assertWithMsg_return_String_empty" ~:
       assertWithMsg "Prefix: " (return "" :: IO String),
     "assertWithMsg_return_String_empty", [], oneAssert, []),
    ("assertWithMsg_return_String_nonempty" ~:
       assertWithMsg "Prefix: " (return "non-empty" :: IO String),
     "assertWithMsg_return_String_nonempty", [], oneFailOneAssert,
     [Utils.Failure "Prefix: non-empty"]),
    ("assert_return_list_assertSuccess_twice" ~:
       assert (return [assertSuccess, assertSuccess] :: IO [Assertion]),
     "assert_return_list_assertSuccess_twice", [], twoAsserts, []),
    ("assert_return_list_assertFailure_twice" ~:
       assert (return [assertFailure "Fail Message\n",
                       assertFailure "Fail Message 2\n"] :: IO [Assertion]),
     "assert_return_list_assertFailure_twice", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message\n", Utils.Failure "Fail Message 2\n"]),
    ("assert_return_list_assertSuccess_assertFailure" ~:
       assert (return [assertSuccess,
                       assertFailure "Fail Message"] :: IO [Assertion]),
     "assert_return_list_assertSuccess_assertFailure", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("assert_return_list_assertFailure_assertSuccess" ~:
       assert (return [assertFailure "Fail Message",
                       assertSuccess] :: IO [Assertion]),
     "assert_return_list_assertFailure_assertSuccess", [], oneFailTwoAsserts,
     [Utils.Failure "Fail Message"]),
    ("assertWithMsg_return_list_assertSuccess_twice" ~:
       assertWithMsg "Prefix: " (return [assertSuccess, assertSuccess]
                                 :: IO [Assertion]),
     "assertWithMsg_return_list_assertSuccess_twice", [], twoAsserts, []),
    ("assertWithMsg_return_list_assertFailure_twice" ~:
       assertWithMsg "Prefix: " (return [assertFailure "Fail Message\n",
                                         assertFailure "Fail Message 2\n"]
                                 :: IO [Assertion]),
     "assertWithMsg_return_list_assertFailure_twice", [], oneFailTwoAsserts,
     [Utils.Failure "Prefix: Fail Message\n",
      Utils.Failure "Prefix: Fail Message 2\n"]),
    ("assertWithMsg_return_list_assertSuccess_assertFailure" ~:
       assertWithMsg "Prefix: "(return [assertSuccess,
                                        assertFailure "Fail Message"]
                                :: IO [Assertion]),
     "assertWithMsg_return_list_assertSuccess_assertFailure",
     [], oneFailTwoAsserts, [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_return_list_assertFailure_assertSuccess" ~:
       assertWithMsg "Prefix: " (return [assertFailure "Fail Message",
                                         assertSuccess] :: IO [Assertion]),
     "assertWithMsg_return_list_assertFailure_assertSuccess",
     [], oneFailTwoAsserts, [Utils.Failure "Prefix: Fail Message"]),
    ("assert_return_Pass" ~: assert (return Pass :: IO Result),
     "assert_return_Pass", [], oneAssert, []),
    ("assert_return_Fail" ~: assert (return (Fail "Fail Message") :: IO Result),
     "assert_return_Fail", [], oneFailOneAssert, [Utils.Failure "Fail Message"]),
    ("assert_return_Error" ~:
       assert (return (Error "Error Message") :: IO Result),
     "assert_return_Error", [], oneError, [Utils.Error "Error Message"]),
    ("assertWithMsg_return_Pass" ~:
       assertWithMsg "Message" (return Pass :: IO Result),
     "assertWithMsg_return_Pass", [], oneAssert, []),
    ("assertWithMsg_return_Fail" ~:
       assertWithMsg "Prefix: " (return (Fail "Fail Message") :: IO Result),
     "assertWithMsg_return_Fail", [], oneFailOneAssert,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_return_Error" ~:
       assertWithMsg "Prefix: " (return (Error "Error Message") :: IO Result),
     "assertWithMsg_return_Error", [], oneError,
     [Utils.Error "Prefix: Error Message"]),
    ("assert_return_eventually_Pass" ~:
       assert (return (eventually Pass) :: IO Progress),
     "assert_return_eventually_Pass", [], oneAssert, []),
    ("assert_return_eventually_Fail" ~:
       assert (return (eventually (Fail "Fail Message")) :: IO Progress),
     "assert_return_eventually_Fail", [], oneFailOneAssert,
     [Utils.Failure "Fail Message"]),
    ("assert_return_eventually_Error" ~:
       assert (return (eventually (Error "Error Message")) :: IO Progress),
     "assert_return_eventually_Error", [], oneError,
     [Utils.Error "Error Message"]),
    ("assertWithMsg_return_eventually_Pass" ~:
       assertWithMsg "Prefix: " (return (eventually Pass) :: IO Progress),
     "assertWithMsg_return_eventually_Pass", [], oneAssert, []),
    ("assertWithMsg_return_eventually_Fail" ~:
       assertWithMsg "Prefix: " (return (eventually (Fail "Fail Message"))
                                 :: IO Progress),
     "assertWithMsg_return_eventually_Fail", [], oneFailOneAssert,
     [Utils.Failure "Prefix: Fail Message"]),
    ("assertWithMsg_return_eventually_Error" ~:
       assertWithMsg "Prefix: " (return (eventually (Error "Error Message"))
                                 :: IO Progress),
     "assertWithMsg_return_eventually_Error", [], oneError,
     [Utils.Error "Prefix: Error Message"]),
    -- Assertion operators
    ("assertWithMsg_operator_Pass" ~: True @? "Message",
     "assertWithMsg_operator_Pass", [], oneAssert, []),
    ("assertWithMsg_operator_False" ~: False @? "Message",
     "assertWithMsg_operator_False", [], oneFailOneAssert,
     [Utils.Failure "Message"]),
    ("assertWithMsg_operator_Fail" ~: Fail "Message" @? "Prefix: ",
     "assertWithMsg_operator_Fail", [], oneFailOneAssert,
     [Utils.Failure "Prefix: Message"]),
    ("assertWithMsg_operator_Error" ~: Error "Message" @? "Prefix: ",
     "assertWithMsg_operator_Error", [], oneError,
     [Utils.Error "Prefix: Message"]),
    ("assertEqual_operator_eq" ~: 10 @=? 10,
     "assertEqual_operator_eq", [], oneAssert, []),
    ("assertEqual_operator_neq" ~: 0 @=? 1, "assertEqual_operator_neq",
     [], oneFailOneAssert, [Utils.Failure "expected: 0\nbut got: 1"]),
    ("assertEqual_operator2_eq" ~: 7 @?= 7,
     "assertEqual_operator2_eq", [], oneAssert, []),
    ("assertEqual_operator2_neq" ~: 11 @=? 8, "assertEqual_operator2_neq",
     [], oneFailOneAssert, [Utils.Failure "expected: 11\nbut got: 8"]),
    -- Test-building functions
    ("HUnit_Test_empty" ~: (return () :: IO ()),
     "HUnit_Test_empty", [], zeroCounts, []),
    ("HUnit_Test_Progress_Pass" ~: (return (Finished Pass) :: IO Progress),
     "HUnit_Test_Progress_Pass", [], oneAssert, []),
    ("HUnit_Test_Progress_Fail" ~:
       (return (Finished (Fail "Fail Message")) :: IO Progress),
     "HUnit_Test_Progress_Fail", [], oneFailOneAssert,
     [Utils.Failure "Fail Message"]),
    ("HUnit_Test_Progress_Error" ~:
       (return (Finished (Error "Error Message")) :: IO Progress),
     "HUnit_Test_Progress_Error", [], oneError, [Utils.Error "Error Message"]),
    ("HUnit_Test_Progress_eventually_Pass" ~:
       (return (eventually Pass) :: IO Progress),
     "HUnit_Test_Progress_eventually_Pass", [], oneAssert, []),
    ("HUnit_Test_Progress_eventually_Fail" ~:
       (return (eventually (Fail "Fail Message")) :: IO Progress),
     "HUnit_Test_Progress_eventually_Fail", [], oneFailOneAssert,
     [Utils.Failure "Fail Message"]),
    ("HUnit_Test_Progress_eventually_Error" ~:
       (return (eventually (Error "Error Message")) :: IO Progress),
     "HUnit_Test_Progress_eventually_Error", [], oneError,
     [Utils.Error "Error Message"]),
    ("HUnit_Test_Bool_True" ~: (return True :: IO Bool),
     "HUnit_Test_Bool_True", [], oneAssert, []),
    ("HUnit_Test_Bool_False" ~: (return False :: IO Bool),
     "HUnit_Test_Bool_False", [], oneFailOneAssert, [Utils.Failure ""]),
    ("HUnit_Test_with_msg_True" ~: (True ~? "Failure Message"),
     "HUnit_Test_with_msg_True", [], oneAssert, []),
    ("HUnit_Test_with_msg_False" ~: (False ~? "Fail Message"),
     "HUnit_Test_with_msg_False", [], oneFailOneAssert,
     [Utils.Failure "Fail Message"]),
    ("HUnit_Eq_Test_eq" ~: (4 ~?= 4), "HUnit_Eq_Test_eq", [], oneAssert, []),
    ("HUnit_Eq_Test_neq" ~: (4 ~?= 5), "HUnit_Eq_Test_neq", [],
     oneFailOneAssert, [Utils.Failure "expected: 5\nbut got: 4"]),
    ("HUnit_Eq_Test2_eq" ~: (3 ~=? 3), "HUnit_Eq_Test2_eq", [], oneAssert, []),
    ("HUnit_Eq_Test2_neq" ~: (3 ~=? 2), "HUnit_Eq_Test2_neq", [],
     oneFailOneAssert, [Utils.Failure "expected: 3\nbut got: 2"]),
    (externalTestPass, "externalTestPass", [], zeroCounts, []),
    (externalTestEventuallyPass, "externalTestEventuallyPass", [], zeroCounts,
     [Utils.Progress ".", Utils.Progress "..", Utils.Progress "..."]),
    (externalTestFail, "externalTestFail", [], oneFail,
     [Utils.Failure "External Test Failed"]),
    (externalTestEventuallyFail, "externalTestEventuallyFail", [], oneFail,
     [Utils.Progress ".", Utils.Progress "..", Utils.Progress "...",
      Utils.Failure "External Test Failed"]),
    (externalTestError, "externalTestError", [], oneError,
     [Utils.Error "External Test Error"]),
    (externalTestEventuallyError, "externalTestEventuallyError", [], oneError,
     [Utils.Progress ".", Utils.Progress "..", Utils.Progress "...",
      Utils.Error "External Test Error"]),
    (externalTestException, "externalTestException", [], oneError,
     [Utils.Error "Uncaught exception in test: Exception Message"]),
    -- Test manipulation of tags and test names
    (testName "newName" (test (return () :: IO ())),
     "newName", [], zeroCounts, []),
    (testTags ["tag", "gat"] ("newTags" ~: (return () :: IO ())),
     "newTags", ["tag", "gat"], zeroCounts, []),
    (testNameTags "newNameTags" ["tag", "gat"] (test (return () :: IO ())),
     "newNameTags", ["tag", "gat"], zeroCounts, [])
  ]
{-
miscTests :: [Test]
miscTests =
  let
    anonTest1 = test True
    anonTest2 = test True

    tagsTest1 = testTags ["tag"] True
    tagsTest2 = testTags ["tag", "gat"] True

    nameTest = testName "test1" True

    anonTestCreate =
      TestInstance { name = "anonTestCreate", tags = [], options = [],
                     setOption = (\_ _ -> anonTestCreate),
                     run = if (name anonTest1) /= (name anonTest2)
                             then return (Finished Pass)
                             else
                               return (Finished (Fail "expected unique names")) }
    tagsTestCreate
  in
    [ Test anontestCreate, Test tagsTestCreate ]
-}
tests :: Test
tests = testGroup "Base" (map makeTestCase testCases)
