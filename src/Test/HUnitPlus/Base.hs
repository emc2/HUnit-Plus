{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

-- | Basic definitions for the HUnitPlus library.
--
-- This module contains what you need to create assertions and test
-- cases and combine them into test suites.
--
-- The assertion and test definition operators are the same as those
-- found in the HUnit library.  However, an important note is that the
-- behavior of assertions in HUnit-Plus differs from those in HUnit.
-- HUnit-Plus assertions do /not/ stop executing a test if they fail,
-- and are designed so that multiple assertions can be made by a
-- single test.  HUnit-Plus contains several \"abort\" functions,
-- which can be used to terminate a test immediately.
--
-- HUnit-Plus test execution handles exceptions.  An uncaught
-- exception will cause the test to report an error (along with any
-- failures and/or errors that have occurred so far), and test
-- execution and reporting will continue.
--
-- The data structures for describing tests are the same as those in
-- the "Distribution.TestSuite" module used by cabal's testing
-- facilities.  This allows for easy interfacing with cabal's
-- @detailed@ testing scheme.
--
-- This gives rise to a grid possible use cases: creating a test using
-- the HUnit-Plus facilities vs. executing an existing
-- "Distribution.TestSuite" test which was not created by the
-- facilities in this module, and executing a test with HUnit-Plus vs
-- executing it with another test framework.  The 'executeTest'
-- function is designed to cope with either possible origin of a test,
-- and the 'Testable' instances are designed to produce tests which
-- work as expected in either possible execution environment.
module Test.HUnitPlus.Base(
       -- * Test Definition
       Test(..),
       TestInstance(..),
       TestSuite(..),
       testSuite,

       -- ** Extended Test Creation
       Testable(..),

       (~=?),
       (~?=),
       (~:),
       (~?),


       -- * Assertions
       Assertion,
       assertSuccess,
       assertFailure,
       abortFailure,
       abortError,
       assertBool,
       assertString,
       assertStringWithPrefix,
       assertEqual,
       -- ** Extended Assertion Functionality
       Assertable(..),
       (@=?),
       (@?=),
       (@?),

       -- * Low-level Test Functions
       executeTest,
       logAssert,
       logFailure,
       logError,
       withPrefix,
       getErrors,
       getFailures
       ) where

import Control.Exception hiding (assert)
import Data.Foldable
import Data.IORef
import Data.Typeable
import Data.Word
import Distribution.TestSuite
import Prelude hiding (concat, sum, sequence_)
import System.IO.Unsafe
import System.TimeIt
import Test.HUnitPlus.Reporting

-- | An 'Exception' used to abort test execution immediately.
data TestException =
  TestException {
    -- | Whether this is a failure or an error.
    teError :: !Bool,
    -- | The failure (or error) message.
    teMsg :: !String
  } deriving (Show, Typeable)

instance Exception TestException

-- Test Wrapper Definition
-- =====================
data TestInfo =
  TestInfo {
    -- | Current counts of assertions, tried, failed, and errors.
    tiAsserts :: !Word,
    -- | Events that have been logged
    tiEvents :: ![(Word, String)],
    -- | Whether or not the result of the test computation is already
    -- reflected here.  This is used to differentiate between black
    -- box test and tests we've built with these tools.
    tiIgnoreResult :: !Bool,
    -- | String to attach to every failure message as a prefix.
    tiPrefix :: !String
  }

errorCode :: Word
errorCode = 0

failureCode :: Word
failureCode = 1

sysOutCode :: Word
sysOutCode = 2

sysErrCode :: Word
sysErrCode = 3

{-# NOINLINE testinfo #-}
testinfo :: IORef TestInfo
testinfo = unsafePerformIO $ newIORef undefined

-- | Does the actual work of executing a test.  This maintains the
-- necessary bookkeeping recording assertions and failures, It also
-- sets up exception handlers and times the test.
executeTest :: Reporter us
            -- ^ The reporter to use for reporting results.
            -> State
            -- ^ The HUnit internal state.
            -> us
            -- ^ The reporter state.
            -> IO Progress
            -- ^ The test to run.
            -> IO (Double, State, us)
executeTest rep @ Reporter { reporterCaseProgress = reportCaseProgress }
            ss usInitial runTest =
  let
    -- Run the test until a finished result is produced
    finishTestCase time us action =
      let
        handleExceptions :: SomeException -> IO Progress
        handleExceptions ex =
          case fromException ex of
            Just TestException { teError = True, teMsg = msg } ->
              do
                logError msg
                return (Finished (Error msg))
            Just TestException { teError = False, teMsg = msg } ->
              do
                logFailure msg
                return (Finished (Fail msg))
            Nothing ->
              do
                return (Finished (Error ("Uncaught exception in test: " ++
                                         show ex)))

        caughtAction = catch action handleExceptions
      in do
        (inctime, progress) <- timeItT caughtAction
        case progress of
          Progress msg nextAction ->
            do
              usNext <- reportCaseProgress msg ss us
              finishTestCase (time + inctime) usNext nextAction
          Finished res -> return (res, us, time + inctime)
  in do
    resetTestInfo
    (res, usFinished, time) <- finishTestCase 0 usInitial runTest
    (ssReported, usReported) <- reportTestInfo res rep ss usFinished
    return (time, ssReported, usReported)

-- | Interface between invisible 'TestInfo' and the rest of the test
-- execution framework.
reportTestInfo :: Result -> Reporter us -> State -> us -> IO (State, us)
reportTestInfo result Reporter { reporterError = reportError,
                                 reporterFailure = reportFailure,
                                 reporterSystemOut = reportSystemOut,
                                 reporterSystemErr = reportSystemErr }
               ss @ State { stCounts = c @ Counts { cAsserts = asserts,
                                                    cFailures = failures,
                                                    cErrors = errors } }
               initialUs =
  let
    handleEvent (us, hasFailure, hasError) (code, msg)
      | code == errorCode =
        do
          us' <- reportError msg ss us
          return (us', hasFailure, True)
      | code == failureCode =
        do
          us' <- reportFailure msg ss us
          return (us', True, hasError)
      | code == sysOutCode =
        do
          us' <- reportSystemOut msg ss us
          return (us', hasFailure, hasError)
      | code == sysErrCode =
        do
          us' <- reportSystemErr msg ss us
          return (us', hasFailure, hasError)
      | otherwise = fail ("Internal error: bad code " ++ show code)
  in do
    TestInfo { tiAsserts = currAsserts,
               tiEvents = currEvents,
               tiIgnoreResult = ignoreRes } <- readIORef testinfo
    (eventsUs, hasFailure, hasError) <-
      foldlM handleEvent (initialUs, False, False) (reverse currEvents)
    case result of
      Error msg | not ignoreRes ->
        do
          finalUs <- reportError msg ss eventsUs
          return $! (ss { stCounts =
                             c { cAsserts = asserts + fromIntegral currAsserts,
                                 cErrors = errors + 1 } },
                    finalUs)
      Fail msg | not ignoreRes ->
        do
          finalUs <- reportFailure msg ss eventsUs
          return $! (ss { stCounts =
                            c { cAsserts = asserts + fromIntegral currAsserts,
                                cFailures = failures + 1 } },
                     finalUs)
      _ -> return $! (ss { stCounts =
                             c { cAsserts = asserts + fromIntegral currAsserts,
                                 cFailures =
                                   if hasFailure
                                     then failures + 1
                                     else failures,
                                 cErrors =
                                   if hasError
                                     then errors + 1
                                     else errors } },
                     eventsUs)

-- | Indicate that the result of a test is already reflected in the testinfo.
ignoreResult :: IO ()
ignoreResult = modifyIORef testinfo (\t -> t { tiIgnoreResult = True })

resetTestInfo :: IO ()
resetTestInfo = writeIORef testinfo TestInfo { tiAsserts = 0,
                                               tiEvents = [],
                                               tiIgnoreResult = False,
                                               tiPrefix = "" }

-- | Execute the given computation with a message prefix.
withPrefix :: String -> IO () -> IO ()
withPrefix prefix c =
  do
    t @ TestInfo { tiPrefix = oldprefix } <- readIORef testinfo
    writeIORef testinfo t { tiPrefix = prefix ++ oldprefix }
    c
    modifyIORef testinfo (\t' -> t' { tiPrefix = oldprefix })

-- | Record that one assertion has been checked.
logAssert :: IO ()
logAssert = modifyIORef testinfo (\t -> t { tiAsserts = tiAsserts t + 1 })

-- | Record an error, along with a message.
logError :: String -> IO ()
logError msg =
  modifyIORef testinfo (\t -> t { tiEvents = (errorCode, tiPrefix t ++ msg) :
                                             tiEvents t })

-- | Record a failure, along with a message.
logFailure :: String -> IO ()
logFailure msg =
  modifyIORef testinfo (\t -> t { tiEvents = (failureCode, tiPrefix t ++ msg) :
                                             tiEvents t })

-- | Get a combined failure message, if there is one.
getFailures :: IO (Maybe String)
getFailures =
  do
    TestInfo { tiEvents = events } <- readIORef testinfo
    case map snd (filter ((== failureCode) . fst) events) of
      [] -> return $ Nothing
      fails -> return $ (Just (concat (reverse fails)))

-- | Get a combined failure message, if there is one.
getErrors :: IO (Maybe String)
getErrors =
  do
    TestInfo { tiEvents = events } <- readIORef testinfo
    case map snd (filter ((== errorCode) . fst) events) of
      [] -> return $ Nothing
      errors -> return $ (Just (concat (reverse errors)))

-- Assertion Definition
-- ====================

type Assertion = IO ()

-- Conditional Assertion Functions
-- -------------------------------

-- | Unconditionally signal that a failure has occurred.  This will
-- not stop execution, but will record the failure, resulting in a
-- failed test.
assertFailure :: String
              -- ^ The failure message
              -> Assertion
assertFailure msg = logAssert >> logFailure msg

-- | Signal that an assertion succeeded.  This will log that an
-- assertion has been made.
assertSuccess :: Assertion
assertSuccess = logAssert

-- | Signal than an error has occurred and stop the test immediately.
abortError :: String -> Assertion
abortError msg = throw TestException { teError = True, teMsg = msg }

-- | Signal that a failure has occurred and stop the test immediately.
-- Note that if an error has been logged already, the test will be
-- reported as an error.
abortFailure :: String -> Assertion
abortFailure msg = throw TestException { teError = False, teMsg = msg }

-- | Asserts that the specified condition holds.
assertBool :: String
           -- ^ The message that is displayed if the assertion fails
           -> Bool
           -- ^ The condition
           -> Assertion
assertBool msg b = if b then assertSuccess else assertFailure msg

-- | Signals an assertion failure if a non-empty message (i.e., a message
-- other than @\"\"@) is passed.
assertString :: String
             -- ^ The message that is displayed with the assertion failure 
             -> Assertion
assertString = assertStringWithPrefix ""

-- | Signals an assertion failure if a non-empty message (i.e., a
-- message other than @\"\"@) is passed.  Allows a prefix to be
-- supplied for the assertion failure message.
assertStringWithPrefix :: String
                       -- ^ Prefix to attach to the string if not null
                       -> String
                       -- ^ String to assert is null
                       -> Assertion
assertStringWithPrefix prefix s = assertBool (prefix ++ s) (null s)

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the 
-- actual value.
--  
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual :: (Eq a, Show a)
            => String
            -- ^ The message prefix
            -> a
            -- ^ The expected value 
            -> a
            -- ^ The actual value
            -> Assertion
assertEqual preface expected actual =
  let
    msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\nbut got: " ++ show actual
  in
    assertBool msg (actual == expected)

-- Overloaded `assert` Function
-- ----------------------------

-- | Allows the extension of the assertion mechanism.
-- 
-- Since an 'Assertion' can be a sequence of @Assertion@s and @IO@
-- actions, there is a fair amount of flexibility of what can be
-- achieved.  As a rule, the resulting 'Assertion' should not assert
-- multiple, independent conditions.
-- 
-- If more complex arrangements of assertions are needed, 'Test's and
-- 'Testable' should be used.
class Assertable t where
  -- | Assertion with a failure message
  assertWithMsg :: String -> t -> Assertion

  -- | Assertion with no failure message
  assert :: t -> Assertion
  assert = assertWithMsg ""

instance Assertable () where
  assertWithMsg _ = return

instance Assertable Bool where
  assertWithMsg msg = assertBool msg

instance Assertable Result where
  assertWithMsg _ Pass = assertSuccess
  assertWithMsg "" (Error errstr) = logError errstr
  assertWithMsg prefix (Error errstr) = logError (prefix ++ errstr)
  assertWithMsg "" (Fail failstr) = assertFailure failstr
  assertWithMsg prefix (Fail failstr) = assertFailure (prefix ++ failstr)

instance Assertable Progress where
  assertWithMsg msg (Progress _ cont) = assertWithMsg msg cont
  assertWithMsg msg (Finished res) = assertWithMsg msg res

instance (ListAssertable t) => Assertable [t] where
  assertWithMsg msg = listAssert msg

instance (Assertable t) => Assertable (IO t) where
  assertWithMsg msg t = t >>= assertWithMsg msg

-- | A specialized form of 'Assertable' to handle lists.
class ListAssertable t where
  listAssert :: String -> [t] -> Assertion

instance ListAssertable Char where
  listAssert msg = assertStringWithPrefix msg

instance ListAssertable Assertion where
  listAssert msg asserts = withPrefix msg (sequence_ asserts)

-- Assertion Construction Operators
-- --------------------------------

infix  1 @?, @=?, @?=

-- | Shorthand for 'assertBool'.
(@?) :: (Assertable t) =>
        t
     -- ^ A value of which the asserted condition is predicated
     -> String
     -- ^ A message that is displayed if the assertion fails
     -> Assertion
predi @? msg = assertWithMsg msg predi

-- | Asserts that the specified actual value is equal to the expected value
-- (with the expected value on the left-hand side).
(@=?) :: (Eq a, Show a)
      => a
      -- ^ The expected value
      -> a
      -- ^ The actual value
      -> Assertion
expected @=? actual = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
-- (with the actual value on the left-hand side).
(@?=) :: (Eq a, Show a)
         => a
         -- ^ The actual value
         -> a
         -- ^ The expected value
         -> Assertion
actual @?= expected = assertEqual "" expected actual

-- Test Definition
-- ===============

-- | Definition for a test suite.  This is intended to be a top-level
-- (ie. non-nestable) container for tests.  Test suites have a name, a
-- list of options with default values (which can be overridden either
-- at runtime or statically using 'ExtraOptions'), and a set of
-- 'Test's to be run.
--
-- Individual tests are described using definitions found in cabal's
-- "Distribution.TestSuite" module, to allow for straightforward
-- integration with cabal testing facilities.
data TestSuite =
  TestSuite {
    -- | The name of the test suite.
    suiteName :: !String,
    -- | Whether or not to run the tests concurrently.
    suiteConcurrently :: !Bool,
    -- | A list of all options used by this suite, and the default
    -- values for those options.
    suiteOptions :: ![(String, String)],
    -- | The tests in the suite.
    suiteTests :: ![Test]
  }

-- | Create a test suite from a name and a list of tests.
testSuite :: String
          -- ^ The suite's name.
          -> [Test]
          -- ^ The tests in the suite.
          -> TestSuite
testSuite suitename testlist =
  TestSuite { suiteName = suitename, suiteConcurrently = True,
              suiteOptions = [], suiteTests = testlist }

-- Overloaded `test` Function
-- --------------------------

{-# NOINLINE syntheticName #-}
syntheticName :: String
syntheticName = "__synthetic__"
{-
handleException :: SomeException -> IO ()
handleException e = logError ("Exception occurred during test:\n" ++ show e)
-}
wrapTest :: IO a -> IO Progress
wrapTest t =
  do
    ignoreResult
    _ <- t
    checkTestInfo

checkTestInfo :: IO Progress
checkTestInfo =
  do
    errors <- getErrors
    case errors of
      Nothing ->
        do
          failures <- getFailures
          case failures of
            Nothing -> return $ (Finished Pass)
            Just failstr -> return $ (Finished (Fail failstr))
      Just errstr -> return $ (Finished (Error errstr))

-- | Provides a way to convert data into a @Test@ or set of @Test@.
class Testable t where
  -- | Create a test with a given name and tag set from a @Testable@ value
  testNameTags :: String -> [String] -> t -> Test

  -- | Create a test with a given name and no tags from a @Testable@ value
  testName :: String -> t -> Test
  testName testname t = testNameTags testname [] t

  -- | Create a test with a given name and no tags from a @Testable@ value
  testTags :: [String] -> t -> Test
  testTags tagset t = testNameTags syntheticName tagset t

  -- | Create a test with a synthetic name and no tags from a @Testable@ value
  test :: Testable t => t -> Test
  test t = testNameTags syntheticName [] t

instance Testable Test where
  testNameTags newname newtags g @ Group { groupTests = testlist } =
    g { groupName = newname, groupTests = map (testTags newtags) testlist }
  testNameTags newname newtags (Test t @ TestInstance { tags = oldtags }) =
    Test t { name = newname, tags = newtags ++ oldtags }
  testNameTags newname newtags (ExtraOptions opts t) =
    ExtraOptions opts (testNameTags newname newtags t)

  testTags newtags g @ Group { groupTests = testlist } =
    g { groupTests = map (testTags newtags) testlist }
  testTags newtags (Test t @ TestInstance { tags = oldtags }) =
    Test t { tags = newtags ++ oldtags }
  testTags newtags (ExtraOptions opts t) =
    ExtraOptions opts (testTags newtags t)

  testName newname g @ Group {} = g { groupName = newname }
  testName newname (Test t) = Test t { name = newname }
  testName newname (ExtraOptions opts t) =
    ExtraOptions opts (testName newname t)

  test = id

instance (Assertable t) => Testable (IO t) where
  testNameTags testname testtags t =
    Test TestInstance { name = testname, tags = testtags,
                        run = wrapTest (t >>= assert),
                        options = [], setOption = undefined }

instance (Testable t) => Testable [t] where
  testNameTags testname testtags ts =
    Group { groupName = testname, groupTests = map (testTags testtags) ts,
            concurrently = True }

-- Test Construction Operators
-- ---------------------------

infix  1 ~?, ~=?, ~?=
infixr 0 ~:

-- | Creates a test case resulting from asserting the condition obtained 
--   from the specified 'AssertionPredicable'.
(~?) :: (Assertable t)
     => t
     -- ^ A value of which the asserted condition is predicated
     -> String
     -- ^ A message that is displayed on test failure
     -> Test
predi ~? msg = test (predi @? msg)

-- | Shorthand for a test case that asserts equality (with the expected 
--   value on the left-hand side, and the actual value on the right-hand
--   side).
(~=?) :: (Eq a, Show a)
      => a
      -- ^ The expected value 
      -> a
      -- ^ The actual value
      -> Test
expected ~=? actual = test (expected @=? actual)

-- | Shorthand for a test case that asserts equality (with the actual 
--   value on the left-hand side, and the expected value on the right-hand
--   side).
(~?=) :: (Eq a, Show a)
      => a
      -- ^ The actual value
      -> a
      -- ^ The expected value 
      -> Test
actual ~?= expected = test (actual @?= expected)

-- | Creates a test from the specified 'Testable', with the specified 
--   label attached to it.
-- 
-- Since 'Test' is @Testable@, this can be used as a shorthand way of
-- attaching a 'TestLabel' to one or more tests.
(~:) :: (Testable t) => String -> t -> Test
label ~: t = testName label t
