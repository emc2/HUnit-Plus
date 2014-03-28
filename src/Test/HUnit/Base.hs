{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Basic definitions for the HUnit library.
-- 
--   This module contains what you need to create assertions and test cases and
--   combine them into test suites. 
-- 
--   This module also provides infrastructure for 
--   implementing test controllers (which are used to execute tests). 
--   See "Test.HUnit.Text" for a great example of how to implement a test 
--   controller.
module Test.HUnit.Base(
       -- ** Declaring tests
       Test(..),
       TestInstance(..),
       TestSuite(..),

       (~=?),
       (~?=),
       (~:),
       (~?),

       -- ** Low-level Test Functions
       reportTestInfo,
       logAssert,
       logFailure,
       logError,
       getErrors,
       getFailures,

       -- ** Making assertions
       Assertion,
       assertFailure,
       assertBool,
       assertString,
       assertEqual,
       (@=?),
       (@?=),
       (@?),
       -- ** Extending the assertion functionality
       Assertable(..),
       ListAssertable(..),
       AssertionPredicate,
       AssertionPredicable(..),
       Testable(..),
       ) where

import Control.Exception hiding (assert)
import Data.Foldable
import Data.IORef
import Data.Word
import Distribution.TestSuite
import Prelude hiding (concat, sum)
import System.IO.Unsafe
import Test.HUnit.Reporting


-- Test Wrapper Definition
-- =====================
data TestInfo =
  TestInfo {
    -- | Current counts of assertions, tried, failed, and errors
    tiAsserts :: !Word,
    -- | Failure messages that have been logged
    tiFailures :: ![String],
    -- | Failure messages that have been logged
    tiErrors :: ![String],
    -- | Whether or not the result of the test computation is already
    -- reflected here.  This is used to differentiate between black
    -- box test and tests we've built with these tools.
    tiIgnoreResult :: !Bool
  }

{-# NOINLINE testinfo #-}
testinfo :: IORef TestInfo
testinfo = unsafePerformIO $ newIORef undefined

-- | Interface between invisible @TestInfo@ and the rest of the test
-- execution framework.
reportTestInfo :: Result -> Reporter us -> State -> us -> IO (State, us)
reportTestInfo result Reporter { reporterError = reportError,
                                 reporterFailure = reportFailure }
               ss @ State { stCounts = c @ Counts { cAsserts = asserts,
                                                    cFailures = failures,
                                                    cErrors = errors } }
               initialUs =
  do
    TestInfo { tiAsserts = currAsserts,
               tiFailures = currFailures,
               tiErrors = currErrors,
               tiIgnoreResult = ignoreRes } <- readIORef testinfo
    errorsUs <- foldlM (\us msg -> reportError msg ss us )
                       initialUs currErrors
    failuresUs <- foldlM (\us msg -> reportFailure msg ss us )
                         errorsUs currErrors
    case result of
      Error msg | not ignoreRes ->
        do
          finalUs <- reportError msg ss failuresUs
          return (ss { stCounts =
                          c { cAsserts = asserts + fromIntegral currAsserts,
                              cFailures = failures +
                                          fromIntegral (length currFailures),
                              cErrors = errors + 1 +
                                        fromIntegral (length currErrors) } },
                  finalUs)
      Fail msg | not ignoreRes ->
        do
          finalUs <- reportFailure msg ss failuresUs
          return (ss { stCounts =
                          c { cAsserts = asserts + fromIntegral currAsserts,
                              cFailures = failures + 1 +
                                          fromIntegral (length currFailures),
                              cErrors = errors +
                                        fromIntegral (length currErrors) } },
                  finalUs)
      _ -> return (ss { stCounts =
                           c { cAsserts = asserts + fromIntegral currAsserts,
                               cFailures = failures +
                                           fromIntegral (length currFailures),
                               cErrors = errors +
                                         fromIntegral (length currErrors) } },
                   failuresUs)

-- | Indicate that the result of a test is already reflected in the testinfo
ignoreResult :: IO ()
ignoreResult = modifyIORef testinfo (\t -> t { tiIgnoreResult = True })

resetTestInfo :: IO ()
resetTestInfo = writeIORef testinfo TestInfo { tiAsserts = 0,
                                               tiFailures = [],
                                               tiErrors = [],
                                               tiIgnoreResult = False }

-- | Record that one assertion has been checked.
logAssert :: IO ()
logAssert = modifyIORef testinfo (\t -> t { tiAsserts = tiAsserts t + 1 })

-- | Record an error, along with a message.
logError :: String -> IO ()
logError msg = modifyIORef testinfo (\t -> t { tiErrors = msg : tiErrors t })

-- | Record a failure, along with a message.
logFailure :: String -> IO ()
logFailure msg =
  modifyIORef testinfo (\t -> t { tiFailures = msg : tiFailures t })

-- | Get a combined failure message, if there is one
getFailures :: IO (Maybe String)
getFailures =
  do
    TestInfo { tiFailures = fails } <- readIORef testinfo
    case fails of
      [] -> return Nothing
      _ -> return (Just (concat (reverse fails)))

-- | Get a combined failure message, if there is one
getErrors :: IO (Maybe String)
getErrors =
  do
    TestInfo { tiErrors = errors } <- readIORef testinfo
    case errors of
      [] -> return Nothing
      _ -> return (Just (concat (reverse errors)))

-- Assertion Definition
-- ====================

type Assertion = IO ()

-- Conditional Assertion Functions
-- -------------------------------

-- | Unconditionally signal that a failure has occurred.  This will
-- not stop execution, but will record the error, resulting in a
-- failed test.
assertFailure :: String
              -- ^ The failure message
              -> Assertion
assertFailure msg = logAssert >> logFailure msg

-- | Signal that an assertion succeeded.
assertSuccess :: Assertion
assertSuccess = logAssert

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
assertString s = assertBool s (null s)

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
             "expected: " ++ show expected ++ "\n but got: " ++ show actual
  in
    assertBool msg (actual == expected)

-- Overloaded `assert` Function
-- ----------------------------

-- | Allows the extension of the assertion mechanism.
-- 
-- Since an 'Assertion' can be a sequence of @Assertion@s and @IO@ actions, 
-- there is a fair amount of flexibility of what can be achieved.  As a rule,
-- the resulting @Assertion@ should be the body of a 'TestCase' or part of
-- a @TestCase@; it should not be used to assert multiple, independent 
-- conditions.
-- 
-- If more complex arrangements of assertions are needed, 'Test's and
-- 'Testable' should be used.
class Assertable t
 where assert :: t -> Assertion

instance Assertable ()
 where assert = return

instance Assertable Bool
 where assert = assertBool ""

instance Assertable Progress where
  assert (Progress _ cont) = assert cont
  assert (Finished Pass) = return ()
  assert (Finished (Error errstr)) = logError errstr
  assert (Finished (Fail failstr)) = logFailure failstr

instance (ListAssertable t) => Assertable [t]
 where assert = listAssert

instance (Assertable t) => Assertable (IO t)
 where assert t = assert t

-- | A specialized form of 'Assertable' to handle lists.
class ListAssertable t
 where listAssert :: [t] -> Assertion

instance ListAssertable Char
 where listAssert = assertString


-- Overloaded `assertionPredicate` Function
-- ----------------------------------------

-- | The result of an assertion that hasn't been evaluated yet.
-- 
-- Most test cases follow the following steps:
-- 
-- 1. Do some processing or an action.
-- 
-- 2. Assert certain conditions.
-- 
-- However, this flow is not always suitable.  @AssertionPredicate@ allows for
-- additional steps to be inserted without the initial action to be affected
-- by side effects.  Additionally, clean-up can be done before the test case
-- has a chance to end.  A potential work flow is:
-- 
-- 1. Write data to a file.
-- 
-- 2. Read data from a file, evaluate conditions.
-- 
-- 3. Clean up the file.
-- 
-- 4. Assert that the side effects of the read operation meet certain conditions.
-- 
-- 5. Assert that the conditions evaluated in step 2 are met.
type AssertionPredicate = IO Bool

-- | Used to signify that a data type can be converted to an assertion 
-- predicate.
class AssertionPredicable t
 where assertionPredicate :: t -> AssertionPredicate

instance AssertionPredicable Bool
 where assertionPredicate = return

instance (AssertionPredicable t) => AssertionPredicable (IO t)
 where assertionPredicate = (>>= assertionPredicate)


-- Assertion Construction Operators
-- --------------------------------

infix  1 @?, @=?, @?=

-- | Asserts that the condition obtained from the specified
--   'AssertionPredicable' holds.
(@?) :: (AssertionPredicable t) =>
        t
     -- ^ A value of which the asserted condition is predicated
     -> String
     -- ^ A message that is displayed if the assertion fails
     -> Assertion
predi @? msg = assertionPredicate predi >>= assertBool msg

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(@=?) :: (Eq a, Show a)
      => a
      -- ^ The expected value
      -> a
      -- ^ The actual value
      -> Assertion
expected @=? actual = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
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
-- at runtime or statically using [@ExtraOptions@]), and a set of
-- [@Test@]s to be run.
--
-- Individual tests are described using definitions found in cabal's
-- [@Distribution.TestSuite@] module, to allow for straightforward
-- integration with cabal testing facilities.
data TestSuite =
  TestSuite {
    -- | The name of the test suite
    suiteName :: !String,
    -- | Whether or not to run the tests concurrently
    suiteConcurrently :: !Bool,
    -- | A list of all options used by this suite, and the default
    -- values for those options
    suiteOptions :: [(String, String)],
    -- | The tests in the suite
    suiteTests :: [Test]
  }

-- Overloaded `test` Function
-- --------------------------

{-# NOINLINE syntheticName #-}
syntheticName :: String
syntheticName = "__synthetic__"

handleException :: SomeException -> IO Progress
handleException e =
  do
    logError ("Exception occurred during test:\n" ++ show e)
    checkTestInfo

wrapTest :: IO a -> IO Progress
wrapTest t =
  do
    resetTestInfo
    ignoreResult
    catch (t >> checkTestInfo) handleException

checkTestInfo :: IO Progress
checkTestInfo =
  do
    errors <- getErrors
    case errors of
      Nothing ->
        do
          failures <- getFailures
          case failures of
            Nothing -> return (Finished Pass)
            Just failstr -> return (Finished (Fail failstr))
      Just errstr -> return (Finished (Error errstr))

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
                        run = wrapTest (assert t),
                        options = [], setOption = undefined }
{-
instance Testable (IO Progress) where
  testNameTags testname testtags t =
    Test TestInstance { name = testname, tags = testtags,
                        run = wrapProgressTest t,
                        options = [], setOption = undefined }
-}
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
(~?) :: (AssertionPredicable t)
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
