{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

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
{-
       (~=?),
       (~?=),
       (~:),
       (~?),

       -- ** Making assertions
       assertFailure, {- from Test.HUnit.Lang: -}
       assertBool,
       assertEqual,
       assertString, 
       Assertion, {- from Test.HUnit.Lang: -}
       (@=?),
       (@?=),
       (@?),

       -- ** Extending the assertion functionality
       Assertable(..),
       ListAssertable(..),
       AssertionPredicate,
       AssertionPredicable(..),
       Testable(..),
-}
       -- ** Test execution
       -- $testExecutionNote
       testCasePaths,
       testCaseCount,
       ) where

-- Test Monad Definition
-- =====================
{-
data TestInfo us =
  TestInfo {
    -- | Current counts of assertions, tried, failed, and errors
    tiCounts :: !Counts,
    -- | Reporter state
    tiRepState :: !us,
    -- | Whether or not an error has been logged in this test
    tiError :: !Bool
  }

type TestM us a = StateT (TestInfo us) IO a
-}
-- Assertion Definition
-- ====================

--import Test.HUnit.Lang
import Distribution.TestSuite
import Test.HUnit.Reporting

{-
-- Conditional Assertion Functions
-- -------------------------------

-- | Asserts that the specified condition holds.
assertBool :: String
           -- ^ The message that is displayed if the assertion fails
           -> Bool
           -- ^ The condition
           -> Assertion
assertBool msg b = unless b (assertFailure msg)

-- | Signals an assertion failure if a non-empty message (i.e., a message
-- other than @\"\"@) is passed.
assertString :: String
             -- ^ The message that is displayed with the assertion failure 
             -> Assertion
assertString s = unless (null s) (assertFailure s)

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
    unless (actual == expected) (assertFailure msg)

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

instance (ListAssertable t) => Assertable [t]
 where assert = listAssert

instance (Assertable t) => Assertable (IO t)
 where assert = (>>= assert)

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
-}
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
{-
-- Overloaded `test` Function
-- --------------------------

-- | Provides a way to convert data into a @Test@ or set of @Test@.
class Testable t
 where test :: t -> Test

instance Testable Test
 where test = id

instance (Assertable t) => Testable (IO t)
 where test = TestCase . assert

instance (Testable t) => Testable [t]
 where test = TestList . map test

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
predi ~? msg = TestCase (predi @? msg)

-- | Shorthand for a test case that asserts equality (with the expected 
--   value on the left-hand side, and the actual value on the right-hand
--   side).
(~=?) :: (Eq a, Show a)
      => a
      -- ^ The expected value 
      -> a
      -- ^ The actual value
      -> Test
expected ~=? actual = TestCase (expected @=? actual)

-- | Shorthand for a test case that asserts equality (with the actual 
--   value on the left-hand side, and the expected value on the right-hand
--   side).
(~?=) :: (Eq a, Show a)
      => a
      -- ^ The actual value
      -> a
      -- ^ The expected value 
      -> Test
actual ~?= expected = TestCase (actual @?= expected)

-- | Creates a test from the specified 'Testable', with the specified 
--   label attached to it.
-- 
-- Since 'Test' is @Testable@, this can be used as a shorthand way of
-- attaching a 'TestLabel' to one or more tests.
(~:) :: (Testable t) => String -> t -> Test
label ~: t = TestLabel label (test t)
-}


-- Test Execution
-- ==============

-- $testExecutionNote
-- Note: the rest of the functionality in this module is intended for 
-- implementors of test controllers. If you just want to run your tests cases,
-- simply use a test controller, such as the text-based controller in 
-- "Test.HUnit.Text".

-- | Determines the paths for all 'TestCase's in a tree of @Test@s.
testCasePaths :: Test -> [Path]
testCasePaths =
  let
    tcp p Group { groupName = gname, groupTests = testlist } =
      concat (map (tcp (Label gname : p)) testlist)
    tcp p (ExtraOptions _ t) = tcp p t
    tcp p (Test _) = [p]
  in
    tcp []

-- | Counts the number of 'TestCase's in a tree of @Test@s.
testCaseCount :: Test -> Int
testCaseCount Group { groupTests = ts }   = sum (map testCaseCount ts)
testCaseCount (ExtraOptions _ t) = testCaseCount t
testCaseCount (Test _)    = 1
