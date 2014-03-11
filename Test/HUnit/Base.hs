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
       performTestCase,
       performTest,
       performTestSuite,
       performTestSuites
       ) where

import Control.Monad (unless, foldM)

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

import Distribution.TestSuite
import System.TimeIt
--import Test.HUnit.Lang
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

performTestCase :: Reporter us
                -- ^ Report generator for the test run
                -> State
                -- ^ HUnit internal state
                -> us
                -- ^ State for the report generator
                -> TestInstance
                -- ^ The test to be executed
                -> IO (State, us)
performTestCase Reporter { reporterStartCase = reportStartCase,
                           reporterCaseProgress = reportCaseProgress,
                           reporterEndCase = reportEndCase,
                           reporterError = reportError,
                           reporterFailure = reportFailure }
                ss @ State { counts = c @ Counts { tried = n } } us
                TestInstance { run = runTest, name = testName } =
  let
    -- Counts to use in event of success
    ssSuccess = ss { counts = c { tried = n + 1 } }
    -- Counts to use in event of a failure
    ssFail = ss { counts = c { tried = n + 1, failures = failures c + 1 } }
    -- Counts to use in event of an error
    ssError = ss { counts = c { tried = n + 1, errors = errors c + 1 } }

    finishTestCase us' action =
      do
        progress <- action
        case progress of
          Progress msg nextAction ->
            do
              usNext <- reportCaseProgress msg ss us'
              finishTestCase usNext nextAction
          Finished res -> return (res, us')
  in do
    -- Call the reporter's start case function
    usStarted <- reportStartCase testName ss us
    -- Actually run the test
    (time, (r, usFinished)) <- timeItT (finishTestCase usStarted runTest)
    -- Eventually, will need to report stdout and stderr activity
    case r of
      -- If the test succeeded, just report end of case
      Pass ->
        do
          usEnded <- reportEndCase time ssSuccess usFinished
          return (ssSuccess, usEnded)
      -- If there was a failure, report it, then report end of case
      Fail msg ->
        do
          usFail <- reportFailure msg ssFail usFinished
          usEnded <- reportEndCase time ssFail usFail
          return (ssFail, usEnded)
      -- If there was an error, report it, then report end of case
      Error msg ->
        do
          usError <- reportError msg ssError usFinished
          usEnded <- reportEndCase time ssError usError
          return (ssError, usEnded)

-- | Performs a test run with the specified report generators.  
--
-- This handles the actual running of the tests.  Most developers will want 
-- to use @HUnit.Text.runTestTT@ instead.  A developer could use this function 
-- to execute tests via another IO system, such as a GUI, or to output the 
-- results in a different manner (e.g., upload XML-formatted results to a 
-- webservice).  
--
-- Note that the counts in a start report do not include the test case
-- being started, whereas the counts in a problem report do include the
-- test case just finished.  The principle is that the counts are sampled
-- only between test case executions.  As a result, the number of test
-- case successes always equals the difference of test cases tried and
-- the sum of test case errors and failures.
performTest :: Reporter us
            -- ^ Report generator for the test run
            -> Counts
            -- ^ Initial counts for tests
            -> us
            -- ^ State for the report generator
            -> Test
            -- ^ The test to be executed
            -> IO (Counts, us)
performTest rep initialCounts initialUs initialTest =
  let
    -- Initial state and counts, with empty path and zeroed-out counts
    initState  = State { path = [], counts = initialCounts }

    -- The recursive worker function that actually runs all the tests
    performTest' ss us Group { groupName = gname, groupTests = testlist } =
      let
        -- Update the path for running the group's tests
        oldpath = path ss
        ssWithPath = ss { path = (Label gname) : oldpath }

        foldfun (ss', us') t = performTest' ss' us' t
      in do
        -- Run the tests with the updated path
        (ssAfter, usAfter) <- foldM foldfun (ssWithPath, us) testlist
        -- Return the state, reset to the old path
        return (ssAfter { path = oldpath }, usAfter)
    performTest' ss us (Test testinstance) =
      -- For an individual test, just run the test case
      performTestCase rep ss us testinstance
    performTest' ss us (ExtraOptions _ inner) =
      -- For now, options aren't being handled.  This will need to do
      -- more when they are.
      performTest' ss us inner
  in do
    (ss', us') <- performTest' initState initialUs initialTest
    unless (null (path ss')) $ error "performTest: Final path is nonnull"
    return (counts ss', us')

performTestSuite :: Reporter us
                 -- ^ Report generator to use for running the test suite
                 -> us
                 -- ^ State for the report generator
                 -> TestSuite
                 -- ^ Test suite to be run
                 -> IO (Counts, us)
performTestSuite rep @ Reporter { reporterStartSuite = reportStartSuite,
                                  reporterEndSuite = reportEndSuite }
                 initialUs
                 TestSuite { suiteName = sname, suiteTests = testlist,
                             suiteOptions = suiteOpts } =
  let
    -- XXX fold in calculation of numbers of test cases into the
    -- traversal of the test cases themselves
    initCounts = Counts { cases = fromIntegral (sum (map testCaseCount testlist)),
                          tried = 0, errors = 0, failures = 0 }

    foldfun (c, us) test = performTest rep c us test
  in do
    startedUs <- reportStartSuite sname suiteOpts initialUs
    (time, (finishedCounts, finishedUs)) <-
      timeItT (foldM foldfun (initCounts, startedUs) testlist)
    endedUs <- reportEndSuite time finishedCounts finishedUs
    return (finishedCounts, endedUs)

performTestSuites :: Reporter us
                  -- ^ Report generator to use for running the test suite
                  -> us
                  -- ^ State for the report generator
                  -> [TestSuite]
                  -- ^ Test suite to be run
                  -> IO (Counts, us)
performTestSuites rep @ Reporter { reporterStart = reportStart,
                                   reporterEnd = reportEnd }
                  initialUs suites =
  let
    initialCounts = Counts { cases = 0, tried = 0, errors = 0, failures = 0 }

    combineCounts Counts { cases = cases1, tried = tried1,
                           errors = errors1, failures = failures1 }
                  Counts { cases = cases2, tried = tried2,
                           errors = errors2, failures = failures2 } =
      Counts { cases = cases1 + cases2, tried = tried1 + tried2,
               errors = errors1 + errors2, failures = failures1 + failures2 }

    foldfun (accumCounts, accumUs) suite =
      do
        (suiteCounts, suiteUs) <- performTestSuite rep accumUs suite
        return (combineCounts accumCounts suiteCounts, suiteUs)
  in do
    startedUs <- reportStart initialUs
    (time, (finishedCounts, finishedUs)) <-
      timeItT (foldM foldfun (initialCounts, startedUs) suites)
    endedUs <- reportEnd time finishedCounts finishedUs
    return (finishedCounts, endedUs)
