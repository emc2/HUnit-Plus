{-# OPTIONS_GHC -Wall -Werror #-}

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

       -- ** Test execution
       -- $testExecutionNote
       State(..),
       Counts(..), 
       Path,
       Node(..), 
       testCasePaths,
       testCaseCount,
       Reporter(..),
       defaultReporter,
       performTest
       ) where

import Control.Monad (unless, foldM)


-- Assertion Definition
-- ====================

import Test.HUnit.Lang
import System.TimeIt


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



-- Test Definition
-- ===============

-- | The basic structure used to create an annotated tree of test cases.
data Test
    -- | A single, independent test case composed.
    = TestCase Assertion
    -- | A set of @Test@s sharing the same level in the hierarchy. 
    | TestList [Test]
    -- | A name or description for a subtree of the @Test@s.
    | TestLabel String Test

instance Show Test where
  showsPrec _ (TestCase _)    = showString "TestCase _"
  showsPrec _ (TestList ts)   = showString "TestList " . showList ts
  showsPrec p (TestLabel l t) = showString "TestLabel " . showString l
                                . showChar ' ' . showsPrec p t

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



-- Test Execution
-- ==============

-- $testExecutionNote
-- Note: the rest of the functionality in this module is intended for 
-- implementors of test controllers. If you just want to run your tests cases,
-- simply use a test controller, such as the text-based controller in 
-- "Test.HUnit.Text".

-- | A data structure that hold the results of tests that have been performed
-- up until this point.
data Counts = Counts { cases, tried, errors, failures :: Int }
  deriving (Eq, Show, Read)

-- | Keeps track of the remaining tests and the results of the performed tests.
-- As each test is performed, the path is removed and the counts are
-- updated as appropriate.
data State = State { path :: Path, counts :: Counts }
  deriving (Eq, Show, Read)

-- | Report generator.  This record type contains a number of
-- functions that are called at various points throughout a test run.
data Reporter us = Reporter {
    -- | Called at the beginning of a test run
    reporterStart :: State
                  -- ^ The HUnit internal state
                  -> us
                  -- ^ The user state for this test reporter
                  -> IO us,
    -- | Called at the end of a test run
    reporterEnd :: Double
                -- ^ The total time it took to run the tests
                -> State
                -- ^ The HUnit internal state
                -> us
                -- ^ The user state for this test reporter
                -> IO us,
    -- | Called at the start of a test suite run
    reporterStartSuite :: String
                       -- ^ The name of the test suite
                       -> [(String, String)]
                       -- ^ Options given to the test suite
                       -> State
                       -- ^ The HUnit internal state
                       -> us
                       -- ^ The user state for this test reporter
                       -> IO us,
    -- | Called at the end of a test suite run
    reporterEndSuite :: Double
                     -- ^ The total time it took to run the test suite
                     -> State
                     -- ^ The HUnit internal state
                     -> us
                     -- ^ The user state for this test reporter
                     -> IO us,
    -- | Called at the start of a test suite run
    reporterStartCase :: String
                      -- ^ The name of the test case
                      -> State
                      -- ^ The HUnit internal state
                      -> us
                      -- ^ The user state for this test reporter
                      -> IO us,
    -- | Called at the end of a test suite run
    reporterEndCase :: Double
                    -- ^ The total time it took to run the test suite
                    -> State
                    -- ^ The HUnit internal state
                    -> us
                    -- ^ The user state for this test reporter
                    -> IO us,
    -- | Called when skipping a test case
    reporterSkipCase :: String
                     -- ^ The name of the test case being skipped
                     -> State
                     -- ^ The HUnit internal state
                     -> us
                     -- ^ The user state for this test reporter
                     -> IO us,
    -- | Called to report output printed to the system output stream
    reporterSystemOut :: String
                      -- ^ The content printed to system out
                      -> State
                      -- ^ The HUnit internal state
                      -> us
                      -- ^ The user state for this test reporter
                      -> IO us,
    -- | Called to report output printed to the system error stream
    reporterSystemErr :: String
                      -- ^ The content printed to system out
                      -> State
                      -- ^ The HUnit internal state
                      -> us
                      -- ^ The user state for this test reporter
                      -> IO us,
    -- | Called when a test fails
    reporterFailure :: String
                    -- ^ A message relating to the error
                    -> State
                    -- ^ The HUnit internal state
                    -> us
                    -- ^ The user state for this test reporter
                    -> IO us,
    -- | Called when a test reports an error
    reporterError :: String
                  -- ^ A message relating to the error
                  -> State
                  -- ^ The HUnit internal state
                  -> us
                  -- ^ The user state for this test reporter
                  -> IO us
  }

-- | A reporter containing default actions, which are to do nothing
-- and return the user state unmodified.
defaultReporter :: Reporter a
defaultReporter = Reporter {
    reporterStart = \_ us -> return us,
    reporterEnd = \_ _ us -> return us,
    reporterStartSuite = \_ _ _ us -> return us,
    reporterEndSuite = \_ _ us -> return us,
    reporterStartCase = \_ _ us -> return us,
    reporterEndCase = \_ _ us -> return us,
    reporterSkipCase = \_ _ us -> return us,
    reporterSystemOut = \_ _ us -> return us,
    reporterSystemErr = \_ _ us -> return us,
    reporterFailure = \_ _ us -> return us,
    reporterError = \_ _ us -> return us
  }

-- | Uniquely describes the location of a test within a test hierarchy.
-- Node order is from test case to root.
type Path = [Node]

-- | Composed into 'Path's.
data Node  = ListItem Int | Label String
  deriving (Eq, Show, Read)

-- | Determines the paths for all 'TestCase's in a tree of @Test@s.
testCasePaths :: Test -> [Path]
testCasePaths t0 =
  let
    tcp (TestCase _) p = [p]
    tcp (TestList ts) p =
      concat [ tcp t (ListItem n : p) | (t,n) <- zip ts [0..] ]
    tcp (TestLabel l t) p = tcp t (Label l : p)
  in
    tcp t0 []

-- | Counts the number of 'TestCase's in a tree of @Test@s.
testCaseCount :: Test -> Int
testCaseCount (TestCase _)    = 1
testCaseCount (TestList ts)   = sum (map testCaseCount ts)
testCaseCount (TestLabel _ t) = testCaseCount t

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
            -> us
            -- ^ State for the report generator
            -> Test
            -- ^ The test to be executed
            -> IO (Counts, us)
performTest Reporter { reporterStart = reportStart,
                       reporterEnd = reportEnd,
                       reporterEndCase = reportEndCase,
                       reporterError = reportError,
                       reporterFailure = reportFailure }
            initialUs initialTest =
  let
    initState  = State { path = [], counts = initCounts }
    initCounts = Counts { cases = testCaseCount initialTest, tried = 0,
                          errors = 0, failures = 0}

    withNode node ss0 us0 t =
      let
        path0 = path ss0

        ss1 = ss0 { path = node : path0 }
      in do
        (ss2, us1) <- performTest' ss1 us0 t
        return (ss2 { path = path0 }, us1)

    performTest' ss us (TestCase a) =
      let
        c @ Counts { tried = n } = counts ss
        ssSuccess = ss { counts = c { tried = n + 1 } }
        ssFail = ss { counts = c { tried = n + 1, failures = failures c + 1 } }
        ssError = ss { counts = c { tried = n + 1, errors = errors c + 1 } }
        usStarted = us
      in do
      --usStarted <- reportStart ss us
      (time, r) <- performTestCase a
      case r of
        Nothing ->
          do
            usEnded <- reportEndCase time ssSuccess usStarted
            return (ssSuccess, usEnded)
        Just (True,  m) ->
          do
            usFail <- reportFailure m ssFail usStarted
            usEnded <- reportEndCase time ssFail usFail
            return (ssFail, usEnded)
        Just (False, m) ->
          do
            usError <- reportError m ssError usStarted
            usEnded <- reportEndCase time ssError usError
            return (ssError, usEnded)
    performTest' ss us (TestList ts) =
      let
        foldfun (ss', us') (t, n) = withNode (ListItem n) ss' us' t
      in
        foldM foldfun (ss, us) (zip ts [0..])
    performTest' ss us (TestLabel label t) = withNode (Label label) ss us t
  in do
    usStarted <- reportStart initState initialUs
    (time, (ss', us')) <- timeItT (performTest' initState usStarted initialTest)
    usEnded <- reportEnd time ss' us'
    unless (null (path ss')) $ error "performTest: Final path is nonnull"
    return (counts ss', usEnded)
