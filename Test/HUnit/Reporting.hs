{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Reporting functionality for HUnit-Plus.  Test reporting is now
-- defined using a set of events.  A [@Reporter@] contains handlers
-- for these events, which have access to and update a
-- [@Reporter@]-defined state value.  The handlers in a [@Reporter@]
-- are called at appropriate points during text execution.
module Test.HUnit.Reporting(
       Node(..),
       State(..),
       Counts(..),
       Reporter(..),
       Path,
       defaultReporter,
       ) where

import Data.Word

-- | A data structure that hold the results of tests that have been performed
-- up until this point.
data Counts =
  Counts {
    cases :: !Word,
    tried :: !Word,
    errors :: !Word,
    failures :: !Word
  }
  deriving (Eq, Show, Read)

-- | Keeps track of the remaining tests and the results of the performed tests.
-- As each test is performed, the path is removed and the counts are
-- updated as appropriate.
data State =
  State {
    path :: !Path,
    counts :: !Counts
  }
  deriving (Eq, Show, Read)

-- | Uniquely describes the location of a test within a test hierarchy.
-- Node order is from test case to root.
type Path = [Node]

-- | Composed into 'Path's.
data Node = Label String
  deriving (Eq, Show, Read)

-- | Report generator.  This record type contains a number of
-- functions that are called at various points throughout a test run.
data Reporter us = Reporter {
    -- | Called at the beginning of a test run
    reporterStart :: us
                  -- ^ The user state for this test reporter
                  -> IO us,
    -- | Called at the end of a test run
    reporterEnd :: Double
                -- ^ The total time it took to run the tests
                -> Counts
                -- ^ The counts from running the tests
                -> us
                -- ^ The user state for this test reporter
                -> IO us,
    -- | Called at the start of a test suite run
    reporterStartSuite :: String
                       -- ^ The name of the test suite
                       -> [(String, String)]
                       -- ^ Options given to the test suite
                       -> us
                       -- ^ The user state for this test reporter
                       -> IO us,
    -- | Called at the end of a test suite run
    reporterEndSuite :: Double
                     -- ^ The total time it took to run the test suite
                     -> Counts
                     -- ^ The counts from running the tests
                     -> us
                     -- ^ The user state for this test reporter
                     -> IO us,
    -- | Called at the start of a test case run
    reporterStartCase :: String
                      -- ^ The name of the test case
                      -> State
                      -- ^ The HUnit internal state
                      -> us
                      -- ^ The user state for this test reporter
                      -> IO us,
    -- | Called to report progress of a test case run
    reporterCaseProgress :: String
                         -- ^ The name of the test case
                         -> State
                         -- ^ The HUnit internal state
                         -> us
                         -- ^ The user state for this test reporter
                         -> IO us,
    -- | Called at the end of a test case run
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
    reporterStart = \us -> return us,
    reporterEnd = \_ _ us -> return us,
    reporterStartSuite = \_ _ us -> return us,
    reporterEndSuite = \_ _ us -> return us,
    reporterStartCase = \_ _ us -> return us,
    reporterCaseProgress = \_ _ us -> return us,
    reporterEndCase = \_ _ us -> return us,
    reporterSkipCase = \_ _ us -> return us,
    reporterSystemOut = \_ _ us -> return us,
    reporterSystemErr = \_ _ us -> return us,
    reporterFailure = \_ _ us -> return us,
    reporterError = \_ _ us -> return us
  }
