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
       zeroCounts,
       defaultReporter,
       showPath
       ) where

import Data.Word
import Data.Map(Map)

-- | A data structure that hold the results of tests that have been performed
-- up until this point.
data Counts =
  Counts {
    cCases :: !Word,
    cTried :: !Word,
    cErrors :: !Word,
    cFailures :: !Word,
    cSkipped :: !Word,
    cAsserts :: !Word
  }
  deriving (Eq, Show, Read)

-- | Keeps track of the remaining tests and the results of the performed tests.
-- As each test is performed, the path is removed and the counts are
-- updated as appropriate.
data State =
  State {
    -- | The name of the case or suite currently being run
    stName :: !String,
    -- | The path to the test case currently being run
    stPath :: !Path,
    -- | The current test statistics
    stCounts :: !Counts,
    -- | The current option values
    stOptions :: Map String String
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
    reporterStart :: IO us,
    -- | Called at the end of a test run
    reporterEnd :: Double
                -- ^ The total time it took to run the tests
                -> Counts
                -- ^ The counts from running the tests
                -> us
                -- ^ The user state for this test reporter
                -> IO us,
    -- | Called at the start of a test suite run
    reporterStartSuite :: State
                       -- ^ Options given to the test suite
                       -> us
                       -- ^ The user state for this test reporter
                       -> IO us,
    -- | Called at the end of a test suite run
    reporterEndSuite :: Double
                     -- ^ The total time it took to run the test suite
                     -> State
                     -- ^ The counts from running the tests
                     -> us
                     -- ^ The user state for this test reporter
                     -> IO us,
    -- | Called at the start of a test case run
    reporterStartCase :: State
                      -- ^ The HUnit internal state
                      -> us
                      -- ^ The user state for this test reporter
                      -> IO us,
    -- | Called to report progress of a test case run
    reporterCaseProgress :: String
                         -- ^ A progress message
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
    reporterSkipCase :: State
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

-- | A [@Counts@] with all zero counts
zeroCounts :: Counts
zeroCounts = Counts { cCases = 0, cTried = 0, cErrors = 0,
                      cFailures = 0, cAsserts = 0, cSkipped = 0 }

-- | A reporter containing default actions, which are to do nothing
-- and return the user state unmodified.
defaultReporter :: Reporter a
defaultReporter = Reporter {
    reporterStart = fail "Must define a reporterStart value",
    reporterEnd = \_ _ us -> return us,
    reporterStartSuite = \_ us -> return us,
    reporterEndSuite = \_ _ us -> return us,
    reporterStartCase = \_ us -> return us,
    reporterCaseProgress = \_ _ us -> return us,
    reporterEndCase = \_ _ us -> return us,
    reporterSkipCase = \_ us -> return us,
    reporterSystemOut = \_ _ us -> return us,
    reporterSystemErr = \_ _ us -> return us,
    reporterFailure = \_ _ us -> return us,
    reporterError = \_ _ us -> return us
  }

-- | Converts a test case path to a string, separating adjacent elements by 
--   the colon (\':\'). An element of the path is quoted (as with 'show') when
--   there is potential ambiguity.

showPath :: Path -> String
showPath [] = ""
showPath nodes =
  let
    f b a = a ++ "." ++ b
    showNode (Label label) = safe label (show label)
    safe s ss = if '.' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s
  in
    foldl1 f (map showNode nodes)
