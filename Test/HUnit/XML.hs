{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Test controller for running HUnit tests and reporting results as
--   JUnit-style XML reports.
module Test.HUnit.XML(
       -- * XML Generation
       propertyElem,
       propertiesElem,
       systemOutElem,
       systemErrElem,
       failureElem,
       errorElem,
       testcaseElem,
       skippedTestElem,
       testSuiteElem,
       testSuitesElem,
       -- * Reporter
       xmlReporter
       ) where

import Data.Map(Map)
import Data.Time
import Data.Word
import Network.HostName
import System.Locale
import Test.HUnit.Reporting(Reporter(..), State(..), Counts(..),
                            defaultReporter, showPath)
import Text.XML.Expat.Tree

import qualified Data.Map as Map

-- | Generate an element for a property definition
propertyElem :: (String, String)
             -- ^ The name/value pair
             -> Node String String
propertyElem (name, value) = Element { eName = "property", eChildren = [],
                                       eAttributes = [("name", name),
                                                      ("value", value)] }

-- | Generate an element for a set of property definitions
propertiesElem :: [(String, String)]
               -- ^ A list of name/value pairs to make into properties
               -> Node String String
propertiesElem props = Element { eName = "properties", eAttributes = [],
                                 eChildren = map propertyElem props }

-- | Generate an element representing output to stdout
systemOutElem :: String
              -- ^ The stdout output
              -> Node String String
systemOutElem content = Element { eName = "system-out", eAttributes = [],
                                  eChildren = [Text content] }

-- | Generate an element representing output to stderr
systemErrElem :: String
              -- ^ The stderr output
              -> Node String String
systemErrElem content = Element { eName = "system-err", eAttributes = [],
                                  eChildren = [Text content] }

-- | Generate an element representing a test failure.
failureElem :: String
            -- ^ A message associated with the failure
            -> Node String String
failureElem message = Element { eAttributes = [("message", message)],
                                eName = "failure", eChildren = [] }

-- | Generate an element representing an error in a test.
errorElem :: String
          -- ^ A message associated with the error
          -> Node String String
errorElem message = Element { eAttributes = [("message", message)],
                              eName = "error", eChildren = [] }

-- | Generate an element for a single test case.
testcaseElem :: String
             -- ^ The name of the test
             -> String
             -- ^ The path to the test (reported as "classname")
             -> Word
             -- ^ The number of assertions in the test
             -> Double
             -- ^ The execution time of the test
             -> [Node String String]
             -- ^ Elements representing the events that happened
             -- during test execution.
             -> Node String String
testcaseElem name classname assertions time children =
  Element { eName = "testcase", eChildren = children,
            eAttributes = [("name", name),
                           ("classname", classname),
                           ("assertions", show assertions),
                           ("time", show time)] }

-- | Generate an element for a skipped test case
skippedTestElem :: String
                -- ^ The name of the test
                -> String
                -- ^ The path of the test
                -> Node String String
skippedTestElem name classname =
  let
    skippedElem = Element { eName = "skipped", eAttributes = [],
                            eChildren = [] }
  in
    Element { eAttributes = [("name", name), ("classname", classname)],
              eName = "testcase", eChildren = [skippedElem] }

-- | Generate an element for a test suite run
testSuiteElem :: String
              -- ^ The name of the test suite
              -> Map String String
              -- ^ The properties defined for this suite
              -> Word
              -- ^ The number of tests
              -> Word
              -- ^ The number of failures
              -> Word
              -- ^ The number of errors
              -> Word
              -- ^ The number of skipped tests
              -> String
              -- ^ The hostname of the machine on which this was run
              -> UTCTime
              -- ^ The timestamp at which time this was run
              -> Double
              -- ^ The execution time for the test suite
              -> [Node String String]
              -- ^ The testcases and output nodes for the test suite
              -> Node String String
testSuiteElem name propmap tests failures errors skipped
              hostname timestamp time content =
  let
    props = Map.assocs propmap
    timestr = formatTime defaultTimeLocale "%c" timestamp
  in
    Element { eName = "testsuite", eChildren = propertiesElem props : content,
              eAttributes = [("name", name),
                             ("hostname", hostname),
                             ("timestamp", timestr),
                             ("time", show time),
                             ("tests", show tests),
                             ("failures", show failures),
                             ("errors", show errors),
                             ("skipped", show skipped)] }

-- | Generate the top-level element containing all test suites
testSuitesElem :: Double
               -- ^ The execution time of all suites
               -> [Node String String]
               -- ^ Elements representing all the test suites
               -> Node String String
testSuitesElem time suites =
  Element { eName = "testsuites", eChildren = suites,
            eAttributes = [("time ", show time)] }

-- | A reporter that generates JUnit XML reports
xmlReporter :: Reporter [[Node String String]]
xmlReporter =
  let
    reportStart = return [[]]

    reportEnd time _ [suites] = return [[testSuitesElem time (reverse suites)]]
    reportEnd _ _ _ = fail "Extra information on node stack"

    reportStartSuite _ stack = return ([] : stack)

    reportEndSuite time State { stName = name, stOptions = options,
                                stCounts = Counts { cTried = tried,
                                                    cErrors = errors,
                                                    cFailures = failures,
                                                    cSkipped = skipped } }
                   (events : rest : stack) =
      do
        hostname <- getHostName
        timestamp <- getCurrentTime
        return ((testSuiteElem name options tried failures errors skipped
                               hostname timestamp time (reverse events) :
                 rest) : stack)
    reportEndSuite _ _ _ = fail "Node stack underflow"

    reportStartCase _ stack = return ([] : stack)

    reportEndCase time State { stName = name, stPath = testpath,
                               stCounts = Counts { cAsserts = asserts } }
                  (events : rest : stack) =
      return ((testcaseElem name (showPath testpath)
                            asserts time (reverse events) : rest) : stack)
    reportEndCase _ _ _ = fail "Node stack underflow"

    reportSkipCase State { stName = name, stPath = testpath } (rest : stack) =
      return ((skippedTestElem name (showPath testpath) : rest) : stack)
    reportSkipCase _ _ = fail "Node stack underflow"

    reportFailure msg _ (rest : stack) =
      return ((failureElem msg : rest) : stack)
    reportFailure _ _ _ = fail "Node stack underflow"

    reportError msg _ (rest : stack) =
      return ((errorElem msg : rest) : stack)
    reportError _ _ _ = fail "Node stack underflow"

    reportSystemOut msg _ (rest : stack) =
      return ((systemOutElem msg : rest) : stack)
    reportSystemOut _ _ _ = fail "Node stack underflow"

    reportSystemErr msg _ (rest : stack) =
      return ((systemErrElem msg : rest) : stack)
    reportSystemErr _ _ _ = fail "Node stack underflow"
  in
    defaultReporter {
      reporterStart = reportStart,
      reporterEnd = reportEnd,
      reporterStartSuite = reportStartSuite,
      reporterEndSuite = reportEndSuite,
      reporterStartCase = reportStartCase,
      reporterEndCase = reportEndCase,
      reporterSkipCase = reportSkipCase,
      reporterFailure = reportFailure,
      reporterError = reportError,
      reporterSystemOut = reportSystemOut,
      reporterSystemErr = reportSystemErr
    }

