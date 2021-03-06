{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Reporter' for running HUnit tests and reporting results as
-- JUnit-style XML reports.  This uses the hexpat library for XML
-- generation.  This module also contains functions for creating the
-- various nodes in a JUnit XML report.
module Test.HUnitPlus.XML(
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

import Data.HashMap.Strict(HashMap)
import Data.List(sort)
import Data.Time
import Network.HostName
import Test.HUnitPlus.Reporting(Reporter(..), State(..), Counts(..),
                                defaultReporter, showPath)
import Text.XML.Expat.Tree

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Strict

-- | Generate an element for a property definition
propertyElem :: (Strict.Text, Strict.Text)
             -- ^ The name/value pair
             -> Node Strict.Text Strict.Text
propertyElem (name, value) = Element { eName = "property", eChildren = [],
                                       eAttributes = [("name", name),
                                                      ("value", value)] }

-- | Generate an element for a set of property definitions
propertiesElem :: [(Strict.Text, Strict.Text)]
               -- ^ A list of name/value pairs to make into properties
               -> Node Strict.Text Strict.Text
propertiesElem props = Element { eName = "properties", eAttributes = [],
                                 eChildren = map propertyElem props }

-- | Generate an element representing output to stdout
systemOutElem :: Strict.Text
              -- ^ The stdout output
              -> Node Strict.Text Strict.Text
systemOutElem content = Element { eName = "system-out", eAttributes = [],
                                  eChildren = [Text content] }

-- | Generate an element representing output to stderr
systemErrElem :: Strict.Text
              -- ^ The stderr output
              -> Node Strict.Text Strict.Text
systemErrElem content = Element { eName = "system-err", eAttributes = [],
                                  eChildren = [Text content] }

-- | Generate an element representing a test failure.
failureElem :: Strict.Text
            -- ^ A message associated with the failure
            -> Node Strict.Text Strict.Text
failureElem message = Element { eAttributes = [("message", message)],
                                eName = "failure", eChildren = [] }

-- | Generate an element representing an error in a test.
errorElem :: Strict.Text
          -- ^ A message associated with the error
          -> Node Strict.Text Strict.Text
errorElem message = Element { eAttributes = [("message", message)],
                              eName = "error", eChildren = [] }

-- | Generate an element for a single test case.
testcaseElem :: Strict.Text
             -- ^ The name of the test
             -> Strict.Text
             -- ^ The path to the test (reported as \"classname\")
             -> Word
             -- ^ The number of assertions in the test
             -> Double
             -- ^ The execution time of the test
             -> [Node Strict.Text Strict.Text]
             -- ^ Elements representing the events that happened
             -- during test execution.
             -> Node Strict.Text Strict.Text
testcaseElem name classname assertions time children =
  Element { eName = "testcase", eChildren = children,
            eAttributes = [("name", name),
                           ("classname", classname),
                           ("assertions", Strict.pack (show assertions)),
                           ("time", Strict.pack (show time))] }

-- | Generate an element for a skipped test case
skippedTestElem :: Strict.Text
                -- ^ The name of the test
                -> Strict.Text
                -- ^ The path of the test
                -> Node Strict.Text Strict.Text
skippedTestElem name classname =
  let
    skippedElem = Element { eName = "skipped", eAttributes = [],
                            eChildren = [] }
  in
    Element { eAttributes = [("name", name), ("classname", classname)],
              eName = "testcase", eChildren = [skippedElem] }

-- | Generate an element for a test suite run
testSuiteElem :: Strict.Text
              -- ^ The name of the test suite
              -> HashMap Strict.Text Strict.Text
              -- ^ The properties defined for this suite
              -> Word
              -- ^ The number of tests
              -> Word
              -- ^ The number of failures
              -> Word
              -- ^ The number of errors
              -> Word
              -- ^ The number of skipped tests
              -> Strict.Text
              -- ^ The hostname of the machine on which this was run
              -> UTCTime
              -- ^ The timestamp at which time this was run
              -> Double
              -- ^ The execution time for the test suite
              -> [Node Strict.Text Strict.Text]
              -- ^ The testcases and output nodes for the test suite
              -> Node Strict.Text Strict.Text
testSuiteElem name propmap tests failures errors skipped
              hostname timestamp time content =
  let
    contentWithProps =
      case sort (HashMap.toList propmap) of
        [] -> content
        props -> propertiesElem props : content
    timestr = formatTime defaultTimeLocale "%c" timestamp
  in
    Element { eName = "testsuite", eChildren = contentWithProps,
              eAttributes = [("name", name),
                             ("hostname", hostname),
                             ("timestamp", Strict.pack timestr),
                             ("time", Strict.pack (show time)),
                             ("tests", Strict.pack (show tests)),
                             ("failures", Strict.pack (show failures)),
                             ("errors", Strict.pack (show errors)),
                             ("skipped", Strict.pack (show skipped))] }

-- | Generate the top-level element containing all test suites
testSuitesElem :: Double
               -- ^ The execution time of all suites
               -> [Node Strict.Text Strict.Text]
               -- ^ Elements representing all the test suites
               -> Node Strict.Text Strict.Text
testSuitesElem time suites =
  Element { eName = "testsuites", eChildren = suites,
            eAttributes = [("time", Strict.pack (show time))] }

-- | A reporter that generates JUnit XML reports
xmlReporter :: Reporter [[Node Strict.Text Strict.Text]]
xmlReporter =
  let
    reportStart = return [[]]

    reportEnd time _ [suites] = return [[testSuitesElem time (reverse suites)]]
    reportEnd _ _ _ = fail "Extra information on node stack"

    reportStartSuite _ stack = return ([] : stack)

    reportEndSuite time State { stName = name, stOptions = options,
                                stCounts = Counts { cCases = cases,
                                                    cErrors = errors,
                                                    cFailures = failures,
                                                    cSkipped = skipped } }
                   (events : rest : stack) =
      do
        hostname <- getHostName
        timestamp <- getCurrentTime
        return ((testSuiteElem name options cases failures errors skipped
                               (Strict.pack hostname) timestamp time
                               (reverse events) : rest) : stack)
    reportEndSuite _ _ stack =
      fail ("Node stack underflow in end suite.\n" ++ show stack)

    reportStartCase _ stack = return ([] : stack)

    reportEndCase time State { stName = name, stPath = testpath,
                               stCounts = Counts { cCaseAsserts = asserts } }
                  (events : rest : stack) =
      return ((testcaseElem name (showPath testpath)
                            asserts time (reverse events) : rest) : stack)
    reportEndCase _ _ _ = fail "Node stack underflow in end case"

    reportSkipCase State { stName = name, stPath = testpath } (rest : stack) =
      return ((skippedTestElem name (showPath testpath) : rest) : stack)
    reportSkipCase _ _ = fail "Node stack underflow in skip case"

    reportFailure msg _ (rest : stack) =
      return ((failureElem msg : rest) : stack)
    reportFailure _ _ _ = fail "Node stack underflow in report failure"

    reportError msg _ (rest : stack) =
      return ((errorElem msg : rest) : stack)
    reportError _ _ _ = fail "Node stack underflow in report error"

    reportSystemOut msg _ (rest : stack) =
      return ((systemOutElem msg : rest) : stack)
    reportSystemOut _ _ _ = fail "Node stack underflow in system out"

    reportSystemErr msg _ (rest : stack) =
      return ((systemErrElem msg : rest) : stack)
    reportSystemErr _ _ _ = fail "Node stack underflow in system err"
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
