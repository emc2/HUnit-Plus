{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Test controller for running HUnit tests and reporting results as
--   JUnit-style XML reports.
module Test.HUnit.XML(
       propertyElem,
       propertiesElem,
       systemOutElem,
       systemErrElem,
       failureElem,
       errorElem,
       testcaseElem,
       skippedTestElem,
       testSuiteElem,
       testSuitesElem
       ) where

--import Text.XML.Expat.Format
import Text.XML.Expat.Tree
import Data.Time
import System.Locale

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
             -> Int
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
              -> [(String, String)]
              -- ^ The properties defined for this suite
              -> Int
              -- ^ The number of tests
              -> Int
              -- ^ The number of failures
              -> Int
              -- ^ The number of errors
              -> Int
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
testSuiteElem name props tests failures errors skipped
              hostname timestamp time content =
  let
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

{-
reporter = defaultReporter {
    reporterSkipCase = skippedTestElem,
    reporterSystemOut = systemOutElem,
    reporterSystemErr = systemErrElem,
    reporterFailure = failureElem,
    reporterError = errorElem
  }
-}
