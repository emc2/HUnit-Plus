module Tests.Test.HUnit where

import Distribution.TestSuite
import qualified Tests.Test.HUnit.Execution as Execution
import qualified Tests.Test.HUnit.Filter as Filter
import qualified Tests.Test.HUnit.Text as Text
import qualified Tests.Test.HUnit.XML as XML

tests :: Test
tests = testGroup "HUnit" [Execution.tests, Filter.tests, Text.tests, XML.tests]
