module Tests.Test.HUnit where

import Distribution.TestSuite
import qualified Tests.Test.HUnit.Base as Base
import qualified Tests.Test.HUnit.Execution as Execution
import qualified Tests.Test.HUnit.Filter as Filter
import qualified Tests.Test.HUnit.Main as Main
import qualified Tests.Test.HUnit.Reporting as Reporting
import qualified Tests.Test.HUnit.Text as Text
import qualified Tests.Test.HUnit.XML as XML

tests :: Test
tests = testGroup "HUnit" [Base.tests, Execution.tests, Filter.tests,
                           Reporting.tests, Main.tests, Text.tests, XML.tests]
