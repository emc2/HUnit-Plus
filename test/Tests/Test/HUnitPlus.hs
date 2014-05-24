module Tests.Test.HUnitPlus where

import Distribution.TestSuite
import qualified Tests.Test.HUnitPlus.Base as Base
import qualified Tests.Test.HUnitPlus.Execution as Execution
import qualified Tests.Test.HUnitPlus.Filter as Filter
import qualified Tests.Test.HUnitPlus.Main as Main
import qualified Tests.Test.HUnitPlus.Reporting as Reporting
import qualified Tests.Test.HUnitPlus.Text as Text
import qualified Tests.Test.HUnitPlus.XML as XML

tests :: Test
tests = testGroup "HUnitPlus" [Base.tests, Execution.tests, Filter.tests,
                               Reporting.tests, Main.tests,
                               Text.tests, XML.tests]
