module Tests.Test where

import Distribution.TestSuite
import qualified Tests.Test.HUnitPlus as HUnitPlus

tests :: Test
tests = testGroup "Test" [HUnitPlus.tests]
