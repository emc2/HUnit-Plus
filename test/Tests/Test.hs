module Tests.Test where

import Distribution.TestSuite
import qualified Tests.Test.HUnit as HUnit

tests :: Test
tests = testGroup "Test" [HUnit.tests]
