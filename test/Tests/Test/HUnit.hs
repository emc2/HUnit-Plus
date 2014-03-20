module Tests.Test.HUnit where

import Distribution.TestSuite
import qualified Tests.Test.HUnit.XML as XML

tests :: Test
tests = testGroup "HUnit" [XML.tests]
