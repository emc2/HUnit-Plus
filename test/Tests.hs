module Tests where

import Distribution.TestSuite
import qualified Tests.Test as Test

tests :: IO [Test]
tests = return [Test.tests]
