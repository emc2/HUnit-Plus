{-# OPTIONS_GHC -Wall -Werror #-}

-- | The legacy test definitions, from the HUnit library.  These are
-- deprecated in favor of the test definitions from the Cabal
-- [@Distribution.TestSuite@] module, plus the [@TestSuite@]
-- definition in [@Test.HUnit.Base@].  They are included to provide
-- backward-compatibility with existing HUnit test suites.
module Test.HUnit.Legacy(
       Test(..)
       ) where

import Test.HUnit.Base hiding (Test)

-- Test Definition
-- ===============

-- | The basic structure used to create an annotated tree of test cases.
data Test
    -- | A single, independent test case composed.
    = TestCase Assertion
    -- | A set of @Test@s sharing the same level in the hierarchy.
    | TestList [Test]
    -- | A name or description for a subtree of the @Test@s.
    | TestLabel String Test

instance Show Test where
  showsPrec _ (TestCase _)    = showString "TestCase _"
  showsPrec _ (TestList ts)   = showString "TestList " . showList ts
  showsPrec p (TestLabel l t) = showString "TestLabel " . showString l
                                . showChar ' ' . showsPrec p t

instance Testable Test where
  testNameTags testname testtags (TestCase a) = testNameTags testname testtags a
  testNameTags testname testtags (TestList l) = testNameTags testname testtags l
  testNameTags _ testtags (TestLabel testname t) =
    testNameTags testname testtags t
