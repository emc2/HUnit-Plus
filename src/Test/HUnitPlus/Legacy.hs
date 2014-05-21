{-# OPTIONS_GHC -Wall -Werror #-}

-- | The legacy test definitions for compatibility with the original
-- HUnit library.  These are not guaranteed to be compatible for all
-- cases, but they should work for most.  The "Testable" instance
-- converts them into "Distribution.TestSuite" tests, with no tags.
--
-- These are deprecated in favor of the test definitions from the
-- Cabal "Distribution.TestSuite" module, plus the "TestSuite"
-- definition in "Test.HUnitPlus.Base".
module Test.HUnitPlus.Legacy(
       Test(..)
       ) where

import Test.HUnitPlus.Base hiding (Test)

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
