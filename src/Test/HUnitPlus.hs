-- | HUnit-Plus is a unit testing framework for Haskell, based on the
-- original HUnit framework.
--
-- In the Haskell module where your tests will reside, import module 
-- "Test.HUnitPlus":
--
-- > import Test.HUnitPlus
--
-- There are a number of facilities which can be used to define test
-- cases.
-- 
-- > test1 = "test1" ~: (assertEqual "for (foo 3)," (1,2) (foo 3))
-- > test2 = "test2" ~: (do (x,y) <- partA 3
-- >                       assertEqual "for the first result of partA," 5 x
-- >                       b <- partB y
-- >                       assertBool ("(partB " ++ show y ++ ") failed") b)
--
-- You can also use the datatypes in "Distribution.TestSuite" to
-- define your tests, or import tests that were defined that way.
--
-- > test3 = Test (TestInstance { ... })
-- >
-- > testgroup1 = Group { ... }
--
-- Additionally, you can use the "Test.HUnitPlus.Legacy" module to
-- define tests in the old HUnit fashion.  The test-creation operators
-- will convert them into HUnit-Plus tests.
--
-- > import qualified Test.HUnitPlus.Legacy as Legacy
-- > test4 = "test4" ~: Test (assertEqual "a == b" a b)
-- > testgroup2 = TestList [TestLabel "testFoo" testBar,
-- >                        TestLabel "testBar" testBar]
--
-- You can add tags to tests as well:
--
-- > test1Tags = testTags ["demo"] test1
-- > test5 = testNameTags "test5" ["demo"] (a @?= b)
--
-- You can also create tests from groups of tests:
--
-- > testgroup3 = "group3" ~: [ test1, test2, testgroup1, test3 ]
--
-- At the top level, group tests into suites.
--
-- > suite = TestSuite { suiteName = "suite",
-- >                     suiteTests = [ testgroup3, test4 ],
-- >                     ... }
--
-- The 'createMain' function in "Test.HUnitPlus.Main" can be used to
-- easily define a @main@ for a test execution program.
--
-- > main = createMain suite
--
-- The resulting program has a number of options for executing tests
-- and reporting the results, which are documented in
-- "Test.HUnitPlus.Main", as well as in the program's \"usage\"
-- output.  Briefly, you can execute all tests by running the program
-- without arguments.
--
-- > $> ./testprog
--
-- You can select tests to be run by supplying a filter:
--
-- > $> ./testprog group3.test1
-- > $> ./testprog @demo
--
-- You can also generate various kinds of reports, and control console
-- output:
--
-- > $> ./testprog --xmlreport --txtreport --consolemode=quiet
--
-- You can also use the 'topLevel' function to supply options to test
-- execution and get the result, allowing limited integration with a
-- larger test execution framework.  ("Test.HUnitPlus.Execution" has
-- even more options).
module Test.HUnitPlus(
       module Test.HUnitPlus.Base,
       module Test.HUnitPlus.Main
       ) where

import Test.HUnitPlus.Base
import Test.HUnitPlus.Main

