module Tests.Test.HUnit.Filter where

import Data.List
import Distribution.TestSuite
import Test.HUnit.Filter

import qualified Data.Set as Set

suiteNames :: [[String]]
suiteNames = [[], ["Suite1"], ["Suite1"], ["Suite1", "Suite2"]]

paths :: [[String]]
paths = [[], ["Outer"], ["Outer", "Middle"], ["Outer", "Middle", "Inner"]]

tagNames :: [[String]]
tagNames = [[], ["tag1"], ["tag2"], ["tag1", "tag2"]]

suiteString :: [String] -> String
suiteString [] = ""
suiteString suites = intercalate "," suites ++ "::"

pathString :: [String] -> String
pathString [] = ""
pathString path = intercalate "." path

tagsString :: [String] -> String
tagsString [] = ""
tagsString tags = '@' : intercalate "," tags

makeFilterString :: [String] -> [String] -> [String] -> String
makeFilterString suites path tags =
  suiteString suites ++ pathString path ++ tagsString tags

tagsSelector :: [String] -> Selector -> Selector
tagsSelector [] inner = inner
tagsSelector tags inner = Tags { tagsNames = Set.fromList tags,
                                 tagsInner = inner }

pathSelector :: [String] -> Selector
pathSelector [] = allSelector
pathSelector (elem : path) =
  Path { pathElem = elem, pathInner = pathSelector path }

makeFilter :: [String] -> Selector -> Filter
makeFilter names selector = Filter { filterSuites = Set.fromList names,
                                     filterSelector = selector }

makeFilterParseTest :: [String] -> [String] -> [String] -> Test
makeFilterParseTest suites path tags =
  let
    name = "parseFilter_" ++ intercalate "_" suites ++ "__" ++
           intercalate "_" path ++ "__" ++ intercalate "_" tags
    string = suiteString suites ++ pathString path ++ tagsString tags
    expected = makeFilter suites (tagsSelector tags (pathSelector path))

    runTest :: IO Progress
    runTest =
      do case parseFilter "test input" string of
          Left e -> return (Finished (Fail ("Parse failed: " ++ e)))
          Right actual
            | expected == actual -> return (Finished Pass)
            | otherwise ->
              return (Finished (Fail ("expected " ++ show expected ++
                                      "\nactual " ++ show actual)))

    testInstance = TestInstance { name = name, run = runTest,
                                  tags = [], options = [],
                                  setOption = (\_ _ -> Right testInstance) }
  in
    Test testInstance

filterParseTests :: [Test]
filterParseTests =
  foldr (\suiteName tests ->
          foldr (\path tests ->
                  foldr (\tag tests ->
                          makeFilterParseTest suiteName path tag : tests)
                        tests tagNames)
                tests paths)
        [] suiteNames

tests :: Test
tests = testGroup "Filter" filterParseTests
