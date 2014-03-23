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
suiteString suites = "[" ++ intercalate "," suites ++ "]"

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

suitesName :: [String] -> String
suitesName [] = "no_suites"
suitesName suites = intercalate "_" suites

pathName :: [String] -> String
pathName [] = "no_path"
pathName path = intercalate "_" path

tagsName :: [String] -> String
tagsName [] = "no_tags"
tagsName tags = intercalate "_" tags

makeFilterParseTest :: [String] -> [String] -> [String] -> Test
makeFilterParseTest suites path tags =
  let
    name = "parseFilter_" ++ suitesName suites ++ "__" ++
           pathName path ++ "__" ++ tagsName tags
    string = suiteString suites ++ pathString path ++ tagsString tags
    expected = makeFilter suites (tagsSelector tags (pathSelector path))

    runTest :: IO Progress
    runTest =
      do case parseFilter "test input" string of
          Left e -> return (Finished (Fail ("Parse of " ++ string ++
                                            " failed: " ++ e)))
          Right actual
            | expected == actual -> return (Finished Pass)
            | otherwise ->
              return (Finished (Fail ("In parse of " ++ string ++
                                      "\nexpected " ++ show expected ++
                                      "\nactual " ++ show actual)))

    testInstance = TestInstance { name = name, run = runTest,
                                  tags = [], options = [],
                                  setOption = (\_ _ -> Right testInstance) }
  in
    Test testInstance

filterComponents :: [([String], [String], [String])]
filterComponents =
  foldr (\suiteName tests ->
          foldr (\path tests ->
                  foldr (\tag tests -> (suiteName, path, tag) : tests)
                        tests tagNames)
                tests paths)
        [] suiteNames

whitespaceStrings :: [String]
whitespaceStrings = [ "", " ", "\t" ]

commentStrings = [ "#", "# [Suite]"]

filterParseTests :: [Test]
filterParseTests = map (\(suiteName, path, tag) ->
                         makeFilterParseTest suiteName path tag)
                       filterComponents

makeFileParserTest :: String -> String -> [Filter] -> Test
makeFileParserTest name content expected =
  let
    runTest :: IO Progress
    runTest =
      do case parseFilterFileContent name content of
          Left e -> return (Finished (Fail ("Parse of\n************\n" ++
                                            content ++
                                            "\n************\nfailed: " ++
                                            concat e)))
          Right actual
            | expected == actual -> return (Finished Pass)
            | otherwise ->
              return (Finished (Fail ("In parse of\n************\n" ++ content ++
                                      "\n************\nexpected " ++
                                      show expected ++ "\nactual " ++
                                      show actual)))

    testInstance =
      TestInstance { name = name, tags = [], run = runTest, options = [],
                     setOption = (\_ _ -> Right testInstance) }
  in
    Test testInstance

emptyFileTests :: [(String, String)]
emptyFileTests = [("empty", ""),
                  ("whitespace", "  "),
                  ("tab", "\t"),
                  ("newline", "\n"),
                  ("comment", "# Outer@tag"),
                  ("comment_comment", "# Outer@tag # hello"),
                  ("comment_newline_comment", "# Outer@tag\n# hello"),
                  ("whitespace_comment", " # Outer@tag"),
                  ("tab_comment", "\t# Outer@tag"),
                  ("newline_comment", "\n# [Suite]"),
                  ("newline_whitespace_comment", "\n # [Suite]"),
                  ("newline_tab_comment", "\n\t# [Suite]"),
                  ("comment_newline", "# Outer@tag\n"),
                  ("comment_newline_whitespace", "# Outer@tag\n "),
                  ("comment_newline_tab", "# Outer@tag\n\t"),
                  ("whitespace_comment_newline", " # Outer@tag\n"),
                  ("whitespace_comment_newline_whitespace", " # Outer@tag\n "),
                  ("whitespace_comment_newline_tab", " # Outer@tag\n\t"),
                  ("tab_comment_newline", "\t# Outer@tag\n"),
                  ("tab_comment_newline_whitespace", "\t# Outer@tag\n "),
                  ("tab_comment_newline_tab", "\t# Outer@tag\n\t")]

emptyFileParserTests =
  map (\(name, content) -> makeFileParserTest ("parseFilterFile_" ++ name)
                                              content []) emptyFileTests

testlist =
  emptyFileParserTests ++
  filterParseTests

tests :: Test
tests = testGroup "Filter" testlist
