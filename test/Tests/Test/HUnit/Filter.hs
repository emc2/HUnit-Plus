module Tests.Test.HUnit.Filter(tests) where

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
    name = suitesName suites ++ "__" ++ pathName path ++ "__" ++ tagsName tags
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
  foldl (\tests suiteName ->
          foldl (\tests path ->
                  foldl (\tests tag -> (suiteName, path, tag) : tests)
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

fileTests :: [(String, String, [Filter])]
fileTests =
  let
    simplePath = Path { pathElem = "Outer",
                        pathInner = Path { pathElem = "Inner",
                                           pathInner = allSelector
                                         }
                      }
    simplePathStr = "Outer.Inner"

    onlyTags = Tags { tagsNames = Set.fromList ["tag1", "tag2"],
                      tagsInner = allSelector }
    onlyTagsStr = "@tag1,tag2"
    pathTags = Tags { tagsNames = Set.fromList ["tag1", "tag2"],
                      tagsInner = simplePath }
    pathTagsStr = "Outer.Inner@tag1,tag2"
    suiteAllFilter = Filter { filterSuites = Set.fromList ["Suite1", "Suite2"],
                              filterSelector = allSelector }
    suiteFilterStr = "[Suite1,Suite2]"
    simplePathFilter = Filter { filterSuites = Set.empty,
                                filterSelector = simplePath }
    suitePathFilter = Filter { filterSuites = Set.fromList ["Suite1", "Suite2"],
                               filterSelector = simplePath }
    suitePathStr = suiteFilterStr ++ simplePathStr
    onlyTagsFilter = Filter { filterSuites = Set.empty,
                                filterSelector = onlyTags }
    suiteTagsFilter = Filter { filterSuites = Set.fromList ["Suite1", "Suite2"],
                               filterSelector = onlyTags }
    suiteTagsStr = suiteFilterStr ++ onlyTagsStr
    pathTagsFilter = Filter { filterSuites = Set.empty,
                                filterSelector = pathTags }
    suitePathTagsFilter =
      Filter { filterSuites = Set.fromList ["Suite1", "Suite2"],
               filterSelector = pathTags }
    suitePathTagsStr = suiteFilterStr ++ pathTagsStr

    suiteSequenceStrs = [ simplePathStr,
                          onlyTagsStr,
                          pathTagsStr,
                          suiteFilterStr,
                          suitePathStr,
                          suiteTagsStr,
                          suitePathTagsStr ]
    suiteSequence = [ simplePathFilter,
                      onlyTagsFilter,
                      pathTagsFilter,
                      suiteAllFilter,
                      suitePathFilter,
                      suiteTagsFilter,
                      suitePathTagsFilter ]
  in
  [("empty", "", []),
   ("space", "  ", []),
   ("tab", "\t", []),
   ("newline", "\n", []),
   ("comment", "# Outer@tag", []),
   ("comment_comment", "# Outer@tag # hello", []),
   ("comment_newline_comment", "# Outer@tag\n# hello", []),
   ("space_comment", " # Outer@tag", []),
   ("tab_comment", "\t# Outer@tag", []),
   ("newline_comment", "\n# [Suite]", []),
   ("newline_space_comment", "\n # [Suite]", []),
   ("newline_tab_comment", "\n\t# [Suite]", []),
   ("comment_newline", "# Outer@tag\n", []),
   ("comment_newline_space", "# Outer@tag\n ", []),
   ("comment_newline_tab", "# Outer@tag\n\t", []),
   ("space_comment_newline", " # Outer@tag\n", []),
   ("space_comment_newline_space", " # Outer@tag\n ", []),
   ("space_comment_newline_tab", " # Outer@tag\n\t", []),
   ("tab_comment_newline", "\t# Outer@tag\n", []),
   ("tab_comment_newline_space", "\t# Outer@tag\n ", []),
   ("tab_comment_newline_tab", "\t# Outer@tag\n\t", []),
   ("suiteAll", suiteFilterStr, [suiteAllFilter]),
   ("space_suiteAll", " " ++ suiteFilterStr, [suiteAllFilter]),
   ("tab_suiteAll", "\t" ++ suiteFilterStr, [suiteAllFilter]),
   ("newline_suiteAll", "\n" ++ suiteFilterStr, [suiteAllFilter]),
   ("comment_suiteAll", "# comment\n" ++ suiteFilterStr, [suiteAllFilter]),
   ("suiteAll_space", suiteFilterStr ++ " ", [suiteAllFilter]),
   ("suiteAll_tab", suiteFilterStr ++ "\t", [suiteAllFilter]),
   ("suiteAll_newline", suiteFilterStr ++ "\n", [suiteAllFilter]),
   ("suiteAll_comment", suiteFilterStr ++ "# comment\n", [suiteAllFilter]),
   ("suiteAll_newline_suiteAll",
    suiteFilterStr ++ "\n" ++ suiteFilterStr,
    [suiteAllFilter, suiteAllFilter]),
   ("suiteAll_comment_suiteAll",
    suiteFilterStr ++ "# comment\n" ++ suiteFilterStr,
    [suiteAllFilter, suiteAllFilter]),
   ("simplePath", simplePathStr, [simplePathFilter]),
   ("space_simplePath", " " ++ simplePathStr, [simplePathFilter]),
   ("tab_simplePath", "\t" ++ simplePathStr, [simplePathFilter]),
   ("newline_simplePath", "\n" ++ simplePathStr, [simplePathFilter]),
   ("comment_simplePath", "# comment\n" ++ simplePathStr, [simplePathFilter]),
   ("simplePath_space", simplePathStr ++ " ", [simplePathFilter]),
   ("simplePath_tab", simplePathStr ++ "\t", [simplePathFilter]),
   ("simplePath_newline", simplePathStr ++ "\n", [simplePathFilter]),
   ("simplePath_comment", simplePathStr ++ "# comment\n", [simplePathFilter]),
   ("simplePath_newline_simplePath",
    simplePathStr ++ "\n" ++ simplePathStr,
    [simplePathFilter, simplePathFilter]),
   ("simplePath_comment_simplePath",
    simplePathStr ++ "# comment\n" ++ simplePathStr,
    [simplePathFilter, simplePathFilter]),
   ("suitePath", suitePathStr, [suitePathFilter]),
   ("space_suitePath", " " ++ suitePathStr, [suitePathFilter]),
   ("tab_suitePath", "\t" ++ suitePathStr, [suitePathFilter]),
   ("newline_suitePath", "\n" ++ suitePathStr, [suitePathFilter]),
   ("comment_suitePath", "# comment\n" ++ suitePathStr, [suitePathFilter]),
   ("suitePath_space", suitePathStr ++ " ", [suitePathFilter]),
   ("suitePath_tab", suitePathStr ++ "\t", [suitePathFilter]),
   ("suitePath_newline", suitePathStr ++ "\n", [suitePathFilter]),
   ("suitePath_comment", suitePathStr ++ "# comment\n", [suitePathFilter]),
   ("suitePath_newline_suitePath",
    suitePathStr ++ "\n" ++ suitePathStr,
    [suitePathFilter, suitePathFilter]),
   ("suitePath_comment_suitePath",
    suitePathStr ++ "# comment\n" ++ suitePathStr,
    [suitePathFilter, suitePathFilter]),
   ("onlyTags", onlyTagsStr, [onlyTagsFilter]),
   ("space_onlyTags", " " ++ onlyTagsStr, [onlyTagsFilter]),
   ("tab_onlyTags", "\t" ++ onlyTagsStr, [onlyTagsFilter]),
   ("newline_onlyTags", "\n" ++ onlyTagsStr, [onlyTagsFilter]),
   ("comment_onlyTags", "# comment\n" ++ onlyTagsStr, [onlyTagsFilter]),
   ("onlyTags_space", onlyTagsStr ++ " ", [onlyTagsFilter]),
   ("onlyTags_tab", onlyTagsStr ++ "\t", [onlyTagsFilter]),
   ("onlyTags_newline", onlyTagsStr ++ "\n", [onlyTagsFilter]),
   ("onlyTags_comment", onlyTagsStr ++ "# comment\n", [onlyTagsFilter]),
   ("onlyTags_newline_onlyTags",
    onlyTagsStr ++ "\n" ++ onlyTagsStr,
    [onlyTagsFilter, onlyTagsFilter]),
   ("onlyTags_comment_onlyTags",
    onlyTagsStr ++ "# comment\n" ++ onlyTagsStr,
    [onlyTagsFilter, onlyTagsFilter]),
   ("suiteTags", suiteTagsStr, [suiteTagsFilter]),
   ("space_suiteTags", " " ++ suiteTagsStr, [suiteTagsFilter]),
   ("tab_suiteTags", "\t" ++ suiteTagsStr, [suiteTagsFilter]),
   ("newline_suiteTags", "\n" ++ suiteTagsStr, [suiteTagsFilter]),
   ("comment_suiteTags", "# comment\n" ++ suiteTagsStr, [suiteTagsFilter]),
   ("suiteTags_space", suiteTagsStr ++ " ", [suiteTagsFilter]),
   ("suiteTags_tab", suiteTagsStr ++ "\t", [suiteTagsFilter]),
   ("suiteTags_newline", suiteTagsStr ++ "\n", [suiteTagsFilter]),
   ("suiteTags_comment", suiteTagsStr ++ "# comment\n", [suiteTagsFilter]),
   ("suiteTags_newline_suiteTags",
    suiteTagsStr ++ "\n" ++ suiteTagsStr,
    [suiteTagsFilter, suiteTagsFilter]),
   ("suiteTags_comment_suiteTags",
    suiteTagsStr ++ "# comment\n" ++ suiteTagsStr,
    [suiteTagsFilter, suiteTagsFilter]),
   ("pathTags", pathTagsStr, [pathTagsFilter]),
   ("space_pathTags", " " ++ pathTagsStr, [pathTagsFilter]),
   ("tab_pathTags", "\t" ++ pathTagsStr, [pathTagsFilter]),
   ("newline_pathTags", "\n" ++ pathTagsStr, [pathTagsFilter]),
   ("comment_pathTags", "# comment\n" ++ pathTagsStr, [pathTagsFilter]),
   ("pathTags_space", pathTagsStr ++ " ", [pathTagsFilter]),
   ("pathTags_tab", pathTagsStr ++ "\t", [pathTagsFilter]),
   ("pathTags_newline", pathTagsStr ++ "\n", [pathTagsFilter]),
   ("pathTags_comment", pathTagsStr ++ "# comment\n", [pathTagsFilter]),
   ("pathTags_newline_pathTags",
    pathTagsStr ++ "\n" ++ pathTagsStr,
    [pathTagsFilter, pathTagsFilter]),
   ("pathTags_comment_pathTags",
    pathTagsStr ++ "# comment\n" ++ pathTagsStr,
    [pathTagsFilter, pathTagsFilter]),
   ("suitePathTags", suitePathTagsStr, [suitePathTagsFilter]),
   ("space_suitePathTags", " " ++ suitePathTagsStr, [suitePathTagsFilter]),
   ("tab_suitePathTags", "\t" ++ suitePathTagsStr, [suitePathTagsFilter]),
   ("newline_suitePathTags", "\n" ++ suitePathTagsStr, [suitePathTagsFilter]),
   ("comment_suitePathTags", "# comment\n" ++ suitePathTagsStr,
    [suitePathTagsFilter]),
   ("suitePathTags_space", suitePathTagsStr ++ " ", [suitePathTagsFilter]),
   ("suitePathTags_tab", suitePathTagsStr ++ "\t", [suitePathTagsFilter]),
   ("suitePathTags_newline", suitePathTagsStr ++ "\n", [suitePathTagsFilter]),
   ("suitePathTags_comment", suitePathTagsStr ++ "# comment\n",
    [suitePathTagsFilter]),
   ("suitePathTags_newline_suitePathTags",
    suitePathTagsStr ++ "\n" ++ suitePathTagsStr,
    [suitePathTagsFilter, suitePathTagsFilter]),
   ("suitePathTags_comment_suitePathTags",
    suitePathTagsStr ++ "# comment\n" ++ suitePathTagsStr,
    [suitePathTagsFilter, suitePathTagsFilter]),
   ("sequence", intercalate "\n" suiteSequenceStrs, suiteSequence),
   ("commented_sequence", intercalate "# comment \n" suiteSequenceStrs,
    suiteSequence),
   ("indented_sequence", concat (map (++ "\n  ") suiteSequenceStrs),
    suiteSequence)
  ]

fileParserTests =
  map (\(name, content, expected) -> makeFileParserTest name content expected)
      fileTests

innerPath :: Selector -> Selector
innerPath inner = Path { pathElem = "Inner", pathInner = inner }

outerPath :: Selector -> Selector
outerPath inner = Path { pathElem = "Outer", pathInner = inner }

outerInnerPath :: Selector -> Selector
outerInnerPath = outerPath . innerPath

tag1 :: Selector -> Selector
tag1 inner = Tags { tagsNames = Set.singleton "tag1", tagsInner = inner }

tag2 :: Selector -> Selector
tag2 inner = Tags { tagsNames = Set.singleton "tag2", tagsInner = inner }

tag12 :: Selector -> Selector
tag12 inner = Tags { tagsNames = Set.fromList [ "tag1", "tag2" ],
                     tagsInner = inner }

union2 :: Selector -> Selector -> Selector
union2 inner1 inner2 = Union { unionInners = Set.fromList [ inner1, inner2 ] }

union3 :: Selector -> Selector -> Selector -> Selector
union3 inner1 inner2 inner3 =
  Union { unionInners = Set.fromList [ inner1, inner2, inner3 ] }

normalizeSelectorTestCases :: [(String, Selector, Selector)]
normalizeSelectorTestCases =
  [("All", allSelector, allSelector),
   ("Outer", outerPath allSelector, outerPath allSelector),
   ("Outer_Inner", outerInnerPath allSelector, outerInnerPath allSelector),
   ("tag1", tag1 allSelector, tag1 allSelector),
   ("tag12", tag12 allSelector, tag12 allSelector),
   ("union__all__all", union2 allSelector allSelector, allSelector),
   ("union__Outer__all", union2 (outerPath allSelector) allSelector,
    allSelector),
   ("union__Outer_Inner__all", union2 (outerInnerPath allSelector) allSelector,
    allSelector),
   ("union__Outer__tag1", union2 (outerPath allSelector) (tag1 allSelector),
    union2 (outerPath allSelector) (tag1 allSelector)),
   ("union__Outer_Inner__tag1",
    union2 (outerInnerPath allSelector) (tag1 allSelector),
    union2 (outerInnerPath allSelector) (tag1 allSelector)),
   ("union__Outer__tag12", union2 (outerPath allSelector) (tag12 allSelector),
    union2 (outerPath allSelector) (tag12 allSelector)),
   ("union__Inner__tag12", union2 (innerPath allSelector) (tag12 allSelector),
    union2 (innerPath allSelector) (tag12 allSelector)),
   ("union__Outer_Inner__tag12",
    union2 (outerInnerPath allSelector) (tag12 allSelector),
    union2 (outerInnerPath allSelector) (tag12 allSelector)),
   ("union__Outer__Outer",
    union2 (outerPath allSelector) (outerPath allSelector),
    outerPath allSelector),
   ("union__Outer__Inner",
    union2 (outerPath allSelector) (innerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union__Outer__Outer_Inner",
    union2 (outerPath allSelector) (outerInnerPath allSelector),
    outerPath allSelector),
   ("union__Inner__Outer_Inner",
    union2 (innerPath allSelector) (outerInnerPath allSelector),
    union2 (innerPath allSelector) (outerInnerPath allSelector)),
   ("union__Outer_Inner__Outer_Inner",
    union2 (outerInnerPath allSelector) (outerInnerPath allSelector),
    outerInnerPath allSelector),
   ("union__tag1__all", union2 allSelector (tag1 allSelector), allSelector),
   ("union__tag12__all", union2 allSelector (tag12 allSelector), allSelector),
   ("union__tag1__tag1", union2 (tag1 allSelector) (tag1 allSelector),
    tag1 allSelector),
   ("union__tag2__tag1", union2 (tag1 allSelector) (tag2 allSelector),
    tag12 allSelector),
   ("union__tag12__tag1", union2 (tag1 allSelector) (tag12 allSelector),
    tag12 allSelector),
   ("union__tag12__tag2", union2 (tag2 allSelector) (tag12 allSelector),
    tag12 allSelector),
   ("union__tag12__tag12", union2 (tag12 allSelector) (tag12 allSelector),
    tag12 allSelector),
   ("tag1_tag2", tag1 (tag2 allSelector), tag12 allSelector),
   ("tag2_tag1", tag2 (tag1 allSelector), tag12 allSelector),
   ("tag12_tag2", tag12 (tag2 allSelector), tag12 allSelector),
   ("tag2_tag12", tag2 (tag12 allSelector), tag12 allSelector),
   ("tag1_tag12", tag1 (tag12 allSelector), tag12 allSelector),
   ("tag12_tag1", tag12 (tag1 allSelector), tag12 allSelector),
   ("tag1_Outer", tag1 (outerPath allSelector), tag1 (outerPath allSelector)),
   ("tag1_Outer_Inner", tag1 (outerInnerPath allSelector),
    tag1 (outerInnerPath allSelector)),
   ("tag12_Outer", tag12 (outerPath allSelector), tag12 (outerPath allSelector)),
   ("tag12_Outer_Inner", tag12 (outerInnerPath allSelector),
    tag12 (outerInnerPath allSelector)),
   ("Outer_tag1", outerPath (tag1 allSelector), tag1 (outerPath allSelector)),
   ("Outer_tag2", outerPath (tag2 allSelector), tag2 (outerPath allSelector)),
   ("Outer_tag12", outerPath (tag12 allSelector), tag12 (outerPath allSelector)),
   ("Inner_tag1", innerPath (tag1 allSelector), tag1 (innerPath allSelector)),
   ("Inner_tag2", innerPath (tag2 allSelector), tag2 (innerPath allSelector)),
   ("Inner_tag12", innerPath (tag12 allSelector), tag12 (innerPath allSelector)),
   ("Outer_Inner_tag1", outerInnerPath (tag1 allSelector),
    tag1 (outerInnerPath allSelector)),
   ("Outer_Inner_tag2", outerInnerPath (tag2 allSelector),
    tag2 (outerInnerPath allSelector)),
   ("Outer_Inner_tag12", outerInnerPath (tag12 allSelector),
    tag12 (outerInnerPath allSelector)),
   ("union__tag1_Outer__tag1_Outer",
    union2 (tag1 (outerPath allSelector)) (tag1 (outerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union__tag1_Outer__tag2_Outer",
    union2 (tag1 (outerPath allSelector)) (tag2 (outerPath allSelector)),
    tag12 (outerPath allSelector)),
   ("union__tag1_Outer__tag12_Outer",
    union2 (tag1 (outerPath allSelector)) (tag12 (outerPath allSelector)),
    tag12 (outerPath allSelector)),
   ("union__tag2_Outer__tag12_Outer",
    union2 (tag2 (outerPath allSelector)) (tag12 (outerPath allSelector)),
    tag12 (outerPath allSelector)),
   ("union__tag1_Outer__tag1_Inner",
    union2 (tag1 (outerPath allSelector)) (tag1 (innerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union__tag1_Outer__tag2_Inner",
    union2 (tag1 (outerPath allSelector)) (tag2 (innerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag2 (innerPath allSelector))),
   ("union__tag1_Outer__tag12_Inner",
    union2 (tag1 (outerPath allSelector)) (tag12 (innerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag12 (innerPath allSelector))),
   ("union__tag2_Outer__tag12_Inner",
    union2 (tag2 (outerPath allSelector)) (tag12 (innerPath allSelector)),
    union2 (tag2 (outerPath allSelector)) (tag12 (innerPath allSelector))),
   ("union__tag1_Outer__tag1_Outer_Inner",
    union2 (tag1 (outerPath allSelector)) (tag1 (outerInnerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union__tag1_Outer__tag2_Outer_Inner",
    union2 (tag1 (outerPath allSelector)) (tag2 (outerInnerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union__tag1_Outer__tag12_Outer_Inner",
    union2 (tag1 (outerPath allSelector)) (tag12 (outerInnerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union__tag2_Outer__tag12_Outer_Inner",
    union2 (tag2 (outerPath allSelector)) (tag12 (outerInnerPath allSelector)),
    union2 (tag2 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union__Outer_tag1__Outer_tag1",
    union2 (outerPath (tag1 allSelector)) (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union__Outer_tag1__Outer_tag2",
    union2 (outerPath (tag1 allSelector)) (outerPath (tag2 allSelector)),
    tag12 (outerPath allSelector)),
   ("union__Outer_tag1__Outer_tag12",
    union2 (outerPath (tag1 allSelector)) (outerPath (tag12 allSelector)),
    tag12 (outerPath allSelector)),
   ("union__Outer_tag2__Outer_tag12",
    union2 (outerPath (tag2 allSelector)) (outerPath (tag12 allSelector)),
    tag12 (outerPath allSelector)),
   ("union__Outer_tag1__Inner_tag1",
    union2 (outerPath (tag1 allSelector)) (innerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union__Outer_tag1__Inner_tag2",
    union2 (outerPath (tag1 allSelector)) (innerPath (tag2 allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag2 (innerPath allSelector))),
   ("union__Outer_tag1__Inner_tag12",
    union2 (outerPath (tag1 allSelector)) (innerPath (tag12 allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag12 (innerPath allSelector))),
   ("union__Outer_tag2__Inner_tag12",
    union2 (outerPath (tag2 allSelector)) (innerPath (tag12 allSelector)),
    union2 (tag2 (outerPath allSelector)) (tag12 (innerPath allSelector))),
   ("union__Outer_tag1__Outer_Inner_tag1",
    union2 (outerPath (tag1 allSelector)) (outerInnerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union__Outer_tag1__Outer_Inner_tag2",
    union2 (outerPath (tag1 allSelector)) (outerInnerPath (tag2 allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union__Outer_tag1__Outer_Inner_tag12",
    union2 (outerPath (tag1 allSelector)) (outerInnerPath (tag12 allSelector)),
    union2 (tag2 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union__Outer_tag2__Outer_Inner_tag12",
    union2 (outerPath (tag2 allSelector)) (outerInnerPath (tag12 allSelector)),
    union2 (tag2 (outerPath allSelector)) (tag12 (outerInnerPath allSelector))),
   ("union___union__Outer__All___All",
    union2 (union2 (outerPath allSelector) allSelector) allSelector,
    allSelector),
   ("union___union__Outer__All___Outer",
    union2 (union2 (outerPath allSelector) allSelector) (outerPath allSelector),
    allSelector),
   ("union___union__Outer__All___Inner",
    union2 (union2 (outerPath allSelector) allSelector) (innerPath allSelector),
    allSelector),
   ("union___union__Outer__All___Outer_Inner",
    union2 (union2 (outerPath allSelector) allSelector)
           (outerInnerPath allSelector),
    allSelector),
   ("union___union__Outer__Outer___All",
    union2 (union2 (outerPath allSelector) (outerPath allSelector)) allSelector,
    allSelector),
   ("union___union__Outer__Outer___Outer",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (outerPath allSelector),
    (outerPath allSelector)),
   ("union___union__Outer__Outer___Inner",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (innerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer__Outer___Outer_Inner",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (outerInnerPath allSelector),
    (outerPath allSelector)),
   ("union___union__Outer__Inner___All",
    union2 (union2 (outerPath allSelector) (innerPath allSelector)) allSelector,
    allSelector),
   ("union___union__Outer__Inner___Outer",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (outerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer__Inner___Inner",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (innerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer__Inner___Outer_Inner",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (outerInnerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer__Outer_Inner___All",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           allSelector,
    allSelector),
   ("union___union__Outer__Outer_Inner___Outer",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerPath allSelector),
    outerPath allSelector),
   ("union___union__Outer__Outer_Inner___Inner",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer__Outer_Inner___Outer_Inner",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath allSelector),
    outerPath allSelector),
   ("union___union__Inner__All___All",
    union2 (union2 (innerPath allSelector) allSelector) allSelector,
    allSelector),
   ("union___union__Inner__All___Outer",
    union2 (union2 (innerPath allSelector) allSelector) (outerPath allSelector),
    allSelector),
   ("union___union__Inner__All___Inner",
    union2 (union2 (innerPath allSelector) allSelector) (innerPath allSelector),
    allSelector),
   ("union___union__Inner__All___Outer_Inner",
    union2 (union2 (innerPath allSelector) allSelector)
           (outerInnerPath allSelector),
    allSelector),
   ("union___union__Inner__Inner___All",
    union2 (union2 (innerPath allSelector) (innerPath allSelector)) allSelector,
    allSelector),
   ("union___union__Inner__Inner___Outer",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (outerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Inner__Inner___Inner",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (innerPath allSelector),
    innerPath allSelector),
   ("union___union__Inner__Inner___Outer_Inner",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (outerInnerPath allSelector),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union___union__Inner__Outer_Inner___All",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (outerPath allSelector),
    allSelector),
   ("union___union__Inner__Outer_Inner___Outer",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (outerPath allSelector),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union___union__Inner__Outer_Inner___Inner",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (innerPath allSelector),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union___union__Inner__Outer_Inner___Outer_Inner",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath allSelector),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer_Inner__All___All",
    union2 (union2 (outerInnerPath allSelector) allSelector) allSelector,
    allSelector),
   ("union___union__Outer_Inner__All___Outer",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (outerPath allSelector),
    allSelector),
   ("union___union__Outer_Inner__All___Inner",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (innerPath allSelector),
    allSelector),
   ("union___union__Outer_Inner__All___Outer_Inner",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (outerInnerPath allSelector),
    allSelector),
   ("union___union__Outer_Inner__Outer_Inner___All",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           allSelector,
    allSelector),
   ("union___union__Outer_Inner__Outer_Inner___Outer",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (outerPath allSelector),
    outerPath allSelector),
   ("union___union__Outer_Inner__Outer_Inner___Inner",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (innerPath allSelector),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union___union__Outer_Inner__Outer_Inner___Outer_Inner",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath allSelector),
    outerInnerPath allSelector),
   ("union___union__Outer__All___tag1",
    union2 (union2 (outerPath allSelector) allSelector) (tag1 allSelector),
    allSelector),
   ("union___union__tag1__All___Outer",
    union2 (union2 (tag1 allSelector) allSelector) (outerPath allSelector),
    allSelector),
   ("union___union__tag1__Outer___All",
    union2 (union2 (tag1 allSelector) (outerPath allSelector)) allSelector,
    allSelector),
   ("union___union__tag1__All___tag1",
    union2 (union2 (tag1 allSelector) allSelector) (tag1 allSelector),
    allSelector),
   ("union___union__tag1__tag1___All",
    union2 (union2 (tag1 allSelector) (tag1 allSelector)) allSelector,
    allSelector),
   ("union___union__tag1__All___tag2",
    union2 (union2 (tag1 allSelector) allSelector) (tag2 allSelector),
    allSelector),
   ("union___union__tag1__tag2___All",
    union2 (union2 (tag1 allSelector) (tag2 allSelector)) allSelector,
    allSelector),
   ("union___union__tag1__All___tag12",
    union2 (union2 (tag1 allSelector) allSelector) (tag12 allSelector),
    allSelector),
   ("union___union__tag1__tag12___All",
    union2 (union2 (tag1 allSelector) (tag12 allSelector)) allSelector,
    allSelector),
   ("tag1_Outer_tag1", tag1 (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("tag1_Outer_Inner_tag1", tag1 (outerInnerPath (tag1 allSelector)),
    tag1 (outerInnerPath allSelector)),
   ("tag1_Outer_tag2", tag1 (outerPath (tag2 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag1_Outer_Inner_tag2", tag1 (outerInnerPath (tag2 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag1_Outer_tag12", tag1 (outerPath (tag12 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag1_Outer_Inner_tag12", tag1 (outerInnerPath (tag12 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag2_Outer_tag1", tag2 (outerPath (tag1 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag2_Outer_Inner_tag1", tag2 (outerInnerPath (tag1 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag2_Outer_tag2", tag2 (outerPath (tag2 allSelector)),
    tag2 (outerPath allSelector)),
   ("tag2_Outer_Inner_tag2", tag2 (outerInnerPath (tag2 allSelector)),
    tag2 (outerInnerPath allSelector)),
   ("tag2_Outer_tag12", tag2 (outerPath (tag12 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag2_Outer_Inner_tag12", tag2 (outerInnerPath (tag12 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag12_Outer_tag1", tag12 (outerPath (tag1 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag12_Outer_Inner_tag1", tag12 (outerInnerPath (tag1 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag12_Outer_tag2", tag12 (outerPath (tag2 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag12_Outer_Inner_tag2", tag12 (outerInnerPath (tag2 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("tag12_Outer_tag12", tag12 (outerPath (tag12 allSelector)),
    tag12 (outerPath allSelector)),
   ("tag12_Outer_Inner_tag12", tag12 (outerInnerPath (tag12 allSelector)),
    tag12 (outerInnerPath allSelector)),
   ("union__tag1_Outer__Outer_tag1",
    union2 (tag1 (outerPath allSelector)) (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__All____Outer",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (outerPath allSelector),
    union2 (tag1 allSelector) (outerPath allSelector)),
   ("union____tag1___union__Outer__All____Inner",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (innerPath allSelector),
    union2 (tag1 allSelector) (innerPath allSelector)),
   ("union____tag1___union__Outer__All____Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (outerInnerPath allSelector),
    union2 (tag1 allSelector) (outerInnerPath allSelector)),
   ("union____union__Outer__All____tag1___Outer",
    union2 (union2 (outerPath allSelector) allSelector)
           (tag1 (outerPath allSelector)),
    allSelector),
   ("union____union__Outer__All____tag1___Inner",
    union2 (union2 (outerPath allSelector) allSelector)
           (tag1 (innerPath allSelector)),
    allSelector),
   ("union____union__Outer__All____tag1___Outer_Inner",
    union2 (union2 (outerPath allSelector) allSelector)
           (tag1 (outerInnerPath allSelector)),
    allSelector),
   ("union____union__Outer__All____Outer___tag1",
    union2 (union2 (outerPath allSelector) allSelector)
           (outerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Outer__All____Inner___tag1",
    union2 (union2 (outerPath allSelector) allSelector)
           (innerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Outer__All____Outer_Inner___tag1",
    union2 (union2 (outerPath allSelector) allSelector)
           (outerInnerPath (tag1 allSelector)),
    allSelector),
   ("union____tag1___union__Outer__All____tag1___Outer",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (tag1 (outerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__All____tag1___Inner",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (tag1 (innerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__All____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (tag1 (outerInnerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__All____Outer___tag1",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (outerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__All____Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (innerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__All____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) allSelector))
           (outerInnerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer__Outer____All",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           allSelector,
    allSelector),
   ("union____tag1___union__Outer__Outer____Outer",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (outerPath allSelector),
    outerPath allSelector),
   ("union____tag1___union__Outer__Outer____Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (innerPath allSelector),
    union2 (tag1 (outerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Outer__Outer____Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (outerInnerPath allSelector),
    union2 (tag1 (outerPath allSelector)) (outerInnerPath allSelector)),
   ("union____union__Outer__Outer____tag1___Outer",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (tag1 (outerPath allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer____tag1___Inner",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (tag1 (innerPath allSelector)),
    union2 (outerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Outer__Outer____tag1___Outer_Inner",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    union2 (outerPath allSelector) (tag1 (outerInnerPath allSelector))),
   ("union____union__Outer__Outer____Outer___tag1",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (outerPath (tag1 allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer____Inner___tag1",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (innerPath (tag1 allSelector)),
    union2 (outerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Outer__Outer____Outer_Inner___tag1",
    union2 (union2 (outerPath allSelector) (outerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    union2 (outerPath allSelector) (tag1 (outerInnerPath allSelector))),
   ("union____tag1___union__Outer__Outer____tag1___Outer",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer____tag1___Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (tag1 (innerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Outer____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer____Outer___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer____Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (innerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Outer____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Inner____All",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           allSelector,
    allSelector),
   ("union____tag1___union__Outer__Inner____Outer",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (outerPath allSelector),
    union2 (outerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____Inner",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (innerPath allSelector),
    union2 (tag1 (outerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Outer__Inner____Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (outerInnerPath allSelector),
    union3 (tag1 (outerPath allSelector)) (tag1 (innerPath allSelector))
           (outerInnerPath allSelector)),
   ("union____union__Outer__Inner____tag1___Outer",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (tag1 (outerPath allSelector)),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union____union__Outer__Inner____tag1___Inner",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (tag1 (innerPath allSelector)),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union____union__Outer__Inner____tag1___Outer_Inner",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    union3 (outerPath allSelector) (innerPath allSelector)
           (tag1 (outerInnerPath allSelector))),
   ("union____union__Outer__Inner____Outer___tag1",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (outerPath (tag1 allSelector)),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union____union__Outer__Inner____Inner___tag1",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (innerPath (tag1 allSelector)),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union____union__Outer__Inner____Outer_Inner___tag1",
    union2 (union2 (outerPath allSelector) (innerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    union3 (outerPath allSelector) (innerPath allSelector)
           (tag1 (outerInnerPath allSelector))),
   ("union____tag1___union__Outer__Inner____tag1___Outer",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____tag1___Inner",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (tag1 (innerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____Outer___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (innerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Inner____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (innerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer__Outer_Inner____All",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           allSelector,
    allSelector),
   ("union____tag1___union__Outer__Outer_Inner____Outer",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerPath allSelector),
    outerPath allSelector),
   ("union____tag1___union__Outer__Outer_Inner____Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerPath allSelector),
    union2 (tag1 (outerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Outer__Outer_Inner____Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerInnerPath allSelector),
    union2 (tag1 (outerPath allSelector)) (outerInnerPath allSelector)),
   ("union____union__Outer__Outer_Inner____tag1___Outer",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerPath allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer_Inner____tag1___Inner",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerPath allSelector)),
    union2 (outerPath allSelector) (innerPath allSelector)),
   ("union____union__Outer__Outer_Inner____tag1___Outer_Inner",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer_Inner____Outer___tag1",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerPath (tag1 allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer_Inner____Inner___tag1",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerPath (tag1 allSelector)),
    outerPath allSelector),
   ("union____union__Outer__Outer_Inner____Outer_Inner___tag1",
    union2 (union2 (outerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    outerPath allSelector),
   ("union____tag1___union__Outer__Outer_Inner____tag1___Outer",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer_Inner____tag1___Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (outerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag1 (innerPath allSelector))),
   ("union____tag1___union__Outer__Outer_Inner____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer_Inner____Outer___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer__Outer_Inner____Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerPath (tag1 allSelector)),
    union2 (tag1 (outerPath allSelector)) (tag1 (innerPath allSelector))),
   ("union____tag1___union__Outer__Outer_Inner____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerPath allSelector) (outerInnerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Inner__All____All",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           allSelector,
    allSelector),
   ("union____tag1___union__Inner__All____Outer",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (outerPath allSelector),
    union2 (tag1 allSelector) (outerPath allSelector)),
   ("union____tag1___union__Inner__All____Inner",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (innerPath allSelector),
    union2 (tag1 allSelector) (innerPath allSelector)),
   ("union____tag1___union__Inner__All____Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (outerInnerPath allSelector),
    union2 (tag1 allSelector) (outerInnerPath allSelector)),
   ("union____union__Inner__All____tag1___Outer",
    union2 (union2 (innerPath allSelector) allSelector)
           (tag1 (outerPath allSelector)),
    allSelector),
   ("union____union__Inner__All____tag1___Inner",
    union2 (union2 (innerPath allSelector) allSelector)
           (tag1 (innerPath allSelector)),
    allSelector),
   ("union____union__Inner__All____tag1___Outer_Inner",
    union2 (union2 (innerPath allSelector) allSelector)
           (tag1 (outerInnerPath allSelector)),
    allSelector),
   ("union____union__Inner__All____Outer___tag1",
    union2 (union2 (innerPath allSelector) allSelector)
           (outerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Inner__All____Inner___tag1",
    union2 (union2 (innerPath allSelector) allSelector)
           (innerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Inner__All____Outer_Inner___tag1",
    union2 (union2 (innerPath allSelector) allSelector)
           (outerInnerPath (tag1 allSelector)),
    allSelector),
   ("union____tag1___union__Inner__All____tag1___Outer",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (tag1 (outerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__All____tag1___Inner",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (tag1 (innerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__All____tag1___Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (tag1 (outerInnerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__All____Outer___tag1",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (outerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__All____Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (innerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__All____Outer_Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) allSelector))
           (outerInnerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Inner__Inner____All",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           allSelector,
    allSelector),
   ("union____tag1___union__Inner__Inner____Outer",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (outerPath allSelector),
    union2 (outerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____tag1___union__Inner__Inner____Inner",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (innerPath allSelector),
    innerPath allSelector),
   ("union____tag1___union__Inner__Inner____Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (outerInnerPath allSelector),
    union2 (outerInnerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Inner__Inner____tag1___Outer",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (tag1 (outerPath allSelector)),
    union2 (tag1 (outerPath allSelector)) (innerPath allSelector)),
   ("union____union__Inner__Inner____tag1___Inner",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (tag1 (innerPath allSelector)),
    innerPath allSelector),
   ("union____union__Inner__Inner____tag1___Outer_Inner",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    union2 (tag1 (outerInnerPath allSelector)) (innerPath allSelector)),
   ("union____union__Inner__Inner____Outer___tag1",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (outerPath (tag1 allSelector)),
    union2 (tag1 (outerPath allSelector)) (innerPath allSelector)),
   ("union____union__Inner__Inner____Inner___tag1",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (innerPath (tag1 allSelector)),
    innerPath allSelector),
   ("union____union__Inner__Inner____Outer_Inner___tag1",
    union2 (union2 (innerPath allSelector) (innerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    union2 (tag1 (outerInnerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Inner__Inner____tag1___Outer",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Inner____tag1___Inner",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (tag1 (innerPath allSelector)),
    tag1 (innerPath allSelector)),
   ("union____tag1___union__Inner__Inner____tag1___Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Inner____Outer___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Inner____Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (innerPath (tag1 allSelector)),
    tag1 (innerPath allSelector)),
   ("union____tag1___union__Inner__Inner____Outer_Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (innerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____All",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (outerPath allSelector),
    allSelector),
   ("union____tag1___union__Inner__Outer_Inner____Outer",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (outerPath allSelector),
    union3 (outerPath allSelector) (tag1 (outerInnerPath allSelector))
           (tag1 (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____Inner",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (innerPath allSelector),
    union2 (tag1 (outerInnerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Inner__Outer_Inner____Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (outerInnerPath allSelector),
    union2 (outerInnerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Inner__Outer_Inner____tag1___Outer",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerPath allSelector)),
    union3 (outerInnerPath allSelector) (innerPath allSelector)
           (tag1 (outerPath allSelector))),
   ("union____union__Inner__Outer_Inner____tag1___Inner",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (tag1 (innerPath allSelector)),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union____union__Inner__Outer_Inner____tag1___Outer_Inner",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union____union__Inner__Outer_Inner____Outer___tag1",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (outerPath (tag1 allSelector)),
    union3 (outerInnerPath allSelector) (innerPath allSelector)
           (tag1 (outerPath allSelector))),
   ("union____union__Inner__Outer_Inner____Inner___tag1",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (innerPath (tag1 allSelector)),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union____union__Inner__Outer_Inner____Outer_Inner___tag1",
    union2 (union2 (innerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    union2 (outerInnerPath allSelector) (innerPath allSelector)),
   ("union____tag1___union__Inner__Outer_Inner____tag1___Outer",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____tag1___Inner",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (innerPath allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____tag1___Outer_Inner",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____Outer___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (union2 (outerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (innerPath (tag1 allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Inner__Outer_Inner____Outer_Inner___tag1",
    union2 (tag1 (union2 (innerPath allSelector) (outerInnerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer_Inner__All____All",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           allSelector,
    allSelector),
   ("union____tag1___union__Outer_Inner__All____Outer",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (outerPath allSelector),
    union2 (tag1 allSelector) (outerPath allSelector)),
   ("union____tag1___union__Outer_Inner__All____Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (innerPath allSelector),
    union2 (tag1 allSelector) (innerPath allSelector)),
   ("union____tag1___union__Outer_Inner__All____Outer_Inner",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (outerInnerPath allSelector),
    union2 (tag1 allSelector) (outerInnerPath allSelector)),
   ("union____union__Outer_Inner__All____tag1___Outer",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (tag1 (outerPath allSelector)),
    allSelector),
   ("union____union__Outer_Inner__All____tag1___Inner",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (tag1 (innerPath allSelector)),
    allSelector),
   ("union____union__Outer_Inner__All____tag1___Outer_Inner",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (tag1 (outerInnerPath allSelector)),
    allSelector),
   ("union____union__Outer_Inner__All____Outer___tag1",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (outerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Outer_Inner__All____Inner___tag1",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (innerPath (tag1 allSelector)),
    allSelector),
   ("union____union__Outer_Inner__All____Outer_Inner___tag1",
    union2 (union2 (outerInnerPath allSelector) allSelector)
           (outerInnerPath (tag1 allSelector)),
    allSelector),
   ("union____tag1___union__Outer_Inner__All____tag1___Outer",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (tag1 (outerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__All____tag1___Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (tag1 (innerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__All____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (tag1 (outerInnerPath allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__All____Outer___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (outerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__All____Inner___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (innerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__All____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector) allSelector))
           (outerInnerPath (tag1 allSelector)),
    tag1 allSelector),
   ("union____tag1___union__Outer_Inner__Outer_Inner____All",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           allSelector,
    allSelector),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Outer",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (outerPath allSelector),
    outerPath allSelector),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (innerPath allSelector),
    union2 (tag1 (outerInnerPath allSelector)) (innerPath allSelector)),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Outer_Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (outerInnerPath allSelector),
    outerInnerPath allSelector),
   ("union____union__Outer_Inner__Outer_Inner____tag1___Outer",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerPath allSelector)),
    union2 (outerInnerPath allSelector) (tag1 (outerPath allSelector))),
   ("union____union__Outer_Inner__Outer_Inner____tag1___Inner",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (tag1 (innerPath allSelector)),
    union2 (outerInnerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Outer_Inner__Outer_Inner____tag1___Outer_Inner",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (tag1 (outerInnerPath allSelector)),
    outerInnerPath allSelector),
   ("union____union__Outer_Inner__Outer_Inner____Outer___tag1",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (outerPath (tag1 allSelector)),
    union2 (outerInnerPath allSelector) (tag1 (outerPath allSelector))),
   ("union____union__Outer_Inner__Outer_Inner____Inner___tag1",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (innerPath (tag1 allSelector)),
    union2 (outerInnerPath allSelector) (tag1 (innerPath allSelector))),
   ("union____union__Outer_Inner__Outer_Inner____Outer_Inner___tag1",
    union2 (union2 (outerInnerPath allSelector) (outerInnerPath allSelector))
           (outerInnerPath (tag1 allSelector)),
    outerInnerPath allSelector),
   ("union____tag1___union__Outer_Inner__Outer_Inner____tag1___Outer",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (tag1 (outerPath allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer_Inner__Outer_Inner____tag1___Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (tag1 (innerPath allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer_Inner__Outer_Inner____tag1___Outer_Inner",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (tag1 (outerInnerPath allSelector)),
    tag1 (outerInnerPath allSelector)),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Outer___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (outerPath (tag1 allSelector)),
    tag1 (outerPath allSelector)),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Inner___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (innerPath (tag1 allSelector)),
    tag1 (union2 (outerInnerPath allSelector) (innerPath allSelector))),
   ("union____tag1___union__Outer_Inner__Outer_Inner____Outer_Inner___tag1",
    union2 (tag1 (union2 (outerInnerPath allSelector)
                         (outerInnerPath allSelector)))
           (outerInnerPath (tag1 allSelector)),
    tag1 (outerInnerPath allSelector)),
   ("union____tag1___union__tag1__All____tag1",
    union2 (tag1 (union2 (tag1 allSelector) allSelector)) (tag1 allSelector),
    tag1 allSelector),
   ("union____tag1___union__tag1__tag1____All",
    union2 (tag1 (union2 (tag1 allSelector) (tag1 allSelector))) allSelector,
    allSelector),
   ("union____tag2___union__tag1__All____tag1",
    union2 (tag2 (union2 (tag1 allSelector) allSelector)) (tag1 allSelector),
    tag12 allSelector),
   ("union____tag2___union__tag1__tag1____All",
    union2 (tag2 (union2 (tag1 allSelector) (tag1 allSelector))) allSelector,
    allSelector),
   ("union____tag12___union__tag1__All____tag1",
    union2 (tag12 (union2 (tag1 allSelector) allSelector)) (tag1 allSelector),
    tag12 allSelector),
   ("union____tag12___union__tag1__tag1____All",
    union2 (tag12 (union2 (tag1 allSelector) (tag1 allSelector))) allSelector,
    allSelector),
   ("union____tag1___union__tag2__All____tag2",
    union2 (tag1 (union2 (tag2 allSelector) allSelector)) (tag2 allSelector),
    tag12 allSelector),
   ("union____tag1___union__tag2__tag2____All",
    union2 (tag1 (union2 (tag2 allSelector) (tag2 allSelector))) allSelector,
    allSelector),
   ("union____tag2___union__tag2__All____tag2",
    union2 (tag2 (union2 (tag2 allSelector) allSelector)) (tag2 allSelector),
    tag2 allSelector),
   ("union____tag2___union__tag2__tag2____All",
    union2 (tag2 (union2 (tag2 allSelector) (tag2 allSelector))) allSelector,
    allSelector),
   ("union____tag12___union__tag2__All____tag2",
    union2 (tag12 (union2 (tag2 allSelector) allSelector)) (tag2 allSelector),
    tag12 allSelector),
   ("union____tag12___union__tag2__tag2____All",
    union2 (tag12 (union2 (tag2 allSelector) (tag2 allSelector))) allSelector,
    allSelector),
   ("union____tag1___union__tag12__All____tag12",
    union2 (tag1 (union2 (tag12 allSelector) allSelector)) (tag12 allSelector),
    tag12 allSelector),
   ("union____tag1___union__tag12__tag12____All",
    union2 (tag1 (union2 (tag12 allSelector) (tag12 allSelector))) allSelector,
    allSelector),
   ("union____tag2___union__tag12__All____tag12",
    union2 (tag2 (union2 (tag12 allSelector) allSelector)) (tag12 allSelector),
    tag12 allSelector),
   ("union____tag2___union__tag12__tag12____All",
    union2 (tag2 (union2 (tag12 allSelector) (tag12 allSelector))) allSelector,
    allSelector),
   ("union____tag12___union__tag12__All____tag12",
    union2 (tag12 (union2 (tag12 allSelector) allSelector)) (tag12 allSelector),
    tag12 allSelector),
   ("union____tag12___union__tag12__tag12____All",
    union2 (tag12 (union2 (tag12 allSelector) (tag12 allSelector))) allSelector,
    allSelector)
  ]

normalizeSelectorTests :: [Test]
normalizeSelectorTests =
  let
    makeTest :: (String, Selector, Selector) -> Test
    makeTest (name, input, expected) =
      let
        runTest =
          let
            actual = normalizeSelector input
          in do
            if actual == expected
              then return (Finished Pass)
              else return (Finished (Fail ("Normalizing " ++ show input ++
                                           "\nexpected " ++ show expected ++
                                           "\ngot " ++ show actual)))

        testInstance = TestInstance { name = name, tags = [], options = [],
                                      setOption = (\_ _ -> return testInstance),
                                      run = runTest }
      in
        Test testInstance
  in
    map makeTest normalizeSelectorTestCases

testlist = [ testGroup "fileParser" fileParserTests,
             testGroup "filterParse" filterParseTests,
             testGroup "normalizeSelector" normalizeSelectorTests ]

tests :: Test
tests = testGroup "Filter" testlist
