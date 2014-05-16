module Tests.Test.HUnit.Filter(tests) where

import Data.List
import Data.Map(Map)
import Distribution.TestSuite
import Test.HUnit.Filter

import qualified Data.Map as Map
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

tagsSelector :: [String] -> Selector
tagsSelector [] = allSelector
tagsSelector tags = allSelector { selectorTags = Just $! Set.fromList tags }

pathSelector :: Selector -> [String] -> Selector
pathSelector inner [] = inner
pathSelector inner (elem : path) =
  Selector { selectorInners = Map.singleton elem (pathSelector inner path),
             selectorTags = Nothing }

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
    expected = makeFilter suites (pathSelector (tagsSelector tags) path)

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
    simplePath =
      Selector {
        selectorTags = Nothing,
        selectorInners =
          Map.singleton "Outer"
            Selector { selectorTags = Nothing,
                       selectorInners = Map.singleton "Inner" allSelector }
      }
    simplePathStr = "Outer.Inner"

    onlyTags = allSelector { selectorTags =
                                Just $! Set.fromList ["tag1", "tag2"] }
    onlyTagsStr = "@tag1,tag2"
    pathTags =
      Selector {
        selectorTags = Nothing,
        selectorInners =
          Map.singleton "Outer"
            Selector { selectorTags = Nothing,
                       selectorInners = Map.singleton "Inner" onlyTags }
      }
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
innerPath inner = Selector { selectorInners = Map.singleton "Inner" inner,
                             selectorTags = Nothing }

outerPath :: Selector -> Selector
outerPath inner = Selector { selectorInners = Map.singleton "Outer" inner,
                             selectorTags = Nothing }

outerInnerPath :: Selector -> Selector
outerInnerPath = outerPath . innerPath

outerAndInnerPath :: Selector
outerAndInnerPath =
  Selector { selectorInners = Map.fromList [("Outer", allSelector),
                                            ("Inner", allSelector)],
             selectorTags = Nothing }

outerInnerAndInnerPath :: Selector
outerInnerAndInnerPath =
  Selector { selectorInners = Map.fromList [("Outer", innerPath allSelector),
                                            ("Inner", allSelector)],
             selectorTags = Nothing }

tag1OuterAndInnerPath :: Selector
tag1OuterAndInnerPath =
  Selector { selectorInners = Map.fromList [("Outer", allSelector),
                                            ("Inner", allSelector)],
             selectorTags = Just $! Set.singleton "tag1" }

tag1 :: Selector -> Selector
tag1 inner = inner { selectorTags = Just $! Set.singleton "tag1" }

tag2 :: Selector -> Selector
tag2 inner = inner { selectorTags = Just $! Set.singleton "tag2" }

tag12 :: Selector -> Selector
tag12 inner = inner { selectorTags = Just $! Set.fromList ["tag1", "tag2"] }


combineSelectorTestCases :: [(String, Selector, Selector, Selector)]
combineSelectorTestCases =
  [("all_all", allSelector, allSelector, allSelector),
   ("all_Outer", allSelector, outerPath allSelector, allSelector),
   ("Outer_all", outerPath allSelector, allSelector, allSelector),
   ("all__Outer_Inner", allSelector, outerInnerPath allSelector, allSelector),
   ("Outer_Inner__all", outerInnerPath allSelector, allSelector, allSelector),
   ("Outer_Outer", outerPath allSelector, outerPath allSelector,
    outerPath allSelector),
   ("Outer_Inner", outerPath allSelector, innerPath allSelector,
    outerAndInnerPath),
   ("Inner_Outer", innerPath allSelector, outerPath allSelector,
    outerAndInnerPath),
   ("Outer__Outer_Inner", outerPath allSelector, outerInnerPath allSelector,
    outerPath allSelector),
   ("Outer_Inner__Outer", outerInnerPath allSelector, outerPath allSelector,
    outerPath allSelector),
   ("Inner__Outer_Inner", innerPath allSelector, outerInnerPath allSelector,
    outerInnerAndInnerPath),
   ("Outer_Inner__Inner", outerInnerPath allSelector, innerPath allSelector,
    outerInnerAndInnerPath),
   ("tag1_Outer", tag1 allSelector, outerPath allSelector,
    tag1 (outerPath allSelector)),
   ("Outer_tag1", outerPath allSelector, tag1 allSelector,
    tag1 (outerPath allSelector)),
   ("tag1__Outer_Inner", tag1 allSelector, outerInnerPath allSelector,
    tag1 (outerInnerPath allSelector)),
   ("Outer_Inner__tag1", outerInnerPath allSelector, tag1 allSelector,
    tag1 (outerInnerPath allSelector)),
   ("tag1_Outer__Outer", tag1 (outerPath allSelector), outerPath allSelector,
    tag1 (outerPath allSelector)),
   ("Outer__tag1_Outer", outerPath allSelector, tag1 (outerPath allSelector),
    tag1 (outerPath allSelector)),
   ("tag1_Inner__Outer", tag1 (innerPath allSelector), outerPath allSelector,
    tag1OuterAndInnerPath),
   ("Outer__tag1_Inner", outerPath allSelector, tag1 (innerPath allSelector),
    tag1OuterAndInnerPath),
   ("tag1_Outer__tag1_Inner", tag1 (innerPath allSelector),
    tag1 (outerPath allSelector),
    tag1 outerAndInnerPath),
   ("tag1_Inner__tag1_Outer", tag1 (outerPath allSelector),
    tag1 (innerPath allSelector),
    tag1 outerAndInnerPath),
   ("all_tag1", allSelector, tag1 allSelector, allSelector),
   ("tag1_all", tag1 allSelector, allSelector, allSelector),
   ("all_tag2", allSelector, tag2 allSelector, allSelector),
   ("tag2_all", tag2 allSelector, allSelector, allSelector),
   ("all_tag12", allSelector, tag12 allSelector, allSelector),
   ("tag12_all", tag12 allSelector, allSelector, allSelector),
   ("tag1_tag1", tag1 allSelector, tag1 allSelector, tag1 allSelector),
   ("tag1_tag2", tag1 allSelector, tag2 allSelector, tag12 allSelector),
   ("tag2_tag1", tag2 allSelector, tag1 allSelector, tag12 allSelector),
   ("tag1_tag12", tag1 allSelector, tag12 allSelector, tag12 allSelector),
   ("tag12_tag1", tag12 allSelector, tag1 allSelector, tag12 allSelector),
   ("tag1_tag2", tag1 allSelector, tag2 allSelector, tag12 allSelector),
   ("tag2_tag1", tag2 allSelector, tag1 allSelector, tag12 allSelector),
   ("tag2_tag2", tag2 allSelector, tag2 allSelector, tag2 allSelector),
   ("tag2_tag12", tag2 allSelector, tag12 allSelector, tag12 allSelector),
   ("tag12_tag2", tag12 allSelector, tag2 allSelector, tag12 allSelector),
   ("tag12_tag1", tag12 allSelector, tag1 allSelector, tag12 allSelector),
   ("tag1_tag12", tag1 allSelector, tag12 allSelector, tag12 allSelector),
   ("tag12_tag2", tag12 allSelector, tag2 allSelector, tag12 allSelector),
   ("tag2_tag12", tag2 allSelector, tag12 allSelector, tag12 allSelector),
   ("tag12_tag12", tag12 allSelector, tag12 allSelector, tag12 allSelector),
   ("Outer_tag1__Outer", outerPath (tag1 allSelector), outerPath allSelector,
    outerPath allSelector),
   ("Outer__Outer_tag1", outerPath allSelector, outerPath (tag1 allSelector),
    outerPath allSelector),
   ("Outer_tag1__Outer_tag1", outerPath (tag1 allSelector),
    outerPath (tag1 allSelector), outerPath (tag1 allSelector)),
   ("Outer_tag1__Outer_tag2", outerPath (tag1 allSelector),
    outerPath (tag2 allSelector), outerPath (tag12 allSelector)),
   ("Outer_tag2__Outer_tag1", outerPath (tag2 allSelector),
    outerPath (tag1 allSelector), outerPath (tag12 allSelector)),
   ("Outer_Inner_tag1__Outer_tag1", outerInnerPath (tag1 allSelector),
    outerPath (tag1 allSelector), outerPath (tag1 allSelector)),
   ("Outer_tag1__Outer_Inner_tag1", outerPath (tag1 allSelector),
    outerInnerPath (tag1 allSelector), outerPath (tag1 allSelector)),
   ("tag1_Outer__Outer_tag1", tag1 (outerPath allSelector),
    outerPath (tag1 allSelector), tag1 (outerPath allSelector)),
   ("Outer_tag1__tag1_Outer", outerPath (tag1 allSelector),
    tag1 (outerPath allSelector), tag1 (outerPath allSelector))
  ]

combineSelectorTests :: [Test]
combineSelectorTests =
  let
    makeTest :: (String, Selector, Selector, Selector) -> Test
    makeTest (name, input1, input2, expected) =
      let
        runTest =
          let
            actual = combineSelectors input1 input2
          in do
            if actual == expected
              then return (Finished Pass)
              else return (Finished (Fail ("Combining\n" ++ show input1 ++
                                           "\nwith\n" ++ show input2 ++
                                           "\nexpected\n" ++ show expected ++
                                           "\ngot\n" ++ show actual)))

        testInstance = TestInstance { name = name, tags = [], options = [],
                                      setOption = (\_ _ -> return testInstance),
                                      run = runTest }
      in
        Test testInstance
  in
    map makeTest combineSelectorTestCases

onePath :: Selector
onePath = Selector { selectorInners = Map.singleton "One" allSelector,
                     selectorTags = Nothing }

twoPath :: Selector
twoPath = Selector { selectorInners = Map.singleton "Two" allSelector,
                     selectorTags = Nothing }

oneTwoPath :: Selector
oneTwoPath = Selector { selectorInners = Map.fromList [("One", allSelector),
                                                       ("Two", allSelector)],
                        selectorTags = Nothing }

emptyOneFilter :: Filter
emptyOneFilter = Filter { filterSuites = Set.empty, filterSelector = onePath }

emptyTwoFilter :: Filter
emptyTwoFilter = Filter { filterSuites = Set.empty, filterSelector = twoPath }

emptyOneTwoFilter :: Filter
emptyOneTwoFilter = Filter { filterSuites = Set.empty,
                             filterSelector = oneTwoPath }

allAFilter :: Filter
allAFilter = Filter { filterSuites = Set.singleton "A",
                      filterSelector = allSelector }

oneAFilter :: Filter
oneAFilter = Filter { filterSuites = Set.singleton "A",
                      filterSelector = onePath }

oneBFilter :: Filter
oneBFilter = Filter { filterSuites = Set.singleton "B",
                      filterSelector = onePath }

oneABFilter :: Filter
oneABFilter = Filter { filterSuites = Set.fromList ["A", "B"],
                       filterSelector = onePath }

oneACFilter :: Filter
oneACFilter = Filter { filterSuites = Set.fromList ["A", "C"],
                       filterSelector = onePath }

twoAFilter :: Filter
twoAFilter = Filter { filterSuites = Set.singleton "A",
                      filterSelector = twoPath }

twoBFilter :: Filter
twoBFilter = Filter { filterSuites = Set.singleton "B",
                      filterSelector = twoPath }

twoABFilter :: Filter
twoABFilter = Filter { filterSuites = Set.fromList ["A", "B"],
                       filterSelector = twoPath }

twoACFilter :: Filter
twoACFilter = Filter { filterSuites = Set.fromList ["A", "C"],
                       filterSelector = twoPath }

oneTwoAFilter :: Filter
oneTwoAFilter = Filter { filterSuites = Set.singleton "A",
                         filterSelector = oneTwoPath }

oneTwoBFilter :: Filter
oneTwoBFilter = Filter { filterSuites = Set.singleton "B",
                         filterSelector = oneTwoPath }

oneTwoABFilter :: Filter
oneTwoABFilter = Filter { filterSuites = Set.fromList ["A", "B"],
                          filterSelector = oneTwoPath }

oneTwoACFilter :: Filter
oneTwoACFilter = Filter { filterSuites = Set.fromList ["A", "C"],
                          filterSelector = oneTwoPath }

allBFilter :: Filter
allBFilter = Filter { filterSuites = Set.singleton "B",
                      filterSelector = allSelector }

allCFilter :: Filter
allCFilter = Filter { filterSuites = Set.singleton "C",
                      filterSelector = allSelector }

allABFilter :: Filter
allABFilter = Filter { filterSuites = Set.fromList ["A", "B"],
                       filterSelector = allSelector }

allACFilter :: Filter
allACFilter = Filter { filterSuites = Set.fromList ["A", "C"],
                       filterSelector = allSelector }

allBCFilter :: Filter
allBCFilter = Filter { filterSuites = Set.fromList ["B", "C"],
                       filterSelector = allSelector }


filterTestCases :: [(String, [String], [Filter], [(String, Selector)])]
filterTestCases = [
    ("A_nil", ["A"], [], [("A", allSelector)]),
    ("AB_nil", ["A", "B"], [], [("A", allSelector), ("B", allSelector)]),
    ("ABC_nil", ["A", "B", "C"], [],
     [("A", allSelector), ("B", allSelector), ("C", allSelector)]),
    ("A_emptyOne", ["A"], [emptyOneFilter], [("A", onePath)]),
    ("AB_emptyOne", ["A", "B"], [emptyOneFilter],
     [("A", onePath), ("B", onePath)]),
    ("ABC_emptyOne", ["A", "B", "C"], [emptyOneFilter],
     [("A", onePath), ("B", onePath), ("C", onePath)]),
    ("A_emptyOne_emptyTwo", ["A"], [emptyOneFilter, emptyTwoFilter],
     [("A", oneTwoPath)]),
    ("AB_emptyOne_emptyTwo", ["A", "B"], [emptyOneFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("ABC_emptyOne_emptyTwo", ["A", "B", "C"], [emptyOneFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("A_emptyOne_emptyOneTwo", ["A"], [emptyOneFilter, emptyOneTwoFilter],
     [("A", oneTwoPath)]),
    ("AB_emptyOne_emptyOneTwo", ["A", "B"], [emptyOneFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("ABC_emptyOne_emptyOneTwo", ["A", "B", "C"], [emptyOneFilter,
                                                   emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("A_allA", ["A"], [allAFilter], [("A", allSelector)]),
    ("A_OneA", ["A"], [oneAFilter], [("A", onePath)]),
    ("A_OneA_OneA", ["A"], [oneAFilter, oneAFilter], [("A", onePath)]),
    ("A_OneA_TwoA", ["A"], [oneAFilter, twoAFilter], [("A", oneTwoPath)]),
    ("A_OneA_OneTwoA", ["A"], [oneAFilter, oneTwoAFilter], [("A", oneTwoPath)]),
    ("A_OneA_allA", ["A"], [oneAFilter, allAFilter], [("A", allSelector)]),
    ("A_allA_allA", ["A"], [allAFilter, allAFilter], [("A", allSelector)]),
    ("A_OneA_emptyOne", ["A"], [oneAFilter, emptyOneFilter], [("A", onePath)]),
    ("A_TwoA_emptyOne", ["A"], [twoAFilter, emptyOneFilter],
     [("A", oneTwoPath)]),
    ("A_OneTwoA_emptyOne", ["A"], [oneTwoAFilter, emptyOneFilter],
     [("A", oneTwoPath)]),
    ("A_allA_emptyOne", ["A"], [allAFilter, emptyOneFilter],
     [("A", allSelector)]),
    ("A_OneA_emptyTwo", ["A"], [oneAFilter, emptyTwoFilter],
     [("A", oneTwoPath)]),
    ("A_TwoA_emptyTwo", ["A"], [twoAFilter, emptyTwoFilter],
     [("A", twoPath)]),
    ("A_OneTwoA_emptyTwo", ["A"], [oneTwoAFilter, emptyTwoFilter],
     [("A", oneTwoPath)]),
    ("A_allA_emptyTwo", ["A"], [allAFilter, emptyTwoFilter],
     [("A", allSelector)]),
    ("A_OneA_emptyOneTwo", ["A"], [oneAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath)]),
    ("A_TwoA_emptyOneTwo", ["A"], [twoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath)]),
    ("A_OneTwoA_emptyOneTwo", ["A"], [oneTwoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath)]),
    ("A_allA_emptyOneTwo", ["A"], [allAFilter, emptyOneTwoFilter],
     [("A", allSelector)]),
    ("AB_allA", ["A", "B"], [allAFilter], [("A", allSelector)]),
    ("AB_OneA", ["A", "B"], [oneAFilter], [("A", onePath)]),
    ("AB_OneA_OneA", ["A", "B"], [oneAFilter, oneAFilter], [("A", onePath)]),
    ("AB_OneA_TwoA", ["A", "B"], [oneAFilter, twoAFilter], [("A", oneTwoPath)]),
    ("AB_OneA_OneTwoA", ["A", "B"], [oneAFilter, oneTwoAFilter],
     [("A", oneTwoPath)]),
    ("AB_OneA_allA", ["A", "B"], [oneAFilter, allAFilter], [("A", allSelector)]),
    ("AB_allA_allA", ["A", "B"], [allAFilter, allAFilter], [("A", allSelector)]),
    ("AB_OneA_OneB", ["A", "B"], [oneAFilter, oneBFilter],
     [("A", onePath), ("B", onePath)]),
    ("AB_allA_OneB", ["A", "B"], [allAFilter, oneBFilter],
     [("A", allSelector), ("B", onePath)]),
    ("AB_OneA_allB", ["A", "B"], [oneAFilter, allBFilter],
     [("A", onePath), ("B", allSelector)]),
    ("AB_allA_allB", ["A", "B"], [allAFilter, allBFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("AB_OneA_OneAB", ["A", "B"], [oneAFilter, oneABFilter],
     [("A", onePath), ("B", onePath)]),
    ("AB_OneA_TwoAB", ["A", "B"], [oneAFilter, twoABFilter],
     [("A", oneTwoPath), ("B", twoPath)]),
    ("AB_OneA_OneTwoAB", ["A", "B"], [oneAFilter, oneTwoABFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("AB_allA_OneAB", ["A", "B"], [allAFilter, oneABFilter],
     [("A", allSelector), ("B", onePath)]),
    ("AB_allA_TwoAB", ["A", "B"], [allAFilter, twoABFilter],
     [("A", allSelector), ("B", twoPath)]),
    ("AB_allA_OneTwoAB", ["A", "B"], [allAFilter, oneTwoABFilter],
     [("A", allSelector), ("B", oneTwoPath)]),
    ("AB_OneA_allAB", ["A", "B"], [oneAFilter, allABFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("AB_allA_allAB", ["A", "B"], [allAFilter, allABFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("AB_OneA_emptyOne", ["A", "B"], [oneAFilter, emptyOneFilter],
     [("A", onePath), ("B", onePath)]),
    ("AB_TwoA_emptyOne", ["A", "B"], [twoAFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", onePath)]),
    ("AB_OneTwoA_emptyOne", ["A", "B"], [oneTwoAFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", onePath)]),
    ("AB_allA_emptyOne", ["A", "B"], [allAFilter, emptyOneFilter],
     [("A", allSelector), ("B", onePath)]),
    ("AB_OneA_emptyTwo", ["A", "B"], [oneAFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", twoPath)]),
    ("AB_TwoA_emptyTwo", ["A", "B"], [twoAFilter, emptyTwoFilter],
     [("A", twoPath), ("B", twoPath)]),
    ("AB_OneTwoA_emptyTwo", ["A", "B"], [oneTwoAFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", twoPath)]),
    ("AB_allA_emptyTwo", ["A", "B"], [allAFilter, emptyTwoFilter],
     [("A", allSelector), ("B", twoPath)]),
    ("AB_OneA_emptyOneTwo", ["A", "B"], [oneAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("AB_TwoA_emptyOneTwo", ["A", "B"], [twoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("AB_OneTwoA_emptyOneTwo", ["A", "B"], [oneTwoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("AB_allA_emptyOneTwo", ["A", "B"], [allAFilter, emptyOneTwoFilter],
     [("A", allSelector), ("B", oneTwoPath)]),
    ("ABC_allA", ["A", "B", "C"], [allAFilter], [("A", allSelector)]),
    ("ABC_OneA", ["A", "B", "C"], [oneAFilter], [("A", onePath)]),
    ("ABC_OneA_OneA", ["A", "B", "C"], [oneAFilter, oneAFilter],
     [("A", onePath)]),
    ("ABC_OneA_TwoA", ["A", "B", "C"], [oneAFilter, twoAFilter],
     [("A", oneTwoPath)]),
    ("ABC_OneA_OneTwoA", ["A", "B", "C"], [oneAFilter, oneTwoAFilter],
     [("A", oneTwoPath)]),
    ("ABC_OneA_allA", ["A", "B", "C"], [oneAFilter, allAFilter],
     [("A", allSelector)]),
    ("ABC_allA_allA", ["A", "B", "C"], [allAFilter, allAFilter],
     [("A", allSelector)]),
    ("ABC_OneA_OneB", ["A", "B", "C"], [oneAFilter, oneBFilter],
     [("A", onePath), ("B", onePath)]),
    ("ABC_allA_OneB", ["A", "B", "C"], [allAFilter, oneBFilter],
     [("A", allSelector), ("B", onePath)]),
    ("ABC_OneA_allB", ["A", "B", "C"], [oneAFilter, allBFilter],
     [("A", onePath), ("B", allSelector)]),
    ("ABC_allA_allB", ["A", "B", "C"], [allAFilter, allBFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("ABC_OneA_OneAB", ["A", "B", "C"], [oneAFilter, oneABFilter],
     [("A", onePath), ("B", onePath)]),
    ("ABC_OneA_TwoAB", ["A", "B", "C"], [oneAFilter, twoABFilter],
     [("A", oneTwoPath), ("B", twoPath)]),
    ("ABC_OneA_OneTwoAB", ["A", "B", "C"], [oneAFilter, oneTwoABFilter],
     [("A", oneTwoPath), ("B", oneTwoPath)]),
    ("ABC_OneA_OneAC", ["A", "B", "C"], [oneAFilter, oneACFilter],
     [("A", onePath), ("C", onePath)]),
    ("ABC_OneA_TwoAC", ["A", "B", "C"], [oneAFilter, twoACFilter],
     [("A", oneTwoPath), ("C", twoPath)]),
    ("ABC_OneA_OneTwoAC", ["A", "B", "C"], [oneAFilter, oneTwoACFilter],
     [("A", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_OneAB_OneAC", ["A", "B", "C"], [oneABFilter, oneACFilter],
     [("A", onePath), ("B", onePath), ("C", onePath)]),
    ("ABC_OneAB_TwoAC", ["A", "B", "C"], [oneABFilter, twoACFilter],
     [("A", oneTwoPath), ("B", onePath), ("C", twoPath)]),
    ("ABC_OneAB_OneTwoAC", ["A", "B", "C"], [oneABFilter, oneTwoACFilter],
     [("A", oneTwoPath), ("B", onePath), ("C", oneTwoPath)]),
    ("ABC_allA_OneAB", ["A", "B", "C"], [allAFilter, oneABFilter],
     [("A", allSelector), ("B", onePath)]),
    ("ABC_allA_TwoAB", ["A", "B", "C"], [allAFilter, twoABFilter],
     [("A", allSelector), ("B", twoPath)]),
    ("ABC_allA_OneTwoAB", ["A", "B", "C"], [allAFilter, oneTwoABFilter],
     [("A", allSelector), ("B", oneTwoPath)]),
    ("ABC_allAC_OneAB", ["A", "B", "C"], [allACFilter, oneABFilter],
     [("A", allSelector), ("B", onePath), ("C", allSelector)]),
    ("ABC_allAC_TwoAB", ["A", "B", "C"], [allACFilter, twoABFilter],
     [("A", allSelector), ("B", twoPath), ("C", allSelector)]),
    ("ABC_allAC_OneTwoAB", ["A", "B", "C"], [allACFilter, oneTwoABFilter],
     [("A", allSelector), ("B", oneTwoPath), ("C", allSelector)]),
    ("ABC_OneA_allAB", ["A", "B", "C"], [oneAFilter, allABFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("ABC_allA_allAB", ["A", "B", "C"], [allAFilter, allABFilter],
     [("A", allSelector), ("B", allSelector)]),
    ("ABC_OneA_allAB", ["A", "B", "C"], [oneACFilter, allABFilter],
     [("A", allSelector), ("B", allSelector), ("C", onePath)]),
    ("ABC_allA_allAB", ["A", "B", "C"], [allACFilter, allABFilter],
     [("A", allSelector), ("B", allSelector), ("C", allSelector)]),
    ("ABC_OneA_emptyOne", ["A", "B", "C"], [oneAFilter, emptyOneFilter],
     [("A", onePath), ("B", onePath), ("C", onePath)]),
    ("ABC_TwoA_emptyOne", ["A", "B", "C"], [twoAFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", onePath), ("C", onePath)]),
    ("ABC_OneTwoA_emptyOne", ["A", "B", "C"], [oneTwoAFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", onePath), ("C", onePath)]),
    ("ABC_allA_emptyOne", ["A", "B", "C"], [allAFilter, emptyOneFilter],
     [("A", allSelector), ("B", onePath), ("C", onePath)]),
    ("ABC_OneAB_emptyOne", ["A", "B", "C"], [oneABFilter, emptyOneFilter],
     [("A", onePath), ("B", onePath), ("C", onePath)]),
    ("ABC_TwoAB_emptyOne", ["A", "B", "C"], [twoABFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", onePath)]),
    ("ABC_OneTwoAB_emptyOne", ["A", "B", "C"], [oneTwoABFilter, emptyOneFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", onePath)]),
    ("ABC_allAB_emptyOne", ["A", "B", "C"], [allABFilter, emptyOneFilter],
     [("A", allSelector), ("B", allSelector), ("C", onePath)]),
    ("ABC_OneA_emptyTwo", ["A", "B", "C"], [oneAFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", twoPath), ("C", twoPath)]),
    ("ABC_TwoA_emptyTwo", ["A", "B", "C"], [twoAFilter, emptyTwoFilter],
     [("A", twoPath), ("B", twoPath), ("C", twoPath)]),
    ("ABC_OneTwoA_emptyTwo", ["A", "B", "C"], [oneTwoAFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", twoPath), ("C", twoPath)]),
    ("ABC_allA_emptyTwo", ["A", "B", "C"], [allAFilter, emptyTwoFilter],
     [("A", allSelector), ("B", twoPath), ("C", twoPath)]),
    ("ABC_OneAB_emptyTwo", ["A", "B", "C"], [oneABFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", twoPath)]),
    ("ABC_TwoAB_emptyTwo", ["A", "B", "C"], [twoABFilter, emptyTwoFilter],
     [("A", twoPath), ("B", twoPath), ("C", twoPath)]),
    ("ABC_OneTwoAB_emptyTwo", ["A", "B", "C"], [oneTwoABFilter, emptyTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", twoPath)]),
    ("ABC_allAB_emptyTwo", ["A", "B", "C"], [allABFilter, emptyTwoFilter],
     [("A", allSelector), ("B", allSelector), ("C", twoPath)]),
    ("ABC_OneA_emptyOneTwo", ["A", "B", "C"], [oneAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_TwoA_emptyOneTwo", ["A", "B", "C"], [twoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_OneTwoA_emptyOneTwo", ["A", "B", "C"],
     [oneTwoAFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_allA_emptyOneTwo", ["A", "B", "C"], [allAFilter, emptyOneTwoFilter],
     [("A", allSelector), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_OneAB_emptyOneTwo", ["A", "B", "C"], [oneABFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_TwoAB_emptyOneTwo", ["A", "B", "C"], [twoABFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_OneTwoAB_emptyOneTwo", ["A", "B", "C"],
     [oneTwoABFilter, emptyOneTwoFilter],
     [("A", oneTwoPath), ("B", oneTwoPath), ("C", oneTwoPath)]),
    ("ABC_allAB_emptyOneTwo", ["A", "B", "C"], [allABFilter, emptyOneTwoFilter],
     [("A", allSelector), ("B", allSelector), ("C", oneTwoPath)])
  ]

filterTests :: [Test]
filterTests =
  let
    makeTest :: (String, [String], [Filter], [(String, Selector)]) -> Test
    makeTest (name, suites, filters, expected) =
      let
        runTest =
          let
            actual = Map.assocs (suiteSelectors suites filters)
          in do
            if actual == expected
              then return (Finished Pass)
              else return (Finished (Fail ("Combining\n" ++ show filters ++
                                           "\nwith suites\n" ++ show suites ++
                                           "\nexpected\n" ++ show expected ++
                                           "\ngot\n" ++ show actual)))

        testInstance = TestInstance { name = name, tags = [], options = [],
                                      setOption = (\_ _ -> return testInstance),
                                      run = runTest }
      in
        Test testInstance
  in
    map makeTest filterTestCases


testlist = [ testGroup "fileParser" fileParserTests,
             testGroup "filterParse" filterParseTests,
             testGroup "combineSelectors" combineSelectorTests,
             testGroup "suiteSelectors" filterTests ]

tests :: Test
tests = testGroup "Filter" testlist
