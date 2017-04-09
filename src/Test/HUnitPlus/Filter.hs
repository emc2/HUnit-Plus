{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Sets HUnit-Plus tests can be specified using 'Filter's.  These
-- are used by "Test.HUnitPlus.Execution" and "Test.HUnitPlus.Main" to
-- select which tests are run.  Filters can specify tests belonging to
-- a certain suite, starting with a certain path, having a certain
-- tag, or combinations thereof.
--
-- Filters are optimized for the behavior of programs created by the
-- 'createMain' function, which runs a test if it matches /any/ of the
-- filters specified.  There is also a string format for filters,
-- which is how filters are specified in testlist files and
-- command-line arguments.  The format is optimized for simplicity,
-- and as such, it is not necessarily possible to describe a given
-- "Filter" with a single textual representation of a filter.
--
-- The format for filters is as follows:
--
-- \[/suite/\]\[/path/\]\[/tags/\]
--
-- Where at least one of the /suite/, /path/, or /tags/ elements are present
--
-- The /suite/ element is a comma-separated list of suite names (alphanumeric,
-- no spaces), enclosed in brackets ('[' ']').
--
-- The /path/ element is a series of path elements (alphanumeric, no
-- spaces), separated by dots ('.').
--
-- The /tags/ element consists of a '\@' character, followed by a
-- comma-separated list of tag names (alphanumeric, no spaces).
--
-- The following are examples of textual filters, and their meanings:
--
-- * @first.second.third@: Run all tests starting with the path
--   @first.second.third@.  If there is a test named
--   @first.second.third@, it will be run.
--
-- * @[unit]@: Run all tests in the suite 'unit'.
--
-- * @[unit,stress]@: Run all tests in the suites 'unit' and 'stress'
--
-- * @\@parser@: Run all tests with the 'parser' tag
--
-- * @\@parser,lexer@: Run all tests with the 'parser' /or/ the 'lexer' tags.
--
-- * @backend.codegen\@asm@: Run all tests starting with the path
--   @backend.codegen@ with the 'asm' tag.
--
-- * @[stress]\@net@: Run all tests in the 'stress' suite with the tag 'net'.
--
-- * @[perf,profile]inner.outer@: Run all tests in the 'perf' and
--   'profile' suites that start with the path @inner.outer@.
--
-- * @[whitebox]network.protocol\@security@: Run all tests in the
--   'whitebox' suite beginning with the path @network.protocol@ that
--   have the 'security' tag.
--
-- The most common use case of filters is to select a single failing
-- test to run, as part of fixing it.  In this case, a single filter
-- consisting of the path to the test will have this effect.
module Test.HUnitPlus.Filter(
       Selector(..),
       Filter(..),
       OptionMap,
       combineTags,
       passFilter,
       allSelector,
       combineSelectors,
       suiteSelectors,
       parseFilter,
       parseFilterFile,
       parseFilterFileContent
       ) where

import Control.Exception
import Data.Foldable(foldl)
import Data.Either
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.Maybe
import Data.HashSet(HashSet)
import Data.List(sort)
import Prelude hiding (foldl, elem)
import System.IO.Error
import Text.Parsec hiding (try)
import Text.Parsec.Text

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict

type OptionMap = HashMap Strict.Text Strict.Text

-- | A tree-like structure that represents a set of tests within a
-- given suite.
data Selector =
    Selector {
      -- | @Selector@s for subgroups of this one.  The entry for each
      -- path element contains the @Selector@ to be used for that
      -- group (or test).  An empty map actually means 'select all
      -- tests'.
      selectorInners :: HashMap Strict.Text Selector,
      -- | Tags by which to filter all tests.  The empty set actually
      -- means 'run all tests regardless of tags'.  'Nothing' means
      -- that all tests will be skipped (though this will be
      -- overridden by any @Selector@s in @selectorInners@.
      selectorTags :: !(Maybe (HashSet Strict.Text))
    }
    deriving (Eq, Show)

-- | Specifies zero or more test suites, to which the given 'Selector'
-- is then applied.  If no test suites are specified, then the
-- 'Selector' applies to all test suites.
data Filter =
  Filter {
    -- | The test suites to which the 'Selector' applies.  The empty
    -- set actually means 'all suites'.
    filterSuites :: !(HashSet Strict.Text),
    -- | The 'Selector' to apply.
    filterSelector :: !(HashSet Selector),
    filterOptions :: !(HashMap Strict.Text Strict.Text)
  }
  deriving (Eq, Show)

instance Ord Selector where
  compare Selector { selectorInners = inners1, selectorTags = Just tags1 }
          Selector { selectorInners = inners2, selectorTags = Just tags2 } =
    let
      sortedtags1 = sort (HashSet.toList tags1)
      sortedtags2 = sort (HashSet.toList tags2)
      sortedinners1 = sort (HashMap.toList inners1)
      sortedinners2 = sort (HashMap.toList inners2)
    in
      case compare sortedtags1 sortedtags2 of
        EQ -> compare sortedinners1 sortedinners2
        out -> out
  compare Selector { selectorTags = Nothing }
          Selector { selectorTags = Just _ } = LT
  compare Selector { selectorTags = Just _ }
          Selector { selectorTags = Nothing } = GT
  compare Selector { selectorInners = inners1, selectorTags = Nothing }
          Selector { selectorInners = inners2, selectorTags = Nothing } =
    let
      sortedinners1 = sort (HashMap.toList inners1)
      sortedinners2 = sort (HashMap.toList inners2)
    in
      compare sortedinners1 sortedinners2

instance Hashable Selector where
  hashWithSalt s Selector { selectorInners = inners,
                            selectorTags = Just tags } =
    let
      sortedtags = sort (HashSet.toList tags)
      sortedinners = sort (HashMap.toList inners)
    in
      s `hashWithSalt` sortedinners `hashWithSalt` sortedtags
  hashWithSalt s Selector { selectorInners = inners,
                            selectorTags = Nothing } =
    let
      sortedinners = sort (HashMap.toList inners)
    in
      s `hashWithSalt` sortedinners

-- | Combine two 'selectorTags' fields into one.  This operation represents the
-- union of the tests that are selected by the two fields.
combineTags :: Maybe (HashSet Strict.Text) -> Maybe (HashSet Strict.Text) ->
               Maybe (HashSet Strict.Text)
-- Nothing means we can't execute, so if the other side says we can,
-- we can.
combineTags Nothing t = t
combineTags t Nothing = t
combineTags (Just a) (Just b)
  -- The empty set means we execute everything, so it absorbs
  | HashSet.null a || HashSet.null b = Just $! HashSet.empty
  -- Otherwise, we do set union
  | otherwise = Just $! HashSet.union a b

-- | Take the difference of one set of tags from another.
diffTags :: Maybe (HashSet Strict.Text) -> Maybe (HashSet Strict.Text) ->
            Maybe (HashSet Strict.Text)
-- Nothing means we can't execute, so if the other side says we can,
-- we can.
diffTags Nothing _ = Nothing
diffTags t Nothing = t
diffTags (Just a) (Just b)
  | HashSet.null a = Just HashSet.empty
  | HashSet.null b = Nothing
  -- Otherwise, we do set union
  | otherwise =
    let
      diff = HashSet.difference a b
    in
      if diff == HashSet.empty
        then Nothing
        else Just $! diff

-- | A 'Filter' that selects all tests in all suites.
passFilter :: Filter
passFilter = Filter { filterSuites = HashSet.empty,
                      filterSelector = HashSet.singleton allSelector,
                      filterOptions = HashMap.empty }

-- | A 'Selector' that selects all tests.
allSelector :: Selector
allSelector = Selector { selectorInners = HashMap.empty,
                         selectorTags = Just HashSet.empty }

noOptionsAllSelector :: HashMap OptionMap Selector
noOptionsAllSelector = HashMap.singleton HashMap.empty allSelector

-- | Eliminate redundant nested tags from a Selector.
reduceSelector :: Maybe (HashSet Strict.Text) -> Selector -> Maybe Selector
reduceSelector parentTags s @ Selector { selectorInners = inners,
                                         selectorTags = tags } =
  let
    newTags = diffTags tags parentTags
    newParentTags = combineTags parentTags tags
    newInners = HashMap.mapMaybe (reduceSelector newParentTags) inners
  in
    -- This selector goes away if we eliminate all inners and all tags
    if isNothing newTags && HashMap.null newInners
      then Nothing
      else Just $! s { selectorInners = inners, selectorTags = tags }

-- | Combine two 'Selector's into a single 'Selector'.
combineSelectors :: Selector -> Selector -> Selector
combineSelectors selector1 selector2 =
  let
    tryCombineSelectors :: Maybe (HashSet Strict.Text) ->
                           Selector -> Selector ->
                           Maybe Selector
    tryCombineSelectors parentTags
                        s1 @ Selector { selectorInners = inners1,
                                        selectorTags = tags1 }
                        s2 @ Selector { selectorInners = inners2,
                                        selectorTags = tags2 }
        -- Short-circuit case for allSelector.
      | s1 == allSelector || s2 == allSelector = Just allSelector
      | otherwise =
        let
          combinedTags = combineTags tags1 tags2
          newTags = diffTags combinedTags parentTags
          newParentTags = combineTags combinedTags parentTags

          -- | First pass: pull in everything from inners2.  This will
          -- combine everything that can be combined and attempt to
          -- reduce the rest.
          firstpass :: HashMap Strict.Text Selector ->
                       Strict.Text -> Selector ->
                       HashMap Strict.Text Selector
          firstpass accum elem inner =
            case HashMap.lookup elem inners1 of
              -- If it exists in both, try to combine.
              Just inner' ->
                case tryCombineSelectors newParentTags inner inner' of
                -- If we get back an entry, insert it.
                  Just entry -> HashMap.insert elem entry accum
                  -- We might have reduced it to nothing.
                  Nothing -> accum
              -- Otherwise, attempt to reduce.
              Nothing -> case reduceSelector newParentTags inner of
                -- If we get back an entry, insert it.
                Just entry -> HashMap.insert elem entry accum
                -- If we reduce it to nothing, leave it out.
                Nothing -> accum

          -- | Second pass: pull in everything from inners1.
          secondpass :: HashMap Strict.Text Selector ->
                        Strict.Text -> Selector ->
                        HashMap Strict.Text Selector
          secondpass accum elem inner =
            case HashMap.lookup elem accum of
              -- If there's nothing there, it means we either had
              -- nothing, or we combined and reduced to nothing in the
              -- first pass.
              Nothing -> case HashMap.lookup elem inners2 of
                -- If we find an entry in inners2, then we combined
                -- and reduced to nothing.
                Just _ -> accum
                -- Otherwise, try to reduce the entry and insert it
                Nothing -> case reduceSelector newParentTags inner of
                  Just entry -> HashMap.insert elem entry accum
                  Nothing -> accum
              -- If there's something already there, it's because we
              -- combined successfully in the first pass.
              Just _ -> accum

          firstPassMap = HashMap.foldlWithKey' firstpass HashMap.empty inners2
          newInners = HashMap.foldlWithKey' secondpass firstPassMap inners1
        in
          if isNothing newTags && HashMap.null newInners
            then Nothing
            else Just $! Selector { selectorInners = newInners,
                                    selectorTags = newTags }
  in case tryCombineSelectors Nothing selector1 selector2 of
      Just out -> out
      Nothing -> error ("Got Nothing back from combineSelectors " ++
                        show selector1 ++ " " ++ show selector2)

-- | Collect all the selectors from filters that apply to all suites.
collectUniversals :: Filter
                  -> HashMap OptionMap (HashSet Selector)
                  -> HashMap OptionMap (HashSet Selector)
collectUniversals Filter { filterSuites = suites,
                           filterOptions = options,
                           filterSelector = selector } accum
  | HashSet.null suites =
    HashMap.insertWith HashSet.union options selector accum
  | otherwise = accum

-- | Build a map from suite names to the selectors that get run on them.
collectSelectors :: Filter
                 -- ^ The current filter
                 -> HashMap Strict.Text (HashMap OptionMap (HashSet Selector))
                 -- ^ The map from suites to sets of selectors that
                 -- run on them.
                 -> HashMap Strict.Text (HashMap OptionMap (HashSet Selector))
collectSelectors Filter { filterSuites = suites, filterOptions = options,
                          filterSelector = selector } suitemap =
  let
    foldfun accum suite =
      HashMap.insertWith (HashMap.unionWith HashSet.union) suite
                         (HashMap.singleton options selector) accum
  in
    foldl foldfun suitemap suites

-- | Take a list of test suite names and a list of 'Filter's, and
-- build a 'HashMap' that says for each test suite, what (combined)
-- 'Selector' should be used to select tests.
suiteSelectors :: [Strict.Text]
               -- ^ The names of all test suites.
               -> [Filter]
               -- ^ The list of 'Filter's from which to build the map.
               -> HashMap Strict.Text (HashMap OptionMap Selector)
suiteSelectors allsuites filters
  -- Short-circuit case if we have no filters, we run everything
  | null filters =
    foldl (\suitemap suite -> HashMap.insert suite noOptionsAllSelector
                                             suitemap)
          HashMap.empty allsuites
  | otherwise =
    let
      -- First, pull out all the universals
      universals = foldr collectUniversals HashMap.empty filters
      -- If we have any universals, then seed the initial map with them,
      -- otherwise, use the empty map.
      initMap =
        if not (HashMap.null universals)
          then foldl (\suitemap suite ->
                       HashMap.insert suite universals suitemap)
                     HashMap.empty allsuites
          else HashMap.empty

      -- Now collect all the suite-specific selectors
      suiteMap :: HashMap Strict.Text (HashMap OptionMap (HashSet Selector))
      suiteMap = foldr collectSelectors initMap filters
    in
      HashMap.map (HashMap.map (foldl1 combineSelectors . HashSet.toList))
                  suiteMap

nameParser :: Parser Strict.Text
nameParser =
  do
    out <- many1 (oneOf ("abcdefghijklmnopqrstuvwxyz" ++
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-_"))
    return $! Strict.pack out

valueParser :: Parser Strict.Text
valueParser =
  do
    out <- between (char '\"') (char '\"')
                   (many1 (oneOf ("abcdefghijklmnopqrstuvwxyz" ++
                                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" ++
                                 "~!@#$%^&*()[]{}<>:;,\'.+=-_?/\\|")))
    return $! Strict.pack out


optionParser :: Parser (Strict.Text, Strict.Text)
optionParser =
  do
    key <- valueParser
    _ <- char '='
    val <- valueParser
    return (key, val)

optionsParser :: Parser [(Strict.Text, Strict.Text)]
optionsParser = char '?' >> many1 optionParser

namesParser :: Parser [Strict.Text]
namesParser = sepBy1 nameParser (string ",")

pathParser :: Parser [Strict.Text]
pathParser = sepBy nameParser (string ".")

suitesParser :: Parser [Strict.Text]
suitesParser = between (string "[") (string "]") namesParser

tagsParser :: Parser [Strict.Text]
tagsParser = char '@' >> namesParser

filterParser :: Parser ([Strict.Text], [Strict.Text], [Strict.Text],
                        [(Strict.Text, Strict.Text)])
filterParser =
  do
    suites <- option [] suitesParser
    path <- pathParser
    tagselector <- option [] tagsParser
    options <- option [] optionsParser
    return (suites, path, tagselector, options)

makeFilter :: ([Strict.Text], [Strict.Text], [Strict.Text],
               [(Strict.Text, Strict.Text)]) -> Filter
makeFilter (suites, path, tags, options) =
  let
    withTags = case tags of
      [] -> allSelector
      _ -> allSelector { selectorTags = Just $! HashSet.fromList tags }

    genPath [] = withTags
    genPath (elem : rest) =
      let
        innermap = HashMap.singleton elem $! genPath rest
      in
        Selector { selectorInners = innermap, selectorTags = Nothing }

    withPath = genPath path
  in
   Filter { filterSuites = HashSet.fromList suites,
            filterSelector = HashSet.singleton withPath,
            filterOptions = HashMap.fromList options }

-- | Parse a 'Filter' expression.  The format for filter expressions is
-- described in the module documentation.
parseFilter :: String
            -- ^ The name of the source.
            -> Strict.Text
            -- ^ The input.
            -> Either Strict.Text Filter
parseFilter sourcename input =
  case parse filterParser sourcename input of
    Left e -> Left (Strict.pack (show e))
    Right res -> Right (makeFilter res)

commentParser :: Parser ()
commentParser =
  do
    _ <- char '#'
    _ <- many (noneOf "\n")
    return ()

lineParser :: Parser (Maybe Filter)
lineParser =
  do
    _ <- many space
    content <- filterParser
    _ <- many space
    optional commentParser
    case content of
      ([], [], [], []) -> return Nothing
      _ -> return (Just $! makeFilter content)

-- | Parse content from a testlist file.  The file must contain one
-- filter per line.  Leading and trailing spaces are ignored, as are
-- lines that contain no filter.  A @\#@ will cause the parser to skip
-- the rest of the line.
parseFilterFileContent :: String
                       -- ^ The name of the input file.
                       -> Strict.Text
                       -- ^ The file content.
                       -> Either [Strict.Text] [Filter]
parseFilterFileContent sourcename input =
  let
    inputlines = Strict.lines input
    results = map (parse lineParser sourcename) inputlines
  in case partitionEithers results of
    ([], maybes) -> Right $! catMaybes maybes
    (errs, _) -> Left $! map (Strict.pack . show) errs

-- | Given a 'FilePath', get the contents of the file and parse it as
-- a testlist file.
parseFilterFile :: FilePath -> IO (Either [Strict.Text] [Filter])
parseFilterFile filename =
  do
    input <- try (Strict.readFile filename)
    case input of
      Left e
        | isAlreadyInUseError e ->
          return (Left [Strict.concat ["Error reading testlist file ",
                                       Strict.pack filename,
                                       ": File is already in use"]])
        | isDoesNotExistError e ->
          return (Left [Strict.concat ["Error reading testlist file ",
                                       Strict.pack filename,
                                       ": File does not exist"]])
        | isPermissionError e ->
          return (Left [Strict.concat ["Error reading testlist file ",
                                       Strict.pack filename,
                                       ": Permission denied"]])
        | otherwise ->
          return (Left [Strict.concat ["Cannot read testlist file ",
                                       Strict.pack filename,
                                       ": Miscellaneous error"]])
      Right contents ->
        case parseFilterFileContent filename contents of
          Left errs -> return (Left errs)
          Right out -> return (Right out)
