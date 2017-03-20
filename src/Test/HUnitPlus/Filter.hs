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
import Text.ParserCombinators.Parsec hiding (try)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

-- | A tree-like structure that represents a set of tests within a
-- given suite.
data Selector =
    Selector {
      -- | @Selector@s for subgroups of this one.  The entry for each
      -- path element contains the @Selector@ to be used for that
      -- group (or test).  An empty map actually means 'select all
      -- tests'.
      selectorInners :: HashMap String Selector,
      -- | Tags by which to filter all tests.  The empty set actually
      -- means 'run all tests regardless of tags'.  'Nothing' means
      -- that all tests will be skipped (though this will be
      -- overridden by any @Selector@s in @selectorInners@.
      selectorTags :: !(Maybe (HashSet String))
    }
    deriving (Eq, Show)

-- | Specifies zero or more test suites, to which the given 'Selector'
-- is then applied.  If no test suites are specified, then the
-- 'Selector' applies to all test suites.
data Filter =
  Filter {
    -- | The test suites to which the 'Selector' applies.  The empty
    -- set actually means 'all suites'.
    filterSuites :: !(HashSet String),
    -- | The 'Selector' to apply.
    filterSelector :: !Selector
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
combineTags :: Maybe (HashSet String) -> Maybe (HashSet String) ->
               Maybe (HashSet String)
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
diffTags :: Maybe (HashSet String) -> Maybe (HashSet String) ->
            Maybe (HashSet String)
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
                      filterSelector = allSelector }

-- | A 'Selector' that selects all tests.
allSelector :: Selector
allSelector = Selector { selectorInners = HashMap.empty,
                         selectorTags = Just HashSet.empty }

reduceSelector :: Maybe (HashSet String) -> Selector -> Maybe Selector
reduceSelector parentTags Selector { selectorInners = inners,
                                     selectorTags = tags } =
  let
    newTags = diffTags tags parentTags
    newParentTags = combineTags parentTags tags
    newInners = HashMap.mapMaybe (reduceSelector newParentTags) inners
  in
    if isNothing newTags && HashMap.null newInners
      then Nothing
      else Just $! Selector { selectorInners = inners, selectorTags = tags }

-- | Combine two 'Selector's into a single 'Selector'.
combineSelectors :: Selector -> Selector -> Selector
combineSelectors selector1 selector2 =
  let
    combineSelectors' :: Maybe (HashSet String) -> Selector -> Selector ->
                         Maybe Selector
    combineSelectors' parentTags
                      s1 @ Selector { selectorInners = inners1,
                                      selectorTags = tags1 }
                      s2 @ Selector { selectorInners = inners2,
                                      selectorTags = tags2 }
      | s1 == allSelector || s2 == allSelector = Just allSelector
      | otherwise =
        let
          combinedTags = combineTags tags1 tags2
          newTags = diffTags combinedTags parentTags
          newParentTags = combineTags combinedTags parentTags

          firstpass :: HashMap String Selector -> String -> Selector ->
                       HashMap String Selector
          firstpass accum elem inner =
            case HashMap.lookup elem inners1 of
              Just inner' ->
                case combineSelectors' newParentTags inner inner' of
                  Just entry -> HashMap.insert elem entry accum
                  Nothing -> accum
              Nothing -> case reduceSelector newParentTags inner of
                Just entry -> HashMap.insert elem entry accum
                Nothing -> accum

          secondpass :: HashMap String Selector -> String -> Selector ->
                        HashMap String Selector
          secondpass accum elem inner =
            case HashMap.lookup elem accum of
              Nothing -> case reduceSelector newParentTags inner of
                Just entry -> HashMap.insert elem entry accum
                Nothing -> accum
              Just _ -> accum

          firstPassMap = HashMap.foldlWithKey' firstpass HashMap.empty inners2
          newInners = HashMap.foldlWithKey' secondpass firstPassMap inners1
        in
          if isNothing newTags && HashMap.null newInners
            then Nothing
            else Just $! Selector { selectorInners = newInners,
                                    selectorTags = newTags }
  in
    case combineSelectors' Nothing selector1 selector2 of
      Just out -> out
      Nothing -> error ("Got Nothing back from combineSelectors " ++
                        show selector1 ++ " " ++ show selector2)

-- | Collect all the selectors from filters that apply to all suites.
collectUniversals :: Filter -> HashSet Selector -> HashSet Selector
collectUniversals Filter { filterSuites = suites,
                           filterSelector = selector } accum
  | suites == HashSet.empty = HashSet.insert selector accum
  | otherwise = accum

-- | Build a map from suite names to the selectors that get run on them.
collectSelectors :: Filter
                 -- ^ The current filter
                 -> HashMap String (HashSet Selector)
                 -- ^ The map from suites to
                 -> HashMap String (HashSet Selector)
collectSelectors Filter { filterSuites = suites, filterSelector = selector }
                 suitemap =
    foldl (\suitemap' suite -> HashMap.insertWith HashSet.union suite
                                              (HashSet.singleton selector)
                                              suitemap')
          suitemap suites

-- | Take a list of test suite names and a list of 'Filter's, and
-- build a 'HashMap' that says for each test suite, what (combined)
-- 'Selector' should be used to select tests.
suiteSelectors :: [String]
               -- ^ The names of all test suites.
               -> [Filter]
               -- ^ The list of 'Filter's from which to build the map.
               -> HashMap String Selector
suiteSelectors allsuites filters
  -- Short-circuit case if we have no filters, we run everything
  | filters == [] =
    foldl (\suitemap suite -> HashMap.insert suite allSelector suitemap)
          HashMap.empty allsuites
  | otherwise =
    let
      -- First, pull out all the universals
      universals = foldr collectUniversals HashSet.empty filters
      -- If we have any universals, then seed the initial map with them,
      -- otherwise, use the empty map.
      initMap =
        if universals /= HashSet.empty
          then foldl (\suitemap suite ->
                       HashMap.insert suite universals suitemap)
                     HashMap.empty allsuites
          else HashMap.empty

      -- Now collect all the suite-specific selectors
      suiteMap :: HashMap String (HashSet Selector)
      suiteMap = foldr collectSelectors initMap filters
    in
      HashMap.map (foldl1 combineSelectors . HashSet.toList) suiteMap

nameParser :: GenParser Char st Char
nameParser =
  oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-_"

namesParser :: GenParser Char st [String]
namesParser = sepBy1 (many1 nameParser) (string ",")

pathParser :: GenParser Char st [String]
pathParser = sepBy (many1 nameParser) (string ".")

suitesParser :: GenParser Char st [String]
suitesParser = between (string "[") (string "]") namesParser

tagsParser :: GenParser Char st [String]
tagsParser = char '@' >> namesParser

filterParser :: GenParser Char st ([String], [String], [String])
filterParser =
  do
    suites <- option [] suitesParser
    path <- pathParser
    tagselector <- option [] tagsParser
    return (suites, path, tagselector)

makeFilter :: ([String], [String], [String]) -> Filter
makeFilter (suites, path, tags) =
  let
    withTags = case tags of
      [] -> allSelector
      _ -> allSelector { selectorTags = Just $! HashSet.fromList tags }

    genPath [] = withTags
    genPath (elem : rest) =
      Selector { selectorInners = HashMap.singleton elem $! genPath rest,
                 selectorTags = Nothing }

    withPath = genPath path
  in
   Filter { filterSuites = HashSet.fromList suites, filterSelector = withPath }

-- | Parse a 'Filter' expression.  The format for filter expressions is
-- described in the module documentation.
parseFilter :: String
            -- ^ The name of the source.
            -> String
            -- ^ The input.
            -> Either String Filter
parseFilter sourcename input =
  case parse filterParser sourcename input of
    Left e -> Left (show e)
    Right res -> Right (makeFilter res)

commentParser :: GenParser Char st ()
commentParser =
  do
    _ <- char '#'
    _ <- many (noneOf "\n")
    return $ ()

lineParser :: GenParser Char st (Maybe Filter)
lineParser =
  do
    _ <- many space
    content <- filterParser
    _ <- many space
    optional commentParser
    case content of
      ([], [], []) -> return $ Nothing
      _ -> return $ (Just (makeFilter content))

-- | Parse content from a testlist file.  The file must contain one
-- filter per line.  Leading and trailing spaces are ignored, as are
-- lines that contain no filter.  A @\#@ will cause the parser to skip
-- the rest of the line.
parseFilterFileContent :: String
                       -- ^ The name of the input file.
                       -> String
                       -- ^ The file content.
                       -> Either [String] [Filter]
parseFilterFileContent sourcename input =
  let
    inputlines = lines input
    results = map (parse lineParser sourcename) inputlines
  in case partitionEithers results of
    ([], maybes) -> Right (catMaybes maybes)
    (errs, _) -> Left (map show errs)

-- | Given a 'FilePath', get the contents of the file and parse it as
-- a testlist file.
parseFilterFile :: FilePath -> IO (Either [String] [Filter])
parseFilterFile filename =
  do
    input <- try (readFile filename)
    case input of
      Left e
        | isAlreadyInUseError e ->
          return (Left ["Error reading testlist file " ++ filename ++
                        ": File is already in use"])
        | isDoesNotExistError e ->
          return (Left ["Error reading testlist file " ++ filename ++
                        ": File does not exist"])
        | isPermissionError e ->
          return (Left ["Error reading testlist file " ++ filename ++
                        ": Permission denied"])
        | otherwise ->
          return (Left ["Cannot read testlist file " ++ filename ++
                        ": Miscellaneous error"])
      Right contents ->
        case parseFilterFileContent filename contents of
          Left errs -> return (Left errs)
          Right out -> return (Right out)
