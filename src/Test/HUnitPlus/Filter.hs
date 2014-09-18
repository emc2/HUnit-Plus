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
import Data.Map(Map)
import Data.Maybe
import Data.Set(Set)
import Prelude hiding (foldl, elem)
import System.IO.Error
import Text.ParserCombinators.Parsec hiding (try)

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | A tree-like structure that represents a set of tests within a
-- given suite.
data Selector =
    Selector {
      -- | @Selector@s for subgroups of this one.  The entry for each
      -- path element contains the @Selector@ to be used for that
      -- group (or test).  An empty map actually means 'select all
      -- tests'.
      selectorInners :: Map String Selector,
      -- | Tags by which to filter all tests.  The empty set actually
      -- means 'run all tests regardless of tags'.  'Nothing' means
      -- that all tests will be skipped (though this will be
      -- overridden by any @Selector@s in @selectorInners@.
      selectorTags :: !(Maybe (Set String))
    }
    deriving (Eq, Ord, Show)

-- | Specifies zero or more test suites, to which the given 'Selector'
-- is then applied.  If no test suites are specified, then the
-- 'Selector' applies to all test suites.
data Filter =
  Filter {
    -- | The test suites to which the 'Selector' applies.  The empty
    -- set actually means 'all suites'.
    filterSuites :: !(Set String),
    -- | The 'Selector' to apply.
    filterSelector :: !Selector
  }
  deriving (Ord, Eq, Show)

-- | Combine two 'selectorTags' fields into one.  This operation represents the
-- union of the tests that are selected by the two fields.
combineTags :: Maybe (Set String) -> Maybe (Set String) -> Maybe (Set String)
-- Nothing means we can't execute, so if the other side says we can,
-- we can.
combineTags Nothing t = t
combineTags t Nothing = t
combineTags (Just a) (Just b)
  -- The empty set means we execute everything, so it absorbs
  | a == Set.empty || b == Set.empty = Just $! Set.empty
  -- Otherwise, we do set union
  | otherwise = Just $! Set.union a b

-- | Take the difference of one set of tags from another.
diffTags :: Maybe (Set String) -> Maybe (Set String) -> Maybe (Set String)
-- Nothing means we can't execute, so if the other side says we can,
-- we can.
diffTags Nothing _ = Nothing
diffTags t Nothing = t
diffTags (Just a) (Just b)
  | a == Set.empty = Just Set.empty
  | b == Set.empty = Nothing
  -- Otherwise, we do set union
  | otherwise =
    let
      diff = Set.difference a b
    in
      if diff == Set.empty
        then Nothing
        else Just $! diff

-- | A 'Filter' that selects all tests in all suites.
passFilter :: Filter
passFilter = Filter { filterSuites = Set.empty, filterSelector = allSelector }

-- | A 'Selector' that selects all tests.
allSelector :: Selector
allSelector = Selector { selectorInners = Map.empty,
                         selectorTags = Just Set.empty }

reduceSelector :: Maybe (Set String) -> Selector -> Maybe Selector
reduceSelector parentTags Selector { selectorInners = inners,
                                     selectorTags = tags } =
  let
    newTags = diffTags tags parentTags
    newParentTags = combineTags parentTags tags
    newInners = Map.mapMaybe (reduceSelector newParentTags) inners
  in
    if newTags == Nothing && newInners == Map.empty
      then Nothing
      else Just $! Selector { selectorInners = inners, selectorTags = tags }

-- | Combine two 'Selector's into a single 'Selector'.
combineSelectors :: Selector -> Selector -> Selector
combineSelectors selector1 selector2 =
  let
    combineSelectors' :: Maybe (Set String) -> Selector -> Selector ->
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

          firstpass :: Map String Selector -> String -> Selector ->
                       Map String Selector
          firstpass accum elem inner =
            case Map.lookup elem inners1 of
              Just inner' -> case combineSelectors' newParentTags inner inner' of
                Just entry -> Map.insert elem entry accum
                Nothing -> accum
              Nothing -> case reduceSelector newParentTags inner of
                Just entry -> Map.insert elem entry accum
                Nothing -> accum

          secondpass :: Map String Selector -> String -> Selector ->
                        Map String Selector
          secondpass accum elem inner =
            case Map.lookup elem accum of
              Nothing -> case reduceSelector newParentTags inner of
                Just entry -> Map.insert elem entry accum
                Nothing -> accum
              Just _ -> accum

          firstPassMap = Map.foldlWithKey firstpass Map.empty inners2
          newInners = Map.foldlWithKey secondpass firstPassMap inners1
        in
          if newTags == Nothing && newInners == Map.empty
            then Nothing
            else Just $! Selector { selectorInners = newInners,
                                    selectorTags = newTags }
  in
    case combineSelectors' Nothing selector1 selector2 of
      Just out -> out
      Nothing -> error ("Got Nothing back from combineSelectors " ++
                        show selector1 ++ " " ++ show selector2)

-- | Collect all the selectors from filters that apply to all suites.
collectUniversals :: Filter -> Set Selector -> Set Selector
collectUniversals Filter { filterSuites = suites,
                           filterSelector = selector } accum
  | suites == Set.empty = Set.insert selector accum
  | otherwise = accum

-- | Build a map from suite names to the selectors that get run on them.
collectSelectors :: Filter
                 -- ^ The current filter
                 -> Map String (Set Selector)
                 -- ^ The map from suites to 
                 -> Map String (Set Selector)
collectSelectors Filter { filterSuites = suites, filterSelector = selector }
                 suitemap =
    foldl (\suitemap' suite -> Map.insertWith Set.union suite
                                              (Set.singleton selector)
                                              suitemap')
          suitemap suites

-- | Take a list of test suite names and a list of 'Filter's, and
-- build a 'Map' that says for each test suite, what (combined)
-- 'Selector' should be used to select tests.
suiteSelectors :: [String]
               -- ^ The names of all test suites.
               -> [Filter]
               -- ^ The list of 'Filter's from which to build the map.
               -> Map String Selector
suiteSelectors allsuites filters
  -- Short-circuit case if we have no filters, we run everything
  | filters == [] =
    foldl (\suitemap suite -> Map.insert suite allSelector suitemap)
          Map.empty allsuites
  | otherwise =
    let
      -- First, pull out all the universals
      universals = foldr collectUniversals Set.empty filters
      -- If we have any universals, then seed the initial map with them,
      -- otherwise, use the empty map.
      initMap =
        if universals /= Set.empty
          then foldl (\suitemap suite -> Map.insert suite universals suitemap)
                     Map.empty allsuites
          else Map.empty

      -- Now collect all the suite-specific selectors
      suiteMap :: Map String (Set Selector)
      suiteMap = foldr collectSelectors initMap filters
    in
      Map.map (foldl1 combineSelectors . Set.elems) suiteMap

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
    suites <- option [] (suitesParser)
    path <- pathParser
    tagselector <- option [] tagsParser
    return (suites, path, tagselector)

makeFilter :: ([String], [String], [String]) -> Filter
makeFilter (suites, path, tags) =
  let
    withTags = case tags of
      [] -> allSelector
      _ -> allSelector { selectorTags = Just $! Set.fromList tags }

    genPath [] = withTags
    genPath (elem : rest) =
      Selector { selectorInners = Map.singleton elem $! genPath rest,
                 selectorTags = Nothing }

    withPath = genPath path
  in
   Filter { filterSuites = Set.fromList suites, filterSelector = withPath }

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
