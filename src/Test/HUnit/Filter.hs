{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Filters for running tests.  These are used by
-- [@Test.HUnit.Execution@] to select which tests are run.  It is
-- important to note that [@Filter@]s and [@Selector@]s may specify
-- that a test should be run multiple times, but with different options.
module Test.HUnit.Filter(
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

-- | A @Selector@ is a tree structure used to select and run tests
-- within one or more suites.
data Selector =
    Selector {
      -- | @Selector@s for subgroups of this one, indexed by the leading
      -- path element.
      selectorInners :: Map String Selector,
      -- | Tags by which to filter all tests, or @Nothing@ if we cannot
      -- execute local tests.  Note that the empty set actually means
      -- "run all tests regardless of tags".
      selectorTags :: !(Maybe (Set String))
    }
    deriving (Eq, Ord, Show)

-- | A @Filter@ specifies zero or more test suites, to which a
-- @Selector@ is then applied.  If no test suites are specified,
-- then the @Selector@ applies to all test suites.
data Filter =
  Filter {
    -- | The test suites to which the [@Selector@] applies.
    filterSuites :: !(Set String),
    -- | The [@Selector@] to apply.
    filterSelector :: !Selector
  }
  deriving (Ord, Eq, Show)

-- | Combine two tags fields into one.
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

-- | A @Filter@ that selects everything
passFilter :: Filter
passFilter = Filter { filterSuites = Set.empty, filterSelector = allSelector }

-- | A @Selector@ that selects everything
allSelector :: Selector
allSelector = Selector { selectorInners = Map.empty,
                         selectorTags = Just Set.empty }

-- | Combine two selectors into a single one
combineSelectors :: Selector -> Selector -> Selector
combineSelectors s1 @ Selector { selectorInners = inners1, selectorTags = tags1 }
                 s2 @ Selector { selectorInners = inners2, selectorTags = tags2 }
  | s1 == allSelector || s2 == allSelector = allSelector
  | otherwise =
    Selector { selectorInners = Map.unionWith combineSelectors inners1 inners2,
               selectorTags = combineTags tags1 tags2 }

-- | Collect all the selectors from filters that apply to all suites
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

-- | Take a list of test suite names and a list of filters, and build
-- a map that says for each test suite, which (normalized) selectors
-- should be run.
suiteSelectors :: [String]
               -- ^ The names of all test suites
               -> [Filter]
               -- ^ The list of filters from which to build the map
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

namesParser :: GenParser Char st [String]
namesParser = sepBy1 (many1 alphaNum) (string ",")

pathParser :: GenParser Char st [String]
pathParser = sepBy (many1 alphaNum) (string ".")

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

-- | Parse a Filter expression
parseFilter :: String
            -- ^ The name of the source
            -> String
            -- ^ The input
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

-- | Parse content from a filter file
parseFilterFileContent :: String
                       -- ^ The name of the input file
                       -> String
                       -- ^ The file content
                       -> Either [String] [Filter]
parseFilterFileContent sourcename input =
  let
    inputlines = lines input
    results = map (parse lineParser sourcename) inputlines
  in case partitionEithers results of
    ([], maybes) -> Right (catMaybes maybes)
    (errs, _) -> Left (map show errs)

-- | Parse the contents of a testlist file
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
