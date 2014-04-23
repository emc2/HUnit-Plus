{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Filters for running tests.  These are used by
-- [@Test.HUnit.Execution@] to select which tests are run.  It is
-- important to note that [@Filter@]s and [@Selector@]s may specify
-- that a test should be run multiple times, but with different options.
module Test.HUnit.Filter(
       Selector(..),
       Filter(..),
       passFilter,
       allSelector,
       normalizeSelector,
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
--import Debug.Trace
import Prelude hiding (foldl, elem)
import System.IO.Error
import Text.ParserCombinators.Parsec hiding (try)

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | A [@Selector@] is a tree structure used to select and run tests
-- within one or more suites.
data Selector =
  -- | A test set created from a union of multiple test sets.
    Union {
      -- | The elements of the union.
      unionInners :: Set Selector
    }
  -- | Append a path element to the context path, then apply the inner
  -- selector to all tests that begin with the context path.
  | Path {
      -- | The name to add to the current context path.
      pathElem :: !String,
      -- | The selector to apply in the new context.
      pathInner :: Selector
    }
  -- | Add tags to the set of tags used to filter tests.  Note that if
  -- that set is empty, all tests will be selected.
  | Tags {
      -- | The tags to add to the current set of tags.
      tagsNames :: !(Set String),
      -- | The selector from which to filter tests by tags.
      tagsInner :: Selector 
    }
    deriving (Eq, Ord, Show)

-- | A [@Filter@] specifies zero or more test suites, to which a
-- [@Selector@] is then applied.  If no test suites are specified,
-- then the [@Selector@] applies to all test suites.
data Filter =
  Filter {
    -- | The test suites to which the [@Selector@] applies.
    filterSuites :: !(Set String),
    -- | The [@Selector@] to apply.
    filterSelector :: !Selector
  }
  deriving (Ord, Eq, Show)

-- | A [@Filter@] that selects everything
passFilter :: Filter
passFilter = Filter { filterSuites = Set.empty, filterSelector = allSelector }

-- | A [@Selector@] that selects everything
allSelector :: Selector
allSelector = Union { unionInners = Set.empty }

-- | Gather up path elements into sets of inners grouped by the
-- leading path element.
collectPathSets :: Map String (Set Selector)
                -- ^ A map from beginning path elements to the inner selectors
                -> Selector
                -- ^ The selector being collected
                -> Map String (Set Selector)
collectPathSets pathmap Path { pathElem = elem, pathInner = inner } =
  Map.insertWith Set.union elem (Set.singleton inner) pathmap
collectPathSets _ selector = error ("Should not see " ++ show selector)

-- | Generate a selector from an entry in a path map
genPathSelector :: String -> Set Selector -> Selector
genPathSelector elem pathset
  -- All subsumes everything else, so short-circuit if it's in the pathset
  | Set.member allSelector pathset = Path { pathElem = elem,
                                            pathInner = allSelector }
  | otherwise =
    case Set.elems pathset of
      -- For singleton sets, build a single path element
      [ inner ] ->  Path { pathElem = elem, pathInner = inner }
      -- Otherwise, build a union
      _ -> Path { pathElem = elem, pathInner = Union { unionInners = pathset } }

-- | Turn all cases of { tag1 } -> selector, { tag2 } -> selector into
-- { tag1, tag2 } -> selector
combineTagSets :: Map (Set String) (Set Selector) ->
                  Map (Set String) (Set Selector)
combineTagSets pathmap =
  let
    combinefunc :: Set String -> Set String -> Set String
    combinefunc a b
      | a == Set.empty || b == Set.empty = Set.empty
      | otherwise = Set.union a b
    -- Build up a reverse map, from selectors to tag sets.  This will
    -- combine all tag sets for each selector.
    revmap :: Map Selector (Set String)
    revmap =
      Map.foldWithKey (\tagset selectors accum ->
                        foldl (\accum' selector ->
                                Map.insertWith combinefunc selector
                                               tagset accum')
                              accum selectors)
                      Map.empty pathmap
  in
    -- Now rebuild the original map by reversing it again.
    Map.foldWithKey (\selector tagset accum ->
                      Map.insertWith Set.union tagset
                                     (Set.singleton selector) accum)
                    Map.empty revmap

-- | Transform a tag set into a single normal-form Selector
normalizeTagMapEntry :: Set Selector -> Selector
normalizeTagMapEntry tagset
  -- Short-circuit case: if the set contains an All, then it subsumes
  -- everything else
  | Set.member allSelector tagset = allSelector
  | otherwise = case Set.elems tagset of
    -- Second short-circuit case: singleton set doesn't need to go
    -- through the collectPathSets logic.
    [ inner ] -> inner
    _ ->
      let
        -- Build the path map
        pathmap :: Map String (Set Selector)
        pathmap = foldl collectPathSets Map.empty tagset
        -- Then convert it into a set of union elements
        inners = Map.foldWithKey (\elem pathset accum ->
                                   Set.insert (genPathSelector elem pathset)
                                              accum)
                       Set.empty $! pathmap
      in case Set.elems inners of
        -- If the set of union elements is a singleton, just return
        -- the one element
        [ inner ] -> inner
        -- Otherwise, build a union
        _ -> Union { unionInners = inners }

-- | Walk all nested Union and Tags elements, and group all elements
-- into categories by which tags they filter for.
collectTagSets :: Set String
               -- ^ The selector being collected
               -> Map (Set String) (Set Selector)
               -- ^ The current set of tags
               -> Selector
               -- ^ A map from tag sets to selectors that filter by them
               -> Map (Set String) (Set Selector)
-- For unions, fold over the inners
collectTagSets tagset tagmap elem @ Union { unionInners = inners }
  | Set.empty == inners =
    Map.insertWith Set.union tagset (Set.singleton elem) tagmap
  | otherwise =
    foldl (collectTagSets tagset) tagmap (Set.map normalizeSelector inners)
-- For tags, add tags to the current tag set, and descend
collectTagSets tagset tagmap Tags { tagsNames = tags, tagsInner = inner } =
  collectTagSets (Set.union tagset tags) tagmap inner
-- For everything else, add the element to the mapping for the current tag set
collectTagSets tagset tagmap elem =
  Map.insertWith Set.union tagset
                 (Set.singleton (normalizeSelector elem)) tagmap

-- | Generate a selector from an entry in a tagset map
genTagSetSelector :: Set String -> Set Selector -> Selector
genTagSetSelector tags tagset
  | tags == Set.empty = normalizeTagMapEntry tagset
  | otherwise = Tags { tagsInner = normalizeTagMapEntry tagset,
                       tagsNames = tags }

-- | Normalize a Selector
normalizeSelector :: Selector -> Selector
-- For this case, we have three degeneracies to worry about:
-- overlapping union members, nested unions, singleton unions.
normalizeSelector u @ Union { unionInners = inners } =
  let
    norminners = Set.map normalizeSelector inners
  in case Set.elems inners of
    -- Turn an empty union into an All
    [] -> u
    -- For singleton sets, just normalize the single element
    [ inner ] -> inner
    -- Fast short-circuit case if inners contains an All
    _ -> if Set.member allSelector norminners then allSelector
      else
        let
          -- Build the tag map
          tagmap :: Map (Set String) (Set Selector)
          tagmap = combineTagSets
                     (foldl (collectTagSets Set.empty) Map.empty norminners)
          -- Then convert it into a set of union elements
          newinners =
            Map.foldWithKey (\tags tagset accum ->
                              Set.insert (genTagSetSelector tags tagset) accum)
                            Set.empty $! tagmap
        in case Map.lookup Set.empty tagmap of
          -- If we have a zero-tag set that contains All, it subsumes everything
          Just notagmap | Set.member allSelector notagmap -> allSelector
          _ -> case Set.elems newinners of
            -- If the set of union elements is a singleton, just return
            -- the one element
            [ inner ] -> inner
            -- Otherwise, build a union
            _ -> Union { unionInners = newinners }
-- For paths, move any tags elements to the outside
normalizeSelector p @ Path { pathInner = inner } =
  case normalizeSelector inner of
    -- For tags, swap places
    t @ Tags { tagsInner = inner' } ->
      t { tagsInner = p { pathInner = inner' } }
    -- Otherwise, install the normalized inner
    inner' -> p { pathInner = inner' }
-- For a tags element, combine two immediately nested tags
normalizeSelector t @ Tags { tagsNames = tags, tagsInner = inner } =
  case normalizeSelector inner of
    -- Combine immediately nested tags
    Tags { tagsNames = innertags, tagsInner = inner' } ->
      Tags { tagsNames = Set.union tags innertags, tagsInner = inner' }
    -- Otherwise, install the normalized inner
    inner' -> t { tagsInner = inner' }

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
                               (Set.singleton selector) suitemap')
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
      suiteMap = foldr collectSelectors initMap filters

      -- Last thing we want to do is normalize all the map entries
      mapfun selectors = normalizeSelector Union { unionInners = selectors }
    in
      Map.map mapfun suiteMap

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
    genPath [] = allSelector
    genPath (elem : rest) = Path { pathElem = elem, pathInner = genPath rest }

    withPath = genPath path
    withTags = case tags of
      [] -> withPath
      _ -> Tags { tagsNames = Set.fromList tags, tagsInner = withPath }
  in
   Filter { filterSuites = Set.fromList suites, filterSelector = withTags }

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
