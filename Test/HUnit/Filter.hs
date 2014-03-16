{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}

-- | Filters for running tests.  These are used by
-- [@Test.HUnit.Execution@] to select which tests are run.  It is
-- important to note that [@Filter@]s and [@Selector@]s may specify
-- that a test should be run multiple times, but with different options.
module Test.HUnit.Filter(
       Selector(..),
       Filter(..),
       passFilter,
       all,
       normalizeSelector
       ) where

import Data.Foldable(foldr)
import Data.Set(Set)
import Data.Map(Map)
import Prelude hiding (foldr, elem)

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
  -- | Run all tests without any further modifications.
  | All
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

-- | A [@Filter@] that selects everything
passFilter :: Filter
passFilter = Filter { filterSuites = Set.empty, filterSelector = allSelector }

-- | A [@Selector@] that selects everything
allSelector :: Selector
allSelector = Union { unionInners = Set.empty }

-- | Gather up path elements into sets of inners grouped by the
-- leading path element.
collectPathSets :: Selector
                -- ^ The selector being collected
                -> Map String (Set Selector)
                -- ^ A map from beginning path elements to the inner selectors
                -> Map String (Set Selector)
collectPathSets Path { pathElem = elem, pathInner = inner } pathmap =
  Map.insertWith Set.union elem (Set.singleton inner) pathmap
collectPathSets selector _ = error ("Should not see " ++ show selector)

-- | Generate a selector from an entry in a path map
genPathSelector :: String -> Set Selector -> Selector
genPathSelector elem pathset
  -- All subsumes everything else, so short-circuit if it's in the pathset
  | Set.member All pathset = Path { pathElem = elem, pathInner = All }
  | otherwise =
    case Set.elems pathset of
      -- For singleton sets, build a single path element
      [ inner ] ->  Path { pathElem = elem, pathInner = inner }
      -- Otherwise, build a union
      _ -> Path { pathElem = elem, pathInner = Union { unionInners = pathset } }

-- | Transform a tag set into a single normal-form Selector
normalizeTagMapEntry :: Set Selector -> Selector
normalizeTagMapEntry tagset
  -- Short-circuit case: if the set contains an All, then it subsumes
  -- everything else
  | Set.member All tagset = All
  | otherwise = case Set.elems tagset of
    -- Second short-circuit case: singleton set doesn't need to go
    -- through the collectPathSets logic.
    [ inner ] -> inner
    _ ->
      let
        -- Build the path map
        pathmap = foldr collectPathSets Map.empty tagset
        -- Then convert it into a set of union elements
        inners = Map.foldWithKey (\elem pathset accum ->
                                   Set.insert (genPathSelector elem pathset)
                                              accum)
                       Set.empty pathmap
      in case Set.elems inners of
        -- If the set of union elements is a singleton, just return
        -- the one element
        [ inner ] -> inner
        -- Otherwise, build a union
        _ -> Union { unionInners = inners }

-- | Walk all nested Union and Tags elements, and group all elements
-- into categories by which tags they filter for.
collectTagSets :: Set String
               -- ^ The current set of tags
               -> Selector
               -- ^ The selector being collected
               -> Map (Set String) (Set Selector)
               -- ^ A map from tag sets to selectors that filter by them
               -> Map (Set String) (Set Selector)
-- For unions, fold over the inners
collectTagSets tagset Union { unionInners = inners } tagmap =
  foldr (collectTagSets tagset) tagmap (Set.map normalizeSelector inners)
-- For tags, add tags to the current tag set, and descend
collectTagSets tagset Tags { tagsNames = tags, tagsInner = inner } tagmap =
  collectTagSets (Set.union tagset tags) inner tagmap
-- For everything else, add the element to the mapping for the current tag set
collectTagSets tagset elem tagmap =
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
normalizeSelector Union { unionInners = inners } =
  let
    norminners = Set.map normalizeSelector inners
  in case Set.elems norminners of
    -- Turn an empty union into an All
    [] -> All
    -- For singleton sets, just normalize the single element
    [ inner ] -> inner
    -- Fast short-circuit case if inners contains an All
    _ -> if Set.member All norminners then All
      else
        let
          -- Build the tag map
          tagmap  = foldr (collectTagSets Set.empty) Map.empty norminners
          -- Then convert it into a set of union elements
          newinners =
            Map.foldWithKey (\tags tagset accum ->
                              Set.insert (genTagSetSelector tags tagset) accum)
                            Set.empty tagmap
        in case Map.lookup Set.empty tagmap of
          -- If we have a zero-tag set that contains All, it subsumes everything
          Just notagmap | Set.member All notagmap -> All
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
-- All normalizes to itself
normalizeSelector All = All
