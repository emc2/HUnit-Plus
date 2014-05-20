module Main(main) where

import Data.Word
import Distribution.TestSuite
import System.Environment
import Test.HUnitPlus.Base
import Test.HUnitPlus.Execution
import Test.HUnitPlus.Filter
import Test.HUnitPlus.Reporting

import qualified Data.HashTable.IO as HashTable
import qualified Data.Map as Map
import qualified Data.Set as Set

simpleTest :: Test
simpleTest =
  let
    runTest = return (Finished Pass)

    testInstance = TestInstance { name = "synthetic_test", tags = [],
                                  setOption = (\_ _ -> Right testInstance),
                                  options = [], run = runTest }
  in
    Test testInstance

makeTestTree :: Word -> Word -> Word -> [Test] -> IO Test
makeTestTree _ _ 0 tail = return $! testGroup "synthetic_group" tail
makeTestTree maxwidth 0 width tail =
    makeTestTree maxwidth 0 (width - 1) $! (simpleTest : tail)
makeTestTree maxwidth height width tail =
  do
    branch <- makeTestTree maxwidth (height - 1) maxwidth []
    makeTestTree maxwidth height (width - 1) $! (branch : tail)

makeFilters :: [Selector] -> Word -> [Filter] -> [Filter]
makeFilters _ 0 tail = tail
makeFilters selectors count tail =
  let
    makeFilters' :: [Selector] -> [Filter] -> [Filter]
    makeFilters' [] tail = tail
    makeFilters' (selector : rest) tail =
      makeFilters' rest (Filter { filterSuites = Set.singleton (show count),
                                  filterSelector = selector } : tail)
  in
    makeFilters' selectors tail

makeSuites :: Word -> [String] -> [String]
makeSuites 0 tail = tail
makeSuites count tail = makeSuites (count - 1) (show count : tail)

makeFakeSuites :: Word -> [TestSuite] -> [TestSuite]
makeFakeSuites 0 tail = tail
makeFakeSuites count tail =
  makeFakeSuites (count - 1) (TestSuite { suiteName = show count,
                                          suiteTests = [simpleTest],
                                          suiteConcurrently = False,
                                          suiteOptions = [] } : tail)

makeAllSelectors :: Word -> [Selector] -> [Selector]
makeAllSelectors 0 tail = tail
makeAllSelectors count tail = makeAllSelectors (count - 1) (allSelector : tail)

quietReporter = defaultReporter { reporterStart = return () }

runPerf :: Bool -> [String] -> IO ()
runPerf _ ("baseline" : rest) = runPerf True rest
runPerf dryrun ["run_tests", widthstr, heightstr] =
  let
    width :: Word
    width = read widthstr

    height :: Word
    height = read heightstr
  in do
    selectormap <- HashTable.fromListWithSizeHint 1 [("suite", allSelector)]
    tree <- makeTestTree width height width []
    _ <- performTestSuites quietReporter selectormap
                           [TestSuite { suiteName = "suite", suiteTests = [tree],
                                        suiteConcurrently = False,
                                        suiteOptions = [] }]
    return ()
runPerf dryrun ["filters", selectorsstr, suitesstr] =
  let
    numsuites :: Word
    numsuites = read suitesstr

    numselectors :: Word
    numselectors = read selectorsstr

    selectors = makeAllSelectors numselectors []
    filters = makeFilters selectors numsuites []

    suites = makeSuites numsuites []

    fakesuite = makeFakeSuites numsuites []
  in do
    selectormap <- suiteSelectors suites filters
    _ <- performTestSuites quietReporter selectormap fakesuite
    return ()

main :: IO ()
main =
  do
    args <- getArgs
    runPerf False args
