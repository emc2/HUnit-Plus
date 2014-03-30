module Main where

import Control.Monad
import Data.ByteString.Lazy hiding (intercalate, reverse, putStr)
import Data.List
import Data.Word
import Distribution.TestSuite
import Prelude hiding (writeFile)
import System.Exit
import Tests
import Text.XML.Expat.Format
import Text.XML.Expat.Tree

-- We can't use HUnit-Plus to test HUnit-Plus, so whip out a quick and
-- simple test execution program.

data Counts =
  Counts {
    cTried :: !Word,
    cErrors :: !Word,
    cFailures :: !Word
  }

instance Show Counts where
  show Counts { cTried = tried, cErrors = errors, cFailures = failures } =
    "Tried: " ++ show tried ++ "  Errors: " ++ show errors ++
    "  Failures: " ++ show failures

zeroCounts :: Counts
zeroCounts = Counts { cTried = 0, cErrors = 0, cFailures = 0 }

finishTest :: IO Progress -> IO Result
finishTest runTest =
  do
    res <- runTest
    case res of
      Progress _ rest -> finishTest rest
      Finished res -> return res

runTest :: [String] -> (Counts, [Node String String]) -> Test ->
           IO (Counts, [Node String String])
runTest path (counts, trees) (Test TestInstance { run = runTest,
                                                  name = tname }) =
  let
    pathstr = intercalate "." (reverse path)
    Counts { cTried = tried, cErrors = errors, cFailures = failed } = counts
  in do
    res <- finishTest runTest
    case res of
      Pass -> return
        (counts { cTried = tried + 1 },
         Element { eName = "testcase", eChildren = [],
                   eAttributes = [("name", tname),
                                  ("classname", pathstr)] } :
         trees)
      Error str -> return
        (counts { cTried = tried + 1, cErrors = errors + 1 },
         Element { eName = "testcase", eAttributes = [("name", tname),
                                                      ("classname", pathstr)],
                   eChildren = [Element { eName = "error",
                                          eChildren = [Text str],
                                          eAttributes = [] }] } :
         trees)
      Fail str -> return
        (counts { cTried = tried + 1, cFailures = failed + 1 },
         Element { eName = "testcase", eAttributes = [("name", tname),
                                                      ("classname", pathstr)],
                   eChildren = [Element { eName = "failure",
                                          eChildren = [Text str],
                                          eAttributes = [] }] } :
         trees)

runTest path (counts, trees) Group { groupName = gname, groupTests = gtests } =
  let
    newpath = gname : path
  in
    foldM (runTest newpath) (counts, trees) gtests
runTest _ _ (ExtraOptions _ _) = error "ExtraOptions not supported"

wrapTrees :: Counts -> [Node String String] -> Node String String
wrapTrees Counts { cTried = tried, cErrors = errors, cFailures = failed } trees =
  Element { eName = "testsuites", eAttributes = [],
            eChildren = [Element { eName = "testsuite",
                                   eAttributes = [("name", "Test"),
                                                  ("package", "HUnit-Plus"),
                                                  ("tests", show tried),
                                                  ("failures", show failed),
                                                  ("errors", show errors),
                                                  ("skipped", "0")],
                                   eChildren = trees }] }

main :: IO ()
main =
  do
    tests' <- tests
    (counts, trees) <- foldM (runTest []) (zeroCounts, []) tests'
    writeFile "report.xml" (format (indent 2 (wrapTrees counts trees)))
    putStr (show counts ++ "\n")
    case counts of
      Counts { cErrors = 0, cFailures = 0 } -> exitSuccess
      _ -> exitFailure
