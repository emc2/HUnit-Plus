module Tests.Test.HUnit.XML where

import Distribution.TestSuite
import Text.XML.Expat.Tree

runReporterTest :: IO [[Node String String]] -> Node String String -> IO Result
runReporterTest test expected =
  do
    [[actual]] <- test
    if actual == expected
      then return Pass
      else return (Fail ("Expected " ++ show expected ++
                         " but got " ++ show actual))

runReporterTests :: [(IO [[Node String String]], Node String String)] ->
                    IO Progress
runReporterTests [] = return (Finished Pass)
runReporterTests ((test, expected) : rest) =
  do
    res <- runReporterTest test expected
    case res of
      Pass -> runReporterTests rest
      _ -> return (Finished res)

reporterTestCases :: [(IO [[Node String String]], Node String String)]
reporterTestCases = []

reporterTest =
  TestInstance { name = "Reporter", options = [],
                 setOption = (\_ _ -> Right reporterTest),
                 tags = [], run = runReporterTests reporterTestCases }

tests :: Test
tests = testGroup "XML" [ Test reporterTest ]
