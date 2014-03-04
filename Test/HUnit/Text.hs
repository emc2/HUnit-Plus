{-# OPTIONS_GHC -Wall -Werror #-}

-- | Text-based test controller for running HUnit tests and reporting
--   results as text, usually to a terminal.

module Test.HUnit.Text(
       PutText(..),
       putTextToHandle,
       putTextToShowS,
       runTestText,
       showPath,
       showCounts,
       runTestTT
       ) where

import Test.HUnit.Base

import Control.Monad (when)
import System.IO (Handle, stderr, hPutStr, hPutStrLn)
import Text.Printf(printf)


-- | As the general text-based test controller ('runTestText') executes a
--   test, it reports each test case start, error, and failure by
--   constructing a string and passing it to the function embodied in a
--   'PutText'.  A report string is known as a \"line\", although it includes
--   no line terminator; the function in a 'PutText' is responsible for
--   terminating lines appropriately.  Besides the line, the function
--   receives a flag indicating the intended \"persistence\" of the line:
--   'True' indicates that the line should be part of the final overall
--   report; 'False' indicates that the line merely indicates progress of
--   the test execution.  Each progress line shows the current values of
--   the cumulative test execution counts; a final, persistent line shows
--   the final count values.
-- 
--   The 'PutText' function is also passed, and returns, an arbitrary state
--   value (called 'st' here).  The initial state value is given in the
--   'PutText'; the final value is returned by 'runTestText'.

data PutText st = PutText (String -> st -> IO st) st

-- | Two reporting schemes are defined here.  @putTextToHandle@ writes
-- report lines to a given handle.  'putTextToString' accumulates
-- persistent lines for return as a whole by 'runTestText'.
-- 
-- @putTextToHandle@ writes persistent lines to the given handle,
-- following each by a newline character.  In addition, if the given flag
-- is @True@, it writes progress lines to the handle as well.  A progress
-- line is written with no line termination, so that it can be
-- overwritten by the next report line.  As overwriting involves writing
-- carriage return and blank characters, its proper effect is usually
-- only obtained on terminal devices.

putTextToHandle :: Handle -> PutText ()
putTextToHandle handle = PutText (\line () -> hPutStr handle line) ()

-- | Accumulates persistent lines for return by 'runTestText'.  The
-- accumulated lines are represented by a @'ShowS' ('String' ->
-- 'String')@ function whose first argument is the string to be
-- appended to the accumulated report lines.
putTextToShowS :: PutText ShowS
putTextToShowS =
  PutText (\line func -> return (\rest -> func (line ++ rest))) id


-- | Executes a test, processing each report line according to the given 
--   reporting scheme.  The reporting scheme's state is threaded through calls 
--   to the reporting scheme's function and finally returned, along with final 
--   count values.
runTestText :: PutText us
            -- ^ A function which accumulates output
            -> Bool
            -- ^ Whether or not to run the test in verbose mode.
            -> Test
            -- ^ The test to run
            -> IO (Counts, us)
runTestText (PutText put us0) verbose t =
  let
    reportProblem p0 p1 msg ss us =
      let
        kind = if null path' then p0 else p1
        path' = showPath (path ss)
        line = "### " ++ kind ++ path' ++ ": " ++ msg ++ "\n"
      in
        put line us

    reportOutput p0 p1 msg ss us =
      let
        kind = if null path' then p0 else p1
        path' = showPath (path ss)
        line = "### " ++ kind ++ path' ++ ": " ++ msg ++ "\n"
      in
        if verbose then put line us
        else return us

    reportStartCase _ ss us =
      let
        path' = showPath (path ss)
        line = if null path' then "Test case starting\n"
               else "Test case " ++ path' ++ " starting\n"
      in
        if verbose then put line us
        else return us

    reportEndCase time ss us =
      let
        path' = showPath (path ss)
        timestr = printf "%.6f" time
        line = if null path' then "Test completed in " ++ timestr ++ " sec\n"
               else "Test " ++ path' ++ " completed in " ++ timestr ++ " sec\n"
      in
        if verbose then put line us
        else return us

    reporter = defaultReporter {
        reporterStartCase = reportStartCase,
        reporterEndCase = reportEndCase,
        reporterSystemOut = reportOutput "STDOUT " "STDOUT from ",
        reporterSystemErr = reportOutput "STDERR" "STDERR from ",
        reporterError = reportProblem "Error " "Error in ",
        reporterFailure = reportProblem "Failure" "Failure in "
      }
  in do
    (counts', us1) <- performTest reporter us0 t
    us2 <- put (showCounts counts' ++ "\n") us1
    return (counts', us2)

-- | Converts test execution counts to a string.

showCounts :: Counts -> String
showCounts Counts { cases = cases', tried = tried',
                    errors = errors', failures = failures' } =
  "Cases: " ++ show cases' ++ "  Tried: " ++ show tried' ++
  "  Errors: " ++ show errors' ++ "  Failures: " ++ show failures'


-- | Converts a test case path to a string, separating adjacent elements by 
--   the colon (\':\'). An element of the path is quoted (as with 'show') when
--   there is potential ambiguity.

showPath :: Path -> String
showPath [] = ""
showPath nodes =
  let
    f b a = a ++ ":" ++ b
    showNode (ListItem n) = show n
    showNode (Label label) = safe label (show label)
    safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s
  in
    foldl1 f (map showNode nodes)

-- | Provides the \"standard\" text-based test controller. Reporting is made to
--   standard error, and progress reports are included. For possible 
--   programmatic use, the final counts are returned.
-- 
--   The \"TT\" in the name suggests \"Text-based reporting to the Terminal\".

runTestTT :: Test -> IO Counts
runTestTT t =
  let
    put line pers (-1) = do when pers (hPutStrLn stderr line); return (-1)
    put line True  cnt = do hPutStrLn stderr (erase cnt ++ line); return 0
    put line False _   = do hPutStr stderr ('\r' : line); return (length line)

    -- The "erasing" strategy with a single '\r' relies on the fact that the
    -- lengths of successive summary lines are monotonically nondecreasing.
    erase cnt = if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"

    reportProblem p0 p1 msg ss us =
      let
        line = "### " ++ kind ++ path' ++ '\n' : msg
        path' = showPath (path ss)
        kind = if null path' then p0 else p1
      in
        put line True us

    reporter = defaultReporter {
        reporterEndCase = (\_ ss us -> put (showCounts (counts ss)) False us),
        reporterError = reportProblem "Error:" "Error in:   ",
        reporterFailure = reportProblem "Failure:" "Failure in: "
      }

  in do
    (counts', us1) <- performTest reporter 0 t
    0 <- put (showCounts counts') True us1
    return counts'
