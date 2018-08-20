--  File     : proj1test.hs
--  RCS      : $Id$
--  Author   : Peter Schachte
--  Origin   : Sat Aug 20 22:06:04 2011
--  Purpose  : Test program for proj1 project submissions

module Main where

import Data.List
import System.Environment
import System.Exit
import Proj1


usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage:  " ++ prog ++ " person person"
    putStrLn "Play a guessing game, where the computer tries to guess the"
    putStrLn "lineup of two different people you specify.  Each person is"
    putStrLn "a three letter abbreviation of a person's height, hair colour"
    putStrLn "and sex, in that order.  Height is one of S or T; hair colour"
    putStrLn "is one of B, R, or D; and sex is one of M or F."


-- | Main program.  Gets the target from the command line (as three
--   separate command line arguments, each a note letter (upper case)
--   followed by an octave number.  Runs the user's initialGuess and
--   nextGuess functions repeatedly until they guess correctly.
--   Counts guesses, and prints a bit of running commentary as it goes.
main :: IO ()
main = do
  args <- getArgs
  case mapM parsePerson args of
    Nothing -> do
      putStrLn "Error in person specification.\n"
      usage
      exitFailure
    Just target@[_,_] -> do
      if nub target /= target
        then do
            putStrLn "The specified target must not have repeats.\n"
            usage
            exitFailure
        else do
            let (guess,other) = initialGuess
            loop target guess other 1
    Just target -> do
      putStrLn "Invalid command line.\n"
      usage
      exitFailure


loop :: [Person] -> [Person] -> Proj1.GameState -> Int -> IO ()
loop target guess other guesses = do
    putStrLn $ "Your guess " ++ show guesses ++ ":  "
               ++ intercalate " " (map show guess)
    let answer = feedback target guess
    putStrLn $ "My answer:  " ++ show answer
    if answer == (length target,0,0,0)
      then do
          putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
      else do
          let (guess',other') = nextGuess (guess,other) answer
          loop target guess' other' (guesses+1)
