module Main where

import Control.Monad (liftM)
import Data.Set (toList)
import System.Environment

import CompoundWord
import DeclinationReader

-- |Main function which takes the word file as an argument and outputs
-- all declinations.
main = do
  -- Not using cmdargs, yet.
  [file] <- getArgs
  words <- liftM lines $ readFile file
  declinations <- readExampleDeclinations
  let (broken,declinated) = processWords declinations words
  putStrLn "# word,declination"
  mapM_ (csvM_ putStrLn) declinated
  putStrLn ""
  putStrLn "# unknown words"
  putStrLn $ unlines $ broken
  putStrLn "# ending,occurences"
  putStrLn $ unlines $ map endingStr $ countMatches declinations words

csvM_ act (word,ds) = mapM_ csvAct $ toList ds
  where csvAct x = act $ word ++ "," ++ x

endingStr (ending,count) = ending ++ "," ++ show count
