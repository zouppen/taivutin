module Main where

import Control.Monad (liftM)
import Data.Set (toList)
import GHC.Exts (sortWith)
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
  putStrLn "# unknown words (sorted by suffix)"
  putStrLn $ unlines $ sortWith reverse broken
  putStrLn "# ending,occurences (sorted by occurence count, descending)"
  putStrLn $ unlines $ map endingStr $ sortWith (negate.snd) $ countMatches declinations words

csvM_ act (word,ds) = mapM_ csvAct $ toList ds
  where csvAct x = act $ word ++ "," ++ x

endingStr (ending,count) = ending ++ "," ++ show count
