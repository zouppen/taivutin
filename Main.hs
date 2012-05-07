module Main where

import DeclinationReader
import CompoundWord
import System.Environment

-- |Main function which takes the word file as an argument and outputs
-- all declinations.
main = do
  -- Not using cmdargs, yet.
  [file] <- getArgs
  words <- readFile file
  declinations <- readExampleDeclinations
  putStrLn $ unlines $ processWords declinations $ lines words
