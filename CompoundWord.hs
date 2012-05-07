module CompoundWord where

import Data.Char (toLower)
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Common

readExampleCompoundWords = do
  s <- readFile "examples/streets_in_jyvaskyla.txt"
  return $ lines s  

processWords :: Declinations -> [String] -> [String]
processWords ds words = S.toList $ S.unions $ map (findDeclinations ds) words

-- |Returns all possible declinations of a given word.
findDeclinations :: Declinations -> String -> Set String
findDeclinations ds word = if ans == S.empty
                           then error $ "Unable to declinate: " ++ word
                           else ans
  where ans = S.unions $ map (allEndings word) ds

allEndings :: String -> Declination -> Set String
allEndings word (ending,ds) = case checkEnding word ending of
  Just base -> S.map (base++) ds
  Nothing -> S.empty

-- |Returns the first part of a compound word if there's a match with the given ending.
checkEnding :: String -> String -> Maybe String
checkEnding word ending = case isSuffixOf (n ending) (n word) of
  True -> Just $ take (length word - length ending) word
  False -> Nothing
  where n = map toLower
