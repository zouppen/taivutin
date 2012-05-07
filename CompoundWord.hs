module CompoundWord where

import Data.Char (toLower)
import Data.Either
import Data.List
import Data.Maybe
import Data.Map (fromListWith,toList)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Exts (sortWith)
import Common

readExampleCompoundWords = do
  s <- readFile "examples/streets_in_jyvaskyla.txt"
  return $ lines s  

processWords :: Declinations -> [String] -> ([String],Declinations)
processWords ds words = partitionEithers $ map (findDeclinations ds) words

-- |Returns all possible declinations of a given word.
findDeclinations :: Declinations -> String -> Either String Declination
findDeclinations ds word = if ans == S.empty
                           then Left word        -- Unable to declinate
                           else Right (word,ans) -- All possible declinations
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

-- |Helper for getting the matching word ending in statistical analysis.
countMatch word ending = do 
  checkEnding word ending
  return ending

-- |Returns match count for each ending, sorted descending.
countMatches :: Declinations -> [String] -> [(String,Int)]
countMatches ds words = sortWith (negate.snd) $ toList $ fromListWith (+) counts
  where
    endings = map fst ds
    counts = map countSingle words    
    countSingle word = case catMaybes $ map (countMatch word) endings of
      [a] -> (a,1)
      [] -> ("UNKNOWN",1)
      xs -> error $ "Multiple match: " ++ show xs
