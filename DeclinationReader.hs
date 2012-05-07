module DeclinationReader where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.Pandoc
import Common

data Types = BaseForm String
           | Declinations [String]
           deriving (Show)

readExampleDeclinations :: IO Declinations
readExampleDeclinations = do
  s <- readFile "examples/vartalot.md"
  return $ extractDeclinations readMarkdown s

-- |Reads declinations from given data and with given reader
extractDeclinations :: (ParserState -> String -> Pandoc) -> String -> Declinations
extractDeclinations parser = groupTypes . catMaybes . map extractor . takeBlocks . parsed
  where
    parsed = parser defaultParserState
    takeBlocks (Pandoc _ bs) = bs

extractor :: Block -> Maybe Types
extractor (Header 2 xs) = Just $ BaseForm $ inlineToText xs
extractor (BulletList xs) = Just $ Declinations $ map blockToText xs
extractor _ = Nothing

-- |Groups heading-list pairs. Adds the base form of a word to the
-- list if not there already.
groupTypes :: [Types] -> Declinations
groupTypes ((BaseForm base):(Declinations ds):xs) = (base,S.fromList (base:ds)):groupTypes xs
groupTypes ((BaseForm base):xs) = (base,S.singleton base):groupTypes xs -- No definitions given yet.
groupTypes [] = []
groupTypes _ = error "Invalid input, contains wrong ordering of lists and headers"

blockToText :: [Block] -> String
blockToText bs = writePlain defaultWriterOptions $ Pandoc (Meta [] [] []) bs

inlineToText :: [Inline] -> String
inlineToText xs = blockToText [Plain xs]