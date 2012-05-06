module Taivutin where

import Data.Maybe (catMaybes)
import Text.Pandoc

data Types = BaseForm String
           | Declinations [String]
           deriving (Show)

readExample = readFile "vartalot.md" >>= return . extractDeclinations readMarkdown 

-- |Reads declinations from given data and with given reader
extractDeclinations :: (ParserState -> String -> Pandoc) -> String -> [(String, [String])]
extractDeclinations parser = groupTypes . catMaybes . map extractor . takeBlocks . parsed
  where
    parsed = parser defaultParserState
    takeBlocks (Pandoc _ bs) = bs

extractor :: Block -> Maybe Types
extractor (Header 2 xs) = Just $ BaseForm $ inlineToText xs
extractor (BulletList xs) = Just $ Declinations $ map blockToText xs
extractor _ = Nothing

groupTypes :: [Types] -> [(String, [String])]
groupTypes ((BaseForm base):(Declinations ds):xs) = (base,ds):groupTypes xs
groupTypes [] = []
groupTypes _ = error "Invalid input, contains wrong ordering of lists and headers"

blockToText :: [Block] -> String
blockToText bs = writePlain defaultWriterOptions $ Pandoc (Meta [] [] []) bs

inlineToText :: [Inline] -> String
inlineToText xs = blockToText [Plain xs]