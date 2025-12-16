{-# LANGUAGE OverloadedStrings #-}
-- | Text rewrites the doc comment emitters share.
module Apigen.Language.DocText
    ( normalizeLines
    , rewriteIdentifiers
    , isIdentChar
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

-- | Trim each line's leading space (which Cimple keeps after @*@) and
-- the blank lines at either end.
normalizeLines :: Text -> Text
normalizeLines =
      Text.intercalate "\n"
    . dropWhileTrailing Text.null
    . dropWhile Text.null
    . map trimLeadingSpace
    . Text.splitOn "\n"
  where
    trimLeadingSpace = Text.dropWhile (== ' ')
    dropWhileTrailing p = reverse . dropWhile p . reverse

rewriteIdentifiers :: (Text -> Maybe Text) -> Text -> Text
rewriteIdentifiers f = go
  where
    go t = case Text.break isIdentStart t of
        (lead, rest) | Text.null rest -> lead
                     | otherwise ->
                         let (ident, after) = Text.span isIdentChar rest
                             repl = case f ident of
                                 Just r  -> r
                                 Nothing -> ident
                         in lead <> repl <> go after
    isIdentStart c = isIdentChar c && not (c >= '0' && c <= '9')

isIdentChar :: Char -> Bool
isIdentChar c =
    (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c == '_'
