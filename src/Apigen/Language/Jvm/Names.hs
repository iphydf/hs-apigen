{-# LANGUAGE OverloadedStrings #-}
-- | Java and Kotlin spellings of C's snake case names.
module Apigen.Language.Jvm.Names
    ( pascalCase
    , camelCase
    , capitalise
    ) where

import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

-- | @friend_add@ is @FriendAdd@.
pascalCase :: Text -> Text
pascalCase = Text.concat . map capitalise . Text.split (== '_')

-- | @friend_add@ is @friendAdd@.
camelCase :: Text -> Text
camelCase t = case Text.split (== '_') t of
    []         -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capitalise rest)

-- | @friend@ is @Friend@.
capitalise :: Text -> Text
capitalise s = case Text.uncons (Text.toLower s) of
    Nothing     -> ""
    Just (c, t) -> Text.cons (Char.toUpper c) t
