{-# LANGUAGE OverloadedStrings #-}
-- | How C names become Haskell names.
module Apigen.Language.Haskell.Names
    ( typeName
    , valueName
    , varName
    , lowerFirst
    ) where

import           Data.Char (toLower, toUpper)
import           Data.Text (Text)
import qualified Data.Text as Text

-- | @User_Status@ is @UserStatus@, @tox_friend_add@ is @ToxFriendAdd@.
typeName :: Text -> Text
typeName = Text.concat . map capitalise . words'

-- | @tox_friend_add@ is @toxFriendAdd@: functions keep their C name's words.
valueName :: Text -> Text
valueName = lowerFirst . typeName

-- | A variable for the C parameter @name@, primed where it would be a keyword
-- or shadow a Prelude name.
varName :: Text -> Text
varName name
    | v `elem` reserved = v <> "'"
    | otherwise         = v
  where
    v = valueName name

lowerFirst :: Text -> Text
lowerFirst t = case Text.uncons t of
    Just (c, rest) -> Text.cons (toLower c) rest
    Nothing        -> t

words' :: Text -> [Text]
words' = filter (not . Text.null) . Text.splitOn "_"

capitalise :: Text -> Text
capitalise w = case Text.uncons (Text.toLower w) of
    Just (c, rest) -> Text.cons (toUpper c) rest
    Nothing        -> w

reserved :: [Text]
reserved =
    [ "case", "class", "data", "default", "deriving", "do", "else", "if"
    , "import", "in", "infix", "instance", "let", "module", "newtype", "of"
    , "then", "type", "where"
    -- Prelude names the bindings would shadow.
    , "error", "id", "length", "lines", "max", "min", "words"
    ]
