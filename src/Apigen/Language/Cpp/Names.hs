{-# LANGUAGE OverloadedStrings #-}
-- | C++ names for the C API's.
module Apigen.Language.Cpp.Names
    ( pascal
    , words'
    , unwords'
    , enumTypeName
    , enumeratorName
    , idTypeName
    , arrayTypeName
    , commandWords
    , objectOf
    ) where

import           Apigen.Namespace (errorStem, shortNamespace, splitNamespace)
import           Data.Char        (toLower, toUpper)
import           Data.List        (isPrefixOf)
import           Data.Text        (Text)
import qualified Data.Text        as Text

words' :: Text -> [Text]
words' = filter (not . Text.null) . Text.splitOn "_" . Text.toLower

unwords' :: [Text] -> Text
unwords' = Text.intercalate "_"

pascal :: [Text] -> Text
pascal = Text.concat . map capitalise
  where
    capitalise w = case Text.uncons w of
        Just (c, rest) -> Text.cons (toUpper c) (Text.map toLower rest)
        Nothing        -> w

-- | @Tox_User_Status@ is @UserStatus@, @Tox_Err_Set_Info@ is @SetInfoError@.
-- Another library's enum clashing with one of the root's gets its short
-- word: toxav's @Toxav_Err_New@ is @AvNewError@.
enumTypeName :: [Text] -> [Text] -> Text -> Text
enumTypeName nss allNames name = case splitNamespace nss name of
    Nothing -> pascal (words' name)
    Just (ns, bare) -> clash ns bare $ case errorStem nss name of
        Just err -> pascal (words' err) <> "Error"
        Nothing  -> pascal (words' bare)
  where
    clash ns bare n = case nss of
        root : _ | ns /= root, pascal [root] <> "_" <> bare `elem` allNames
            -> pascal [shortNamespace nss ns] <> n
        _ -> n

-- | An enumerator's name, without its enum's prefix: @NotFound@.
enumeratorName :: Text -> Text
enumeratorName m = case m of
    -- Its abbreviation would read as a word.
    "SENDQ" -> "SendQueueFull"
    _       -> pascal (words' m)

-- | @Friend_Number@ is @FriendNumber@.
idTypeName :: Text -> Text
idTypeName = pascal . words'

-- | @Public_Key@ is @PublicKey@, and @Group_Chat_Id@ @GroupChatId@.
arrayTypeName :: Text -> Text
arrayTypeName = pascal . words'

-- | Things functions act on, longest first, as their C names start with them
-- after their library's word.
objects :: [[Text]]
objects =
    [ ["conference", "offline", "peer"]
    , ["conference", "peer"]
    , ["group", "peer"]
    , ["group", "self"]
    , ["group", "invite"]
    , ["conference"]
    , ["group"]
    , ["friend"]
    , ["file"]
    , ["self"]
    , ["audio"]
    , ["video"]
    ]

verbs :: [Text]
verbs =
    [ "accept", "add", "answer", "control", "delete", "disconnect"
    , "get", "invite", "join", "kick", "leave", "new", "reconnect", "seek"
    , "send", "set"
    ]

-- | A function's words, verb first: @tox_friend_get_name@ is @get friend name@.
-- A repeated word goes (@get file id@). Order stays if the verb isn't next
-- to the object: @tox_friend_by_public_key@.
commandWords :: [Text] -> Text -> [Text]
commandWords nss cName = dedupe $ case [ v : o ++ rest | (o, v : rest) <- objectsAndRests nss cName, v `elem` verbs ] of
    ws : _ -> ws
    []     -> functionWords nss cName
  where
    dedupe (a : b : rest) | a == b = dedupe (b : rest)
    dedupe (a : rest)     = a : dedupe rest
    dedupe []             = []

functionWords :: [Text] -> Text -> [Text]
functionWords nss cName = case words' cName of
    p : rest | p `elem` nss -> rest
    other                   -> other

objectsAndRests :: [Text] -> Text -> [([Text], [Text])]
objectsAndRests nss cName = [ (o, drop (length o) ws) | o <- objects, o `isPrefixOf` ws ]
  where
    ws = functionWords nss cName

-- | What a function acts on, by its name: @["group", "peer"]@.
objectOf :: [Text] -> Text -> Maybe [Text]
objectOf nss cName = case objectsAndRests nss cName of
    (o, _) : _ -> Just o
    []         -> Nothing
