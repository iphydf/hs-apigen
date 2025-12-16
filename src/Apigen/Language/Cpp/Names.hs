{-# LANGUAGE OverloadedStrings #-}
-- | C++ names for the C API's.
module Apigen.Language.Cpp.Names
    ( pascal
    , words'
    , unwords'
    , enumTypeName
    , enumeratorName
    , idTypeName
    , unitTypeName
    , arrayTypeName
    , functionWords
    ) where

import           Apigen.Namespace (errorStem, shortNamespace, splitNamespace)
import           Data.Char        (toLower, toUpper)
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
-- Another library's enums start with its short word: toxav's
-- @Toxav_Call_Control@ is @AvCallControl@ and @Toxav_Err_New@ @AvNewError@.
enumTypeName :: [Text] -> Text -> Text
enumTypeName nss name = case splitNamespace nss name of
    Nothing -> pascal (words' name)
    Just (ns, bare) -> library ns <> case errorStem nss name of
        Just err -> pascal (words' err) <> "Error"
        Nothing  -> pascal (words' bare)
  where
    library ns = case nss of
        root : _ | ns /= root -> pascal [shortNamespace nss ns]
        _                     -> ""

-- | An enumerator's name, without its enum's prefix: @NotFound@.
enumeratorName :: Text -> Text
enumeratorName = pascal . words'

-- | @Friend_Number@ is @FriendNumber@.
idTypeName :: Text -> Text
idTypeName = pascal . words'

-- | @Av_Bit_Rate@ is @AvBitRate@.
unitTypeName :: Text -> Text
unitTypeName = pascal . words'

-- | @Public_Key@ is @PublicKey@, and @Group_Chat_Id@ @GroupChatId@.
arrayTypeName :: Text -> Text
arrayTypeName = pascal . words'

-- | A function's words, without its library's: @tox_friend_get_name@ is
-- @friend get name@.
functionWords :: [Text] -> Text -> [Text]
functionWords nss cName = case words' cName of
    p : rest | p `elem` nss -> rest
    other                   -> other
