{-# LANGUAGE OverloadedStrings #-}
-- | The words C names start with to say which library they're in.
--
-- Every C name starts with its library's word, cased as its kind is:
-- @lib_friend_add@, @Lib_Err_Friend_Add@, @LIB_ERR_FRIEND_ADD_OK@. A set
-- of headers can have more than one library, and the root object's comes
-- first.
module Apigen.Namespace
    ( namespaces
    , rootNamespace
    , splitNamespace
    , namespaceOf
    , stripNamespace
    , errorStem
    , shortNamespace
    , resourceNamespace
    , libraryHandle
    , secondaryHandles
    ) where

import qualified Apigen.Knowledge as Knowledge
import           Apigen.Semantic
import           Data.List        (find)
import           Data.Text        (Text)
import qualified Data.Text        as Text

-- | The libraries' words, lower case, the root's first.
namespaces :: SemanticModel -> [Text]
namespaces = Knowledge.libraryWords

-- | The root object's library word: @lib@ for @lib_new@.
rootNamespace :: SemanticModel -> Text
rootNamespace = Knowledge.rootLibrary

-- | A name's library word (lower case) and the rest, in the name's own case.
splitNamespace :: [Text] -> Text -> Maybe (Text, Text)
splitNamespace nss name = case Text.breakOn "_" name of
    (w, rest) | Text.toLower w `elem` nss, Just bare <- Text.stripPrefix "_" rest, not (Text.null bare)
        -> Just (Text.toLower w, bare)
    _ -> Nothing

namespaceOf :: [Text] -> Text -> Maybe Text
namespaceOf nss = fmap fst . splitNamespace nss

-- | A name without its library word, or the name if it has none.
stripNamespace :: [Text] -> Text -> Text
stripNamespace nss name = maybe name snd (splitNamespace nss name)

-- | An error enum's name without its library word and @Err@:
-- @Friend_Add@ for @Lib_Err_Friend_Add@.
errorStem :: [Text] -> Text -> Maybe Text
errorStem nss name = do
    (_, bare) <- splitNamespace nss name
    case Text.breakOn "_" bare of
        (w, rest) | Text.toLower w == "err" -> Text.stripPrefix "_" rest
        _                                   -> Nothing

-- | A library's word without the root's at its start: @av@ for @libav@
-- if the root's is @lib@.
shortNamespace :: [Text] -> Text -> Text
shortNamespace nss ns = case nss of
    root : _ | ns /= root, Just short <- Text.stripPrefix root ns, not (Text.null short) -> short
    _ -> ns

-- | The library a resource's functions are in, by their prefix.
resourceNamespace :: [Text] -> SResource -> Maybe Text
resourceNamespace nss res = case Text.toLower (fst (Text.breakOn "_" (cPrefix res))) of
    w | w `elem` nss -> Just w
    _                -> Nothing

-- | A library's handle: the resource its functions start with just its
-- word, like @lib_iterate@.
libraryHandle :: SemanticModel -> Text -> Maybe SResource
libraryHandle model ns = find (\r -> cPrefix r == ns <> "_") (resources model)

-- | The handles of the libraries other than the root's.
secondaryHandles :: SemanticModel -> [SResource]
secondaryHandles = Knowledge.libraryHandles
