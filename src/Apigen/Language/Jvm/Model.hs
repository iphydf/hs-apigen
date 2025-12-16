{-# LANGUAGE OverloadedStrings #-}
-- | What the JVM bindings take from the knowledge: which resources each JNI
-- class binds, and names built from the libraries' words.
module Apigen.Language.Jvm.Model
    ( coreResourceNames
    , avResourceNames
    , cryptoResourceNames
    , exceptionClassFor
    , libraryMethodName
    , classPrefix
    , inOtherLibrary
    , inStandalone
    , isOtherLibraryName
    , standalonePrefix
    , selfPrefix
    , isSettingsName
    , rootIterate
    ) where

import qualified Apigen.Knowledge as K
import           Apigen.Model     (rootMembers)
import           Apigen.Namespace (errorStem, splitNamespace)
import           Apigen.Semantic
import           Data.List        (find)
import           Data.Text        (Text)
import qualified Data.Text        as Text

-- | The root and what its ids name. Not the settings: they cross JNI as a
-- message the root's constructor takes.
coreResourceNames :: SemanticModel -> [Text]
coreResourceNames = map resourceName . rootMembers

-- | The other libraries' handles.
avResourceNames :: SemanticModel -> [Text]
avResourceNames = map resourceName . K.libraryHandles

-- | The standalone handles.
cryptoResourceNames :: SemanticModel -> [Text]
cryptoResourceNames model = map resourceName (K.handlesOfKind model StandaloneHandle)

-- | An error enum's exception: @Tox_Err_Friend_Add@ is
-- @ToxFriendAddException@, and @Toxav_Err_Call@ @ToxavCallException@.
exceptionClassFor :: SemanticModel -> (Text -> Text) -> Text -> Text
exceptionClassFor model pascal err = case (splitNamespace nss err, errorStem nss err) of
    (Just (ns, _), Just stem) -> capitalise ns <> pascal stem <> "Exception"
    _                         -> err
  where
    nss = K.libraryWords model
    capitalise w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

-- | A function's JNI name: its library's word, then the rest in Pascal
-- case, as @toxFriendAdd@ and @toxavCall@.
libraryMethodName :: SemanticModel -> (Text -> Text) -> (Text -> Text) -> Text -> Text
libraryMethodName model pascalRest otherwise' n = case splitNamespace (K.libraryWords model) n of
    Just (ns, rest) -> ns <> pascalRest rest
    Nothing         -> otherwise' n

-- | Classes start with the root's name: @ToxFriendNumber@.
classPrefix :: SemanticModel -> Text
classPrefix model = maybe "" resourceName (find isRoot (resources model))

-- | Whether a header is another library's, like toxav.h.
inOtherLibrary :: SemanticModel -> Text -> Bool
inOtherLibrary model file = file `elem` map resourceFile (K.libraryHandles model)

-- | Whether a header is a standalone handle's, like toxencryptsave.h.
inStandalone :: SemanticModel -> Text -> Bool
inStandalone model file = file `elem` map resourceFile (K.handlesOfKind model StandaloneHandle)

-- | Whether a name is in another library's namespace, like @Toxav_Err_Call@.
isOtherLibraryName :: SemanticModel -> Text -> Bool
isOtherLibraryName model n = case splitNamespace (K.libraryWords model) n of
    Just (ns, _) -> ns /= K.rootLibrary model
    Nothing      -> False

-- | What the standalone handles' functions and constants start with, before
-- their own word: @tox_pass_@ for @tox_pass_key_@, @TOX_PASS_@ in upper case.
standalonePrefix :: SemanticModel -> Text
standalonePrefix model = case K.handlesOfKind model StandaloneHandle of
    r : _ -> Text.intercalate "_" (init' (Text.splitOn "_" (Text.dropWhileEnd (== '_') (cPrefix r)))) <> "_"
    []    -> ""
  where
    init' xs = take (length xs - 1) xs

-- | The root's own functions start with this: @tox_self_@.
selfPrefix :: SemanticModel -> Text
selfPrefix model = K.rootLibrary model <> "_self_"

-- | Whether a handle name is the settings'.
isSettingsName :: SemanticModel -> Text -> Bool
isSettingsName model n = n `elem` map resourceName (K.handlesOfKind model SettingsHandle)

-- | The root's iterate function: @tox_iterate@.
rootIterate :: SemanticModel -> Text
rootIterate model = case [ methodName m | r <- resources model, isRoot r, m <- methods r, methodRole m == IterateRole ] of
    f : _ -> f
    []    -> ""
