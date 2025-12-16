{-# LANGUAGE OverloadedStrings #-}
-- | Lookups in the semantic model that the Haskell backend's modules share.
module Apigen.Language.Haskell.Model
    ( mappingOf
    , allMethods
    , findEnum
    , isErrorEnum
    , enumValued
    , idNewtype
    , idUnderlying
    , unitNewtype
    , arrayType
    , handleName
    , handlePtr
    , callbackType
    , callbackTypeOf
    , rawModuleFor
    , rawModuleName
    , safeModule
    , rawModule
    , handleResources
    , foreignPtrHandles
    , cErrorEnumName
    , sizeCompanions
    , defineValue
    ) where

import qualified Apigen.Knowledge              as K
import           Apigen.Language.Haskell.Names (typeName)
import           Apigen.Model                  (allMethods, arrayTypeOf,
                                                defineValue, findEnum, idModel,
                                                mappingOf, sizeCompanions)
import           Apigen.Namespace              (libraryHandle, namespaceOf,
                                                namespaces, resourceNamespace)
import           Apigen.Semantic
import           Apigen.SizeExpr               (SizeExpr)
import           Data.List                     (find)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           System.FilePath               (takeFileName)

isErrorEnum :: SEnumModel -> Bool
isErrorEnum e = enumKind e == ErrorEnum

-- | An enum C passes as a plain integer that clients may extend or combine:
-- constants, not a closed sum type.
-- | The open enum a C function's or callback's integer parameter holds:
-- @tox_file_send@'s @kind@ is a @FileKind@.
enumValued :: SemanticModel -> Text -> Text -> Maybe Text
enumValued model fn param =
    typeName . enumSemanticName <$> (K.enumParam model (fn, param) >>= findEnum model)

idNewtype :: SemanticModel -> Text -> Maybe Text
idNewtype model name = typeName . idName <$> idModel model name

-- | A unit's newtype: @AvBitRate@.
unitNewtype :: Text -> Text
unitNewtype = typeName

idUnderlying :: SemanticModel -> Text -> SType
idUnderlying model name = maybe (SUInt 32) idType (idModel model name)

arrayType :: SemanticModel -> SizeExpr -> Maybe Text
arrayType model size = typeName <$> arrayTypeOf model size

-- | The Haskell name of a handle resource: @Tox@, @ToxAv@, @PassKey@.
handleName :: SResource -> Text
handleName = typeName . resourceName

handlePtr :: SemanticModel -> Text -> Text
handlePtr model name = case find ((== name) . resourceName) (resources model) of
    Just r  -> handleName r <> "Ptr"
    Nothing -> typeName name <> "Ptr"

callbackType :: Text -> Text
callbackType = typeName

callbackTypeOf :: SemanticModel -> Text -> Text
callbackTypeOf model name =
    case find (\cb -> cbName cb == name || cbCName cb == name) (callbacks model) of
        Just cb -> callbackType (cbName cb)
        Nothing -> callbackType name

-- | The module importing a resource's function: its header's, or for
-- another library's function, that library's.
rawModuleFor :: SemanticModel -> SResource -> Text -> Text
rawModuleFor model res fn = rawModuleName model $ case namespaceOf nss fn of
    Just ns | Just ns /= resourceNamespace nss res, Just lib <- libraryHandle model ns -> resourceFile lib
    _ -> resourceFile res
  where
    nss = namespaces model

rawModuleName :: SemanticModel -> Text -> Text
rawModuleName model file = rawModule model $ case lookup base (headerModuleNames (targets model)) of
    Just m  -> m
    -- The root library's own header.
    Nothing | Text.null word -> maybe "" handleName (find isRoot (resources model))
            | otherwise      -> typeName word
  where
    base = Text.pack (takeFileName (Text.unpack file))
    root = K.rootLibrary model
    word = stripPrefixes (Text.dropEnd 2 base)
    stripPrefixes t = maybe (maybe t id (Text.stripPrefix root t)) id (Text.stripPrefix (root <> "_") t)

-- | A safe module, @Network.Tox.C.Friend@, or with no word the top one.
safeModule :: SemanticModel -> Text -> Text
safeModule model w = Text.intercalate "." (filter (not . Text.null) [haskellModules (targets model), w])

-- | A raw module: @FFI.Tox.Friend@.
rawModule :: SemanticModel -> Text -> Text
rawModule model w = haskellRawModules (targets model) <> "." <> w

-- | Handles the safe API has a type for: the root, the other libraries'
-- and the standalone ones. The rest are internal to a module.
handleResources :: SemanticModel -> [Text]
handleResources model =
    [ resourceName r | r <- resources model
    , K.resourceKind model r `elem` [RootHandle, LibraryHandle, StandaloneHandle] ]

-- | Handles the safe API frees with a finalizer, so the raw API imports
-- their destructor as a function pointer: the standalone ones.
foreignPtrHandles :: SemanticModel -> [Text]
foreignPtrHandles model = map resourceName (K.handlesOfKind model StandaloneHandle)

-- | C enum of an error type, for messages about unknown values.
cErrorEnumName :: SemanticModel -> Text -> Text
cErrorEnumName model name = maybe name enumName (findEnum model name)
