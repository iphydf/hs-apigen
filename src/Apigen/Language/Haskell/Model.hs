{-# LANGUAGE OverloadedStrings #-}
-- | Lookups in the semantic model that the Haskell backend's modules share.
module Apigen.Language.Haskell.Model
    ( mappingOf
    , allMethods
    , findEnum
    , isErrorEnum
    , isOpenEnum
    , idNewtype
    , idUnderlying
    , arrayType
    , handleName
    , handlePtr
    , callbackType
    , callbackTypeOf
    , rawModuleFor
    , rawModuleName
    , handleResources
    , foreignPtrHandles
    , cErrorEnumName
    , sizeCompanions
    , defineValue
    ) where

import qualified Apigen.Knowledge              as K
import           Apigen.Language.Haskell.Names (typeName)
import           Apigen.Model                  (allMethods, defineValue,
                                                findEnum, idModel, mappingOf,
                                                sizeCompanions)
import           Apigen.Namespace              (libraryHandle, namespaceOf,
                                                namespaces, resourceNamespace)
import           Apigen.Semantic
import           Data.List                     (find)
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           System.FilePath               (takeFileName)

isErrorEnum :: SEnumModel -> Bool
isErrorEnum e = enumKind e == ErrorEnum

-- | An enum C passes as a plain integer that clients may extend or combine:
-- constants, not a closed sum type.
isOpenEnum :: SemanticModel -> SEnumModel -> Bool
isOpenEnum _ e = enumKind e `elem` [OpenEnum, FlagsEnum]

idNewtype :: SemanticModel -> Text -> Maybe Text
idNewtype model name = typeName . idName <$> idModel model name

idUnderlying :: SemanticModel -> Text -> SType
idUnderlying model name = maybe (SUInt 32) idType (idModel model name)

arrayType :: SemanticModel -> Text -> Maybe Text
arrayType model size = typeName <$> lookup size (arrayTypes model)

-- | The Haskell name of a handle resource: @Tox@, @ToxAV@, @PassKey@.
handleName :: SResource -> Text
handleName r
    | "_" `Text.isInfixOf` c = typeName (stripOwn c)
    | otherwise              = c
  where
    c = cName r
    -- Its functions' library word.
    own = fst (Text.breakOn "_" (cPrefix r))
    stripOwn t = case Text.breakOn "_" t of
        (w, rest) | Text.toLower w == own, Just bare <- Text.stripPrefix "_" rest -> bare
        _ -> t

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
rawModuleName model file = "FFI.Tox." <> case lookup base headerModules of
    Just m  -> m
    Nothing -> typeName (stripPrefixes (Text.dropEnd 2 base))
  where
    base = Text.pack (takeFileName (Text.unpack file))
    root = K.rootLibrary model
    stripPrefixes t = maybe (maybe t id (Text.stripPrefix root t)) id (Text.stripPrefix (root <> "_") t)
    headerModules =
        [ ("tox.h", "Tox")
        , ("toxencryptsave.h", "EncryptSave")
        ]

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
