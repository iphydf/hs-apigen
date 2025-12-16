{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Naming and type mapping shared by the Kotlin interfaces and the
-- @Tox*Impl.kt@ classes.
module Apigen.Language.Jvm.Kotlin.Common
    ( -- * Naming
      pascalCase
    , camelCase
    , kotlinMethodName
    , kotlinNameOf
    , kotlinPropertyName

      -- * Type rendering
    , renderType
    , renderParamType
    , renderMethodParamType

      -- * Path-id args
    , pathIdArgs
    , pathIdArgsFor

      -- * Method filtering
    , isSkippableMethod
    , isSkippableMethodFor
    ) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.Jvm.Conventions (hasNoJniShim,
                                                  interfaceAllowsStatics,
                                                  isHiddenFromPublicApi,
                                                  isLifecycleConstructor,
                                                  kotlinMethodNameOverride,
                                                  paramWrapperFor)
import           Apigen.Language.Jvm.Model       (classPrefix, selfPrefix)
import           Apigen.Model                    (isEnumNamer)
import           Apigen.Namespace                (splitNamespace)
import           Apigen.Semantic                 (SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SResource (..),
                                                  SResourceType (..),
                                                  SType (..),
                                                  SemanticModel (..))
import qualified Data.Char                       as Char
import qualified Data.List                       as List
import           Data.Maybe                      (isJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

--------------------------------------------------------------------------------
-- Naming
--------------------------------------------------------------------------------

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCase :: Text -> Text
camelCase t = case Text.split (== '_') t of
    []         -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing      -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

kotlinMethodName :: SemanticModel -> SMethod -> Text
kotlinMethodName model m = case kotlinMethodNameOverride (methodName m) of
    Just n  -> n
    Nothing -> camelCase (stripLongestPrefix (methodName m))
  where
    stripLongestPrefix t = case mapMaybe (`Text.stripPrefix` t) prefixes of
        (rest : _) -> rest
        []         -> t
    -- The root's own, then the libraries', longest first. A standalone
    -- handle's isn't stripped: @passKeyDerive@.
    prefixes = selfPrefix model : List.sortOn (negate . Text.length) [ w <> "_" | w <- K.libraryWords model ]
    mapMaybe f = foldr go []
      where
        go x acc = case f x of
            Just y  -> y : acc
            Nothing -> acc

-- | Name of a method emitted as a @val@: @tox.savedata@, not
-- @tox.getSavedata@.
kotlinPropertyName :: SemanticModel -> SMethod -> Text
kotlinPropertyName model m = stripGetPrefix (kotlinMethodName model m)
  where
    stripGetPrefix t = case Text.stripPrefix "get" t of
        Just (Text.uncons -> Just (c, cs)) | Char.isUpper c ->
            Text.cons (Char.toLower c) cs
        _ -> t

kotlinNameOf :: SemanticModel -> Text -> Text
kotlinNameOf model n
    | isJust (splitNamespace (K.libraryWords model) n) = pascalCase n
    | otherwise = classPrefix model <> pascalCase n

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

-- | Only wrappers by name apply to callback parameters.
renderParamType :: SemanticModel -> SParameter -> Text
renderParamType model p = case paramWrapperFor model "" p of
    Just cls -> cls
    Nothing  -> renderType model (paramType p)

renderMethodParamType :: SemanticModel -> Text {- C method name -} -> SParameter -> Text
renderMethodParamType model cMethod p = case paramWrapperFor model cMethod p of
    Just cls -> cls
    Nothing  -> renderType model (paramType p)

renderType :: SemanticModel -> SType -> Text
renderType model ty = case ty of
    SVoid -> "Unit"
    SBool -> "Boolean"
    SInt n -> intWidth n
    SUInt n -> intWidth n
    SSizeT -> "Long"
    SString -> "String"
    SBytes -> "ByteArray"
    SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
        Just typedefName -> classPrefix model <> pascalCase typedefName
        Nothing          -> "ByteArray"
    SFixedList (SInt 16) _ _ -> "ShortArray"
    SFixedList (SUInt 16) _ _ -> "ShortArray"
    SFixedList (SInt 8) _ _ -> "ByteArray"
    SFixedList (SUInt 8) _ _ -> "ByteArray"
    SFixedList (SInt 32) _ _ -> "IntArray"
    SFixedList (SUInt 32) _ _ -> "IntArray"
    SFixedList element _ _ -> "List<" <> renderType model element <> ">"
    SEnum name -> kotlinNameOf model name
    SHandle name -> pascalCase name
    SCallback name -> kotlinNameOf model name <> "Callback"
    SResourceId name -> kotlinNameOf model name
    SList (SUInt 8) -> "ByteArray"
    SList (SInt 8) -> "ByteArray"
    SList (SInt 16) -> "ShortArray"
    SList (SUInt 16) -> "ShortArray"
    SList (SInt _) -> "IntArray"
    SList (SUInt _) -> "IntArray"
    SList element -> "List<" <> renderType model element <> ">"
  where
    -- 16-bit scalars are @Int@, as in 'Apigen.Language.Jvm.Java.jniType'.
    intWidth 8  = "Byte"
    intWidth 16 = "Int"
    intWidth 32 = "Int"
    intWidth 64 = "Long"
    intWidth _  = "Int"

--------------------------------------------------------------------------------
-- Path-id args
--------------------------------------------------------------------------------

pathIdArgs :: SemanticModel -> SResource -> [(Text, Text)]
pathIdArgs model r = pathIdArgsFor model r True

-- | Optionally without the resource's own id (for constructors).
pathIdArgsFor :: SemanticModel -> SResource -> Bool -> [(Text, Text)]
pathIdArgsFor model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case List.find ((== pname) . resourceName) (resources model) of
            Just parentRes -> pathIdArgs model parentRes
            Nothing        -> []
        Nothing -> []
    ownArg = case resourceType r of
        ResId (SResourceId idTypeName) ->
            let shortName = stripParentPrefix idTypeName
            in [(camelCase shortName, classPrefix model <> pascalCase idTypeName)]
        _ -> []

    stripParentPrefix name = case parent r of
        Just p -> case Text.stripPrefix (p <> "_") name of
            Just rest -> rest
            Nothing   -> name
        Nothing -> name

--------------------------------------------------------------------------------
-- Method filtering
--------------------------------------------------------------------------------

isSkippableMethod :: SemanticModel -> SMethod -> Bool
isSkippableMethod model = isSkippableMethodFor model ""

-- | For interface @iface@ (@ToxCrypto@ keeps statics).
isSkippableMethodFor :: SemanticModel -> Text -> SMethod -> Bool
isSkippableMethodFor model iface m
    | isHiddenFromPublicApi model name = True
    | otherwise = case methodRole m of
        -- Sub-resource constructors like @tox_friend_add@ are public.
        Constructor   -> isLifecycleConstructor model name
        Destructor    -> True
        RegistrarRole -> True
        StaticRole
            | interfaceAllowsStatics iface -> isEnumNamer m || isCStaticHelper name
            | otherwise                    -> True
        _             -> isIterationDriver name || hasNoJniShim model m
  where
    name = methodName m
    -- Covered by @Enum.name@ and the @*Constants@ objects.
    isCStaticHelper n = "_length" `Text.isSuffixOf` n
    -- Emitted as one generic @iterate(handler, state)@.
    isIterationDriver n = any (`Text.isSuffixOf` n) ["_iterate", "_iteration_interval"]
