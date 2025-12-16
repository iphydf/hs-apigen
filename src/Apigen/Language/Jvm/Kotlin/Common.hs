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

import           Apigen.Language.Jvm.Conventions (interfaceAllowsStatics,
                                                  isHiddenFromPublicApi,
                                                  isLifecycleConstructor,
                                                  kotlinMethodNameOverride,
                                                  paramWrapperFor)
import           Apigen.Semantic                 (SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SResource (..),
                                                  SResourceType (..),
                                                  SType (..),
                                                  SemanticModel (..))
import qualified Data.Char                       as Char
import qualified Data.List                       as List
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

kotlinMethodName :: SMethod -> Text
kotlinMethodName m = case kotlinMethodNameOverride (methodName m) of
    Just n  -> n
    Nothing -> camelCase (stripLongestPrefix (methodName m))
  where
    stripLongestPrefix t = case mapMaybe (`Text.stripPrefix` t) prefixes of
        (rest : _) -> rest
        []         -> t
    -- Longest first. @tox_pass_@ isn't stripped: @passKeyDerive@.
    prefixes =
        [ "tox_self_"
        , "toxav_"
        , "tox_"
        ]
    mapMaybe f = foldr go []
      where
        go x acc = case f x of
            Just y  -> y : acc
            Nothing -> acc

-- | Name for a method emitted as a @val@ property: @tox.savedata@, not
-- @tox.getSavedata@.
kotlinPropertyName :: SMethod -> Text
kotlinPropertyName m = stripGetPrefix (kotlinMethodName m)
  where
    stripGetPrefix t = case Text.stripPrefix "get" t of
        Just rest@(Text.uncons -> Just (c, cs)) | Char.isUpper c ->
            Text.cons (Char.toLower c) cs
        _ -> t

kotlinNameOf :: Text -> Text
kotlinNameOf n
    | "Toxav_" `Text.isPrefixOf` n = pascalCase n
    | "Tox_" `Text.isPrefixOf` n = pascalCase n
    | otherwise = "Tox" <> pascalCase n

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

-- | A callback parameter's type: only wrappers by name alone apply.
renderParamType :: SemanticModel -> SParameter -> Text
renderParamType model p = case paramWrapperFor "" p of
    Just cls -> cls
    Nothing  -> renderType model (paramType p)

renderMethodParamType :: SemanticModel -> Text {- C method name -} -> SParameter -> Text
renderMethodParamType model cMethod p = case paramWrapperFor cMethod p of
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
        Just typedefName -> "Tox" <> pascalCase typedefName
        Nothing          -> "ByteArray"
    SFixedList (SInt 16) _ _ -> "ShortArray"
    SFixedList (SUInt 16) _ _ -> "ShortArray"
    SFixedList (SInt 8) _ _ -> "ByteArray"
    SFixedList (SUInt 8) _ _ -> "ByteArray"
    SFixedList (SInt 32) _ _ -> "IntArray"
    SFixedList (SUInt 32) _ _ -> "IntArray"
    SFixedList element _ _ -> "List<" <> renderType model element <> ">"
    SEnum name -> kotlinNameOf name
    SHandle name -> pascalCase name
    SCallback name -> kotlinNameOf name <> "Callback"
    SResourceId name -> kotlinNameOf name
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

-- | The parent ids that go before a flattened method's own parameters.
pathIdArgs :: SemanticModel -> SResource -> [(Text, Text)]
pathIdArgs model r = pathIdArgsFor model r True

-- | As 'pathIdArgs', optionally without the resource's own id (for
-- constructors).
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
            in [(camelCase shortName, "Tox" <> pascalCase idTypeName)]
        _ -> []

    stripParentPrefix name = case parent r of
        Just p -> case Text.stripPrefix (p <> "_") name of
            Just rest -> rest
            Nothing   -> name
        Nothing -> name

--------------------------------------------------------------------------------
-- Method filtering
--------------------------------------------------------------------------------

-- | Methods not on the public interface as regular methods.
isSkippableMethod :: SemanticModel -> SMethod -> Bool
isSkippableMethod model = isSkippableMethodFor model ""

-- | As 'isSkippableMethod', for interface @iface@ (e.g. @ToxCrypto@
-- keeps statics).
isSkippableMethodFor :: SemanticModel -> Text -> SMethod -> Bool
isSkippableMethodFor model iface m
    | isHiddenFromPublicApi model name = True
    | otherwise = case methodRole m of
        -- Sub-resource constructors like @tox_friend_add@ are public.
        Constructor   -> isLifecycleConstructor name
        Destructor    -> True
        RegistrarRole -> True
        StaticRole
            | interfaceAllowsStatics iface -> isCStaticHelper name
            | otherwise                    -> True
        _             -> isIterationDriver name
  where
    name = methodName m
    -- Covered by @Enum.name@ and the @*Constants@ objects.
    isCStaticHelper n =
        "_to_string" `Text.isSuffixOf` n
            || "_length" `Text.isSuffixOf` n
    -- Emitted as one generic @iterate(handler, state)@.
    isIterationDriver n = n `elem`
        [ "tox_iterate"
        , "tox_iteration_interval"
        , "toxav_iterate"
        , "toxav_iteration_interval"
        , "toxav_audio_iterate"
        , "toxav_audio_iteration_interval"
        , "toxav_video_iterate"
        , "toxav_video_iteration_interval"
        -- Conference AV: in toxav.h but works on a Tox, with no JNI shim.
        , "toxav_group_send_audio"
        , "toxav_groupchat_av_enabled"
        , "toxav_groupchat_disable_av"
        ]
