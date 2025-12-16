{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Helpers shared by 'Apigen.Language.Jvm.Kotlin' (public-API surface)
-- and 'Apigen.Language.Jvm.Kotlin.Impl' (the @Tox*Impl.kt@ classes).
--
-- Both walk the same semantic model and need to agree on naming, type
-- mapping, and the path-id args derived from the resource hierarchy.
-- Keeping these in one place prevents one side from drifting out of
-- sync with the other.
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
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCase :: Text -> Text
camelCase t = case Text.split (== '_') t of
    [] -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

-- | C method name -> Kotlin method name. Consults the Conventions
-- module first for hand-tuned renames (e.g. @toxav_audio_set_bit_rate@
-- → @setAudioBitRate@). Otherwise strips the subsystem prefix
-- (tox_, toxav_, tox_pass_) and the @self_@ accessor marker, then
-- camelCases.
kotlinMethodName :: SMethod -> Text
kotlinMethodName m = case kotlinMethodNameOverride (methodName m) of
    Just n -> n
    -- Strip the longest matching subsystem prefix. Order matters:
    -- @tox_self_@ must be tried *before* @tox_@, otherwise the bare
    -- @tox_@ strip wins and the @self_@ leaks into the Kotlin name.
    Nothing -> camelCase (stripLongestPrefix (methodName m))
  where
    stripLongestPrefix t = case mapMaybe (`Text.stripPrefix` t) prefixes of
        (rest : _) -> rest
        []         -> t
    -- Longest first. Note: @tox_pass_@ is NOT in the list — the
    -- @pass@ stays as part of the Kotlin name (e.g.
    -- @tox_pass_key_derive@ → @passKeyDerive@). Individual cases like
    -- @tox_pass_key_encrypt@ → @encrypt@ go through the explicit
    -- 'kotlinMethodNameOverride' table.
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

-- | Kotlin name for a method emitted as a @val foo: T@ property
-- accessor (zero-arg, non-void return). Strips a leading
-- @get@ from the camelCased base — Kotlin properties read as
-- @tox.savedata@ rather than @tox.getSavedata@ (the Java getter
-- @getSavedata()@ is still auto-derived by the compiler for
-- Java/Scala callers).
kotlinPropertyName :: SMethod -> Text
kotlinPropertyName m = stripGetPrefix (kotlinMethodName m)
  where
    stripGetPrefix t = case Text.stripPrefix "get" t of
        Just rest@(Text.uncons -> Just (c, cs)) | Char.isUpper c ->
            Text.cons (Char.toLower c) cs
        _ -> t

-- | Apply the @Tox@ prefix convention. Names already prefixed
-- (@Tox_*@, @Toxav_*@) are pascalCased as-is; bare names get @Tox@
-- prepended.
kotlinNameOf :: Text -> Text
kotlinNameOf n
    | "Toxav_" `Text.isPrefixOf` n = pascalCase n
    | "Tox_" `Text.isPrefixOf` n = pascalCase n
    | otherwise = "Tox" <> pascalCase n

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

-- | Render the Kotlin type for a callback parameter. Consults
-- 'Apigen.Language.Jvm.Conventions.paramWrapperFor' with an empty
-- method context — only the name-only conventions
-- (BitRate, SampleCount, …) apply to callbacks. Falls through to
-- 'renderType' based on the SType when no wrapper matches.
renderParamType :: SemanticModel -> SParameter -> Text
renderParamType model p = case paramWrapperFor "" p of
    Just cls -> cls
    Nothing  -> renderType model (paramType p)

-- | Method-aware variant of 'renderParamType'. Uses the C method name
-- to look up wrappers that only apply in specific method contexts
-- (e.g. @ToxName@ derived for @tox_self_set_name@'s @name@ param).
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
        Nothing -> "ByteArray"
    SFixedList (SInt 16) _ _ -> "ShortArray"
    SFixedList (SUInt 16) _ _ -> "ShortArray"
    SFixedList (SInt 8) _ _ -> "ByteArray"
    SFixedList (SUInt 8) _ _ -> "ByteArray"
    SFixedList (SInt 32) _ _ -> "IntArray"
    SFixedList (SUInt 32) _ _ -> "IntArray"
    SFixedList element _ _ -> "List<" <> renderType model element <> ">"
    SEnum name -> kotlinNameOf name
    -- Handles to opaque C resources (like @Tox *@, @Tox_Pass_Key *@)
    -- render as PascalCase Kotlin class names — @Pass_Key@ → @PassKey@.
    -- The primitive C handles (@uint8_t *@, @char *@, @void *@) are
    -- not 'SHandle' here; they're routed via 'SBytes' / 'SList' before
    -- reaching the renderer.
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
    -- 16-bit scalars widen to @Int@ at the JNI surface (matches what
    -- @Apigen.Language.Jvm.Java.jniType@ emits). Using @Short@ here
    -- forces every JNI call to pre-cast.
    intWidth 8 = "Byte"
    intWidth 16 = "Int"
    intWidth 32 = "Int"
    intWidth 64 = "Long"
    intWidth _ = "Int"

--------------------------------------------------------------------------------
-- Path-id args
--------------------------------------------------------------------------------

-- | Walk the parent chain of @r@ to collect the path-id args that need
-- to be prepended to flattened method signatures.
pathIdArgs :: SemanticModel -> SResource -> [(Text, Text)]
pathIdArgs model r = pathIdArgsFor model r True

-- | As 'pathIdArgs' but lets the caller decide whether to include the
-- resource's own ID. Constructors create the resource and so don't
-- take its ID as input — only the parent chain.
pathIdArgsFor :: SemanticModel -> SResource -> Bool -> [(Text, Text)]
pathIdArgsFor model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case List.find ((== pname) . resourceName) (resources model) of
            Just parentRes -> pathIdArgs model parentRes
            Nothing -> []
        Nothing -> []
    ownArg = case resourceType r of
        ResId (SResourceId idTypeName) ->
            let shortName = stripParentPrefix idTypeName
            in [(camelCase shortName, "Tox" <> pascalCase idTypeName)]
        _ -> []

    stripParentPrefix name = case parent r of
        Just p -> case Text.stripPrefix (p <> "_") name of
            Just rest -> rest
            Nothing -> name
        Nothing -> name

--------------------------------------------------------------------------------
-- Method filtering
--------------------------------------------------------------------------------

-- | True for methods that don't appear on the public interface as a
-- regular method: constructors, destructors, registrars, statics, and
-- the iteration drivers (@tox_iterate@ / @toxav_*_iterate@) which
-- are emitted as a special generic @fun <S> iterate(handler, state): S@.
isSkippableMethod :: SemanticModel -> SMethod -> Bool
isSkippableMethod model = isSkippableMethodFor model ""

-- | As 'isSkippableMethod', but parametrised by the interface name
-- (@ToxCore@/@ToxAv@/@ToxCrypto@). Lets per-interface conventions —
-- like ToxCrypto allowing StaticRole methods — override the default
-- skip behaviour.
isSkippableMethodFor :: SemanticModel -> Text -> SMethod -> Bool
isSkippableMethodFor model iface m
    -- Hidden-list wins regardless of role: catches Constructors *and*
    -- regular methods the hand-written API deliberately omits.
    | isHiddenFromPublicApi model name = True
    | otherwise = case methodRole m of
        -- Only the *lifecycle* Constructor (the one that creates the
        -- Impl class's managed resource) is excluded. Sub-resource
        -- Constructors like @tox_friend_add@ are part of the public
        -- API.
        Constructor   -> isLifecycleConstructor name
        Destructor    -> True
        RegistrarRole -> True
        StaticRole
            | interfaceAllowsStatics iface -> isCStaticHelper name
            | otherwise                    -> True
        _             -> isIterationDriver name
  where
    name = methodName m
    -- @*_to_string@ and @*_length@ are C-level convenience helpers:
    -- the string converters duplicate Kotlin's @Enum.name@, and
    -- length constants are already emitted into the @*Constants@
    -- object. Both stay off the interface even when StaticRole is
    -- otherwise allowed.
    isCStaticHelper n =
        "_to_string" `Text.isSuffixOf` n
            || "_length" `Text.isSuffixOf` n
    -- The @tox_iterate@/@toxav_iterate@ family is emitted as a single
    -- generic @fun <S> iterate(handler, state): S@ on the interface,
    -- not as individual functions.
    isIterationDriver n = n `elem`
        [ "tox_iterate"
        , "tox_iteration_interval"
        , "toxav_iterate"
        , "toxav_iteration_interval"
        , "toxav_audio_iterate"
        , "toxav_audio_iteration_interval"
        , "toxav_video_iterate"
        , "toxav_video_iteration_interval"
        -- Legacy AV-groupchat bridge: lives in toxav.h but operates on
        -- Tox/Conference. No hand-written JNI shim.
        , "toxav_group_send_audio"
        , "toxav_groupchat_av_enabled"
        , "toxav_groupchat_disable_av"
        ]
