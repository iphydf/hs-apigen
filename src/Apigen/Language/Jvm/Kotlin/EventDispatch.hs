{-# LANGUAGE OverloadedStrings #-}

-- | Generates @Tox{Core,Av}EventDispatch.kt@, which folds the protobuf
-- events from JNI @iterate()@ through the listener callbacks.
module Apigen.Language.Jvm.Kotlin.EventDispatch (generate) where

import           Apigen.Language.Jvm.Conventions   (paramWrapperClass,
                                                    paramWrapperFor,
                                                    paramWrapperImport)
import           Apigen.Language.Jvm.Kotlin.Common (camelCase, kotlinNameOf,
                                                    pascalCase)
import           Apigen.Semantic                   (SCallbackTypeModel (..),
                                                    SParameter (..), SType (..),
                                                    SemanticModel (..))
import qualified Data.Char                         as Char
import qualified Data.List                         as List
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreEventDispatch.kt"
      , renderDispatch model coreSpec coreCbs
      )
    , ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxAvEventDispatch.kt"
      , renderDispatch model avSpec avCbs
      )
    ]
  where
    coreCbs = List.sortOn cbName [c | c <- callbacks model, not (isAvCb c)]
    avCbs = List.sortOn cbName [c | c <- callbacks model, isAvCb c]

isAvCb :: SCallbackTypeModel -> Bool
isAvCb c = "toxav_" `Text.isPrefixOf` cbName c

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

data DispatchSpec = DispatchSpec
    { dispObject   :: Text -- ^ @ToxCoreEventDispatch@ / @ToxAvEventDispatch@
    , dispListener :: Text -- ^ @ToxCoreEventListener@ / @ToxAvEventListener@
    , dispEvents   :: Text -- ^ @CoreEvents@ / @AvEvents@
    , dispListPkg  :: Text -- ^ @core@ / @av@
    , dispProtoPkg :: Text -- ^ @core.proto@ / @av.proto@
    }

coreSpec, avSpec :: DispatchSpec
coreSpec = DispatchSpec
    { dispObject = "ToxCoreEventDispatch"
    , dispListener = "ToxCoreEventListener"
    , dispEvents = "CoreEvents"
    , dispListPkg = "core"
    , dispProtoPkg = "core.proto"
    }
avSpec = DispatchSpec
    { dispObject = "ToxAvEventDispatch"
    , dispListener = "ToxAvEventListener"
    , dispEvents = "AvEvents"
    , dispListPkg = "av"
    , dispProtoPkg = "av.proto"
    }

--------------------------------------------------------------------------------
-- File body
--------------------------------------------------------------------------------

renderDispatch :: SemanticModel -> DispatchSpec -> [SCallbackTypeModel] -> Text
renderDispatch model spec cbs =
    Text.unlines $
        [ "package im.tox.tox4j.impl.jni"
        , ""
        ]
            ++ importLines model spec cbs
            ++ [ ""
               , "object " <> dispObject spec <> " {"
               ]
            ++ enumCacheBlock model cbs
            ++ helperBlock cbs
            ++ [ "    fun <S> dispatch("
               , "        handler: " <> dispListener spec <> "<S>,"
               , "        eventData: ByteArray?,"
               , "        state: S,"
               , "    ): S {"
               , "        if (eventData == null || eventData.isEmpty()) return state"
               , "        val events = " <> dispEvents spec <> ".parseFrom(eventData)"
               , "        var next = state"
               , "        for (event in events.eventsList) {"
                 -- The arms don't throw on malformed fields, so one bad
                 -- event can't drop the rest of the batch.
               , "            next ="
               , "                when (event.eventTypeCase) {"
               ]
            ++ concatMap (renderBranch model spec) cbs
            ++ [ "                    " <> dispEvents spec <> ".Event.EventTypeCase.EVENTTYPE_NOT_SET -> next"
               , "                }"
               , "        }"
               , "        return next"
               , "    }"
               , "}"
               ]

-- | Caches @<Enum>.values()@, which allocates a new array on each call.
enumCacheBlock :: SemanticModel -> [SCallbackTypeModel] -> [Text]
enumCacheBlock model cbs
    | null caches = []
    | otherwise = concatMap toCache caches ++ [""]
  where
    caches = List.sort . List.nub $
        [ n
        | c <- cbs
        , p <- userParams c
        , Just n <- [enumNameOf (cbCName c) (paramName p) (paramType p)]
        ]
    toCache cls =
        [ "    private val " <> cacheName cls <> " = " <> cls <> ".values()"
        ]

-- | @ClassName@ to @classNameValues@.
cacheName :: Text -> Text
cacheName cls = case Text.uncons cls of
    Nothing        -> "values"
    Just (c, rest) -> Text.singleton (Char.toLower c) <> rest <> "Values"

-- | The Kotlin enum class of a param, if it has one.
enumNameOf
    :: Text {- C method name -}
    -> Text {- param name -}
    -> SType
    -> Maybe Text
enumNameOf _cbCName pname ty = case paramWrapperClass pname of
    -- 'paramWrapperFor' only adds byte-array wrappers, never enums.
    Just cls | isEnumWrapper cls -> Just cls
    _ -> case ty of
        SEnum n -> Just (kotlinNameOf n)
        _       -> Nothing

-- | Helpers that keep the dispatch arms under ktlint's 140-char limit.
helperBlock :: [SCallbackTypeModel] -> [Text]
helperBlock cbs = pcmHelper ++ atOrFirstHelper ++ trailingBlank
  where
    pcmHelper
        | needsPcmHelper =
            [ "    private fun toShortArray(bytes: com.google.protobuf.ByteString): ShortArray {"
            , "        val sb ="
            , "            bytes"
            , "                .asReadOnlyByteBuffer()"
            , "                .order(java.nio.ByteOrder.LITTLE_ENDIAN)"
            , "                .asShortBuffer()"
            , "        return ShortArray(sb.remaining()).also(sb::get)"
            , "    }"
            ]
        | otherwise = []
    atOrFirstHelper
        | needsAtOrFirst =
            [ "    private fun <T> Array<T>.atOrFirst(index: Int): T = getOrNull(index) ?: this[0]"
            ]
        | otherwise = []
    trailingBlank
        | null pcmHelper && null atOrFirstHelper = []
        | otherwise = [""]
    needsAtOrFirst = any (any isSEnum . userParams) cbs
    isSEnum p = case paramType p of
        SEnum _ -> True
        _       -> False
    needsPcmHelper = any (any isShortPcm . cbParams) cbs
    isShortPcm p = case paramType p of
        SList (SInt 16)           -> True
        SList (SUInt 16)          -> True
        SFixedList (SInt 16) _ _  -> True
        SFixedList (SUInt 16) _ _ -> True
        _                         -> False

importLines :: SemanticModel -> DispatchSpec -> [SCallbackTypeModel] -> [Text]
importLines model spec cbs =
    map (\i -> "import " <> i) . List.sort . List.nub $
        baseImports ++ paramImports
  where
    baseImports =
        [ "im.tox.tox4j." <> dispListPkg spec <> ".callbacks." <> dispListener spec
        , "im.tox.tox4j." <> dispProtoPkg spec <> "." <> dispEvents spec
        ]
    paramImports =
        [ imp
        | c <- cbs
        , p <- userParams c
        , imp <- importsForType (effectiveTypeOf p)
        ]
            ++ [ paramWrapperImport cls
               | c <- cbs
               , p <- userParams c
               , Just cls <- [paramWrapperFor (cbCName c) p]
               ]

    -- Same as @effectiveType@ in 'renderBranch'.
    effectiveTypeOf p
        | paramName p == "friend_number" && isUInt (paramType p) =
            SResourceId "Friend_Number"
        | otherwise = paramType p
    isUInt (SUInt _) = True
    isUInt (SInt _)  = True
    isUInt _         = False

    importsForType ty = case ty of
        SResourceId n -> ["im.tox.tox4j.core.data." <> kotlinNameOf n]
        SEnum n -> ["im.tox.tox4j." <> enumPkg n <> ".enums." <> kotlinNameOf n]
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing      -> []
        _ -> []
    enumPkg n
        | "Toxav_" `Text.isPrefixOf` n = "av"
        | otherwise = "core"

--------------------------------------------------------------------------------
-- Per-callback branch
--------------------------------------------------------------------------------

renderBranch :: SemanticModel -> DispatchSpec -> SCallbackTypeModel -> [Text]
renderBranch model spec c =
    let
        accessor = "event." <> eventAccessor c <> "."
        params = userParams c
        callArgs = [wrapParam model (cbCName c) accessor (effectiveType p) | p <- params] ++ ["next"]
    in
        [ "                    " <> dispEvents spec <> ".Event.EventTypeCase." <> screamingSnake (cbBase c) <> " ->"
        , "                        handler." <> listenerMethodName c <> "("
        ]
            ++ [ "                            " <> arg <> "," | arg <- callArgs ]
            ++ [ "                        )" ]
  where
    -- Toxav callbacks pass @friend_number@ as a raw @uint32@, but the
    -- listener takes a @ToxFriendNumber@.
    effectiveType p
        | paramName p == "friend_number" && isUInt (paramType p) =
            p {paramType = SResourceId "Friend_Number"}
        | otherwise = p
    isUInt (SUInt _) = True
    isUInt (SInt _)  = True
    isUInt _         = False

-- | The parameters the listener sees: all but the receiver.
userParams :: SCallbackTypeModel -> [SParameter]
userParams c = filter keep (cbParams c)
  where
    keep p = case paramType p of
        SHandle _ -> False
        _         -> True

-- | Converts a proto field to the type the listener takes.
wrapParam :: SemanticModel -> Text -> Text -> SParameter -> Text
wrapParam model cbCName prefix p =
    case paramWrapperFor cbCName p of
        Just cls -> wrapWithConvention cls field (paramType p)
        Nothing  -> structuralWrap
  where
    field = prefix <> kotlinFieldName p
    structuralWrap = case paramType p of
        SResourceId n -> kotlinNameOf n <> "(" <> field <> ")"
        -- A value from a newer c-toxcore can be out of range: use the
        -- first entry rather than throw.
        SEnum n ->
            let cls = kotlinNameOf n
            in cacheName cls <> ".atOrFirst(" <> field <> ".number)"
        SBytes -> field <> ".toByteArray()"
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> "Tox" <> pascalCase typedef <> "(" <> field <> ".toByteArray())"
            Nothing -> field <> ".toByteArray()"
        SList (SInt 16) -> "toShortArray(" <> field <> ")"
        SList (SUInt 16) -> "toShortArray(" <> field <> ")"
        SFixedList (SInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SFixedList (SUInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SInt 8 -> field <> ".toByte()"
        SUInt 8 -> field <> ".toByte()"
        _ -> field

-- | Wraps a proto field in a convention-registered class.
wrapWithConvention :: Text -> Text -> SType -> Text
wrapWithConvention cls field ty
    -- An unknown value gets the first entry rather than throwing.
    | isEnumWrapper cls =
        let cache = cacheName cls
        in cache <> ".firstOrNull { it.value == " <> narrowed <> " } ?: " <> cache <> "[0]"
    -- The constructor would throw on an out-of-range value.
    | hasCoercingFactory cls = cls <> ".fromInt(" <> narrowed <> ")"
    | otherwise              = cls <> "(" <> narrowed <> ")"
  where
    narrowed = case ty of
        SSizeT          -> field <> ".toInt()"
        SUInt 64        -> field <> ".toInt()"
        SInt 64         -> field <> ".toInt()"
        SBytes          -> field <> ".toByteArray()"
        SFixedBytes _ _ -> field <> ".toByteArray()"
        _               -> field

-- | The wrappers in 'paramWrapperClass' that are enums, not value
-- classes.
isEnumWrapper :: Text -> Bool
isEnumWrapper = (`elem` ["AudioChannels", "SamplingRate"])

-- | Value classes with a range-clamping @fromInt@ factory. A new ranged
-- value class needs one, and an entry here.
hasCoercingFactory :: Text -> Bool
hasCoercingFactory = (`elem` ["Width", "Height", "SampleCount"])

kotlinFieldName :: SParameter -> Text
kotlinFieldName p = camelCase (paramName p)

--------------------------------------------------------------------------------
-- Naming helpers
--------------------------------------------------------------------------------

-- | @tox_friend_name_cb@ to @friend_name@.
cbBase :: SCallbackTypeModel -> Text
cbBase c = stripCb (stripPrefix (cbName c))
  where
    stripPrefix n = case Text.stripPrefix "tox_" n of
        Just rest -> rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> rest
            Nothing   -> n
    stripCb n = case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing   -> n

-- | The event's field in the proto @oneof@.
eventAccessor :: SCallbackTypeModel -> Text
eventAccessor = camelCase . cbBase

screamingSnake :: Text -> Text
screamingSnake = Text.toUpper

listenerMethodName :: SCallbackTypeModel -> Text
listenerMethodName c = camelCase (cbBase c)
