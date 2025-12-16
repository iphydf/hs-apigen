{-# LANGUAGE OverloadedStrings #-}

-- | Generates @lib/src/main/java/im/tox/tox4j/impl/jni/Tox{Core,Av}EventDispatch.kt@.
--
-- The dispatcher parses a protobuf @AvEvents@/@CoreEvents@ payload
-- emitted by JNI iterate(), then folds each event through the matching
-- listener callback. Because the @event_type@ oneof is the discriminator,
-- the generated code switches on @eventTypeCase@ and pulls each event
-- payload out of its corresponding accessor.
--
-- Per-event branch shape:
--
-- @
-- AvEvents.Event.EventTypeCase.CALL ->
--     handler.call(
--         ToxFriendNumber(event.call.friendNumber),
--         event.call.audioEnabled,
--         event.call.videoEnabled,
--         next,
--     )
-- @
--
-- Param wrapping mirrors 'Apigen.Language.Jvm.Kotlin.Impl': value
-- classes round-trip through their constructor, enums through
-- @values()[ordinal]@, @bytes@ fields through @.toByteArray()@, every
-- other field passes through.
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
                 -- Defensive lookups inside each arm
                 -- (@firstOrNull@ for enums, @fromInt@ for ranged
                 -- value classes, @atOrFirst@ for enum-by-ordinal)
                 -- absorb malformed proto fields without throwing,
                 -- so a single bad event can't drop the rest of the
                 -- batch. No outer try\/catch is needed.
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

-- | Cache @<Enum>.values()@ at the object level. Kotlin's
-- @values()@ returns a freshly cloned array each call (per the JLS);
-- the dispatch path references it twice per arm (the @getOrNull@
-- lookup and the @[0]@ fallback) and runs once per event per
-- @iterate@, so the allocations add up. The cache pays the cost
-- once at class load.
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

-- | Lowercase the head of @ClassName@ → @className_VALUES@. Matches
-- Kotlin convention for top-level vals (lowerCamelCase).
cacheName :: Text -> Text
cacheName cls = case Text.uncons cls of
    Nothing -> "values"
    Just (c, rest) -> Text.singleton (Char.toLower c) <> rest <> "Values"

-- | Pick the Kotlin enum class name for a param's wrapping path.
-- Either the convention-registered wrapper class (AudioChannels,
-- SamplingRate, ...) when 'isEnumWrapper' returns True, or the
-- 'kotlinNameOf'-translated 'SEnum' name otherwise. 'Nothing' for
-- non-enum params.
enumNameOf
    :: Text {- C method name -}
    -> Text {- param name -}
    -> SType
    -> Maybe Text
enumNameOf _cbCName pname ty = case paramWrapperClass pname of
    -- Enum-wrapper detection runs off the AV vocabulary directly
    -- (which is name-only and doesn't care about the method
    -- context). Byte-array derivation never produces an enum
    -- wrapper, so consulting 'paramWrapperFor' here would add
    -- nothing — and would require an 'SParameter' to gate the
    -- byte-array branch correctly.
    Just cls | isEnumWrapper cls -> Just cls
    _ -> case ty of
        SEnum n -> Just (kotlinNameOf n)
        _       -> Nothing

-- | Per-dispatcher private helpers. The 16-bit PCM decoder is hoisted
-- out of the dispatch arms so the inline form doesn't push lines past
-- ktlint's 140-char limit. @atOrFirst@ shortens the ordinal-fallback
-- pattern (used per enum-typed dispatch arm) so long-named enums
-- like @toxGroupPrivacyStateValues@ don't push the line over 140
-- chars either.
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
        _ -> False
    needsPcmHelper = any (any isShortPcm . cbParams) cbs
    isShortPcm p = case paramType p of
        SList (SInt 16)         -> True
        SList (SUInt 16)        -> True
        SFixedList (SInt 16) _ _  -> True
        SFixedList (SUInt 16) _ _ -> True
        _                       -> False

importLines :: SemanticModel -> DispatchSpec -> [SCallbackTypeModel] -> [Text]
importLines model spec cbs =
    map (\i -> "import " <> i) . List.sort . List.nub $
        baseImports ++ paramImports
  where
    -- The per-event proto messages are accessed via @event.<field>@
    -- and don't need direct imports (Kotlin resolves them through the
    -- containing 'AvEvents'/'CoreEvents' type).
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
            -- Convention-registered wrappers (BitRate, AudioChannels,
            -- SampleCount, …) ride in via their hand-curated import
            -- paths so 'wrapWithConvention' can refer to them by
            -- short name.
            ++ [ paramWrapperImport cls
               | c <- cbs
               , p <- userParams c
               , Just cls <- [paramWrapperFor (cbCName c) p]
               ]

    -- Match @effectiveType@ in 'renderBranch': AV callbacks treat
    -- @friend_number@ as a 'Friend_Number' resource ID.
    effectiveTypeOf p
        | paramName p == "friend_number" && isUInt (paramType p) =
            SResourceId "Friend_Number"
        | otherwise = paramType p
    isUInt (SUInt _) = True
    isUInt (SInt _) = True
    isUInt _ = False

    importsForType ty = case ty of
        SResourceId n -> ["im.tox.tox4j.core.data." <> kotlinNameOf n]
        SEnum n -> ["im.tox.tox4j." <> enumPkg n <> ".enums." <> kotlinNameOf n]
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing -> []
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
    -- Toxav callbacks model @friend_number@ as raw @uint32@ but the
    -- listener interface wraps it in @ToxFriendNumber@. Mirror the
    -- Kotlin callback generator's rewrite so dispatch lines up.
    effectiveType p
        | paramName p == "friend_number" && isUInt (paramType p) =
            p {paramType = SResourceId "Friend_Number"}
        | otherwise = p
    isUInt (SUInt _) = True
    isUInt (SInt _) = True
    isUInt _ = False

-- | Strip the receiver and the void-pointer user_data; everything else
-- is a listener-visible parameter.
userParams :: SCallbackTypeModel -> [SParameter]
userParams c = filter keep (cbParams c)
  where
    keep p = case paramType p of
        SHandle _ -> False
        _ -> True

-- | Wrap one proto field accessor into the Kotlin shape the listener
-- expects: convention-registered value class / enum first (BitRate,
-- AudioChannels, …), then ID newtypes / @SEnum@ ordinal lookup /
-- @bytes@ → @toByteArray()@ / fixed-bytes typedef wrap / 16-bit
-- PCM short-buffer view / 8-bit narrowing, pass-through otherwise.
wrapParam :: SemanticModel -> Text -> Text -> SParameter -> Text
wrapParam model cbCName prefix p =
    case paramWrapperFor cbCName p of
        Just cls -> wrapWithConvention cls field (paramType p)
        Nothing  -> structuralWrap
  where
    field = prefix <> kotlinFieldName p
    structuralWrap = case paramType p of
        SResourceId n -> kotlinNameOf n <> "(" <> field <> ")"
        -- Use 'getOrNull' rather than indexing — a future c-toxcore enum
        -- extension would send a number that's out of the Kotlin enum's
        -- range, and crashing the dispatch loop on an unrecognised value
        -- is worse than substituting the zero-ordinal default. Both
        -- ends use the cached @<enum>Values@ array (see 'enumCacheBlock')
        -- to avoid the two @values()@ allocations per arm.
        -- 'atOrFirst' (a private extension defined at object level)
        -- shortens the lookup so long-named caches don't push the
        -- line past ktlint's 140-char ceiling, and the body
        -- @getOrNull(n) ?: this[0]@ keeps the same out-of-range
        -- fallback semantics as the dispatcher-side helpers above.
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

-- | Wrap a proto field with a convention-registered class. Value
-- classes get the constructor call. Three width adapters are folded
-- into the constructor argument:
--
--   * @SSizeT@ / @S(U)Int 64@ — proto delivers a Java @long@, the
--     wrapper's underlying @value@ is @Int@, so we @.toInt()@ first.
--   * @SBytes@ / @SFixedBytes@ — proto delivers a @ByteString@, but
--     value classes wrap a @ByteArray@, so we @.toByteArray()@ first.
--   * everything else — pass through.
--
-- Coercing factories ('fromInt') are preferred over the plain
-- constructor for value classes whose @init@ block @require@s a
-- range — proto can deliver out-of-range values (uint32 width
-- exceeding the 16-bit wrapper, uint64 sample count widening to a
-- negative Int after @.toInt()@), and a thrown
-- IllegalArgumentException would bail the dispatcher's
-- @for (event in events.eventsList)@ loop and drop the rest of
-- the batch.
--
-- Enums get a @.values().filter { it.value == X }[0]@ lookup so the
-- proto's raw @uint32@ maps to the matching enum entry.
wrapWithConvention :: Text -> Text -> SType -> Text
wrapWithConvention cls field ty
    -- 'firstOrNull' rather than 'filter{}[0]' — an unknown channel /
    -- sample-rate value (e.g. a new c-toxcore channel layout) would
    -- otherwise IndexOutOfBounds the dispatch loop. Substitute the
    -- first enum value when nothing matches. Uses the cached
    -- @<enum>Values@ array (see 'enumCacheBlock') so the lookup
    -- doesn't reallocate the values array on each event.
    | isEnumWrapper cls =
        let cache = cacheName cls
        in cache <> ".firstOrNull { it.value == " <> narrowed <> " } ?: " <> cache <> "[0]"
    -- Coercing factory ('Width.fromInt' etc.) for value classes
    -- whose @init@ block ranges-checks the input: untrusted proto
    -- can deliver an out-of-range value that the plain constructor
    -- would reject with IllegalArgumentException, bailing the
    -- dispatcher loop and dropping the rest of the batch.
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

-- | Hand-curated list of convention wrappers that are Kotlin enums
-- (their value is a public 'value' field). Everything else in
-- 'paramWrapperFor' is treated as a value class.
--
-- **Sync with 'paramWrapperClass' in "Apigen.Language.Jvm.Conventions"**:
-- any new enum-shaped wrapper added there for a dispatcher-visible
-- field must also be added here, or the dispatcher will fall through
-- to plain-constructor wrapping and crash on unknown values.
isEnumWrapper :: Text -> Bool
isEnumWrapper = (`elem` ["AudioChannels", "SamplingRate"])

-- | Value-class wrappers that expose a coercing @fromInt@ factory
-- on their companion object. The dispatcher uses these for proto
-- fields whose raw range exceeds the value class's range
-- (uint16 wrappers, non-negative-only counts).
--
-- **Sync with the hand-written value classes** in
-- @jvm-toxcore-c\/lib\/src\/main\/kotlin\/im\/tox\/tox4j\/av\/data\/@:
-- any new ranged-int value class (one whose @init@ block calls
-- @require(value in …)@) needs both a @fromInt@ companion factory
-- in its source file and an entry here. Without the entry, the
-- dispatcher uses the plain constructor and a malformed proto
-- field throws @IllegalArgumentException@, dropping the rest of
-- the event batch.
hasCoercingFactory :: Text -> Bool
hasCoercingFactory = (`elem` ["Width", "Height", "SampleCount"])

-- | Toxav callbacks use raw @uint32 friend_number@ but the listener
-- expects @ToxFriendNumber@. The Kotlin callback generator already
-- rewrites this; mirror the rule here so the dispatcher matches.
kotlinFieldName :: SParameter -> Text
kotlinFieldName p = camelCase (paramName p)

--------------------------------------------------------------------------------
-- Naming helpers
--------------------------------------------------------------------------------

-- | @tox_friend_name_cb@ → @FriendName@. The wire message name in
-- 'Apigen.Language.Jvm.Proto' uses the same convention.
cbBase :: SCallbackTypeModel -> Text
cbBase c = stripCb (stripPrefix (cbName c))
  where
    stripPrefix n = case Text.stripPrefix "tox_" n of
        Just rest -> rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> rest
            Nothing -> n
    stripCb n = case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing -> n

-- | @tox_friend_name_cb@ → @friendName@. Matches the field name on the
-- top-level @oneof event_type@ in the proto.
eventAccessor :: SCallbackTypeModel -> Text
eventAccessor = camelCase . cbBase

-- | Lowercase callback base → SCREAMING_SNAKE for the @EventTypeCase@
-- enum (@FRIEND_NAME@). Protobuf generates one per oneof field. The
-- input is already snake_case, so it's a straight uppercase.
screamingSnake :: Text -> Text
screamingSnake = Text.toUpper

-- | @tox_friend_name_cb@ → @friendName@ — matches the Kotlin callback
-- interface's method name (set by the Kotlin sub-module).
listenerMethodName :: SCallbackTypeModel -> Text
listenerMethodName c = camelCase (cbBase c)
