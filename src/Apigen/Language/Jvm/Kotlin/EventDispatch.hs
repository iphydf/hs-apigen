{-# LANGUAGE OverloadedStrings #-}

-- | @Tox{Core,Av}EventDispatch.kt@, passing the proto events from JNI
-- @iterate()@ to the listener callbacks.
module Apigen.Language.Jvm.Kotlin.EventDispatch (generate) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.Jvm.Conventions   (paramWrapperClass,
                                                    paramWrapperFor,
                                                    paramWrapperImport)
import           Apigen.Language.Jvm.Kotlin.Common (camelCase, kotlinNameOf,
                                                    pascalCase)
import           Apigen.Language.Jvm.Model         (classPrefix, inOtherLibrary,
                                                    isOtherLibraryName)
import           Apigen.Model                      (callbackIn)
import           Apigen.Namespace                  (stripNamespace)
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
    coreCbs = List.sortOn cbName [c | c <- callbacks model, not (isAvCb model c)]
    avCbs = List.sortOn cbName [c | c <- callbacks model, isAvCb model c]

-- | Another library's callback, like toxav's.
isAvCb :: SemanticModel -> SCallbackTypeModel -> Bool
isAvCb model cb = inOtherLibrary model (cbFile cb)

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

-- | Caches @<Enum>.values()@, which allocates a new array each call.
enumCacheBlock :: SemanticModel -> [SCallbackTypeModel] -> [Text]
enumCacheBlock model cbs
    | null caches = []
    | otherwise = concatMap toCache caches ++ [""]
  where
    caches = List.sort . List.nub $
        [ n
        | c <- cbs
        , p <- userParams c
        , Just n <- [enumNameOf model (cbCName c) (paramName p) (paramType p)]
        ]
    toCache cls =
        [ "    private val " <> cacheName cls <> " = " <> cls <> ".values()"
        ]

cacheName :: Text -> Text
cacheName cls = case Text.uncons cls of
    Nothing        -> "values"
    Just (c, rest) -> Text.singleton (Char.toLower c) <> rest <> "Values"

enumNameOf
    :: SemanticModel
    -> Text {- C method name -}
    -> Text {- param name -}
    -> SType
    -> Maybe Text
enumNameOf model _cbCName pname ty = case paramWrapperClass pname of
    -- 'paramWrapperFor' only adds byte-array wrappers, never enums.
    Just cls | isEnumWrapper cls -> Just cls
    _ -> case ty of
        SEnum n -> Just (kotlinNameOf model n)
        _       -> Nothing

-- | Helpers keeping the dispatch arms under ktlint's 140-char limit.
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
               , Just cls <- [paramWrapperFor model (cbCName c) p]
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
        SResourceId n -> ["im.tox.tox4j.core.data." <> kotlinNameOf model n]
        SEnum n -> ["im.tox.tox4j." <> enumPkg n <> ".enums." <> kotlinNameOf model n]
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data." <> classPrefix model <> pascalCase typedef]
            Nothing      -> []
        _ -> []
    enumPkg n
        | isOtherLibraryName model n = "av"
        | otherwise = "core"

--------------------------------------------------------------------------------
-- Per-callback branch
--------------------------------------------------------------------------------

renderBranch :: SemanticModel -> DispatchSpec -> SCallbackTypeModel -> [Text]
renderBranch model spec c =
    let
        accessor = "event." <> eventAccessor model c <> "."
        params = userParams c
        callArgs = [wrapParam model (cbCName c) accessor (effectiveType p) | p <- params] ++ ["next"]
    in
        [ "                    " <> dispEvents spec <> ".Event.EventTypeCase." <> screamingSnake (cbBase model c) <> " ->"
        , "                        handler." <> listenerMethodName model c <> "("
        ]
            ++ [ "                            " <> arg <> "," | arg <- callArgs ]
            ++ [ "                        )" ]
  where
    -- Toxav callbacks pass @friend_number@ as a raw @uint32@, but the listener
    -- takes a @ToxFriendNumber@.
    effectiveType p
        | paramName p == "friend_number" && isUInt (paramType p) =
            p {paramType = SResourceId "Friend_Number"}
        | otherwise = p
    isUInt (SUInt _) = True
    isUInt (SInt _)  = True
    isUInt _         = False

userParams :: SCallbackTypeModel -> [SParameter]
userParams c = filter keep (cbParams c)
  where
    keep p = case paramType p of
        SHandle _ -> False
        _         -> True

wrapParam :: SemanticModel -> Text -> Text -> SParameter -> Text
wrapParam model cbC prefix p =
    case paramWrapperFor model cbC p of
        Just cls -> wrapWithConvention cls field (paramType p)
        Nothing  -> structuralWrap
  where
    field = prefix <> kotlinFieldName p
    structuralWrap = case paramType p of
        SResourceId n -> kotlinNameOf model n <> "(" <> field <> ")"
        -- Use the first entry rather than throw (a newer c-toxcore can send values we
        -- don't know).
        SEnum n ->
            let cls = kotlinNameOf model n
            in cacheName cls <> ".atOrFirst(" <> field <> ".number)"
        SBytes -> field <> ".toByteArray()"
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> classPrefix model <> pascalCase typedef <> "(" <> field <> ".toByteArray())"
            Nothing -> field <> ".toByteArray()"
        SList (SInt 16) -> "toShortArray(" <> field <> ")"
        SList (SUInt 16) -> "toShortArray(" <> field <> ")"
        SFixedList (SInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SFixedList (SUInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SInt 8 -> field <> ".toByte()"
        SUInt 8 -> field <> ".toByte()"
        _ -> field

wrapWithConvention :: Text -> Text -> SType -> Text
wrapWithConvention cls field ty
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

isEnumWrapper :: Text -> Bool
isEnumWrapper = (`elem` ["AudioChannels", "SamplingRate"])

-- | Value classes with a clamping @fromInt@ factory. A new ranged value
-- class needs one, and an entry here.
hasCoercingFactory :: Text -> Bool
hasCoercingFactory = (`elem` ["Width", "Height", "SampleCount"])

kotlinFieldName :: SParameter -> Text
kotlinFieldName p = camelCase (paramName p)

--------------------------------------------------------------------------------
-- Naming helpers
--------------------------------------------------------------------------------

cbBase :: SemanticModel -> SCallbackTypeModel -> Text
cbBase model c = stripCb (stripNamespace (K.libraryWords model) (cbName c))
  where
    stripCb n = case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing   -> n

eventAccessor :: SemanticModel -> SCallbackTypeModel -> Text
eventAccessor model = camelCase . cbBase model

screamingSnake :: Text -> Text
screamingSnake = Text.toUpper

listenerMethodName :: SemanticModel -> SCallbackTypeModel -> Text
listenerMethodName model c = camelCase (cbBase model c)
