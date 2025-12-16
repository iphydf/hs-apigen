{-# LANGUAGE OverloadedStrings #-}

-- | @Tox{Core,Av}EventDispatch.kt@, passing the proto events from JNI
-- @iterate()@ to the listener callbacks.
module Apigen.Language.Jvm.Kotlin.EventDispatch (generate) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.Jvm.Kotlin.Common (cRange, camelCase,
                                                    kotlinNameOf, pascalCase,
                                                    typeImports)
import           Apigen.Language.Jvm.Model         (Subsystem (..), classPrefix,
                                                    jvmName, jvmSource,
                                                    subsystems)
import           Apigen.Model                      (arrayTypeOf)
import           Apigen.Namespace                  (stripNamespace)
import           Apigen.Semantic                   (SCallbackTypeModel (..),
                                                    SParameter (..),
                                                    SResource (..),
                                                    SResourceKind (..),
                                                    SType (..),
                                                    SemanticModel (..))
import qualified Data.Char                         as Char
import qualified Data.List                         as List
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           System.FilePath                   (takeDirectory, takeFileName)

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ (jvmSource model "java" "impl.jni" (dispObject spec <> ".kt"), renderDispatch model spec cbs)
    | s <- subsystems model, subKind s `elem` [RootHandle, LibraryHandle]
    , let spec = specOf s
          cbs = List.sortOn cbName [ c | c <- callbacks model, inDir s (cbFile c) ]
    ]
  where
    inDir s file = dirOf file == dirOf (resourceFile (subHandle s))
    dirOf = takeFileName . takeDirectory . Text.unpack

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

data DispatchSpec = DispatchSpec
    { dispObject   :: Text -- ^ @ToxCoreEventDispatch@
    , dispListener :: Text -- ^ @ToxCoreEventListener@
    , dispEvents   :: Text -- ^ @CoreEvents@
    , dispListPkg  :: Text -- ^ @core@
    , dispProtoPkg :: Text -- ^ @core.proto@
    }

specOf :: Subsystem -> DispatchSpec
specOf s = DispatchSpec
    { dispObject   = subClass s <> "EventDispatch"
    , dispListener = subClass s <> "EventListener"
    , dispEvents   = pascalCase (subWord s) <> "Events"
    , dispListPkg  = subWord s
    , dispProtoPkg = subWord s <> ".proto"
    }

--------------------------------------------------------------------------------
-- File body
--------------------------------------------------------------------------------

renderDispatch :: SemanticModel -> DispatchSpec -> [SCallbackTypeModel] -> Text
renderDispatch model spec cbs =
    Text.unlines $
        [ "package " <> jvmName model "impl.jni"
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
        , SEnum e <- [paramType p]
        , let n = kotlinNameOf model e
        ]
    toCache cls =
        [ "    private val " <> cacheName cls <> " = " <> cls <> ".values()"
        ]

cacheName :: Text -> Text
cacheName cls = case Text.uncons cls of
    Nothing        -> "values"
    Just (c, rest) -> Text.singleton (Char.toLower c) <> rest <> "Values"

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
        [ jvmName model (dispListPkg spec <> ".callbacks." <> dispListener spec)
        , jvmName model (dispProtoPkg spec <> "." <> dispEvents spec)
        ]
    paramImports =
        [ imp
        | c <- cbs
        , p <- userParams c
        , imp <- typeImports model (paramType p)
        ]

--------------------------------------------------------------------------------
-- Per-callback branch
--------------------------------------------------------------------------------

renderBranch :: SemanticModel -> DispatchSpec -> SCallbackTypeModel -> [Text]
renderBranch model spec c =
    let
        accessor = "event." <> eventAccessor model c <> "."
        params = userParams c
        callArgs = [wrapParam model accessor p | p <- params] ++ ["next"]
    in
        [ "                    " <> dispEvents spec <> ".Event.EventTypeCase." <> Text.toUpper (cbBase model c) <> " ->"
        , "                        handler." <> listenerMethodName model c <> "("
        ]
            ++ [ "                            " <> arg <> "," | arg <- callArgs ]
            ++ [ "                        )" ]

userParams :: SCallbackTypeModel -> [SParameter]
userParams c = filter keep (cbParams c)
  where
    keep p = case paramType p of
        SHandle _ -> False
        _         -> True

wrapParam :: SemanticModel -> Text -> SParameter -> Text
wrapParam model prefix p = wrap (paramType p)
  where
    field = prefix <> kotlinFieldName p
    wrap ty = case ty of
        -- The proto's field is the unit's number, or wider. A malformed one
        -- keeps to C's range rather than throw.
        SScalar n b -> kotlinNameOf model n <> "(" <> inRange (wrap b) <> ")"
          where
            inRange v = maybe v (\(_, lo, hi) -> v <> ".coerceIn(" <> lo <> ", " <> hi <> ")") (cRange ty)
        SResourceId n -> kotlinNameOf model n <> "(" <> field <> ")"
        -- Use the first entry rather than throw (a newer c-toxcore can send values we
        -- don't know).
        SEnum n ->
            let cls = kotlinNameOf model n
            in cacheName cls <> ".atOrFirst(" <> field <> ".number)"
        SBytes -> field <> ".toByteArray()"
        SFixedBytes sizeConst _ -> case arrayTypeOf model sizeConst of
            Just typedef -> classPrefix model <> pascalCase typedef <> "(" <> field <> ".toByteArray())"
            Nothing -> field <> ".toByteArray()"
        SList (SInt 16) -> "toShortArray(" <> field <> ")"
        SList (SUInt 16) -> "toShortArray(" <> field <> ")"
        SFixedList (SInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SFixedList (SUInt 16) _ _ -> "toShortArray(" <> field <> ")"
        SInt 8 -> field <> ".toByte()"
        SUInt 8 -> field <> ".toByte()"
        _ -> field

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

listenerMethodName :: SemanticModel -> SCallbackTypeModel -> Text
listenerMethodName model c = camelCase (cbBase model c)
