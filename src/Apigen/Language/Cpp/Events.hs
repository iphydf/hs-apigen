{-# LANGUAGE OverloadedStrings #-}
-- | Events: a struct for each thing Tox tells the client, and how the engine
-- reads them.
module Apigen.Language.Cpp.Events
    ( Event (..)
    , allEvents
    , eventsHeader
    , eventsSource
    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Namespace          (rootNamespace, secondaryHandles)
import           Apigen.Semantic
import           Data.List                 (find, sortOn)
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

data EventField = EventField
    { efName :: Text
    , efType :: CppType
    , efRead :: Text      -- ^ Its value, from the C event or callback arguments
    }

data Event = Event
    { evName   :: Text
    , evDoc    :: Maybe Text
    , evFields :: [EventField]
    , evSource :: EventSource
    }

data EventSource
    = FromEvents Text Text Text  -- ^ The type's enumerator, the event's getter, its C type
    | FromCallback SResource SEvent SCallbackTypeModel
    | FromLog SCallbackTypeModel

allEvents :: (Text -> Maybe Text) -> SemanticModel -> [Event]
allEvents doc model = sortOn evName (toxEvents doc model ++ avEvents doc model ++ logEvent model)

-- | What a callback's registration says of its event, without that it sets
-- the callback.
registrationDoc :: (Text -> Maybe Text) -> Text -> Maybe Text
registrationDoc doc fn = do
    d <- doc fn
    case filter (not . setter) (Text.splitOn "\n\n" (Text.strip d)) of
        p : _ -> Just ("@brief " <> summary p)
        []    -> Nothing
  where
    setter p = any (`Text.isPrefixOf` summary p) ["Set the callback for the", "Pass NULL to unset."]

toxEvents :: (Text -> Maybe Text) -> SemanticModel -> [Event]
toxEvents doc model = fromMaybe [] $ do
    variant <- K.eventsVariant model
    typeEnum <- findEnum model (variantTypeEnum variant)
    return
        [ Event
            { evName = pascal (words' (memberName m))
            , evDoc = registrationDoc doc (rootNamespace model <> "_callback_" <> short)
            , evFields = mapMaybe (field r) (getters r)
            , evSource = FromEvents (enumerator typeEnum (memberName m)) (memberGetter m) (cName r)
            }
        | m <- variantMembers variant
        , Just r <- [findResource model (memberType m)]
        , let short = Text.toLower (memberName m)
        ]
  where
    enumerator e n = maybe n enumMemberCName (find ((== n) . enumMemberName) (enumMembers e))
    getters r = [ (m, mp) | m <- methods r, let mp = mappingOf model r m
                          , not (isSizeCompanion model (cFunctionName mp)) ]
    field r (m, mp) = do
        let getter = cFunctionName mp
        name <- Text.stripPrefix (cPrefix r <> "get_") getter
        let typed = eventFieldType model r m
            call = getter <> "(ev)"
        ty <- case K.enumParam model (Text.dropWhileEnd (== '_') (cPrefix r), name) of
            Just e  -> Just (TEnum (enumCppName model e))
            Nothing -> typeOf model typed
        value <- case ty of
            TBytes   -> (\len -> "bytes(" <> call <> ", " <> len <> "(ev))") <$> cSizeFunctionName mp
            TArray a -> Just ("array<" <> a <> ">(" <> call <> ")")
            _        -> Just (fromC ty call)
        return (EventField name ty value)

avEvents :: (Text -> Maybe Text) -> SemanticModel -> [Event]
avEvents doc model = fromMaybe [] $ do
    av : _ <- Just (secondaryHandles model)
    return
        [ Event
            { evName = pascal (words' (eventName ev))
            , evDoc = registrationDoc doc (cPrefix av <> "callback_" <> eventName ev)
            , evFields = callbackFields model (cCallback ev) (eventParams ev)
            , evSource = FromCallback av ev cb
            }
        | ev <- events av
        , Just cb <- [find ((== cCallback ev) . cbCName) (callbacks model)]
        ]

logEvent :: SemanticModel -> [Event]
logEvent model =
    [ Event
        { evName = "Log"
        , evDoc = Just "@brief A message Tox logs."
        , evFields = callbackFields model (cbCName cb) (inner cb)
        , evSource = FromLog cb
        }
    | settings <- K.handlesOfKind model SettingsHandle, cb <- K.registeredCallbacks model settings ]
  where
    inner cb = drop 1 (take (length (cbParams cb) - 1) (cbParams cb))

callbackFields :: SemanticModel -> Text -> [SParameter] -> [EventField]
callbackFields model cb ps = case ps of
    [] -> []
    _ | Just (frame, taken) <- receivedFrame (map paramName ps) ->
        EventField "frame" (TNamed frame) "std::move(*frame)" : callbackFields model cb (drop taken ps)
    p : rest -> maybe id (:) (single p) (callbackFields model cb rest)
  where
    single p = do
        let n = paramName p
        ty <- case K.enumParam model (cb, n) of
            Just e -> Just (TEnum (enumCppName model e))
            Nothing
                | "bit_rate" `Text.isSuffixOf` n -> Just (TNamed "Bitrate")
                | otherwise -> typeOf model (paramType p)
        let value = case ty of
                TString          -> "string(" <> n <> ")"
                TNamed "Bitrate" -> "Bitrate{" <> n <> "}"
                _                -> fromC ty n
        return (EventField n ty value)

receivedFrame :: [Text] -> Maybe (Text, Int)
receivedFrame names
    | take 4 names == ["pcm", "sample_count", "channels", "sampling_rate"] = Just ("AudioFrame", 4)
    | take 8 names == ["width", "height", "y", "u", "v", "ystride", "ustride", "vstride"] = Just ("VideoFrame", 8)
    | otherwise = Nothing

-- | @tox/generated/events.hpp@: the event structs.
eventsHeader :: [Event] -> Text
eventsHeader evs = header
    [ "tox/media.hpp", "tox/types.hpp", "<cstdint>", "<string>", "<vector>" ]
    $  ["namespace tox::event {", ""]
    ++ concat
        [ docBlock (evDoc e)
          ++ declaration ("struct " <> evName e) [ renderIn (map evName evs) (efType f) <> " " <> efName f <> ";" | f <- evFields e ]
          ++ [""]
        | e <- evs ]
    ++ ["} // namespace tox::event", ""]
    ++ ["// clang-format off", "#define FOR_EACH_EVENT_TYPE(macro) \\"]
    ++ macroList (map evName evs)
    ++ ["// clang-format on"]

-- | @src/generated/events.cpp@: reading events from C.
eventsSource :: SemanticModel -> [Event] -> Text
eventsSource model evs = source
    [ "generated/c_values.hpp", "generated/engine.hpp", "session_impl.hpp", "tox/events.hpp"
    , "<cstddef>", "<cstdint>", "<string>", "<tox/tox.h>", "<tox/tox_events.h>"
    , "<tox/tox_log_level.h>", "<tox/toxav.h>", "<utility>", "<vector>" ]
    $  ["namespace tox {", "", "namespace {", ""]
    ++ concat [ trampoline e ++ [""] | e <- evs, isCallback (evSource e) ]
    ++ ["} // namespace", ""]
    ++ braced "void process_events(Session::Impl& impl, const Tox_Events* _Nonnull events)"
        ( [ "const uint32_t size = tox_events_get_size(events);" ]
        ++ braced "for (uint32_t i = 0; i < size; ++i)"
            ( [ "const Tox_Event* e = tox_events_get(events, i);" ]
            ++ ["switch (tox_event_get_type(e)) {"]
            ++ concat [ caseOf e enumerator getter cTy | e <- evs, FromEvents enumerator getter cTy <- [evSource e] ]
            ++ [ "// The bindings don't know of newer events."
               , "default:"
               , "    break;"
               , "}" ] ) )
    ++ [""]
    ++ braced "void register_av_callbacks(ToxAV* _Nonnull av, Session::Impl& impl)"
        [ cPrefix r <> "callback_" <> eventName sev <> "(av, " <> trampolineName e <> ", &impl);"
        | e <- evs, FromCallback r sev _ <- [evSource e] ]
    ++ [""]
    ++ braced "void register_log_callback(Tox_Options* _Nonnull options, Session::Impl& impl)"
        [ "tox_options_set_log_callback(options, on_log);"
        , "tox_options_set_log_user_data(options, &impl);"
        ]
    ++ ["", "} // namespace tox"]
  where
    isCallback s = case s of
        FromEvents{} -> False
        _            -> True

    caseOf e enumerator getter cTy =
        [ "case " <> enumerator <> ": {"
        , "    const " <> cTy <> "* ev = " <> getter <> "(e);"
        ]
        ++ indent (pushEvent "impl." e)
        ++ [ "    break;", "}" ]

    pushEvent impl e =
        [ impl <> "push_event(event::" <> evName e <> "{" ]
        ++ indent [ "." <> efName f <> " = " <> efRead f <> "," | f <- evFields e ]
        ++ ["});"]

    trampolineName e = "on_" <> unwords' (words' (Text.concatMap snake (evName e)))
    snake c = if c >= 'A' && c <= 'Z' then Text.pack ['_', c] else Text.singleton c

    trampoline e = case evSource e of
        FromCallback _ _ cb -> callbackFn e cb
        FromLog cb          -> callbackFn e cb
        FromEvents{}        -> []

    callbackFn e cb =
        braced ("void " <> trampolineName e <> "(" <> Text.intercalate ", " (cParams cb) <> ")")
            ( [ "auto& impl = *static_cast<Session::Impl*>(" <> paramName (last (cbParams cb)) <> ");" ]
            ++ frame e
            ++ pushEvent "impl." e )

    frame e = case [ t | EventField "frame" (TNamed t) _ <- evFields e ] of
        ["AudioFrame"] -> receive "receive_audio_frame(pcm, sample_count, channels, sampling_rate)"
        ["VideoFrame"] -> receive "receive_video_frame(width, height, y, u, v, ystride, ustride, vstride)"
        _              -> []
      where
        receive call = ["auto frame = impl." <> call <> ";"] ++ braced "if (!frame)" ["return;"]

    cParams cb = zipWith cParam [0 :: Int ..] (cbParams cb)
      where
        cParam i p
            | i == 0 = cType (paramType p) <> " /*" <> paramName p <> "*/"
            | otherwise = cType (paramType p) <> " " <> paramName p

    cType t = case t of
        SHandle "void"  -> "void* _Nullable"
        SHandle h       -> maybe h cName (findResource model h) <> "* _Nonnull"
        SBool           -> "bool"
        SInt n          -> render (TInt n)
        SUInt n         -> render (TUInt n)
        SSizeT          -> "size_t"
        SString         -> "const char* _Nullable"
        SEnum e         -> maybe e enumName (findEnum model e)
        SResourceId i   -> maybe i idCName (idModel model i)
        SFixedList e _ _ -> "const " <> maybe "void" render (typeOf model e) <> "* _Nonnull"
        _               -> "const uint8_t* _Nonnull"
