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
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Namespace          (secondaryHandles)
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
            , evDoc = registrationDoc doc . eventRegistrar =<< K.rootEvent model short
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
            , evDoc = registrationDoc doc (eventRegistrar ev)
            , evFields = callbackFields model (cCallback ev) (eventParams ev)
            , evSource = FromCallback av ev cb
            }
        | ev <- events av
        , Just cb <- [find ((== cCallback ev) . cbCName) (callbacks model)]
        ]

logEvent :: SemanticModel -> [Event]
logEvent model =
    [ Event
        { evName = pascal (words' (callbackWord (lib model)))
        , evDoc = Just ("@brief A message " <> rootType (lib model) <> " logs.")
        , evFields = callbackFields model (cbCName cb) (inner cb)
        , evSource = FromLog cb
        }
    | settings <- K.handlesOfKind model SettingsHandle, cb <- K.registeredCallbacks model settings ]
  where
    inner cb = drop 1 (take (length (cbParams cb) - 1) (cbParams cb))

callbackFields :: SemanticModel -> Text -> [SParameter] -> [EventField]
callbackFields model cb ps = case ps of
    [] -> []
    _ | Just (frame, 0, taken) <- K.frameIn model ps ->
        EventField "frame" (TFrame (frameTypeName frame)) "std::move(*frame)" : callbackFields model cb (drop taken ps)
    p : rest -> maybe id (:) (single p) (callbackFields model cb rest)
  where
    single p = do
        let n = paramName p
        ty <- case K.enumParam model (cb, n) of
            Just e  -> Just (TEnum (enumCppName model e))
            Nothing -> typeOf model (paramType p)
        let value = case ty of
                TString -> "string(" <> n <> ")"
                _       -> fromC ty n
        return (EventField n ty value)

-- | @tox/generated/events.hpp@: the event structs.
eventsHeader :: Lib -> [Event] -> Text
eventsHeader l evs = header l
    [ own l "media.hpp", own l "types.hpp", "<cstdint>", "<string>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> "::event {", ""]
    ++ concat
        [ docBlock (evDoc e)
          ++ declaration ("struct " <> evName e) [ renderIn (libNamespace l) (map evName evs) (efType f) <> " " <> efName f <> ";" | f <- evFields e ]
          ++ [""]
        | e <- evs ]
    ++ ["} // namespace " <> libNamespace l <> "::event", ""]
    ++ ["// clang-format off", "#define FOR_EACH_EVENT_TYPE(macro) \\"]
    ++ macroList (map evName evs)
    ++ ["// clang-format on"]

-- | @src/generated/events.cpp@: reading events from C.
eventsSource :: SemanticModel -> [Event] -> Text
eventsSource model evs = source l
    [ "generated/c_values.hpp", "generated/engine.hpp", "session_impl.hpp", own l "events.hpp"
    , "<cstddef>", "<cstdint>", "<string>", rootInclude l, eventsInclude l
    , logLevelInclude l, avInclude l, "<utility>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> " {", "", "namespace {", ""]
    ++ concat [ trampoline e ++ [""] | e <- evs, isCallback (evSource e) ]
    ++ ["} // namespace", ""]
    ++ braced ("void process_events(Session::Impl& impl, const " <> eventsType l <> "* _Nonnull events)")
        ( [ "const uint32_t size = " <> eventsSize l <> "(events);" ]
        ++ braced "for (uint32_t i = 0; i < size; ++i)"
            ( [ "const " <> eventType l <> "* e = " <> eventsGet l <> "(events, i);" ]
            ++ ["switch (" <> eventGetType l <> "(e)) {"]
            ++ concat [ caseOf e enumerator getter cTy | e <- evs, FromEvents enumerator getter cTy <- [evSource e] ]
            ++ [ "// The bindings don't know of newer events."
               , "default:"
               , "    break;"
               , "}" ] ) )
    ++ [""]
    ++ braced ("void register_" <> avVar l <> "_callbacks(" <> avType l <> "* _Nonnull " <> avVar l <> ", Session::Impl& impl)")
        [ eventRegistrar sev <> "(" <> avVar l <> ", " <> trampolineName e <> ", &impl);"
        | e <- evs, FromCallback _ sev _ <- [evSource e] ]
    ++ [""]
    ++ braced ("void register_" <> callbackWord l <> "_callback(" <> optionsType l <> "* _Nonnull options, Session::Impl& impl)")
        [ setLogCallback l <> "(options, on_" <> callbackWord l <> ");"
        , setLogUserData l <> "(options, &impl);"
        ]
    ++ ["", "} // namespace " <> libNamespace l]
  where
    l = lib model
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

    -- The session makes a frame of C's arguments: @receive_audio_frame@.
    frame e = case evSource e of
        FromCallback _ _ cb | Just (name, i, taken) <- K.frameIn model (cbParams cb) ->
            let args = map paramName (take taken (drop i (cbParams cb)))
            in ["auto frame = impl.receive_" <> unwords' (words' name) <> "(" <> Text.intercalate ", " args <> ");"]
               ++ braced "if (!frame)" ["return;"]
        _ -> []

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
        SScalar _ b     -> cType b
        SFixedList e _ _ -> "const " <> maybe "void" render (typeOf model e) <> "* _Nonnull"
        _               -> "const uint8_t* _Nonnull"
