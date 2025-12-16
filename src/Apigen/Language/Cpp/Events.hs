{-# LANGUAGE OverloadedStrings #-}
-- | Events: a struct for each thing the library tells the client, a variant
-- of them all, and how C's are read.
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
import           Data.Maybe                (fromMaybe, isNothing, mapMaybe)
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
    | FromCallback               -- ^ Another library's callback, which hand-written code reads
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
        name <- Text.stripPrefix (cPrefix r <> "get_") (shownName model r m)
        let typed = eventFieldType model r m
            call = getter <> "(ev)"
        ty <- case K.enumParam model (Text.dropWhileEnd (== '_') (cPrefix r), name) of
            Just e  -> Just (TEnum (enumCppName model e))
            Nothing -> typeOf model typed
        value <- case ty of
            TBytes   -> (\len -> "c::bytes(" <> call <> ", " <> len <> "(ev))") <$> cSizeFunctionName mp
            TArray a -> Just ("c::array<" <> a <> ">(" <> call <> ")")
            _        -> Just (fromC ty call)
        return (EventField name ty value)

-- | Another library's events, from its callbacks: not frames, which go
-- elsewhere.
avEvents :: (Text -> Maybe Text) -> SemanticModel -> [Event]
avEvents doc model = fromMaybe [] $ do
    av : _ <- Just (secondaryHandles model)
    return
        [ Event
            { evName = pascal (words' (eventName ev))
            , evDoc = registrationDoc doc (eventRegistrar ev)
            , evFields = callbackFields model (cCallback ev) (eventParams ev)
            , evSource = FromCallback
            }
        | ev <- events av
        , isNothing (K.frameIn model (eventParams ev))
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
    []       -> []
    p : rest -> maybe id (:) (single p) (callbackFields model cb rest)
  where
    single p = do
        let n = paramName p
        ty <- case K.enumParam model (cb, n) of
            Just e  -> Just (TEnum (enumCppName model e))
            Nothing -> typeOf model (paramType p)
        let value = case ty of
                TString -> "c::string(" <> n <> ")"
                _       -> fromC ty n
        return (EventField n ty value)

-- | @tox/generated/events.hpp@: the event structs, and their variant.
eventsHeader :: Lib -> [Event] -> Text
eventsHeader l evs = header l
    [ own l "types.hpp", "<cstdint>", "<string>", "<variant>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> "::event {", ""]
    ++ concat
        [ docBlock (evDoc e)
          ++ declaration ("struct " <> evName e) [ renderIn (libNamespace l) (map evName evs) (efType f) <> " " <> efName f <> ";" | f <- evFields e ]
          ++ [""]
        | e <- evs ]
    ++ ["} // namespace " <> libNamespace l <> "::event", ""]
    ++ namespaced l
        ( docBlock (Just ("@brief Something that happened in " <> rootType l <> ", as " <> rootType l <> "::iterate returns it."))
        ++ ["using Event = std::variant<"]
        ++ indent (zipWith (\i e -> "event::" <> evName e <> (if i < length evs then "," else "")) [1 :: Int ..] evs)
        ++ [">;", ""] )

-- | @src/generated/events.cpp@: reading events from C.
eventsSource :: SemanticModel -> [Event] -> Text
eventsSource model evs = source l
    [ "generated/convert.hpp", "core.hpp", "generated/c_values.hpp"
    , "<cstddef>", "<cstdint>", "<string>", rootInclude l, eventsInclude l
    , logLevelInclude l, "<utility>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> " {", "", "namespace {", ""]
    ++ concat [ callbackFn e cb ++ [""] | e <- evs, FromLog cb <- [evSource e] ]
    ++ ["} // namespace", ""]
    ++ braced ("void append_events(std::vector<Event>& out, const " <> eventsType l <> "* _Nonnull events)")
        ( [ "const uint32_t size = " <> eventsSize l <> "(events);" ]
        ++ braced "for (uint32_t i = 0; i < size; ++i)"
            ( [ "const " <> eventType l <> "* e = " <> eventsGet l <> "(events, i);" ]
            ++ braced "if (e == nullptr)" ["continue;"]
            ++ ["switch (" <> eventGetType l <> "(e)) {"]
            ++ concat [ caseOf e enumerator getter cTy | e <- evs, FromEvents enumerator getter cTy <- [evSource e] ]
            ++ [ "// The bindings don't know of newer events."
               , "default:"
               , "    break;"
               , "}" ] ) )
    ++ [""]
    ++ braced ("void set_" <> callbackWord l <> "_callback(" <> optionsType l <> "* _Nonnull options, detail::Core& core)")
        [ setLogCallback l <> "(options, on_" <> callbackWord l <> ");"
        , setLogUserData l <> "(options, &core);"
        ]
    ++ ["", "} // namespace " <> libNamespace l]
  where
    l = lib model
    -- The core's field and the class's parameter: @log_level@.
    levelVar = unwords' (words' (Text.concatMap snake (logLevelType l)))

    caseOf e enumerator getter cTy =
        [ "case " <> enumerator <> ": {"
        , "    const " <> cTy <> "* ev = " <> getter <> "(e);"
        ]
        ++ indent (braced "if (ev == nullptr)" ["break;"])
        ++ indent (pushEvent "out.emplace_back(" e)
        ++ [ "    break;", "}" ]

    pushEvent call e =
        [ call <> "event::" <> evName e <> "{" ]
        ++ indent [ "." <> efName f <> " = " <> efRead f <> "," | f <- evFields e ]
        ++ ["});"]

    trampolineName e = "on_" <> unwords' (words' (Text.concatMap snake (evName e)))
    snake c = if c >= 'A' && c <= 'Z' then Text.pack ['_', c] else Text.singleton c

    -- Kept for the next iterate, at the level asked for and above.
    callbackFn e cb =
        braced ("void " <> trampolineName e <> "(" <> Text.intercalate ", " (cParams cb) <> ")")
            ( [ "// The user data is the Core, set with the callback."
              , "auto* core_ptr = static_cast<detail::Core*>(" <> paramName (last (cbParams cb)) <> ");" ]
            ++ braced "if (core_ptr == nullptr)" ["return;"]
            ++ [ "auto& core = *core_ptr;" ]
            ++ concat [ braced ("if (!core." <> levelVar <> " || " <> paramName p <> " < static_cast<" <> cType (paramType p) <> ">(*core." <> levelVar <> "))") ["return;"]
                      | p@SParameter { paramType = SEnum _ } <- take 1 [ q | q <- cbParams cb, isEnum (paramType q) ] ]
            ++ pushEvent "core.pending.push(" e )
    isEnum SEnum{} = True
    isEnum _       = False

    cParams cb = zipWith cParam [0 :: Int ..] (cbParams cb)
      where
        cParam i p
            | i == 0 = cType (paramType p) <> " /*" <> paramName p <> "*/"
            | otherwise = cType (paramType p) <> " " <> paramName p

    cType t = case t of
        SHandle "void"  -> "void* _Nullable"
        -- The C handle: the class has its name.
        SHandle h       -> "::" <> maybe h cName (findResource model h) <> "* _Nonnull"
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
