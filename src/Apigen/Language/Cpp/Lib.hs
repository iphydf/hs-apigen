{-# LANGUAGE OverloadedStrings #-}
-- | The C library's names the C++ templates use, from the model: its
-- handles, headers, and the few functions the class calls itself.
module Apigen.Language.Cpp.Lib
    ( Lib (..)
    , lib
    , namespaced
    , generatedHeader
    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Names (pascal)
import           Apigen.Language.Cpp.Types (enumCppName)
import           Apigen.Model              (headerOf)
import           Apigen.Namespace          (namespaces, resourceNamespace,
                                            rootNamespace, shortNamespace)
import           Apigen.Semantic
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           System.FilePath           (takeDirectory, takeFileName)

data Lib = Lib
    { libNamespace    :: Text  -- ^ @tox@
    , libName         :: Text  -- ^ The root's directory: @toxcore@.
    , rootType        :: Text  -- ^ @Tox@
    , rootVar         :: Text  -- ^ @tox@
    , avType          :: Text  -- ^ Another library's handle: @ToxAV@.
    , avVar           :: Text  -- ^ @av@
    , eventsType      :: Text  -- ^ @Tox_Events@
    , eventType       :: Text  -- ^ @Tox_Event@
    , optionsType     :: Text  -- ^ @Tox_Options@
    , passKeyType     :: Text  -- ^ @Tox_Pass_Key@
    , rootInclude     :: Text  -- ^ @<tox/tox.h>@
    , avInclude       :: Text
    , eventsInclude   :: Text
    , optionsInclude  :: Text
    , logLevelInclude :: Text
    , cryptoInclude   :: Text
    , eventsSize      :: Text  -- ^ @tox_events_get_size@
    , eventsGet       :: Text  -- ^ @tox_events_get@
    , eventGetType    :: Text  -- ^ @tox_event_get_type@
    , optionsNew      :: Text
    , optionsFree     :: Text
    , setLogCallback  :: Text  -- ^ @tox_options_set_log_callback@
    , setLogUserData  :: Text
    , callbackWord    :: Text  -- ^ The settings' callback's word: @log@ for @tox_log_cb@
    , passKeyFree     :: Text
    , avClass         :: Text  -- ^ The other library's C++ class: @Av@.
    , logLevelType    :: Text  -- ^ The level the log callback gives: @LogLevel@.
    , newErrorType    :: Text  -- ^ The root constructor's error: @NewError@.
    }

lib :: SemanticModel -> Lib
lib model = Lib
    { libNamespace   = cppNamespace (targets model)
    , libName        = Text.pack (takeFileName (takeDirectory (Text.unpack (resourceFile root))))
    , rootType       = cName root
    , rootVar        = var root
    , avType         = maybe "" cName av
    , avVar          = maybe "" var av
    , eventsType     = maybe "" cName eventsRes
    , eventType      = maybe "" cName eventRes
    , optionsType    = maybe "" cName settings
    , passKeyType    = maybe "" cName passKey
    , rootInclude     = include (resourceFile root)
    , avInclude       = maybe "" (include . resourceFile) av
    , eventsInclude  = maybe "" (include . resourceFile) eventsRes
    , optionsInclude  = maybe "" (include . resourceFile) settings
    , logLevelInclude = fromMaybe "" logLevel
    , cryptoInclude  = maybe "" (include . resourceFile) passKey
    , eventsSize     = fromMaybe "" (iterated iterSize)
    , eventsGet      = fromMaybe "" (iterated iterAccessor)
    , eventGetType   = maybe "" ((<> "get_type") . cPrefix) eventRes
    , optionsNew     = role Constructor settings
    , optionsFree    = role Destructor settings
    , setLogCallback = setter isCallback
    , setLogUserData = setter (== SHandle "void")
    , callbackWord   = fromMaybe "" $ listToMaybe
        [ fromMaybe n (Text.stripSuffix "_cb" n)
        | r <- maybe [] pure settings, cb <- K.registeredCallbacks model r, let n = Text.toLower (cbName cb) ]
    , passKeyFree    = role Destructor passKey
    , avClass        = maybe "" (pascal . pure . var) av
    , logLevelType   = fromMaybe "" $ listToMaybe
        [ enumCppName model n
        | r <- maybe [] pure settings, cb <- K.registeredCallbacks model r
        , SEnum n <- map paramType (cbParams cb) ]
    , newErrorType   = fromMaybe "" $ listToMaybe
        [ enumCppName model e | m <- methods root, methodRole m == Constructor, Just e <- [methodErrorType m] ]
    }
  where
    root = fromMaybe (error "Cpp.Lib: no root") (find isRoot (resources model))
    -- As the methods name a handle: its library's short word.
    nss = namespaces model
    var r = shortNamespace nss (fromMaybe (rootNamespace model) (resourceNamespace nss r))
    one = listToMaybe . K.handlesOfKind model
    av = one LibraryHandle
    eventsRes = one ViewList
    settings = one SettingsHandle
    passKey = one StandaloneHandle
    eventRes = do
        r <- eventsRes
        Iterable { iterElement = SHandle e } <- find isIterable (traits r)
        find ((== e) . resourceName) (resources model)
    isIterable Iterable{} = True
    iterated field = do
        r <- eventsRes
        t@Iterable{} <- find isIterable (traits r)
        return (cPrefix r <> field t)
    role rl = maybe "" (\r -> maybe "" methodName (find ((== rl) . methodRole) (methods r)))
    setter p = fromMaybe "" $ do
        r <- settings
        prop <- find (p . propType) (properties r)
        propWrite prop
    isCallback SCallback{} = True
    isCallback _           = False
    -- The header of the level the log callback gives.
    logLevel = listToMaybe
        [ include (enumFile e)
        | r <- maybe [] pure settings, cb <- K.registeredCallbacks model r
        , SEnum n <- map paramType (cbParams cb)
        , e <- enums model, enumName e == n || enumSemanticName e == n ]
    include file = "<" <> includeDir (targets model) <> "/" <> headerOf file <> ">"

-- | In the binding's namespace.
namespaced :: Lib -> [Text] -> [Text]
namespaced l body = ["namespace " <> libNamespace l <> " {", ""] ++ body ++ ["} // namespace " <> libNamespace l]

-- | A generated public header: @include/tox/generated/ids.hpp@.
generatedHeader :: Lib -> FilePath -> FilePath
generatedHeader l name = "include/" <> Text.unpack (libNamespace l) <> "/generated/" <> name
