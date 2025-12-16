{-# LANGUAGE OverloadedStrings #-}
-- | C++ bindings for cpp-toxcore-c: commands, events, proxies and the C API's
-- types.
module Apigen.Language.Cpp
    ( generate
    ) where

import qualified Apigen.Knowledge             as K
import           Apigen.Language.Cpp.Commands
import           Apigen.Language.Cpp.Crypto   (cryptoFiles)
import           Apigen.Language.Cpp.Enums    (enumFiles, errorFiles, idsHeader)
import           Apigen.Language.Cpp.Events   (allEvents, eventsHeader,
                                               eventsSource)
import           Apigen.Language.Cpp.Options  (optionsFiles)
import           Apigen.Language.Cpp.Proxies  (proxyFiles)
import           Apigen.Language.Cpp.Text
import           Apigen.Parser.Docs           (Docs (..))
import           Apigen.Semantic
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model =
    errorFiles memberDoc model
    ++ enumFiles memberDoc model
    ++ [ ("include/tox/generated/ids.hpp", idsHeader model)
       , ("include/tox/generated/commands.hpp", commandsHeader doc cmds)
       , ("src/generated/commands.cpp", executorSource cmds)
       , ("include/tox/generated/events.hpp", eventsHeader evs)
       , ("src/generated/events.cpp", eventsSource model evs)
       , ("src/generated/engine.hpp", engineHeader)
       , ("src/generated/c_values.hpp", cValuesHeader)
       ]
    ++ proxyFiles model doc cmds
    ++ optionsFiles optionDoc model
    ++ cryptoFiles doc model
  where
    cmds = commands model
    evs = allEvents doc model
    doc fn = Map.lookup fn (funcDocs docs)
    memberDoc enum member = Map.lookup (enum, member) (enumMemberDocs docs)
    optionDoc member = do
        settings : _ <- Just (K.handlesOfKind model SettingsHandle)
        Map.lookup (cName settings, member) (structMemberDocs docs)

-- | What the session calls of the generated code.
engineHeader :: Text
engineHeader = header
    [ "tox/commands.hpp", "tox/generated/options.hpp", "tox/session.hpp"
    , "<tox/tox.h>", "<tox/tox_events.h>", "<tox/toxav.h>" ]
    [ "namespace tox {"
    , ""
    , "/** @brief Runs a command, and pushes its result as an event. */"
    , "void execute_command(Tox* _Nonnull tox, ToxAV* _Nonnull av, Session::Impl& impl, AnyCommand& command);"
    , ""
    , "/** @brief Pushes the events of an iteration. */"
    , "void process_events(Session::Impl& impl, const Tox_Events* _Nonnull events);"
    , ""
    , "/** @brief Makes ToxAV's callbacks push their events. */"
    , "void register_av_callbacks(ToxAV* _Nonnull av, Session::Impl& impl);"
    , ""
    , "/** @brief Makes Tox's log messages events. */"
    , "void register_log_callback(Tox_Options* _Nonnull options, Session::Impl& impl);"
    , ""
    , "/** @brief Sets the options in C's, and returns whether there was memory for them. */"
    , "bool apply_options(const Options& options, Tox_Options* _Nonnull c);"
    , ""
    , "} // namespace tox"
    ]

cValuesHeader :: Text
cValuesHeader = header ["<algorithm>", "<cstddef>", "<cstdint>", "<string>", "<vector>"]
    [ "namespace tox {"
    , ""
    , "/** @brief The bytes C gives, which may be NULL when there are none. */"
    , "inline std::vector<uint8_t> bytes(const uint8_t* _Nullable data, size_t length)"
    , "{"
    , "    if (data == nullptr) {"
    , "        return {};"
    , "    }"
    , "    return {data, data + length};"
    , "}"
    , ""
    , "/** @brief A fixed-size byte array C gives. */"
    , "template <typename Array>"
    , "Array array(const uint8_t* _Nonnull data)"
    , "{"
    , "    Array array{};"
    , "    std::copy_n(data, array.data.size(), array.data.begin());"
    , "    return array;"
    , "}"
    , ""
    , "/** @brief A string C gives, which may be NULL when there is none. */"
    , "inline std::string string(const char* _Nullable s)"
    , "{"
    , "    return s != nullptr ? s : \"\";"
    , "}"
    , ""
    , "} // namespace tox"
    ]
