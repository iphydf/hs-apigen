{-# LANGUAGE OverloadedStrings #-}
-- | C++ bindings for cpp-toxcore-c: the root handle's class, the events it
-- gives, and the C API's types.
module Apigen.Language.Cpp
    ( generate
    ) where

import qualified Apigen.Knowledge            as K
import           Apigen.Language.Cpp.Crypto  (cryptoFiles)
import           Apigen.Language.Cpp.Enums   (enumFiles, errorFiles, idsHeader)
import           Apigen.Language.Cpp.Events  (allEvents, eventsHeader,
                                              eventsSource)
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Methods (rootHeader, rootMethods,
                                              rootSource)
import           Apigen.Language.Cpp.Options (optionsFiles)
import           Apigen.Language.Cpp.Text
import           Apigen.Parser.Docs          (Docs (..))
import           Apigen.Semantic
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model =
    errorFiles memberDoc model
    ++ enumFiles memberDoc model
    ++ [ (generatedHeader l "ids.hpp", idsHeader model)
       , (generatedHeader l (Text.unpack (rootVar l) <> ".hpp"), rootHeader l doc ms)
       , ("src/generated/" <> Text.unpack (rootVar l) <> ".cpp", rootSource l ms)
       , (generatedHeader l "events.hpp", eventsHeader l evs)
       , ("src/generated/events.cpp", eventsSource model evs)
       , ("src/generated/convert.hpp", convertHeader l)
       , ("src/generated/c_values.hpp", cValuesHeader l)
       ]
    ++ optionsFiles optionDoc model
    ++ cryptoFiles doc model
  where
    l = lib model
    ms = rootMethods model
    evs = allEvents doc model
    doc fn = Map.lookup fn (funcDocs docs)
    memberDoc enum member = Map.lookup (enum, member) (enumMemberDocs docs)
    optionDoc member = do
        settings : _ <- Just (K.handlesOfKind model SettingsHandle)
        Map.lookup (cName settings, member) (structMemberDocs docs)

-- | What the hand-written class calls of the generated code.
convertHeader :: Lib -> Text
convertHeader l = header l
    [ "core.hpp", own l "generated/events.hpp", own l "generated/options.hpp"
    , eventsInclude l, optionsInclude l, "<vector>" ]
    $ namespaced l
    $ docBlock (Just "@brief Sets the options in C's, and returns whether there was memory for\nthem. C keeps pointers into `options`, so they must outlive `c`'s use.")
    ++ [ "bool apply_options(const Options& options, " <> optionsType l <> "* _Nonnull c);", "" ]
    ++ docBlock (Just "@brief Appends the events of an iteration.")
    ++ [ "void append_events(std::vector<Event>& out, const " <> eventsType l <> "* _Nonnull events);", "" ]
    ++ docBlock (Just ("@brief Makes " <> rootType l <> "'s " <> callbackWord l <> " messages events of `core`."))
    ++ [ "void set_" <> callbackWord l <> "_callback(" <> optionsType l <> "* _Nonnull options, detail::Core& core);", "" ]

cValuesHeader :: Lib -> Text
cValuesHeader l = header l ["<algorithm>", "<cstddef>", "<cstdint>", "<string>", "<vector>"]
    [ "namespace " <> libNamespace l <> "::c {"
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
    , "} // namespace " <> libNamespace l <> "::c"
    ]
