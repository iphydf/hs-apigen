{-# LANGUAGE OverloadedStrings #-}
-- | The C API's enums as C++ enums with the same values, and its IDs and
-- fixed-size byte arrays as distinct types.
module Apigen.Language.Cpp.Enums
    ( errorFiles
    , enumFiles
    , idsHeader
    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Namespace          (namespaces, splitNamespace,
                                            stripNamespace)
import           Apigen.Semantic
import           Data.List                 (nub, sortOn)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

type MemberDoc = Text -> Text -> Maybe Text

usedErrors :: SemanticModel -> [SEnumModel]
usedErrors model = sortOn (enumCppName model . enumName)
    [ e | e <- enums model, enumKind e == ErrorEnum, enumName e `elem` used ]
  where
    used = nub [ maybe n enumName (findEnum model n)
               | (_, m) <- allMethods model, Just n <- [methodErrorType m] ]

-- | Other enums a client sees: not a @tox_events@ event's type.
publicEnums :: SemanticModel -> [SEnumModel]
publicEnums model = sortOn (enumCppName model . enumName)
    [ e | e <- enums model, enumKind e /= ErrorEnum, enumName e `notElem` variantTags model ]

errorFiles :: MemberDoc -> SemanticModel -> [(FilePath, Text)]
errorFiles doc model =
    [ ( "include/tox/generated/errors.hpp"
      , header ["<iosfwd>", "<string>"] $ namespaced (concatMap (enumDecl doc model) errs) )
    , ( "src/generated/errors.cpp"
      , source ["tox/errors.hpp", "<ostream>", "<string>"] $ namespaced (concatMap (enumDefs model) errs) )
    ]
  where
    errs = usedErrors model

enumFiles :: MemberDoc -> SemanticModel -> [(FilePath, Text)]
enumFiles doc model =
    [ ( "include/tox/generated/enums.hpp"
      , header ["<cstdint>", "<iosfwd>", "<string>"] $ namespaced (concatMap (enumDecl doc model) es) )
    , ( "src/generated/enums.cpp"
      , source ["tox/types.hpp", "<array>", "<ostream>", "<string>", "<utility>"] $
            namespaced (concatMap (enumDefs model) es) )
    ]
  where
    es = publicEnums model

namespaced :: [Text] -> [Text]
namespaced body = ["namespace tox {", ""] ++ body ++ ["} // namespace tox"]

-- | Members of a C++ enum: an error enum's without OK, which a result has
-- instead.
members :: SEnumModel -> [SEnumMember]
members e = if enumKind e == ErrorEnum then errorMembers e else enumMembers e

enumDecl :: MemberDoc -> SemanticModel -> SEnumModel -> [Text]
enumDecl doc model e =
    declaration ("enum class " <> name <> underlying)
        (concat [ docBlock (summary <$> doc (enumName e) (enumMemberCName m))
                  ++ [enumeratorName (enumMemberName m) <> " = " <> value m <> ","]
                | m <- members e ])
    ++ [ "std::string to_string(" <> name <> " v);"
       , "std::ostream& operator<<(std::ostream& os, " <> name <> " v);"
       ]
    ++ flagOps
    ++ [""]
  where
    name = enumCppName model (enumName e)
    flags = enumKind e == FlagsEnum
    underlying = if enumKind e `elem` [OpenEnum, FlagsEnum] then " : uint32_t" else ""
    value m = Text.pack (show (enumMemberValue m))
    flagOps
        | not flags = []
        | otherwise =
            [ "constexpr " <> name <> " operator|(" <> name <> " a, " <> name <> " b) noexcept"
            , "{"
            , "    return static_cast<" <> name <> ">(static_cast<uint32_t>(a) | static_cast<uint32_t>(b));"
            , "}"
            , "constexpr " <> name <> " operator&(" <> name <> " a, " <> name <> " b) noexcept"
            , "{"
            , "    return static_cast<" <> name <> ">(static_cast<uint32_t>(a) & static_cast<uint32_t>(b));"
            , "}"
            ]

enumDefs :: SemanticModel -> SEnumModel -> [Text]
enumDefs model e =
    braced ("std::string to_string(" <> name <> " v)") body
    ++ braced ("std::ostream& operator<<(std::ostream& os, " <> name <> " v)") ["return os << to_string(v);"]
    ++ [""]
  where
    name = enumCppName model (enumName e)
    body
        | enumKind e == FlagsEnum = flagsBody
        | otherwise =
            ["switch (v) {"]
            ++ concat [ ["case " <> name <> "::" <> enumeratorName (enumMemberName m) <> ":", "    return \"" <> enumeratorName (enumMemberName m) <> "\";"]
                      | m <- members e ]
            ++ [ "}"
               -- C may give values these bindings don't know.
               , "return \"" <> name <> "(\" + std::to_string(static_cast<int>(v)) + \")\";"
               ]
    (zero, bits) = (filter ((== 0) . enumMemberValue) (members e), filter ((/= 0) . enumMemberValue) (members e))
    flagsBody =
        [ "static constexpr std::array<std::pair<" <> name <> ", const char*>, " <> Text.pack (show (length bits)) <> "> names{{" ]
        ++ indent [ "{" <> name <> "::" <> enumeratorName (enumMemberName m) <> ", \"" <> enumeratorName (enumMemberName m) <> "\"}," | m <- bits ]
        ++ [ "}};"
           , "std::string s;"
           ]
        ++ braced "for (const auto& [flag, flag_name] : names)"
            (braced "if ((v & flag) == flag)"
                (braced "if (!s.empty())" ["s += '|';"] ++ ["s += flag_name;"]))
        ++ [ "return s.empty() ? \"" <> zeroName <> "\" : s;" ]
    zeroName = case zero of
        m : _ -> enumeratorName (enumMemberName m)
        []    -> "0"

-- | @tox/generated/ids.hpp@: the ID and byte array types.
idsHeader :: SemanticModel -> Text
idsHeader model = header ["tox/strong_types.hpp", "<algorithm>", "<cstdint>"] $
    namespaced $
        [ "using " <> idTypeName (idName i) <> " = StrongId<" <> render' (idType i) <> ", struct " <> idTypeName (idName i) <> "Tag>;"
        | i <- sortOn idName (idTypes model) ]
        ++ [""]
        ++ [ "using " <> arrayTypeName n <> " = " <> template n <> "<" <> size c <> ", struct " <> arrayTypeName n <> "Tag>;"
           | (c, n) <- sortOn snd (arrayTypes model) ]
        ++ [""]
        ++ concat (mapMaybe prefixFn (K.arrayPrefixes model))
  where
    render' t = maybe "uint32_t" render (typeOf model t)
    size c = maybe c (Text.pack . show) (defineValue model c)
    nss = namespaces model
    template n = if n `elem` map (stripNamespace nss) (K.secretArrays model) then "SecretByteArray" else "ByteArray"
    prefixFn (whole, part) = do
        (_, w) <- splitNamespace nss whole
        (_, p) <- splitNamespace nss part
        let fn = unwords' (words' p)
        return $
            docBlock (Just ("@brief The " <> Text.replace "_" " " fn <> " " <> article w <> " " <> Text.toLower (Text.replace "_" " " w) <> " starts with."))
            ++ braced ("inline " <> arrayTypeName p <> " " <> fn <> "(const " <> arrayTypeName w <> "& " <> unwords' (words' w) <> ")")
                [ arrayTypeName p <> " " <> fn <> "{};"
                , "std::copy_n(" <> unwords' (words' w) <> ".data.begin(), " <> fn <> ".data.size(), " <> fn <> ".data.begin());"
                , "return " <> fn <> ";"
                ]
            ++ [""]
    article w = if Text.toLower (Text.take 1 w) `elem` ["a", "e", "i", "o", "u"] then "an" else "a"
