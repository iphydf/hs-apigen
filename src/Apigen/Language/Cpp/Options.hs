{-# LANGUAGE OverloadedStrings #-}
-- | Options a Tox instance starts with, as a struct.
module Apigen.Language.Cpp.Options
    ( optionsFiles
    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Semantic
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

data Option = Option
    { optName  :: Text
    , optType  :: CppType
    , optRead  :: Maybe Text  -- ^ Its value in @defaults@, unless it has none
    , optWrite :: Text    -- ^ Setting it in @c@
    , optFails :: Bool    -- ^ Whether setting it can fail
    }

-- | Not the log callback (the class sets it), @experimental_owned_data@
-- (set for the class's use), or C's locking (always on: see
-- 'K.lockingOptions').
options :: SemanticModel -> [Option]
options model = fromMaybe [] $ do
    res : _ <- Just (K.handlesOfKind model SettingsHandle)
    return (mapMaybe (option res) (properties res))
  where
    option res p
        | K.isRawPointer (propType p) || p `elem` K.alwaysOnOptions model || p `elem` K.lockingOptions model = Nothing
        | otherwise = do
            getter <- propRead p
            setter <- propWrite p
            let secret = propType p == SBytes && K.isSecretBytes setter
            ty <- if secret then Just TSecretBytes else typeOf model (propType p)
            let n = propName p
                get = getter <> "(defaults)"
                fails = maybe False ((== SBool) . output) (find ((== setter) . methodName) (methods res))
                (readV, writeArgs) = case propType p of
                    SString -> (Just ("c::string(" <> get <> ")"), ["options." <> n <> ".c_str()"])
                    -- A secret has no default to copy.
                    SBytes | secret -> (Nothing, ["options." <> n <> ".data()", "options." <> n <> ".size()"])
                    SBytes ->
                        ( Just ("c::bytes(" <> get <> ", " <> fromMaybe getter (propSize p) <> "(defaults))")
                        , ["options." <> n <> ".data()", "options." <> n <> ".size()"] )
                    SEnum e ->
                        ( Just (fromC ty get)
                        , ["static_cast<" <> maybe e enumName (findEnum model e) <> ">(options." <> n <> ")"] )
                    SScalar _ _ -> (Just (fromC ty get), ["options." <> n <> ".value"])
                    _ -> (Just get, ["options." <> n])
            return Option
                { optName  = n
                , optType  = ty
                , optRead  = readV
                , optWrite = setter <> "(c, " <> commaSep writeArgs <> ")"
                , optFails = fails
                }

commaSep :: [Text] -> Text
commaSep = Text.intercalate ", "

optionsFiles :: (Text -> Maybe Text) -> SemanticModel -> [(FilePath, Text)]
optionsFiles doc model =
    [ ( generatedHeader l "options.hpp"
      , header l ([own l "types.hpp", "<cstdint>", "<string>"] ++ [ "<vector>" | any ((== TBytes) . optType) opts ]) $
            ["namespace " <> libNamespace l <> " {", ""]
            ++ docBlock (Just ("@brief The options a " <> rootType l <> " instance starts with."))
            ++ declaration "struct Options"
                ( docBlock (Just ("@brief " <> libName l <> "'s defaults.")) ++ ["Options() noexcept;", ""]
                ++ concat [ docBlock (summary <$> doc (optName o)) ++ [render (optType o) <> " " <> optName o <> initial (optType o) <> ";", ""] | o <- opts ] )
            ++ ["", "} // namespace " <> libNamespace l] )
    , ( "src/generated/options.cpp"
      , source l ["generated/c_values.hpp", "generated/convert.hpp", own l "generated/options.hpp", "panic.hpp", "<memory>", optionsInclude l] $
            ["namespace " <> libNamespace l <> " {", ""]
            ++ braced "Options::Options() noexcept"
                ( [ "const std::unique_ptr<" <> optionsType l <> ", void (*)(" <> optionsType l <> "* _Nullable)> c(" <> optionsNew l <> "(nullptr), " <> optionsFree l <> ");" ]
                ++ braced "if (c == nullptr)" ["detail::panic(\"no memory for " <> libName l <> "'s default options\");"]
                ++ [ "const " <> optionsType l <> "* _Nonnull defaults = c.get();" ]
                ++ [ optName o <> " = " <> r <> ";" | o <- opts, Just r <- [optRead o] ] )
            ++ [""]
            ++ braced ("bool apply_options(const Options& options, " <> optionsType l <> "* _Nonnull c)")
                ( [ "// C keeps pointers to the strings and bytes, which outlive its use of"
                  , "// them, so it makes no copy of a secret that isn't erased."
                  ] ++ [ setter <> "(c, false);" | setter <- alwaysOnSetters ]
                ++ lockingComment
                ++ [ setter <> "(c, true);" | setter <- lockingSetters ]
                ++ [ optWrite o <> ";" | o <- opts, not (optFails o) ]
                ++ [ "return " <> allOf [ optWrite o | o <- opts, optFails o ] <> ";" ] )
            ++ ["", "} // namespace " <> libNamespace l] )
    ]
  where
    l = lib model
    opts = options model
    -- Numbers, flags and enums start as zero; the classes start empty.
    initial t = case t of
        TString      -> ""
        TBytes       -> ""
        TSecretBytes -> ""
        _            -> "{}"
    alwaysOnSetters = [ w | p <- K.alwaysOnOptions model, Just w <- [propWrite p] ]
    lockingSetters = [ w | p <- K.lockingOptions model, Just w <- [propWrite p] ]
    lockingComment
        | null lockingSetters = []
        | otherwise =
            [ "// Another library's threads call into the instance behind the"
            , "// class's back. The class still keeps it to one thread at a time."
            ]
    allOf [] = "true"
    allOf cs = Text.intercalate " && " cs
