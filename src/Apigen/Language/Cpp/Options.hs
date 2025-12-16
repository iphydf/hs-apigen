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
    , optRead  :: Text    -- ^ Its value in @defaults@
    , optWrite :: Text    -- ^ Setting it in @c@
    , optFails :: Bool    -- ^ Whether setting it can fail
    }

-- | Not the log callback (the session sets it) or @experimental_owned_data@
-- (always on).
options :: SemanticModel -> [Option]
options model = fromMaybe [] $ do
    res : _ <- Just (K.handlesOfKind model SettingsHandle)
    return (mapMaybe (option res) (properties res))
  where
    option res p
        | K.isRawPointer (propType p) || p `elem` K.alwaysOnOptions model = Nothing
        | otherwise = do
            getter <- propRead p
            setter <- propWrite p
            ty <- typeOf model (propType p)
            let n = propName p
                get = getter <> "(defaults)"
                fails = maybe False ((== SBool) . output) (find ((== setter) . methodName) (methods res))
                (readV, writeArgs) = case propType p of
                    SString -> ("string(" <> get <> ")", ["options." <> n <> ".c_str()"])
                    SBytes ->
                        ( "bytes(" <> get <> ", " <> fromMaybe getter (propSize p) <> "(defaults))"
                        , ["options." <> n <> ".data()", "options." <> n <> ".size()"] )
                    SEnum e ->
                        ( fromC ty get
                        , ["static_cast<" <> maybe e enumName (findEnum model e) <> ">(options." <> n <> ")"] )
                    SScalar _ _ -> (fromC ty get, ["options." <> n <> ".value"])
                    _ -> (get, ["options." <> n])
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
      , header l [own l "types.hpp", "<cstdint>", "<string>", "<vector>"] $
            ["namespace " <> libNamespace l <> " {", ""]
            ++ docBlock (Just ("@brief The options a " <> rootType l <> " instance starts with."))
            ++ declaration "struct Options"
                ( docBlock (Just ("@brief " <> libName l <> "'s defaults.")) ++ ["Options() noexcept;", ""]
                ++ concat [ docBlock (summary <$> doc (optName o)) ++ [render (optType o) <> " " <> optName o <> ";", ""] | o <- opts ] )
            ++ ["", "} // namespace " <> libNamespace l] )
    , ( "src/generated/options.cpp"
      , source l ["generated/c_values.hpp", "generated/engine.hpp", own l "generated/options.hpp", own l "result.hpp", "<memory>", optionsInclude l] $
            ["namespace " <> libNamespace l <> " {", ""]
            ++ braced "Options::Options() noexcept"
                ( [ "const std::unique_ptr<" <> optionsType l <> ", void (*)(" <> optionsType l <> "* _Nullable)> c(" <> optionsNew l <> "(nullptr), " <> optionsFree l <> ");" ]
                ++ braced "if (c == nullptr)" ["detail::panic(\"no memory for " <> libName l <> "'s default options\");"]
                ++ [ "const " <> optionsType l <> "* _Nonnull defaults = c.get();" ]
                ++ [ optName o <> " = " <> optRead o <> ";" | o <- opts ] )
            ++ [""]
            ++ braced ("bool apply_options(const Options& options, " <> optionsType l <> "* _Nonnull c)")
                ( [ "// C copies the strings and bytes, so they needn't outlive this."
                  ] ++ [ setter <> "(c, true);" | setter <- alwaysOnSetters ]
                ++ [ optWrite o <> ";" | o <- opts, not (optFails o) ]
                ++ [ "return " <> allOf [ optWrite o | o <- opts, optFails o ] <> ";" ] )
            ++ ["", "} // namespace " <> libNamespace l] )
    ]
  where
    l = lib model
    opts = options model
    alwaysOnSetters = [ w | p <- K.alwaysOnOptions model, Just w <- [propWrite p] ]
    allOf [] = "true"
    allOf cs = Text.intercalate " && " cs
