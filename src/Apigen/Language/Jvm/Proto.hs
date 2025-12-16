{-# LANGUAGE OverloadedStrings #-}

-- | @Core.proto@ and @Av.proto@.
--
-- Events reach Kotlin as proto messages, not JNI calls into listeners,
-- which ProGuard/R8 can't see.
module Apigen.Language.Jvm.Proto (generate) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.Jvm.Conventions (optionsWireFields)
import           Apigen.Language.Jvm.Model       (inOtherLibrary)
import           Apigen.Model                    (callbackIn, enumIn,
                                                  variantTags)
import           Apigen.Namespace                (stripNamespace)
import           Apigen.Semantic                 (SCallbackTypeModel (..),
                                                  SEnumKind (..),
                                                  SEnumMember (..),
                                                  SEnumModel (..),
                                                  SParameter (..),
                                                  SProperty (..), SType (..),
                                                  SemanticModel (..))
import qualified Data.Char                       as Char
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ ("lib/src/main/proto/Core.proto", coreProto model)
    , ("lib/src/main/proto/Av.proto", avProto model)
    ]

--------------------------------------------------------------------------------
-- Core.proto
--------------------------------------------------------------------------------

coreProto :: SemanticModel -> Text
coreProto model =
    Text.unlines $
        protoHeader "im.tox.tox4j.core.proto"
            ++ concatMap (renderEnumMessage model colliders) (List.sortOn enumName coreEnums)
            ++ renderOptionsMessage model
            ++ concatMap (renderEventMessage model colliders) coreCallbacks
            ++ renderTopLevel model "CoreEvents" colliders coreCallbacks
  where
    coreEnums =
        [ e
        | e <- enums model
        , not (isError e)
        , not (isAvEnum model e)
        , enumName e `notElem` variantTags model -- the oneof tag
        ]
    coreCallbacks =
        [c | c <- callbacks model, not (isAvCallback model c)]
    colliders = collidingEnumNames model coreCallbacks coreEnums

avProto :: SemanticModel -> Text
avProto model =
    Text.unlines $
        protoHeader "im.tox.tox4j.av.proto"
            ++ concatMap (renderEnumMessage model colliders) (List.sortOn enumName avEnums)
            ++ concatMap (renderEventMessage model colliders) avCallbacks
            ++ renderTopLevel model "AvEvents" colliders avCallbacks
  where
    avEnums = [e | e <- enums model, not (isError e), isAvEnum model e]
    avCallbacks = [c | c <- callbacks model, isAvCallback model c]
    colliders = collidingEnumNames model avCallbacks avEnums

-- | Enum names, in proto form, that clash with an event message.
collidingEnumNames :: SemanticModel -> [SCallbackTypeModel] -> [SEnumModel] -> [Text]
collidingEnumNames model cbs es =
    [ pascalCase (stripTox model (enumName e))
    | e <- es
    , pascalCase (stripTox model (enumName e)) `elem` cbBaseNames
    ]
  where
    cbBaseNames =
        [ pascalCase (stripCbSuffix (stripTox model (cbName c)))
        | c <- cbs
        ]

protoHeader :: Text -> [Text]
protoHeader pkg =
    [ "syntax = \"proto3\";"
    , ""
    , "package " <> pkg <> ";"
    , ""
    , "option java_multiple_files = true;"
    , "option optimize_for = LITE_RUNTIME;"
    , ""
    ]

--------------------------------------------------------------------------------
-- Options message
--------------------------------------------------------------------------------

-- | @Tox_Options@ for @toxNew@. Field numbers are positional (the message
-- never leaves the process), and enums are ordinals.
renderOptionsMessage :: SemanticModel -> [Text]
renderOptionsMessage model =
    [ "message Options {" ]
        ++ [ "  " <> fieldType (propType p) <> " " <> propName p <> " = " <> Text.pack (show n) <> ";"
           | (n, p) <- zip [1 :: Int ..] (optionsWireFields model)
           ]
        ++ [ "}"
           , ""
           ]
  where
    fieldType ty = case ty of
        SBool   -> "bool"
        SEnum _ -> "int32"
        SString -> "string"
        SBytes  -> "bytes"
        SUInt _ -> "uint32"
        SInt _  -> "int32"
        SSizeT  -> "uint64"
        _       -> "uint32"

--------------------------------------------------------------------------------
-- Enum messages
--------------------------------------------------------------------------------

isError :: SEnumModel -> Bool
isError e = enumKind e == ErrorEnum

-- | Another library's enum or callback, like toxav's.
isAvEnum :: SemanticModel -> SEnumModel -> Bool
isAvEnum model e = inOtherLibrary model (enumFile e)

isAvCallback :: SemanticModel -> SCallbackTypeModel -> Bool
isAvCallback model c = inOtherLibrary model (cbFile c)

-- | @message Foo { enum Type { ... } }@, so fields can say @Foo.Type@.
renderEnumMessage :: SemanticModel -> [Text] -> SEnumModel -> [Text]
renderEnumMessage model colliders e =
    [ "message " <> messageName <> " {"
    , "  enum Type {"
    ]
        ++ [ "    " <> stripEnumPrefix (enumName e) (enumMemberCName m) <> " = " <> Text.pack (show idx) <> ";"
           | (idx, m) <- zip [0 :: Int ..] (enumMembers e)
           ]
        ++ [ "  }"
           , "}"
           , ""
           ]
  where
    messageName = enumWireName model colliders (enumName e)

stripEnumPrefix :: Text -> Text -> Text
stripEnumPrefix typeName memberName =
    let prefix = Text.toUpper typeName <> "_"
    in case Text.stripPrefix prefix memberName of
        Just rest -> rest
        Nothing   -> memberName

--------------------------------------------------------------------------------
-- Event messages (one per callback)
--------------------------------------------------------------------------------

-- | A name clashing with an event message gets a @Kind@ suffix.
enumWireName :: SemanticModel -> [Text] -> Text -> Text
enumWireName model colliders n
    | base `elem` colliders = base <> "Kind"
    | otherwise = base
  where
    base = pascalCase (stripTox model n)

renderEventMessage :: SemanticModel -> [Text] -> SCallbackTypeModel -> [Text]
renderEventMessage model colliders c =
    [ "message " <> eventMessageName model c <> " {" ]
        ++ zipWith (renderField model colliders) [1 :: Int ..] (filter keepParam (cbParams c))
        ++ [ "}"
           , ""
           ]
  where
    keepParam p = case paramType p of
        SHandle _ -> False
        _         -> True

eventMessageName :: SemanticModel -> SCallbackTypeModel -> Text
eventMessageName model c = pascalCase (stripCbSuffix (stripTox model (cbName c)))

eventFieldName :: SemanticModel -> SCallbackTypeModel -> Text
eventFieldName model c = Text.toLower (stripCbSuffix (stripTox model (cbName c)))

stripCbSuffix :: Text -> Text
stripCbSuffix n = case Text.stripSuffix "_Cb" n of
    Just rest -> rest
    Nothing -> case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing   -> n

renderField :: SemanticModel -> [Text] -> Int -> SParameter -> Text
renderField model colliders idx p =
    "  " <> protoType model colliders (paramType p) <> " " <> Text.toLower (paramName p) <> " = " <> Text.pack (show idx) <> ";"

-- | Lists are @bytes@, including 16-bit audio samples.
protoType :: SemanticModel -> [Text] -> SType -> Text
protoType model colliders ty = case ty of
    SVoid            -> "// void"
    SBool            -> "bool"
    SInt 8           -> "int32"
    SInt 16          -> "int32"
    SInt 32          -> "int32"
    SInt 64          -> "int64"
    SInt _           -> "int32"
    SUInt 8          -> "uint32"
    SUInt 16         -> "uint32"
    SUInt 32         -> "uint32"
    SUInt 64         -> "uint64"
    SUInt _          -> "uint32"
    SSizeT           -> "uint64"
    SString          -> "string"
    SBytes           -> "bytes"
    SFixedBytes _ _  -> "bytes"
    SFixedList _ _ _ -> "bytes"
    SList (SInt 8)   -> "bytes"
    SList (SUInt 8)  -> "bytes"
    SList _          -> "bytes"
    SEnum n          -> enumWireName model colliders n <> ".Type"
    SHandle _        -> "// SHandle skipped"
    SCallback _      -> "// SCallback skipped"
    SResourceId _    -> "uint32"

--------------------------------------------------------------------------------
-- Top-level events oneof
--------------------------------------------------------------------------------

renderTopLevel :: SemanticModel -> Text -> [Text] -> [SCallbackTypeModel] -> [Text]
renderTopLevel model topName _colliders cs =
    [ "message " <> topName <> " {"
    , "  repeated Event events = 1;"
    , ""
    , "  message Event {"
    , "    oneof event_type {"
    ]
        ++ [ "      " <> eventMessageName model c <> " " <> eventFieldName model c <> " = " <> Text.pack (show idx) <> ";"
           | (idx, c) <- zip [1 :: Int ..] cs
           ]
        ++ [ "    }"
           , "  }"
           , "}"
           ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | A name without its library's word.
stripTox :: SemanticModel -> Text -> Text
stripTox model = stripNamespace (K.libraryWords model)

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t
