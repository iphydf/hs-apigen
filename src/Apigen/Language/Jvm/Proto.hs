{-# LANGUAGE OverloadedStrings #-}

-- | @Core.proto@ and @Av.proto@.
--
-- Events reach Kotlin as proto messages rather than JNI calls into
-- listeners, which ProGuard/R8 can't see.
module Apigen.Language.Jvm.Proto (generate) where

import           Apigen.Language.Jvm.Conventions (optionsWireFields)
import           Apigen.Semantic                 (SCallbackTypeModel (..),
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
            ++ concatMap (renderEnumMessage colliders) (List.sortOn enumName coreEnums)
            ++ renderOptionsMessage model
            ++ concatMap (renderEventMessage colliders) coreCallbacks
            ++ renderTopLevel "CoreEvents" colliders coreCallbacks
  where
    coreEnums =
        [ e
        | e <- enums model
        , not (isError e)
        , not (isAvEnum e)
        , enumName e /= "Tox_Event_Type" -- the oneof tag
        ]
    coreCallbacks =
        [c | c <- callbacks model, not (isAvCallback c)]
    colliders = collidingEnumNames coreCallbacks coreEnums

avProto :: SemanticModel -> Text
avProto model =
    Text.unlines $
        protoHeader "im.tox.tox4j.av.proto"
            ++ concatMap (renderEnumMessage colliders) (List.sortOn enumName avEnums)
            ++ concatMap (renderEventMessage colliders) avCallbacks
            ++ renderTopLevel "AvEvents" colliders avCallbacks
  where
    avEnums = [e | e <- enums model, not (isError e), isAvEnum e]
    avCallbacks = [c | c <- callbacks model, isAvCallback c]
    colliders = collidingEnumNames avCallbacks avEnums

-- | Enum names, in proto form, that clash with an event message.
collidingEnumNames :: [SCallbackTypeModel] -> [SEnumModel] -> [Text]
collidingEnumNames cbs es =
    [ pascalCase (stripTox (enumName e))
    | e <- es
    , pascalCase (stripTox (enumName e)) `elem` cbBaseNames
    ]
  where
    cbBaseNames =
        [ pascalCase (stripCbSuffix (stripTox (cbName c)))
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

-- | The @Tox_Options@ passed to @toxNew@. Field numbers are positional:
-- the message never leaves the process. Enums are ordinals.
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
isError e =
    "Tox_Err_" `Text.isPrefixOf` enumName e
        || "Toxav_Err_" `Text.isPrefixOf` enumName e

isAvEnum :: SEnumModel -> Bool
isAvEnum e = "Toxav_" `Text.isPrefixOf` enumName e

isAvCallback :: SCallbackTypeModel -> Bool
isAvCallback c = "toxav_" `Text.isPrefixOf` cbCName c

-- | @message Foo { enum Type { ... } }@, so fields can say @Foo.Type@.
renderEnumMessage :: [Text] -> SEnumModel -> [Text]
renderEnumMessage colliders e =
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
    messageName = enumWireName colliders (enumName e)

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
enumWireName :: [Text] -> Text -> Text
enumWireName colliders n
    | base `elem` colliders = base <> "Kind"
    | otherwise = base
  where
    base = pascalCase (stripTox n)

renderEventMessage :: [Text] -> SCallbackTypeModel -> [Text]
renderEventMessage colliders c =
    [ "message " <> eventMessageName c <> " {" ]
        ++ zipWith (renderField colliders) [1 :: Int ..] (filter keepParam (cbParams c))
        ++ [ "}"
           , ""
           ]
  where
    keepParam p = case paramType p of
        SHandle _ -> False
        _         -> True

-- | @Tox_Friend_Name_Cb@: @FriendName@.
eventMessageName :: SCallbackTypeModel -> Text
eventMessageName c = pascalCase (stripCbSuffix (stripTox (cbName c)))

-- | @tox_friend_name_cb@: @friend_name@.
eventFieldName :: SCallbackTypeModel -> Text
eventFieldName c = Text.toLower (stripCbSuffix (stripTox (cbName c)))

stripCbSuffix :: Text -> Text
stripCbSuffix n = case Text.stripSuffix "_Cb" n of
    Just rest -> rest
    Nothing -> case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing   -> n

renderField :: [Text] -> Int -> SParameter -> Text
renderField colliders idx p =
    "  " <> protoType colliders (paramType p) <> " " <> Text.toLower (paramName p) <> " = " <> Text.pack (show idx) <> ";"

-- | Lists are @bytes@, including 16-bit audio samples.
protoType :: [Text] -> SType -> Text
protoType colliders ty = case ty of
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
    SEnum n          -> enumWireName colliders n <> ".Type"
    SHandle _        -> "// SHandle skipped"
    SCallback _      -> "// SCallback skipped"
    SResourceId _    -> "uint32"

--------------------------------------------------------------------------------
-- Top-level events oneof
--------------------------------------------------------------------------------

renderTopLevel :: Text -> [Text] -> [SCallbackTypeModel] -> [Text]
renderTopLevel topName _colliders cs =
    [ "message " <> topName <> " {"
    , "  repeated Event events = 1;"
    , ""
    , "  message Event {"
    , "    oneof event_type {"
    ]
        ++ [ "      " <> eventMessageName c <> " " <> eventFieldName c <> " = " <> Text.pack (show idx) <> ";"
           | (idx, c) <- zip [1 :: Int ..] cs
           ]
        ++ [ "    }"
           , "  }"
           , "}"
           ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Strip @Tox_@, @Toxav_@, @tox_@ or @toxav_@.
stripTox :: Text -> Text
stripTox n = foldr try n ["Toxav_", "Tox_", "toxav_", "tox_"]
  where
    try p acc = case Text.stripPrefix p n of
        Just rest -> rest
        Nothing   -> acc

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t
