{-# LANGUAGE OverloadedStrings #-}

-- | Protobuf slice of the JVM binding generator.
--
-- Emits @lib/src/main/proto/Core.proto@ and @lib/src/main/proto/Av.proto@.
--
-- The protobuf middle layer is /load-bearing for Android/ (see
-- @project_jvm_event_layer_protobuf@). Native code reaches Kotlin event
-- listeners via runtime-resolved methodIDs, which is invisible to
-- ProGuard/R8 reachability analysis. Funneling every event through a
-- protobuf message that the Kotlin side decodes keeps the call graph
-- discoverable.
--
-- Schema shape:
--
--   * One @message@ per non-error enum, with its values in a nested
--     @enum Type@ (lets fields qualify them as @Connection.Type@).
--   * One @message@ per callback event, fields mirror the callback
--     parameters minus @SHandle@ receivers and @user_data@.
--   * A top-level @CoreEvents@ / @AvEvents@ with a @repeated Event events@
--     and a @oneof event_type@ over each callback message.
module Apigen.Language.Jvm.Proto (generate) where

import           Apigen.Language.Jvm.Conventions (optionsWireFields)
import           Apigen.Semantic (SCallbackTypeModel (..), SEnumMember (..),
                                  SEnumModel (..), SParameter (..),
                                  SProperty (..), SType (..),
                                  SemanticModel (..))
import qualified Data.Char       as Char
import qualified Data.List       as List
import           Data.Text       (Text)
import qualified Data.Text       as Text

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
        , enumName e /= "Tox_Event_Type" -- discriminator; lives in the oneof tag
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

-- | Set of enum PascalCase base names that clash with a callback
-- event message. Stored post-'stripTox'+'pascalCase' so both
-- 'enumName' ("Tox_Group_Join_Fail") and an 'SEnum' content
-- ("Group_Join_Fail") can be checked uniformly via the same
-- pascalCase'd form.
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

-- | The @Tox_Options@ wire message: ToxCoreImpl's init block
-- serializes the @ToxOptions@ data class into this and hands the
-- bytes to @toxNew@; the C++ shim decodes it and drives the
-- @tox_options_set_*@ family inside a single JNI call. Fields come
-- from 'optionsWireFields' so all three sides stay aligned.
--
-- Field numbers are positional. The blob never leaves the process
-- (both sides are built from the same model), so renumbering across
-- versions is harmless — wire compatibility is not a goal here.
-- Enums ride as their Kotlin ordinal (the same convention the JNI
-- @Enum::valueOf@ tables use), not as proto enums.
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

-- | One @message Foo { enum Type { ... } }@ per enum. The wrapper
-- message gives proto fields a qualified name (@Foo.Type@). When the
-- bare name would clash with a callback event message we append
-- @Kind@ — @Tox_Group_Join_Fail@ ⇒ @GroupJoinFailKind@.
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
        Nothing -> memberName

--------------------------------------------------------------------------------
-- Event messages (one per callback)
--------------------------------------------------------------------------------

-- | Look up an enum's wire-name (the @message Foo@ wrapper). Used to
-- translate @SEnum "Tox_Connection"@ -> @Connection.Type@. Names in
-- the 'colliders' list get a @Kind@ suffix to disambiguate from a
-- same-named callback message.
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
        _ -> True

-- | @Tox_Friend_Name_Cb@ → @FriendName@ — the @_Cb@ suffix is just a
-- type-tag marker on the C side, not semantically meaningful.
eventMessageName :: SCallbackTypeModel -> Text
eventMessageName c = pascalCase (stripCbSuffix (stripTox (cbName c)))

-- | @tox_friend_name_cb@ → @friend_name@.
eventFieldName :: SCallbackTypeModel -> Text
eventFieldName c = Text.toLower (stripCbSuffix (stripTox (cbName c)))

stripCbSuffix :: Text -> Text
stripCbSuffix n = case Text.stripSuffix "_Cb" n of
    Just rest -> rest
    Nothing -> case Text.stripSuffix "_cb" n of
        Just rest -> rest
        Nothing -> n

renderField :: [Text] -> Int -> SParameter -> Text
renderField colliders idx p =
    "  " <> protoType colliders (paramType p) <> " " <> Text.toLower (paramName p) <> " = " <> Text.pack (show idx) <> ";"

-- | Map an 'SType' to its protobuf wire type. Bytes/strings collapse
-- to @bytes@; enums become their wrapper-qualified @Name.Type@; lists
-- of bytes use @bytes@ too (audio PCM is serialized as a raw byte
-- buffer even though its element type is int16).
protoType :: [Text] -> SType -> Text
protoType colliders ty = case ty of
    SVoid -> "// void"
    SBool -> "bool"
    SInt 8 -> "int32"
    SInt 16 -> "int32"
    SInt 32 -> "int32"
    SInt 64 -> "int64"
    SInt _ -> "int32"
    SUInt 8 -> "uint32"
    SUInt 16 -> "uint32"
    SUInt 32 -> "uint32"
    SUInt 64 -> "uint64"
    SUInt _ -> "uint32"
    SSizeT -> "uint64"
    SString -> "string"
    SBytes -> "bytes"
    SFixedBytes _ _ -> "bytes"
    SFixedList _ _ _ -> "bytes"
    SList (SInt 8) -> "bytes"
    SList (SUInt 8) -> "bytes"
    SList _ -> "bytes"
    SEnum n -> enumWireName colliders n <> ".Type"
    SHandle _ -> "// SHandle skipped"
    SCallback _ -> "// SCallback skipped"
    SResourceId _ -> "uint32"

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

-- | Strip the subsystem prefix from a semantic name.
--
-- Enum names use the PascalCase @Tox_@ / @Toxav_@ form;
-- callback names use the snake_case @tox_@ / @toxav_@ form.
-- The semantic model keeps the @toxav_@ prefix on AV callbacks
-- because @commonPrefix@ is just @Tox@.
stripTox :: Text -> Text
stripTox n = foldr try n ["Toxav_", "Tox_", "toxav_", "tox_"]
  where
    try p acc = case Text.stripPrefix p n of
        Just rest -> rest
        Nothing -> acc

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t
