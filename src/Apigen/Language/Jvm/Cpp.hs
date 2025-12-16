{-# LANGUAGE OverloadedStrings #-}

-- | C++ slice of the JVM binding generator.
--
-- Emits the mechanical pieces of
-- @lib/src/main/cpp/{ToxCore,ToxAv,ToxCrypto}/generated/@:
--
--   * @enums.cpp@   — @Enum::ordinal@/@Enum::valueOf@ specializations
--                     so the Kotlin ordinal ↔ C enum mapping stays
--                     in sync.
--   * @errors.cpp@  — @HANDLE@ macro switch per @Tox_Err_*@ enum,
--                     translating the C error to the Java exception
--                     code.
--   * @constants.h@ — @static_assert@ per JNI-visible constant so
--                     a Kotlin-side value drifting from C is caught
--                     at compile time.
--
-- The bigger @impls.h@ / @natives.h@ / JAVA_METHOD body emission still
-- lives on the TODO list — those depend on the same method enumeration
-- as the Java sub-module and are mechanical from there.
module Apigen.Language.Jvm.Cpp (generate) where

import           Apigen.Language.Jvm.Conventions (boolReturnsValue,
                                                  cppExtraNativeRefs,
                                                  errorEnumIsReachable,
                                                  interfaceAllowsStatics,
                                                  isHiddenFromPublicApi,
                                                  optionsWireFields,
                                                  syntheticExceptionFor)
import           Apigen.Semantic (SConstantModel (..), SEnumMember (..),
                                  SEnumModel (..), SMethod (..),
                                  SProperty (..),
                                  SMethodRole (..), SParameter (..),
                                  SResource (..), SResourceType (..),
                                  SType (..), SemanticModel (..))
import qualified Data.Char       as Char
import qualified Data.List       as List
import           Data.Text       (Text)
import qualified Data.Text       as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ ("lib/src/main/cpp/ToxCore/generated/enums.cpp", enumsCpp "ToxCore" (coreEnums model))
    , ("lib/src/main/cpp/ToxAv/generated/enums.cpp", enumsCpp "ToxAv" (avEnums model))
    , ("lib/src/main/cpp/ToxCore/generated/errors.cpp", errorsCpp "ToxCore" (coreErrors model))
    , ("lib/src/main/cpp/ToxAv/generated/errors.cpp", errorsCpp "ToxAv" (avErrors model))
    , ("lib/src/main/cpp/ToxCrypto/generated/errors.cpp", errorsCpp "ToxCrypto" (cryptoErrors model))
    , ("lib/src/main/cpp/ToxCore/generated/constants.h", constantsHeader "core" "ToxCoreConstants" (coreConstants model))
    , ("lib/src/main/cpp/ToxCrypto/generated/constants.h", constantsHeader "crypto" "ToxCryptoConstants" (cryptoConstants model))
    , ("lib/src/main/cpp/ToxCore/generated/natives.h", nativesHeader "ToxCoreJni" (subsystemMethods "ToxCore" coreResourceNames model))
    , ("lib/src/main/cpp/ToxAv/generated/natives.h", nativesHeader "ToxAvJni" (subsystemMethods "ToxAv" avResourceNames model))
    , ("lib/src/main/cpp/ToxCrypto/generated/natives.h", nativesHeader "ToxCryptoJni" (subsystemMethods "ToxCrypto" cryptoResourceNames model))
    , ("lib/src/main/cpp/ToxCore/generated/impls.h", implsHeader "ToxCoreJni" model coreResourceNames)
    , ("lib/src/main/cpp/ToxAv/generated/impls.h", implsHeader "ToxAvJni" model avResourceNames)
    , ("lib/src/main/cpp/ToxCore/generated/options.h", optionsHeader model)
    -- ToxCrypto methods (PassKey lifetime + tox_hash + helpers) are
    -- all hand-written: the shared_ptr discipline for concurrent
    -- encrypt+close and the @with_error_handling@ closure layout
    -- can't be expressed by the existing template helpers. No
    -- generated impls.h for now; the hand-written .cpp files own
    -- every symbol.
    ]

-- | Resource partitioning matches the Java sub-module.
coreResourceNames, avResourceNames, cryptoResourceNames :: [Text]
coreResourceNames =
    [ "Tox"
    , "Friend"
    , "Conference"
    , "Conference_Peer"
    , "Conference_Offline_Peer"
    , "Group"
    , "Group_Peer"
    , "File"
    -- Options has its own instance manager in C++ (not the Tox-level
    -- @instances@); its JAVA_METHOD bodies need a different dispatch
    -- helper. The Java @*Jni.java@ side still declares them; their
    -- impls live hand-written in the Options-specific .cpp files.
    ]
avResourceNames = ["AV"]
cryptoResourceNames = ["Pass_Key"]

-- | All methods that surface through the JNI for a given subsystem.
-- Filters out the same roles as the Java side: registrars and free
-- static helpers don't have a native counterpart. Audio/video iterate
-- variants are also excluded — they have no hand-written shim.
subsystemMethods :: Text -> [Text] -> SemanticModel -> [SMethod]
subsystemMethods iface rNames model =
    [ m
    | r <- resources model
    , resourceName r `elem` rNames
    , m <- methods r
    , not (skipMethod m)
    ]
  where
    -- Hidden-from-public-API methods (e.g. @tox_events_init@, the
    -- passphrase-shot convenience wrappers) have no Kotlin caller
    -- and shouldn't get a JNI binding either.
    skipMethod m
      | isHiddenFromPublicApi model (methodName m) = True
      | otherwise = case methodRole m of
          RegistrarRole -> True
          -- StaticRole methods (@tox_get_salt@, @tox_hash@, @tox_is_data_encrypted@)
          -- have native counterparts on @ToxCrypto@-shaped interfaces;
          -- mirror the Kotlin interface emitter's 'interfaceAllowsStatics'
          -- rule so the natives.h list lines up with the JNI declarations.
          StaticRole
              | interfaceAllowsStatics iface -> isExcludedStatic (methodName m)
              | otherwise                    -> True
          _ -> methodName m `elem`
                  [ "toxav_audio_iterate"
                  , "toxav_audio_iteration_interval"
                  , "toxav_video_iterate"
                  , "toxav_video_iteration_interval"
                  , "toxav_get_tox"
                  , "toxav_group_send_audio"
                  , "toxav_groupchat_av_enabled"
                  , "toxav_groupchat_disable_av"
                  ]
    -- @*_to_string@ and @*_length@ are Kotlin-side conveniences with
    -- no JNI presence; @tox_pass_encrypt@/@tox_pass_decrypt@ are
    -- high-level wrappers that the Kotlin layer doesn't expose
    -- (clients go through @passKeyDerive@ + @encrypt@/@decrypt@).
    -- Drop them even when statics are allowed.
    isExcludedStatic n =
        "_to_string" `Text.isSuffixOf` n
            || "_length" `Text.isSuffixOf` n
            || n `elem` ["tox_pass_encrypt", "tox_pass_decrypt"]

--------------------------------------------------------------------------------
-- enums.cpp
--------------------------------------------------------------------------------

-- | Filter the non-error enums in the model by core/av side.
-- @Tox_Event_Type@ is the events oneof discriminator — it's enum-like
-- in the apigen model but isn't a typedef on the C side, so referring
-- to @Tox_Event_Type@ from C++ would fail to compile.
coreEnums, avEnums :: SemanticModel -> [SEnumModel]
coreEnums m = filter (\e -> not (isErr e) && not (isAv e) && enumName e /= "Tox_Event_Type") (enums m)
avEnums m = filter (\e -> not (isErr e) && isAv e) (enums m)

isErr :: SEnumModel -> Bool
isErr e =
    "Tox_Err_" `Text.isPrefixOf` enumName e
        || "TOX_ERR_" `Text.isPrefixOf` enumName e
        || "Toxav_Err_" `Text.isPrefixOf` enumName e
        || "TOXAV_ERR_" `Text.isPrefixOf` enumName e

isAv :: SEnumModel -> Bool
isAv e = "Toxav_" `Text.isPrefixOf` enumName e || "TOXAV_" `Text.isPrefixOf` enumName e

-- | Render all enum-ordinal mappings into a single file.
--
-- Enums are sorted by typedef so regenerating the file produces a
-- stable diff. The ordinal of each enumerator inside the switch is
-- still its source-order index — Kotlin's enum class iterates the
-- same list, so the ordinals match.
enumsCpp :: Text -> [SEnumModel] -> Text
enumsCpp subsystem es =
    Text.unlines $
        ["#include \"../" <> subsystem <> ".h\"", ""]
            ++ concatMap renderEnumPair (List.sortOn enumName es)

renderEnumPair :: SEnumModel -> [Text]
renderEnumPair e =
    [ "template<>"
    , "jint"
    , "Enum::ordinal<" <> typedef <> "> (JNIEnv *env, " <> typedef <> " valueOf)"
    , "{"
    , "  switch (valueOf)"
    , "    {"
    ]
        ++ [ "    case " <> enumMemberCName m <> ": return " <> Text.pack (show idx) <> ";"
           | (idx, m) <- zip [0 :: Int ..] (enumMembers e)
           ]
        ++ [ "    }"
           , "  tox4j_fatal (\"Invalid enumerator from toxcore\");"
           , "}"
           , ""
           , "template<>"
           , typedef
           , "Enum::valueOf<" <> typedef <> "> (JNIEnv *env, jint ordinal)"
           , "{"
           , "  switch (ordinal)"
           , "    {"
           ]
        ++ [ "    case " <> Text.pack (show idx) <> ": return " <> enumMemberCName m <> ";"
           | (idx, m) <- zip [0 :: Int ..] (enumMembers e)
           ]
        ++ [ "    }"
           , "  tox4j_fatal (\"Invalid enumerator from Java\");"
           , "}"
           , ""
           ]
  where
    typedef = enumName e

--------------------------------------------------------------------------------
-- errors.cpp
--------------------------------------------------------------------------------

coreErrors, avErrors, cryptoErrors :: SemanticModel -> [SEnumModel]
coreErrors m =
    [e | e <- enums m, isErr e, errorEnumIsReachable m (enumName e), not (isAv e), not (isCryptoErr e)]
avErrors m =
    [e | e <- enums m, isErr e, errorEnumIsReachable m (enumName e), isAv e]
cryptoErrors m =
    [e | e <- enums m, isErr e, errorEnumIsReachable m (enumName e), isCryptoErr e]

isCryptoErr :: SEnumModel -> Bool
isCryptoErr e =
    enumName e
        `elem` [ "Tox_Err_Encryption"
               , "Tox_Err_Decryption"
               , "Tox_Err_Key_Derivation"
               , "Tox_Err_Get_Salt"
               ]

errorsCpp :: Text -> [SEnumModel] -> Text
errorsCpp subsystem es =
    Text.unlines $
        ["#include \"../" <> subsystem <> ".h\"", ""]
            ++ concatMap renderErrorHandle (List.sortOn exnNameOf es)
  where
    exnNameOf e = pascalCase (stripErrPrefix (enumName e))

-- | Each @Tox_Err_Foo@ enum becomes a @HANDLE@ block. Failure cases
-- are sorted alphabetically to make regenerated diffs stable.
renderErrorHandle :: SEnumModel -> [Text]
renderErrorHandle e =
    [ "HANDLE (\"" <> exnName <> "\", " <> errType <> ")"
    , "{"
    , "  switch (error)"
    , "    {"
    , "    success_case (" <> macroPrefix <> ");"
    ]
        ++ [ "    failure_case (" <> macroPrefix <> ", " <> code <> ");"
           | code <- List.sort [enumMemberName m | m <- enumMembers e, enumMemberName m /= "OK"]
           ]
        ++ [ "    }"
           , "  return unhandled ();"
           , "}"
           , ""
           ]
  where
    -- @Tox_Err_Friend_Add@ -> errType "Friend_Add", macroPrefix "FRIEND_ADD", exnName "FriendAdd".
    errType = stripErrPrefix (enumName e)
    macroPrefix = Text.toUpper errType
    exnName = pascalCase errType

stripErrPrefix :: Text -> Text
stripErrPrefix n =
    case Text.stripPrefix "Tox_Err_" n of
        Just rest -> rest
        Nothing -> case Text.stripPrefix "Toxav_Err_" n of
            Just rest -> rest
            Nothing -> n

--------------------------------------------------------------------------------
-- constants.h
--------------------------------------------------------------------------------

-- | Constants split by source-header convention (see Kotlin sub-module).
coreConstants, cryptoConstants :: SemanticModel -> [SConstantModel]
coreConstants m = filter (not . isCryptoConst . constantName) (constants m)
cryptoConstants m = filter (isCryptoConst . constantName) (constants m)

isCryptoConst :: Text -> Bool
isCryptoConst n =
    "TOX_PASS_" `Text.isPrefixOf` n || n == "TOX_HASH_LENGTH"

constantsHeader :: Text -> Text -> [SConstantModel] -> Text
constantsHeader pkg objectName cs =
    Text.unlines $
        [ "// im.tox.tox4j." <> pkg <> "." <> objectName <> "$"
        , "static void"
        , "check" <> objectName <> " ()"
        , "{"
        ]
            ++ [ "  static_assert ("
                <> constantName c
                <> " == "
                <> Text.pack (show (constantValue c))
                <> ", \"Java constant out of sync with C\");"
               | c <- List.sortOn constantName cs
               ]
            ++ ["}"]

--------------------------------------------------------------------------------
-- impls.h — JAVA_METHOD bodies
--------------------------------------------------------------------------------

-- | Emit a @JAVA_METHOD@ body per public-API method, picking the
-- dispatch helper that matches the method's shape:
--
--   * @with_instance_noerr@ — no error, primitive args, primitive
--     return.
--   * @with_instance_err@ with @identity@ — has an error pointer, the
--     bool/int return value is passed through.
--   * @with_instance_ign@ — has an error pointer, the @bool@ return
--     is just success/failure (no semantic value).
--
-- Shapes the generator doesn't yet template (byte-array inputs,
-- non-primitive returns, @repeated@ outputs) get a @\/\/ TODO@ stub
-- and stay in the hand-written .cpp files; the generated impls.h
-- coexists with the hand-written shims for those.
--
-- Methods filtered out entirely (no entry in impls.h):
--
--   * Constructors and Destructors — wired by the Impl class's
--     primary constructor\/finalizer in hand-written code.
--   * RegistrarRole, StaticRole — different dispatch paths.
--   * Hidden methods ('isHiddenFromPublicApi') — no Java native
--     declaration to provide a body for.
--   * Iterate drivers and legacy AV groupchat bridges — hand-written.
implsHeader :: Text -> SemanticModel -> [Text] -> Text
implsHeader jniClassName model rNames =
    Text.unlines $
        ("// im.tox.tox4j.impl.jni." <> jniClassName)
            : ""
            : concatMap (renderMethodImpl model) (subsystemMethodsPaired rNames model)

-- | Like 'subsystemMethods' but keeps the owning resource so path-id
-- args can be derived. Same skip rules as 'subsystemMethods' plus
-- the 'isHiddenFromPublicApi' filter so methods that have no Java
-- @native@ declaration also don't get a generated body.
subsystemMethodsPaired :: [Text] -> SemanticModel -> [(SResource, SMethod)]
subsystemMethodsPaired rNames model =
    [ (r, m)
    | r <- resources model
    , resourceName r `elem` rNames
    , m <- methods r
    , not (skipMethod r m)
    , not (isHiddenFromPublicApi model (methodName m))
    ]
  where
    skipMethod r m = case methodRole m of
        RegistrarRole -> True
        StaticRole -> True
        -- Constructor handling: only skip when the owning resource
        -- is a ResHandle (Tox, AV, Pass_Key) — those are managed by
        -- the Impl class's hand-written lifecycle (toxNew /
        -- toxavNew / toxPassKeyDerive). Constructors on ResId
        -- resources (tox_friend_add, tox_conference_new,
        -- tox_group_new, tox_file_send, …) just return an integer
        -- handle and template fine.
        Constructor -> case resourceType r of
            ResHandle -> True
            ResId _   -> False
        Destructor -> True
        _ -> methodName m `elem`
                [ "toxav_audio_iterate"
                , "toxav_audio_iteration_interval"
                , "toxav_video_iterate"
                , "toxav_video_iteration_interval"
                , "toxav_get_tox"
                , "toxav_group_send_audio"
                , "toxav_groupchat_av_enabled"
                , "toxav_groupchat_disable_av"
                ]

renderMethodImpl :: SemanticModel -> (SResource, SMethod) -> [Text]
renderMethodImpl model (r, m)
    -- IterateRole returns the protobuf event accumulator as bytes, not
    -- whatever the C signature says. Routed before 'shouldSkip' because
    -- the @void@ return of @tox_iterate@\/@toxav_iterate@ would otherwise
    -- look like the @with_instance_ign@ shape.
    | methodRole m == IterateRole = renderIterateBody r m
    | shouldSkip m = todoStub m
    -- Methods with a synthetic exception (no C error enum, hand-thrown
    -- on the C++ side) stay hand-written — the throw site is
    -- bespoke. Currently just @tox_conference_get_id@.
    | Just _ <- syntheticExceptionFor model (methodName m) = todoStub m
    | otherwise = case (output m, methodErrorType m) of
        (SBool, Just _) | not (boolReturnsValue (methodName m)) ->
            renderBody model r m "with_instance_ign" Nothing
        -- SFixedBytes return: C3 template. With-error path uses a
        -- stack-allocated buffer + success lambda routed through
        -- with_instance_err; no-error path uses get_vector + a
        -- constant_size sizer through with_instance_noerr.
        (SFixedBytes sizeConst _, Just _) ->
            renderFixedBytesReturn model r m sizeConst
        (SFixedBytes sizeConst _, Nothing) | null (inputs m) ->
            renderFixedBytesReturnNoErr m sizeConst
        -- No-error fixed-bytes with extra args doesn't match
        -- get_vector's signature; fall through to hand-written.
        (SFixedBytes _ _, Nothing) -> todoStub m
        -- SBytes return: C4 size-then-get template. Uses
        -- @get_vector<uint8_t, tox_X_size, tox_X_get>::make@ when
        -- there's no error pointer (the C size getter has the same
        -- signature shape as get_vector expects), or
        -- @get_vector_err<jbyte, decltype(size), decltype(get)>::make
        -- <size, get>@ when there is. The size companion must exist
        -- in the model; if it doesn't, fall back to hand-written.
        (SBytes, Just _) | sizeFnExists -> renderVarBytesReturnErr model r m
        (SBytes, Nothing) | sizeFnExists -> renderVarBytesReturnNoErr m
        (SBytes, _) -> todoStub m
        -- @SList@ of @uint32_t@-shaped element (raw @SUInt 32@ or a
        -- resource ID newtype). Uses @get_vector<uint32_t, size, get,
        -- jint>::make@ — the @ConvertT = jint@ instantiation lets the
        -- C signature stay @uint32_t *@ while the Java side receives
        -- a @jintArray@.
        (SList (SUInt 32), Nothing) | sizeFnExists -> renderUInt32ListReturn m
        (SList (SResourceId _), Nothing) | sizeFnExists -> renderUInt32ListReturn m
        (SList _, _) -> todoStub m
        (_, Just _) -> renderBody model r m "with_instance_err" (Just "identity")
        (_, Nothing) -> renderBody model r m "with_instance_noerr" Nothing
  where
    sizeFnExists =
        let sizeFnName = methodName m <> "_size"
        in any (\rr -> any (\mm -> methodName mm == sizeFnName) (methods rr)) (resources model)

-- | True if the method shape isn't yet templated and needs hand-written
-- dispatch. Skipped shapes get a @\/\/ TODO@ stub and continue to be
-- hand-written; the unimplemented shapes today are byte-array
-- returns, strings, lists, enums (as args), and constructors\/destructors.
--
-- Byte-array IN parameters ('SBytes' \/ 'SFixedBytes' as inputs) are
-- now templated by 'renderBody' (emits @fromJavaArray@ wrappers and
-- routes them through @with_instance_*@ + @conv::from_java@).
shouldSkip :: SMethod -> Bool
shouldSkip m =
    any nonPrimInput (map paramType (inputs m))
        || nonPrimOutput (output m)
        -- @toxav_audio_iterate@\/@toxav_video_iterate@ have @IterateRole@
        -- in the model but are excluded from the JVM API at the
        -- 'skipMethod' layer (no JNI bridge). They never reach
        -- 'renderMethodImpl', so no rule needed here.
        -- Role-based filtering (Constructor on ResHandle, Destructor,
        -- RegistrarRole, StaticRole) is handled at the earlier
        -- 'subsystemMethodsPaired' filter so those methods don't
        -- even reach 'renderMethodImpl'. Anything that gets here
        -- with a Constructor or Destructor role is the ResId-shaped
        -- sub-resource constructor (tox_friend_add etc.) and IS
        -- templated.
  where
    -- Inputs the template handles: primitives, plus byte-array IN
    -- ('SBytes' \/ 'SFixedBytes' as params) — 'renderBody' emits
    -- @fromJavaArray@ wrappers for these and routes through
    -- @with_instance_*@ + @conv::from_java@. The 'SFixedBytes' case
    -- skips when the size constant is a runtime expression
    -- (@width * height@ in @toxav_video_send_frame@) — those need
    -- overflow-safe size_t arithmetic + proper exception throwing
    -- the template can't express cleanly.
    nonPrimInput ty = case ty of
        SBytes -> False
        SFixedBytes sizeConst _ -> not (isConstantIdentifier sizeConst)
        SString -> False      -- batch C5: UTFChars wrapper inline in cArgs
        SList _ -> True
        SFixedList _ _ _ -> True
        SEnum _ -> False      -- batch C6
        _ -> False

    -- A size constant looks like a C identifier when every char is
    -- letter/digit/underscore. Anything else (spaces, @*@, @/@,
    -- @+@) means it's a runtime expression — skip templating.
    isConstantIdentifier t =
        not (Text.null t)
        && Text.all (\c -> (c >= 'A' && c <= 'Z')
                        || (c >= '0' && c <= '9')
                        || c == '_') t

    -- Returns the template handles: primitives, @SFixedBytes@ (C3),
    -- @SBytes@ via the size-then-get pair (C4 — uses get_vector /
    -- get_vector_err with the matching @tox_X_size@ companion), and
    -- @SList@-of-uint32 (friend\/conference\/group lists) via the
    -- same @get_vector@ template with the @jint@ convert target.
    nonPrimOutput ty = case ty of
        SBytes -> False
        SFixedBytes _ _ -> False
        SString -> True
        SList (SUInt 32) -> False
        SList (SResourceId _) -> False
        SList _ -> True
        SFixedList _ _ _ -> True
        -- Enum returns flow through @with_instance_err@'s @identity@
        -- success function; @conversions::to_java@ converts the
        -- raw C enum value to @jint@ on the JVM side. Same shape as
        -- a primitive return — no extra template work needed.
        SEnum _ -> False
        _ -> False

-- | @auto <name>Data = fromJavaArray (env, <name>);@ for each
-- byte-array param, followed by a @tox4j_assert@ size check for
-- 'SFixedBytes' params (with a known size constant). The check
-- mirrors the hand-written shim discipline: if the caller passed a
-- non-null array, its length must match the typedef's @K@.
byteArrayPrologue :: SMethod -> [Text]
byteArrayPrologue m = concatMap paramPrologue (inputs m)
  where
    paramPrologue p = case paramType p of
        SBytes ->
            [ "  auto " <> n <> "Data = fromJavaArray (env, " <> n <> ");" ]
        SFixedBytes sizeConst _ ->
            [ "  auto " <> n <> "Data = fromJavaArray (env, " <> n <> ");"
            , "  tox4j_assert (!" <> n <> " || " <> n <> "Data.size () == " <> sizeConst <> ");"
            ]
        _ -> []
      where
        n = camelCaseParam (paramName p)

-- | Per-param expansion for the C function call. Byte-array params
-- become either a single wrapper arg or a @.data(), .size()@ pair
-- depending on whether the C signature wants a separate length.
-- Enum params get a @Enum::valueOf<T>(env, n)@ cast — the C function
-- expects the typed enum, not a raw int.
paramCArgs :: SParameter -> [Text]
paramCArgs p = case paramType p of
    SFixedBytes _ False -> [name <> "Data"]
    SFixedBytes _ True  -> [name <> "Data.data ()", name <> "Data.size ()"]
    SBytes              -> [name <> "Data.data ()", name <> "Data.size ()"]
    SString             -> ["UTFChars (env, " <> name <> ").data ()"]
    SEnum enumName      -> ["Enum::valueOf<" <> cEnumType enumName <> "> (env, " <> name <> ")"]
    _                    -> [name]
  where
    name = camelCaseParam (paramName p)

--------------------------------------------------------------------------------
-- options.h
--------------------------------------------------------------------------------

-- | @set_options_from_proto@: drives the @tox_options_set_*@ family
-- from the decoded proto @Options@ message inside the @toxNew@ shim.
-- Fields come from 'optionsWireFields' — the same list that emits the
-- proto schema and the Kotlin builder, so a new c-toxcore Options
-- field regenerates all three sides.
--
-- The message owns every string\/bytes buffer and outlives the
-- @tox_new@ call in the shim's scope, so the pointer-storing
-- c-toxcore setters (@proxy_host@, @savedata_data@) are safe without
-- any copy or pool. Those two return @bool@ (they allocate under
-- @experimental_owned_data@); scalar setters return @void@. Returns
-- @false@ on allocation failure — the caller maps that to
-- @TOX_ERR_NEW_MALLOC@.
optionsHeader :: SemanticModel -> Text
optionsHeader model =
    Text.unlines $
        [ "static bool"
        , "set_options_from_proto (JNIEnv *env, Tox_Options *opts, im::tox::tox4j::core::proto::Options const &msg)"
        , "{"
        , "  bool ok = true;"
        ]
            ++ map setterLine (optionsWireFields model)
            ++ [ "  return ok;"
               , "}"
               ]
  where
    setterLine p =
        let n = propName p
            field = "msg." <> n <> " ()"
        in case propType p of
            SEnum e  -> "  tox_options_set_" <> n <> " (opts, Enum::valueOf<" <> cEnumType e <> "> (env, " <> field <> "));"
            SString  -> "  ok = tox_options_set_" <> n <> " (opts, " <> field <> ".c_str ()) && ok;"
            SBytes   -> "  ok = tox_options_set_" <> n <> "_data (opts, reinterpret_cast<uint8_t const *> (" <> field <> ".data ()), " <> field <> ".size ()) && ok;"
            SUInt 16 -> "  tox_options_set_" <> n <> " (opts, static_cast<uint16_t> (" <> field <> "));"
            _        -> "  tox_options_set_" <> n <> " (opts, " <> field <> ");"

-- | Resolve the C enum typedef name from a semantic 'SEnum' name.
-- AV-side enums already carry the @Toxav_@ prefix in the model;
-- core-side enums omit it and need the @Tox_@ prefix added.
cEnumType :: Text -> Text
cEnumType n
    | "Toxav_" `Text.isPrefixOf` n = n
    | "Tox_"   `Text.isPrefixOf` n = n
    | otherwise                    = "Tox_" <> n

-- | JNI type for a parameter. 16-bit ints widen to @jint@ to match
-- the surface 'Apigen.Language.Jvm.Java.jniType' emits.
jcType :: SParameter -> Text
jcType p = case paramType p of
    SBool -> "jboolean"
    SInt 8 -> "jbyte"
    SInt 16 -> "jint"
    SInt 32 -> "jint"
    SInt 64 -> "jlong"
    SInt _ -> "jint"
    SUInt 8 -> "jbyte"
    SUInt 16 -> "jint"
    SUInt 32 -> "jint"
    SUInt 64 -> "jlong"
    SUInt _ -> "jint"
    SBytes -> "jbyteArray"
    SFixedBytes _ _ -> "jbyteArray"
    SString -> "jstring"
    SEnum _ -> "jint"
    SResourceId _ -> "jint"
    _ -> "jobject"

-- | C4 with-error variable-bytes return: route through
-- @get_vector_err<jbyte, decltype(size_fn), decltype(get_fn)>::make
-- <size_fn, get_fn>@ wrapped in @with_instance_err@. The helper
-- handles size+alloc+populate with proper error-path handling.
renderVarBytesReturnErr :: SemanticModel -> SResource -> SMethod -> [Text]
renderVarBytesReturnErr model r m =
    [ "JAVA_METHOD (jbyteArray, " <> jMethodName (methodName m) <> ","
    , "  " <> Text.intercalate ", " (("jint instanceNumber") : pathArgs ++ sigArgs) <> ")"
    , "{"
    ]
    ++ byteArrayPrologue m
    ++
    [ "  return instances.with_instance_err (env, instanceNumber,"
    , "    identity,"
    , "    get_vector_err<jbyte, decltype(" <> sizeFn <> "), decltype(" <> getFn <> ")>::make<"
    , "      " <> sizeFn <> ","
    , "      " <> getFn <> ">" <> trailingArgs
    , "  );"
    , "}"
    , ""
    ]
  where
    getFn = methodName m
    sizeFn = getFn <> "_size"
    pathArgNames = pathArgNamesFor model r (methodRole m /= Constructor)
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jcType p <> " " <> camelCaseParam (paramName p) | p <- inputs m]
    extraArgs = pathArgNames ++ concatMap paramCArgs (inputs m)
    trailingArgs = if null extraArgs then "" else ", " <> Text.intercalate ", " extraArgs

-- | C4 no-error variable-bytes return: @get_vector<uint8_t, size_fn,
-- get_fn>::make@ via @with_instance_noerr@. Both functions must take
-- @(const Tox*)@ only — the same constraint as 'get_vector'.
renderVarBytesReturnNoErr :: SMethod -> [Text]
renderVarBytesReturnNoErr m =
    [ "JAVA_METHOD (jbyteArray, " <> jMethodName (methodName m) <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance_noerr (env, instanceNumber,"
    , "    get_vector<uint8_t,"
    , "      " <> sizeFn <> ","
    , "      " <> getFn <> ">::make"
    , "  );"
    , "}"
    , ""
    ]
  where
    getFn = methodName m
    sizeFn = getFn <> "_size"

-- | C3 with-error fixed-bytes return: declare a stack buffer of the
-- typedef's size, route through @with_instance_err@ with a success
-- lambda that wraps the buffer into a @jbyteArray@ via
-- @toJavaArray@. The buffer is also appended to the C function's
-- positional args.
renderFixedBytesReturn :: SemanticModel -> SResource -> SMethod -> Text -> [Text]
renderFixedBytesReturn model r m sizeConst =
    [ "JAVA_METHOD (jbyteArray, " <> jMethodName (methodName m) <> ","
    , "  " <> Text.intercalate ", " (("jint instanceNumber") : pathArgs ++ sigArgs) <> ")"
    , "{"
    , "  uint8_t result[" <> sizeConst <> "];"
    ]
    ++ byteArrayPrologue m
    ++
    [ "  return instances.with_instance_err (env, instanceNumber,"
    , "    [&] (bool) { return toJavaArray (env, result); },"
    , "    " <> Text.intercalate ", " (methodName m : pathArgNames ++ concatMap paramCArgs (inputs m) ++ ["result"])
    , "  );"
    , "}"
    , ""
    ]
  where
    pathArgNames = pathArgNamesFor model r (methodRole m /= Constructor)
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jcType p <> " " <> camelCaseParam (paramName p) | p <- inputs m]

-- | C3 no-error fixed-bytes return: @get_vector<uint8_t,
-- constant_size<K>::make, tox_func>::make@ wraps the C call + buffer
-- alloc + conversion. Only works for methods whose only non-Tox
-- parameter is the buffer (so the C signature reduces to @void
-- f(const Tox*, uint8_t out[K])@) — which is the only no-error
-- fixed-bytes shape c-toxcore ships today.
renderFixedBytesReturnNoErr :: SMethod -> Text -> [Text]
renderFixedBytesReturnNoErr m sizeConst =
    [ "JAVA_METHOD (jbyteArray, " <> jMethodName (methodName m) <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance_noerr (env, instanceNumber,"
    , "    get_vector<uint8_t, constant_size<" <> sizeConst <> ">::make, " <> methodName m <> ">::make"
    , "  );"
    , "}"
    , ""
    ]

-- | One @JAVA_METHOD@ block. Takes the helper name (e.g.
-- @with_instance_noerr@) and an optional converter to insert as the
-- second arg (for @with_instance_err@'s @identity@).
--
-- Byte-array IN params get a prologue line that wraps the @jbyteArray@
-- in an @ArrayFromJava@ via @fromJavaArray@. The wrapper is then
-- routed through @with_instance_*@'s argument list:
--
--   * 'SFixedBytes' params (e.g. @Tox_Address@) pass the wrapper as a
--     single arg — the C++ @conv::from_java@ specialisation converts
--     it to the @const uint8_t *@ the C function expects.
--   * 'SBytes' params expand to two args: @<name>Data.data ()@ and
--     @<name>Data.size ()@, matching the C @(const uint8_t *, size_t)@
--     calling convention.
renderBody :: SemanticModel -> SResource -> SMethod -> Text -> Maybe Text -> [Text]
renderBody model r m helper conv =
    [ "JAVA_METHOD (" <> jReturnType m <> ", " <> jMethodName (methodName m) <> ","
    , "  " <> Text.intercalate ", " (("jint instanceNumber") : pathArgs ++ sigArgs) <> ")"
    , "{"
    ]
    ++ prologueLines
    ++
    [ "  return instances." <> helper <> " (env, instanceNumber,"
    , "    " <> Text.intercalate ", " convAndFunc
    , "  );"
    , "}"
    , ""
    ]
  where
    pathArgNames = pathArgNamesFor model r (methodRole m /= Constructor)
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jcType p <> " " <> camelCaseParam (paramName p) | p <- inputs m]

    prologueLines = byteArrayPrologue m

    convAndFunc =
        maybe id (:) conv
            $ methodName m : pathArgNames ++ concatMap paramCArgs (inputs m)


-- | Names (only, no types) for the path-id args. Mirrors
-- 'Apigen.Language.Jvm.Kotlin.Common.pathIdArgsFor' but only emits the
-- string name part since impls.h types are all @jint@.
pathArgNamesFor :: SemanticModel -> SResource -> Bool -> [Text]
pathArgNamesFor model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case lookupResource pname of
            Just parentRes -> pathArgNamesFor model parentRes True
            Nothing -> []
        Nothing -> []
    lookupResource pname = case [res | res <- resources model, resourceName res == pname] of
        (res : _) -> Just res
        [] -> Nothing
    ownArg = case resourceType r of
        ResId (SResourceId idTypeName) ->
            let shortName = stripParentPrefix idTypeName
            in [camelCaseLocal shortName]
        _ -> []
    stripParentPrefix name = case parent r of
        Just p -> case Text.stripPrefix (p <> "_") name of
            Just rest -> rest
            Nothing -> name
        Nothing -> name

jReturnType :: SMethod -> Text
jReturnType m = case (output m, methodErrorType m) of
    (SVoid, _) -> "void"
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (SBool, _) -> "jboolean"
    (SInt 8, _) -> "jbyte"
    (SInt 16, _) -> "jint" -- 16-bit widens to jint; see Java.jniType
    (SInt 32, _) -> "jint"
    (SInt 64, _) -> "jlong"
    (SInt _, _) -> "jint"
    (SUInt 8, _) -> "jbyte"
    (SUInt 16, _) -> "jint"
    (SUInt 32, _) -> "jint"
    (SUInt 64, _) -> "jlong"
    (SUInt _, _) -> "jint"
    (SSizeT, _) -> "jlong"
    (SEnum _, _) -> "jint"
    (SResourceId _, _) -> "jint"
    _ -> "jobject"

-- | Mirror the Java sub-module's @jniMethodName@. Local copy to avoid
-- cross-sub-module imports.
jMethodName :: Text -> Text
jMethodName n =
    case Text.stripPrefix "tox_pass_" n of
        Just rest -> "toxPass" <> pascalCaseHead rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> "toxav" <> pascalCaseHead rest
            Nothing -> case Text.stripPrefix "tox_" n of
                Just rest -> "tox" <> pascalCaseHead rest
                Nothing -> camelCaseLocal n
  where
    pascalCaseHead t = case Text.split (== '_') t of
        [] -> ""
        chunks -> Text.concat (map capFirst chunks)
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCaseLocal :: Text -> Text
camelCaseLocal t = case Text.split (== '_') t of
    [] -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

camelCaseParam :: Text -> Text
camelCaseParam = camelCaseLocal

-- | @SList@-of-@uint32_t@ return template. Both raw @SUInt 32@ and
-- resource-id newtypes (which are @uint32_t@ under the hood) share
-- the same @get_vector@ instantiation. The size companion is the
-- method name with @_size@ appended — its existence is verified
-- before this renderer fires.
renderUInt32ListReturn :: SMethod -> [Text]
renderUInt32ListReturn m =
    let
        jName = jMethodName (methodName m)
        getFn = methodName m
        sizeFn = getFn <> "_size"
    in
    [ ""
    , "JAVA_METHOD (jintArray, " <> jName <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance_noerr (env, instanceNumber,"
    , "    get_vector<uint32_t, " <> sizeFn <> ", " <> getFn <> ", jint>::make"
    , "  );"
    , "}"
    , ""
    ]

-- | @IterateRole@ method template. The JNI shim drives one tick of
-- the event loop, then serialises the accumulated @Events@ message
-- to a Java @byte[]@. The accumulator lives on the @ToxInstance@ and
-- is fed by either the C-side out-param (@tox_iterate@) or by
-- internal callbacks the binding hooked up earlier (@toxav_iterate@).
-- Returns @nullptr@ when there are no events — keeps the Java side
-- from allocating an empty array on every idle tick.
renderIterateBody :: SResource -> SMethod -> [Text]
renderIterateBody r m =
    let
        jName = jMethodName (methodName m)
        handleType = cName r
        cFunc = methodName m
        -- @tox_iterate(Tox *, Tox_Events *)@: events come back via
        -- the second out-param. @toxav_iterate(ToxAV *)@: events
        -- accumulate through the registered C callbacks. The lambda
        -- always receives @events@ from @with_instance@; the call
        -- shape is the only difference.
        callArgs = if cFunc == "tox_iterate" then "self, &events" else "self"
    in
    [ ""
    , "JAVA_METHOD (jbyteArray, " <> jName <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance (env, instanceNumber,"
    , "    [=] (" <> handleType <> " *self, Events &events) -> jbyteArray"
    , "      {"
    , "        " <> cFunc <> "(" <> callArgs <> ");"
    , "        if (events.ByteSizeLong () == 0)"
    , "          return nullptr;"
    , ""
    , "        std::vector<char> buffer (events.ByteSizeLong ());"
    , "        if (!events.SerializeToArray (buffer.data (), buffer.size ()))"
    , "          return nullptr;"
    , "        events.Clear ();"
    , ""
    , "        return toJavaArray (env, buffer);"
    , "      }"
    , "  );"
    , "}"
    , ""
    ]

todoStub :: SMethod -> [Text]
todoStub m =
    [ "// TODO: " <> jMethodName (methodName m) <> " — non-primitive args or return; hand-written."
    , ""
    ]

--------------------------------------------------------------------------------
-- natives.h
--------------------------------------------------------------------------------

-- | Emit a @JAVA_METHOD_REF@ / @CXX_FUNCTION_REF@ macro pair per method.
-- These macros are referenced by the CMake build and by the JNI
-- registration glue.
nativesHeader :: Text -> [SMethod] -> Text
nativesHeader jniClass ms =
    Text.unlines $
        ("// im.tox.tox4j.impl.jni." <> jniClass)
            : concatMap render (List.sortOn snd allPairs)
  where
    allPairs =
        [(jniMethodName (methodName m), methodName m) | m <- ms]
            ++ cppExtraNativeRefs jniClass
    render (jniName, cFunc) =
        [ "JAVA_METHOD_REF (" <> jniName <> ")"
        , "CXX_FUNCTION_REF (" <> cFunc <> ")"
        ]

-- | Mirrors @Apigen.Language.Jvm.Java.jniMethodName@: derive the Java
-- method name from the C function name.
jniMethodName :: Text -> Text
jniMethodName n =
    case Text.stripPrefix "tox_pass_" n of
        Just rest -> "toxPass" <> pascalCaseHead rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> "toxav" <> pascalCaseHead rest
            Nothing -> case Text.stripPrefix "tox_" n of
                Just rest -> "tox" <> pascalCaseHead rest
                Nothing -> camelCase n
  where
    pascalCaseHead t = case Text.split (== '_') t of
        [] -> ""
        chunks -> Text.concat (map capFirst chunks)
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCase :: Text -> Text
camelCase t = case Text.split (== '_') t of
    [] -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t
