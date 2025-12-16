{-# LANGUAGE OverloadedStrings #-}

-- | The C++ files in @lib/src/main/cpp/{ToxCore,ToxAv,ToxCrypto}/generated/@.
module Apigen.Language.Jvm.Cpp (generate) where

import           Apigen.Language.Jvm.Conventions (boolReturnsValue,
                                                  cppExtraNativeRefs,
                                                  errorEnumIsReachable,
                                                  interfaceAllowsStatics,
                                                  isHiddenFromPublicApi,
                                                  optionsWireFields,
                                                  syntheticExceptionFor)
import           Apigen.Semantic                 (SConstantModel (..),
                                                  SEnumMember (..),
                                                  SEnumModel (..), SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SProperty (..),
                                                  SResource (..),
                                                  SResourceType (..),
                                                  SType (..),
                                                  SemanticModel (..))
import qualified Data.Char                       as Char
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

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
    -- ToxCrypto has no impls.h: its PassKey lifetime (concurrent
    -- encrypt and close) is hand-written.
    ]

-- | Same partitioning as the Java sub-module.
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
    -- Not Options: it has its own instance manager in C++, so its
    -- bodies are hand-written.
    ]
avResourceNames = ["AV"]
cryptoResourceNames = ["Pass_Key"]

-- | The methods with a JNI native, by the same rules as the Java side.
subsystemMethods :: Text -> [Text] -> SemanticModel -> [SMethod]
subsystemMethods iface rNames model =
    [ m
    | r <- resources model
    , resourceName r `elem` rNames
    , m <- methods r
    , not (skipMethod m)
    ]
  where
    skipMethod m
      | isHiddenFromPublicApi model (methodName m) = True
      | otherwise = case methodRole m of
          RegistrarRole -> True
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
    -- Kotlin-side conveniences, and the passphrase wrappers Kotlin doesn't
    -- expose (it goes through @passKeyDerive@).
    isExcludedStatic n =
        "_to_string" `Text.isSuffixOf` n
            || "_length" `Text.isSuffixOf` n
            || n `elem` ["tox_pass_encrypt", "tox_pass_decrypt"]

--------------------------------------------------------------------------------
-- enums.cpp
--------------------------------------------------------------------------------

-- | Non-error enums. @Tox_Event_Type@ is left out: it isn't a C typedef.
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

-- | Ordinals are source order, as in the Kotlin enum class.
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
    -- @Tox_Err_Friend_Add@: "Friend_Add", "FRIEND_ADD", "FriendAdd".
    errType = stripErrPrefix (enumName e)
    macroPrefix = Text.toUpper errType
    exnName = pascalCase errType

stripErrPrefix :: Text -> Text
stripErrPrefix n =
    case Text.stripPrefix "Tox_Err_" n of
        Just rest -> rest
        Nothing -> case Text.stripPrefix "Toxav_Err_" n of
            Just rest -> rest
            Nothing   -> n

--------------------------------------------------------------------------------
-- constants.h
--------------------------------------------------------------------------------

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
-- impls.h
--------------------------------------------------------------------------------

-- | A @JAVA_METHOD@ body per method. Shapes it can't template get a
-- @\/\/ TODO@ stub and stay hand-written.
implsHeader :: Text -> SemanticModel -> [Text] -> Text
implsHeader jniClassName model rNames =
    Text.unlines $
        ("// im.tox.tox4j.impl.jni." <> jniClassName)
            : ""
            : concatMap (renderMethodImpl model) (subsystemMethodsPaired rNames model)

-- | Like 'subsystemMethods', with the resource for the path-id args.
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
        -- Handle constructors (toxNew etc.) are hand-written. ID ones
        -- (tox_friend_add etc.) just return an integer.
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
    -- Before 'shouldSkip': iterate returns the event bytes, not its C
    -- return type.
    | methodRole m == IterateRole = renderIterateBody r m
    | shouldSkip m = todoStub m
    -- A synthetic exception is thrown by hand-written code.
    | Just _ <- syntheticExceptionFor model (methodName m) = todoStub m
    | otherwise = case (output m, methodErrorType m) of
        (SBool, Just _) | not (boolReturnsValue (methodName m)) ->
            renderBody model r m "with_instance_ign" Nothing
        (SFixedBytes sizeConst _, Just _) ->
            renderFixedBytesReturn model r m sizeConst
        (SFixedBytes sizeConst _, Nothing) | null (inputs m) ->
            renderFixedBytesReturnNoErr m sizeConst
        -- Doesn't fit get_vector's signature.
        (SFixedBytes _ _, Nothing) -> todoStub m
        -- Needs a @_size@ companion function.
        (SBytes, Just _) | sizeFnExists -> renderVarBytesReturnErr model r m
        (SBytes, Nothing) | sizeFnExists -> renderVarBytesReturnNoErr m
        (SBytes, _) -> todoStub m
        (SList (SUInt 32), Nothing) | sizeFnExists -> renderUInt32ListReturn m
        (SList (SResourceId _), Nothing) | sizeFnExists -> renderUInt32ListReturn m
        (SList _, _) -> todoStub m
        (_, Just _) -> renderBody model r m "with_instance_err" (Just "identity")
        (_, Nothing) -> renderBody model r m "with_instance_noerr" Nothing
  where
    sizeFnExists =
        let sizeFnName = methodName m <> "_size"
        in any (\rr -> any (\mm -> methodName mm == sizeFnName) (methods rr)) (resources model)

-- | Whether the method's shape has no template.
shouldSkip :: SMethod -> Bool
shouldSkip m =
    any nonPrimInput (map paramType (inputs m))
        || nonPrimOutput (output m)
  where
    -- A runtime size (@width * height@ in @toxav_video_send_frame@)
    -- needs overflow checks the template doesn't do.
    nonPrimInput ty = case ty of
        SBytes                  -> False
        SFixedBytes sizeConst _ -> not (isConstantIdentifier sizeConst)
        SString                 -> False
        SList _                 -> True
        SFixedList _ _ _        -> True
        SEnum _                 -> False
        _                       -> False

    isConstantIdentifier t =
        not (Text.null t)
        && Text.all (\c -> (c >= 'A' && c <= 'Z')
                        || (c >= '0' && c <= '9')
                        || c == '_') t

    nonPrimOutput ty = case ty of
        SBytes                -> False
        SFixedBytes _ _       -> False
        SString               -> True
        SList (SUInt 32)      -> False
        SList (SResourceId _) -> False
        SList _               -> True
        SFixedList _ _ _      -> True
        SEnum _               -> False
        _                     -> False

-- | Wraps each byte-array param, asserting a fixed-size one's length.
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

-- | The C call's arguments for a param.
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

-- | @set_options_from_proto@, for the @toxNew@ shim. The message outlives
-- @tox_new@, so the setters that keep a pointer into it are safe.
-- Returns @false@ when a setter fails to allocate.
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

-- | The C typedef: core enums lack the @Tox_@ prefix in the model.
cEnumType :: Text -> Text
cEnumType n
    | "Toxav_" `Text.isPrefixOf` n = n
    | "Tox_"   `Text.isPrefixOf` n = n
    | otherwise                    = "Tox_" <> n

-- | 16-bit ints widen to @jint@, as in 'Apigen.Language.Jvm.Java.jniType'.
jcType :: SParameter -> Text
jcType p = case paramType p of
    SBool           -> "jboolean"
    SInt 8          -> "jbyte"
    SInt 16         -> "jint"
    SInt 32         -> "jint"
    SInt 64         -> "jlong"
    SInt _          -> "jint"
    SUInt 8         -> "jbyte"
    SUInt 16        -> "jint"
    SUInt 32        -> "jint"
    SUInt 64        -> "jlong"
    SUInt _         -> "jint"
    SBytes          -> "jbyteArray"
    SFixedBytes _ _ -> "jbyteArray"
    SString         -> "jstring"
    SEnum _         -> "jint"
    SResourceId _   -> "jint"
    _               -> "jobject"

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

-- | Both C functions must take only the @Tox *@, as 'get_vector' needs.
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

-- | Only for @void f(const Tox *, uint8_t out[K])@.
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

-- | One @JAVA_METHOD@ block, through the @with_instance_*@ helper given,
-- with an optional converter as its second argument.
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


-- | 'Apigen.Language.Jvm.Kotlin.Common.pathIdArgsFor' without types:
-- they're all @jint@.
pathArgNamesFor :: SemanticModel -> SResource -> Bool -> [Text]
pathArgNamesFor model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case lookupResource pname of
            Just parentRes -> pathArgNamesFor model parentRes True
            Nothing        -> []
        Nothing -> []
    lookupResource pname = case [res | res <- resources model, resourceName res == pname] of
        (res : _) -> Just res
        []        -> Nothing
    ownArg = case resourceType r of
        ResId (SResourceId idTypeName) ->
            let shortName = stripParentPrefix idTypeName
            in [camelCaseLocal shortName]
        _ -> []
    stripParentPrefix name = case parent r of
        Just p -> case Text.stripPrefix (p <> "_") name of
            Just rest -> rest
            Nothing   -> name
        Nothing -> name

jReturnType :: SMethod -> Text
jReturnType m = case (output m, methodErrorType m) of
    (SVoid, _)                                              -> "void"
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (SBool, _)                                              -> "jboolean"
    (SInt 8, _)                                             -> "jbyte"
    (SInt 16, _)                                            -> "jint" -- 16-bit widens to jint, as in Java.jniType
    (SInt 32, _)                                            -> "jint"
    (SInt 64, _)                                            -> "jlong"
    (SInt _, _)                                             -> "jint"
    (SUInt 8, _)                                            -> "jbyte"
    (SUInt 16, _)                                           -> "jint"
    (SUInt 32, _)                                           -> "jint"
    (SUInt 64, _)                                           -> "jlong"
    (SUInt _, _)                                            -> "jint"
    (SSizeT, _)                                             -> "jlong"
    (SEnum _, _)                                            -> "jint"
    (SResourceId _, _)                                      -> "jint"
    _                                                       -> "jobject"

-- | The Java sub-module's @jniMethodName@.
jMethodName :: Text -> Text
jMethodName n =
    case Text.stripPrefix "tox_pass_" n of
        Just rest -> "toxPass" <> pascalCaseHead rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> "toxav" <> pascalCaseHead rest
            Nothing -> case Text.stripPrefix "tox_" n of
                Just rest -> "tox" <> pascalCaseHead rest
                Nothing   -> camelCaseLocal n
  where
    pascalCaseHead t = case Text.split (== '_') t of
        []     -> ""
        chunks -> Text.concat (map capFirst chunks)
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCaseLocal :: Text -> Text
camelCaseLocal t = case Text.split (== '_') t of
    []         -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing      -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

camelCaseParam :: Text -> Text
camelCaseParam = camelCaseLocal

-- | A list of @uint32_t@ (or resource IDs), with a @_size@ companion.
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

-- | One iteration, returning the accumulated events as bytes, or @null@
-- when there are none so an idle tick allocates nothing.
renderIterateBody :: SResource -> SMethod -> [Text]
renderIterateBody r m =
    let
        jName = jMethodName (methodName m)
        handleType = cName r
        cFunc = methodName m
        -- @toxav_iterate@ fills @events@ through its callbacks instead.
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
    [ "// TODO: " <> jMethodName (methodName m) <> " is hand-written."
    , ""
    ]

--------------------------------------------------------------------------------
-- natives.h
--------------------------------------------------------------------------------

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

jniMethodName :: Text -> Text
jniMethodName n =
    case Text.stripPrefix "tox_pass_" n of
        Just rest -> "toxPass" <> pascalCaseHead rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> "toxav" <> pascalCaseHead rest
            Nothing -> case Text.stripPrefix "tox_" n of
                Just rest -> "tox" <> pascalCaseHead rest
                Nothing   -> camelCase n
  where
    pascalCaseHead t = case Text.split (== '_') t of
        []     -> ""
        chunks -> Text.concat (map capFirst chunks)
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

camelCase :: Text -> Text
camelCase t = case Text.split (== '_') t of
    []         -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirst rest)
  where
    capFirst s = case Text.uncons (Text.toLower s) of
        Nothing      -> ""
        Just (c, t') -> Text.cons (Char.toUpper c) t'

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t
