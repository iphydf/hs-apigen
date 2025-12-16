{-# LANGUAGE OverloadedStrings #-}

-- | C++ files in @lib/src/main/cpp/{ToxCore,ToxAv,ToxCrypto}/generated/@.
module Apigen.Language.Jvm.Cpp (generate) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.Jvm.Conventions (cppExtraNativeRefs,
                                                  errorEnumIsReachable,
                                                  hasNoJniShim,
                                                  interfaceAllowsStatics,
                                                  isCryptoConstant,
                                                  isHiddenFromPublicApi,
                                                  optionsWireFields,
                                                  syntheticExceptionFor)
import           Apigen.Language.Jvm.Jni         (jniReturnTypeC, jniTypeC,
                                                  pathIds)
import           Apigen.Language.Jvm.Model       (Subsystem (..), jniMethodName,
                                                  subsystems)
import qualified Apigen.Language.Jvm.Model       as JM
import           Apigen.Language.Jvm.Names       (camelCase, pascalCase)
import           Apigen.Model                    (findEnum, ignoresReturn,
                                                  isEnumNamer, mappingOf,
                                                  unitBase, variantTags)
import           Apigen.Namespace                (errorStem)
import           Apigen.Semantic                 (CFunctionMapping (..),
                                                  SConstantModel (..),
                                                  SEnumKind (..),
                                                  SEnumMember (..),
                                                  SEnumModel (..), SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SProperty (..),
                                                  SResource (..),
                                                  SResourceKind (..),
                                                  SResourceType (..),
                                                  SType (..),
                                                  SemanticModel (..))
import           Apigen.SizeExpr                 (SizeExpr, renderSizeExpr,
                                                  sizeName)
import qualified Data.List                       as List
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           System.FilePath                 (takeDirectory, takeFileName)

generate :: SemanticModel -> [(FilePath, Text)]
generate model = concat
    [ [ (cppFile s "enums.cpp", enumsCpp model (subClass s) es) | let es = enumsIn s, not (null es) ]
      ++ [ (cppFile s "errors.cpp", errorsCpp model (subClass s) es) | let es = errorsIn s, not (null es) ]
      ++ [ (cppFile s "constants.h", constantsHeader model (subWord s) (subClass s <> "Constants") cs)
         | let cs = constantsIn s, not (null cs) ]
      ++ [ (cppFile s "natives.h", nativesHeader model (subClass s <> "Jni") (subsystemMethods (subClass s) (subResources s) model)) ]
      -- A standalone handle's lifetime is hand-written.
      ++ [ (cppFile s "impls.h", implsHeader (subClass s <> "Jni") model (subResources s)) | subKind s /= StandaloneHandle ]
      ++ [ (cppFile s "options.h", optionsHeader model) | subKind s == RootHandle ]
    | s <- subsystems model ]
  where
    cppFile s name = "lib/src/main/cpp/" <> Text.unpack (subClass s) <> "/generated/" <> name
    inDir s file = dirOf file == dirOf (resourceFile (subHandle s))
    dirOf = takeFileName . takeDirectory . Text.unpack
    -- Variant tags like @Tox_Event_Type@ are left out: they aren't C typedefs.
    enumsIn s = [ e | e <- enums model, not (isErr e), inDir s (enumFile e), enumName e `notElem` variantTags model ]
    errorsIn s = [ e | e <- enums model, isErr e, errorEnumIsReachable model (enumName e), inDir s (enumFile e) ]
    constantsIn s = case subKind s of
        RootHandle       -> filter (not . isCryptoConstant model . constantName) (constants model)
        StandaloneHandle -> filter (isCryptoConstant model . constantName) (constants model)
        _                -> []

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
              | interfaceAllowsStatics model iface -> isEnumNamer m || isExcludedStatic (methodName m)
              | otherwise                    -> True
          _ -> hasNoJniShim model m
    -- Kotlin-side conveniences, and what the Kotlin API leaves out.
    isExcludedStatic n =
        "_length" `Text.isSuffixOf` n


--------------------------------------------------------------------------------
-- enums.cpp
--------------------------------------------------------------------------------

isErr :: SEnumModel -> Bool
isErr e = enumKind e == ErrorEnum

-- | Ordinals are source order, as in the Kotlin enum class.
enumsCpp :: SemanticModel -> Text -> [SEnumModel] -> Text
enumsCpp model subsystem es =
    Text.unlines $
        ["#include \"../" <> subsystem <> ".h\"", ""]
            ++ concatMap (renderEnumPair model) (List.sortOn enumName es)

renderEnumPair :: SemanticModel -> SEnumModel -> [Text]
renderEnumPair model e =
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
           , "  " <> JM.nativeHelper model "fatal" <> " (\"Invalid enumerator from " <> JM.rootLibraryName model <> "\");"
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
           , "  " <> JM.nativeHelper model "fatal" <> " (\"Invalid enumerator from Java\");"
           , "}"
           , ""
           ]
  where
    typedef = enumName e

--------------------------------------------------------------------------------
-- errors.cpp
--------------------------------------------------------------------------------

errorsCpp :: SemanticModel -> Text -> [SEnumModel] -> Text
errorsCpp model subsystem es =
    Text.unlines $
        ["#include \"../" <> subsystem <> ".h\"", ""]
            ++ concatMap (renderErrorHandle model) (List.sortOn exnNameOf es)
  where
    exnNameOf e = pascalCase (stripErrPrefix model (enumName e))

renderErrorHandle :: SemanticModel -> SEnumModel -> [Text]
renderErrorHandle model e =
    [ "HANDLE (\"" <> exnName <> "\", " <> errType <> ")"
    , "{"
    , "  switch (error)"
    , "    {"
    , "    success_case (" <> macroPrefix <> ");"
    ]
        ++ [ "    failure_case (" <> macroPrefix <> ", " <> code <> ");"
           | code <- List.sort [ enumMemberName m | m <- enumMembers e, enumMemberName m /= "OK" ]
           ]
        ++ [ "    }"
           , "  return unhandled ();"
           , "}"
           , ""
           ]
  where
    -- @Tox_Err_Friend_Add@: "Friend_Add", "FRIEND_ADD", "FriendAdd".
    errType = stripErrPrefix model (enumName e)
    macroPrefix = Text.toUpper errType
    exnName = pascalCase errType

stripErrPrefix :: SemanticModel -> Text -> Text
stripErrPrefix model n = fromMaybe n (errorStem (K.libraryWords model) n)

--------------------------------------------------------------------------------
-- constants.h
--------------------------------------------------------------------------------

constantsHeader :: SemanticModel -> Text -> Text -> [SConstantModel] -> Text
constantsHeader model pkg objectName cs =
    Text.unlines $
        [ "// " <> JM.jvmName model (pkg <> "." <> objectName) <> "$"
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
-- comment saying so, and stay hand-written.
implsHeader :: Text -> SemanticModel -> [Text] -> Text
implsHeader jniClassName model rNames =
    Text.unlines $
        ("// " <> JM.jvmName model ("impl.jni." <> jniClassName))
            : ""
            : concatMap (renderMethodImpl model) (subsystemMethodsPaired rNames model)

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
        -- Handle constructors (toxNew etc.) are hand-written. Id ones
        -- (tox_friend_add etc.) just return an integer.
        Constructor -> case resourceType r of
            ResHandle -> True
            ResId _   -> False
        Destructor -> True
        _ -> hasNoJniShim model m

renderMethodImpl :: SemanticModel -> (SResource, SMethod) -> [Text]
renderMethodImpl model (r, m)
    -- Before 'shouldSkip' (iterate returns the event bytes).
    | methodRole m == IterateRole = renderIterateBody model r m
    | shouldSkip m = handWritten model m
    -- A synthetic exception is thrown by hand-written code.
    | Just _ <- syntheticExceptionFor model (methodName m) = handWritten model m
    | otherwise = case (output m, methodErrorType m) of
        (SBool, Just _) | ignoresReturn m ->
            renderBody model r m "with_instance_ign" Nothing
        (SFixedBytes sizeConst _, Just _) ->
            renderFixedBytesReturn model r m sizeConst
        (SFixedBytes sizeConst _, Nothing) | null (inputs m) ->
            renderFixedBytesReturnNoErr model m sizeConst
        -- Doesn't fit get_vector's signature.
        (SFixedBytes _ _, Nothing) -> handWritten model m
        (SBytes, Just _) | Just sizeFn <- sizeFunction -> renderVarBytesReturnErr model r m sizeFn
        (SBytes, Nothing) | Just sizeFn <- sizeFunction -> renderVarBytesReturnNoErr model m sizeFn
        (SBytes, _) -> handWritten model m
        (SList (SUInt 32), Nothing) | Just sizeFn <- sizeFunction -> renderUInt32ListReturn model m sizeFn
        (SList (SResourceId _), Nothing) | Just sizeFn <- sizeFunction -> renderUInt32ListReturn model m sizeFn
        (SList _, _) -> handWritten model m
        (_, Just _) -> renderBody model r m "with_instance_err" (Just "identity")
        (_, Nothing) -> renderBody model r m "with_instance_noerr" Nothing
  where
    sizeFunction = cSizeFunctionName (mappingOf model r m)

shouldSkip :: SMethod -> Bool
shouldSkip m =
    any nonPrimInput (map paramType (inputs m))
        || nonPrimOutput (output m)
  where
    -- A runtime size (@width * height@ in @toxav_video_send_frame@) needs
    -- overflow checks the template doesn't do.
    nonPrimInput ty = case ty of
        SBytes                  -> False
        SFixedBytes size _      -> not (maybe False isConstantIdentifier (sizeName size))
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

byteArrayPrologue :: SemanticModel -> SMethod -> [Text]
byteArrayPrologue model m = concatMap paramPrologue (inputs m)
  where
    paramPrologue p = case paramType p of
        SBytes ->
            [ "  auto " <> n <> "Data = fromJavaArray (env, " <> n <> ");" ]
        SFixedBytes sizeConst _ ->
            [ "  auto " <> n <> "Data = fromJavaArray (env, " <> n <> ");"
            , "  " <> JM.nativeHelper model "assert" <> " (!" <> n <> " || " <> n <> "Data.size () == " <> renderSizeExpr sizeConst <> ");"
            ]
        _ -> []
      where
        n = camelCase (paramName p)

paramCArgs :: SemanticModel -> SParameter -> [Text]
paramCArgs model p = case paramType p of
    SFixedBytes _ False -> [name <> "Data"]
    SFixedBytes _ True  -> [name <> "Data.data ()", name <> "Data.size ()"]
    SBytes              -> [name <> "Data.data ()", name <> "Data.size ()"]
    SString             -> ["UTFChars (env, " <> name <> ").data ()"]
    SEnum enumNm      -> ["Enum::valueOf<" <> cEnumType model enumNm <> "> (env, " <> name <> ")"]
    _                    -> [name]
  where
    name = camelCase (paramName p)

--------------------------------------------------------------------------------
-- options.h
--------------------------------------------------------------------------------

-- | @toxNew@, taking the options as JNI gives them. The hand-written
-- @new_instance@ makes the options and the instance; this sets them. What the
-- setters point to lives until @new_instance@ returns. The setters' @false@
-- means they couldn't allocate. C's locking is always on (see
-- 'K.lockingOptions').
optionsHeader :: SemanticModel -> Text
optionsHeader model =
    Text.unlines $
        [ "JAVA_METHOD (jint, " <> jniMethodName model (JM.rootNew model) <> "," ]
        ++ zipWith (\p end -> "  " <> jniTypeC model (propType p) <> " " <> arg p <> end) fields
            (replicate (length fields - 1) "," ++ [")"])
        ++ [ "{" ]
        ++ concatMap held fields
        ++ [ "  return new_instance (env, [&] (" <> settingsType <> " *opts)"
           , "    {"
           , "      bool ok = true;"
           ]
        ++ [ "      " <> set <> " (opts, true);" | p <- K.lockingOptions model, Just set <- [propWrite p] ]
        ++ map (("      " <>) . setter) fields
        ++ [ "      return ok;"
           , "    });"
           , "}"
           ]
  where
    fields = optionsWireFields model
    arg = camelCase . propName
    settings = listToMaybe (K.handlesOfKind model SettingsHandle)
    settingsType = maybe "" cName settings
    setPrefix = maybe "" ((<> "set_") . cPrefix) settings
    held p = case unitBase (propType p) of
        SString -> ["  UTFChars " <> arg p <> "Chars (env, " <> arg p <> ");"]
        SBytes  -> ["  auto " <> arg p <> "Bytes = fromJavaArray (env, " <> arg p <> ");"]
        _       -> []
    setter p =
        let n = propName p
            set = setPrefix <> n
        in case unitBase (propType p) of
            SEnum e  -> set <> " (opts, Enum::valueOf<" <> cEnumType model e <> "> (env, " <> arg p <> "));"
            SString  -> "ok = " <> set <> " (opts, " <> arg p <> "Chars.data ()) && ok;"
            SBytes   -> "ok = " <> set <> "_data (opts, " <> arg p <> "Bytes.data (), " <> arg p <> "Bytes.size ()) && ok;"
            SUInt 16 -> set <> " (opts, static_cast<uint16_t> (" <> arg p <> "));"
            _        -> set <> " (opts, " <> arg p <> ");"

-- | An enum's C name.
cEnumType :: SemanticModel -> Text -> Text
cEnumType model n = maybe n enumName (findEnum model n)

renderVarBytesReturnErr :: SemanticModel -> SResource -> SMethod -> Text -> [Text]
renderVarBytesReturnErr model r m sizeFn =
    [ "JAVA_METHOD (jbyteArray, " <> jniMethodName model (methodName m) <> ","
    , "  " <> Text.intercalate ", " (("jint instanceNumber") : pathArgs ++ sigArgs) <> ")"
    , "{"
    ]
    ++ byteArrayPrologue model m
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
    pathArgNames = [ camelCase n | (n, _) <- pathIds model r (methodRole m /= Constructor) ]
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jniTypeC model (paramType p) <> " " <> camelCase (paramName p) | p <- inputs m]
    extraArgs = pathArgNames ++ concatMap (paramCArgs model) (inputs m)
    trailingArgs = if null extraArgs then "" else ", " <> Text.intercalate ", " extraArgs

-- | Both C functions must take only the @Tox *@, as 'get_vector' needs.
renderVarBytesReturnNoErr :: SemanticModel -> SMethod -> Text -> [Text]
renderVarBytesReturnNoErr model m sizeFn =
    [ "JAVA_METHOD (jbyteArray, " <> jniMethodName model (methodName m) <> ","
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

renderFixedBytesReturn :: SemanticModel -> SResource -> SMethod -> SizeExpr -> [Text]
renderFixedBytesReturn model r m sizeConst =
    [ "JAVA_METHOD (jbyteArray, " <> jniMethodName model (methodName m) <> ","
    , "  " <> Text.intercalate ", " (("jint instanceNumber") : pathArgs ++ sigArgs) <> ")"
    , "{"
    , "  uint8_t result[" <> renderSizeExpr sizeConst <> "];"
    ]
    ++ byteArrayPrologue model m
    ++
    [ "  return instances.with_instance_err (env, instanceNumber,"
    , "    [&] (bool) { return toJavaArray (env, result); },"
    , "    " <> Text.intercalate ", " (methodName m : pathArgNames ++ concatMap (paramCArgs model) (inputs m) ++ ["result"])
    , "  );"
    , "}"
    , ""
    ]
  where
    pathArgNames = [ camelCase n | (n, _) <- pathIds model r (methodRole m /= Constructor) ]
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jniTypeC model (paramType p) <> " " <> camelCase (paramName p) | p <- inputs m]

-- | Only for @void f(const Tox *, uint8_t out[K])@.
renderFixedBytesReturnNoErr :: SemanticModel -> SMethod -> SizeExpr -> [Text]
renderFixedBytesReturnNoErr model m sizeConst =
    [ "JAVA_METHOD (jbyteArray, " <> jniMethodName model (methodName m) <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance_noerr (env, instanceNumber,"
    , "    get_vector<uint8_t, constant_size<" <> renderSizeExpr sizeConst <> ">::make, " <> methodName m <> ">::make"
    , "  );"
    , "}"
    , ""
    ]

renderBody :: SemanticModel -> SResource -> SMethod -> Text -> Maybe Text -> [Text]
renderBody model r m helper conv =
    [ "JAVA_METHOD (" <> jniReturnTypeC model m <> ", " <> jniMethodName model (methodName m) <> ","
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
    pathArgNames = [ camelCase n | (n, _) <- pathIds model r (methodRole m /= Constructor) ]
    pathArgs = ["jint " <> n | n <- pathArgNames]
    sigArgs = [jniTypeC model (paramType p) <> " " <> camelCase (paramName p) | p <- inputs m]

    prologueLines = byteArrayPrologue model m

    convAndFunc =
        maybe id (:) conv
            $ methodName m : pathArgNames ++ concatMap (paramCArgs model) (inputs m)


renderUInt32ListReturn :: SemanticModel -> SMethod -> Text -> [Text]
renderUInt32ListReturn model m sizeFn =
    let
        jName = jniMethodName model (methodName m)
        getFn = methodName m
        in
    [ "JAVA_METHOD (jintArray, " <> jName <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance_noerr (env, instanceNumber,"
    , "    get_vector<uint32_t, " <> sizeFn <> ", " <> getFn <> ", jint>::make"
    , "  );"
    , "}"
    , ""
    ]

-- | @null@ when there are no events, so an idle tick allocates nothing.
renderIterateBody :: SemanticModel -> SResource -> SMethod -> [Text]
renderIterateBody model r m =
    let
        jName = jniMethodName model (methodName m)
        handleType = cName r
        cFunc = methodName m
        -- @toxav_iterate@'s callbacks have the events from when they were set.
        callArgs = if cFunc == JM.rootIterate model then "self, &events" else "self"
    in
    [ ""
    , "JAVA_METHOD (jbyteArray, " <> jName <> ","
    , "  jint instanceNumber)"
    , "{"
    , "  return instances.with_instance (env, instanceNumber,"
    , "    [=] (" <> handleType <> " *self, Events &events) -> jbyteArray"
    , "      {"
    , "        " <> cFunc <> " (" <> callArgs <> ");"
    , "        std::vector<uint8_t> taken = events.take ();"
    , "        return taken.empty () ? nullptr : toJavaArray (env, taken);"
    , "      }"
    , "  );"
    , "}"
    , ""
    ]

handWritten :: SemanticModel -> SMethod -> [Text]
handWritten model m =
    [ "// " <> jniMethodName model (methodName m) <> " is hand-written."
    , ""
    ]

--------------------------------------------------------------------------------
-- natives.h
--------------------------------------------------------------------------------

nativesHeader :: SemanticModel -> Text -> [SMethod] -> Text
nativesHeader model jniClass ms =
    Text.unlines $
        ("// " <> JM.jvmName model ("impl.jni." <> jniClass))
            : concatMap render (List.sortOn snd allPairs)
  where
    allPairs =
        [(jniMethodName model (methodName m), methodName m) | m <- ms]
            ++ cppExtraNativeRefs model jniClass
    render (jniName, cFunc) =
        [ "JAVA_METHOD_REF (" <> jniName <> ")"
        , "CXX_FUNCTION_REF (" <> cFunc <> ")"
        ]
