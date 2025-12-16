{-# LANGUAGE OverloadedStrings #-}

-- | The @Tox*Jni.java@ native declarations and their javah headers.
module Apigen.Language.Jvm.Java (generate) where

import           Apigen.Language.Jvm.Conventions (boolReturnsValue,
                                                  isHiddenFromPublicApi,
                                                  jniExtraHeaderDecls,
                                                  jniExtraJavaDecls,
                                                  jniExtraJavaImports,
                                                  syntheticExceptionFor)
import           Apigen.Semantic                 (SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
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
    [ ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreJni.java", renderJni model "ToxCoreJni" "core" coreResources)
    , ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxAvJni.java", renderJni model "ToxAvJni" "av" avResources)
    , ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxCryptoJni.java", renderJni model "ToxCryptoJni" "crypto" cryptoResources)
    , ("lib/src/main/cpp/ToxCore/generated/im_tox_tox4j_impl_jni_ToxCoreJni.h", renderJavah model "ToxCoreJni" coreResources)
    , ("lib/src/main/cpp/ToxAv/generated/im_tox_tox4j_impl_jni_ToxAvJni.h", renderJavah model "ToxAvJni" avResources)
    , ("lib/src/main/cpp/ToxCrypto/generated/im_tox_tox4j_impl_jni_ToxCryptoJni.h", renderJavah model "ToxCryptoJni" cryptoResources)
    ]

coreResources, avResources, cryptoResources :: [Text]
coreResources =
    [ "Tox"
    , "Friend"
    , "Conference"
    , "Conference_Peer"
    , "Conference_Offline_Peer"
    , "Group"
    , "Group_Peer"
    , "File"
    -- Not Options: it crosses JNI as a proto message passed to @toxNew@.
    ]
avResources = ["AV"]
cryptoResources = ["Pass_Key"]

renderJni :: SemanticModel -> Text -> Text -> [Text] -> Text
renderJni model className exnPkg resourceNames =
    Text.unlines $
        [ "package im.tox.tox4j.impl.jni;"
        , ""
        ]
            ++ allImports
            ++ [ ""
               , "@SuppressWarnings({\"checkstyle:emptylineseparator\", \"checkstyle:linelength\"})"
               , "public final class " <> className <> " {"
               , "    static {"
               , "      System.loadLibrary(\"tox4j-c\");"
               , "    }"
               , ""
               ]
            ++ map renderDecl methodList
            ++ jniExtraJavaDecls className
            ++ ["}"]
  where
    allImports =
        List.sort . List.nub
            $ imports
            ++ map (\i -> "import " <> i <> ";") (jniExtraJavaImports className)
    methodList =
        [ (r, m)
        | r <- resources model
        , resourceName r `elem` resourceNames
        , m <- methods r
        , not (skipMethod m)
        ]
    skipMethod m =
        byRole (methodRole m) || byName (methodName m) || isHiddenFromPublicApi model (methodName m)
    byRole Constructor   = False
    byRole Destructor    = False
    byRole RegistrarRole = True
    byRole StaticRole    = True
    byRole _             = False
    -- No JNI shim: the AV groupchat functions (which take a Tox, not a
    -- ToxAV) and the split audio/video iterators.
    byName n =
        n `elem`
            [ "toxav_audio_iterate"
            , "toxav_audio_iteration_interval"
            , "toxav_video_iterate"
            , "toxav_video_iteration_interval"
            , "toxav_get_tox"
            , "toxav_group_send_audio"
            , "toxav_groupchat_av_enabled"
            , "toxav_groupchat_disable_av"
            ]

    errorTypes =
        List.sort
            . List.nub
            $ [ exceptionClassName errTy
              | (_, m) <- methodList
              , Just errTy <- [effectiveErrorType m]
              ]
    imports = [ "import im.tox.tox4j." <> exnPkg <> ".exceptions." <> ex <> ";" | ex <- errorTypes ]

    effectiveErrorType m = case methodErrorType m of
        Just errTy -> Just errTy
        Nothing    -> syntheticExceptionFor model (methodName m)

    renderDecl (r, m) =
        let
            includeOwn = methodRole m /= Constructor
            -- Root constructors (@tox_new@ etc.) have no instance yet.
            needsInstance = not (methodRole m == Constructor && parent r == Nothing)
            args = pathIdArgs model r includeOwn ++ jniInputs (inputs m)
            argList = Text.intercalate ", " (instanceArg ++ args)
            instanceArg = if needsInstance then ["int instanceNumber"] else []
            ret = jniReturnType m
            name = jniMethodName m
            thr = case effectiveErrorType m of
                Just errTy -> " throws " <> exceptionClassName errTy
                Nothing    -> ""
        in
            "    static native " <> ret <> " " <> name <> "(" <> argList <> ")" <> thr <> ";"

-- | @Tox_Err_Friend_Add@ -> @ToxFriendAddException@.
exceptionClassName :: Text -> Text
exceptionClassName errTy =
    case Text.stripPrefix "Tox_Err_" errTy of
        Just rest -> "Tox" <> pascalCase rest <> "Exception"
        Nothing -> case Text.stripPrefix "Toxav_Err_" errTy of
            Just rest -> "Toxav" <> pascalCase rest <> "Exception"
            Nothing   -> errTy

-- | The @int@ IDs of the resource and its parents, and its own unless
-- it's a constructor.
pathIdArgs :: SemanticModel -> SResource -> Bool -> [Text]
pathIdArgs model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case List.find ((== pname) . resourceName) (resources model) of
            Just parentRes -> pathIdArgs model parentRes True
            Nothing        -> []
        Nothing -> []
    ownArg = case resourceType r of
        ResId (SResourceId idTypeName) ->
            let shortName = case parent r >>= \p -> Text.stripPrefix (p <> "_") idTypeName of
                    Just rest -> rest
                    Nothing   -> idTypeName
            in ["int " <> camelCase shortName]
        _ -> []

jniInputs :: [SParameter] -> [Text]
jniInputs = map jniParam

jniParam :: SParameter -> Text
jniParam p = jniType (paramType p) <> " " <> camelCase (paramName p)

-- | 16-bit ints widen to @int@, as the hand-written C++ takes @jint@.
jniType :: SType -> Text
jniType ty = case ty of
    SVoid                     -> "void"
    SBool                     -> "boolean"
    SInt 8                    -> "byte"
    SInt 16                   -> "int"
    SInt 32                   -> "int"
    SInt 64                   -> "long"
    SInt _                    -> "int"
    SUInt 8                   -> "byte"
    SUInt 16                  -> "int"
    SUInt 32                  -> "int"
    SUInt 64                  -> "long"
    SUInt _                   -> "int"
    SSizeT                    -> "long"
    SString                   -> "String"
    SBytes                    -> "byte[]"
    SFixedBytes _ _           -> "byte[]"
    SFixedList (SInt 16) _ _  -> "short[]"
    SFixedList (SUInt 16) _ _ -> "short[]"
    SFixedList (SInt 8) _ _   -> "byte[]"
    SFixedList (SUInt 8) _ _  -> "byte[]"
    SFixedList (SInt 32) _ _  -> "int[]"
    SFixedList (SUInt 32) _ _ -> "int[]"
    SFixedList _ _ _          -> "Object" -- unsupported
    SEnum _                   -> "int"
    -- A serialized proto @Options@ message.
    SHandle "Options"         -> "byte[]"
    SHandle _                 -> "int"
    SCallback _               -> "Object"
    SResourceId _             -> "int"
    SList (SInt 16)           -> "short[]"
    SList (SUInt 16)          -> "short[]"
    SList (SInt 8)            -> "byte[]"
    SList (SUInt 8)           -> "byte[]"
    SList _                   -> "int[]"

jniReturnType :: SMethod -> Text
jniReturnType m
    -- The serialized events.
    | methodName m
        `elem` [ "tox_iterate"
               , "toxav_iterate"
               , "toxav_audio_iterate"
               , "toxav_video_iterate"
               ] = "byte[]"
jniReturnType m = case (output m, methodErrorType m) of
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (ty, _)                                                 -> jniType ty

jniMethodName :: SMethod -> Text
jniMethodName m =
    let base = case Text.stripPrefix "tox_pass_" (methodName m) of
            Just rest -> "toxPass" <> pascalCaseHead rest
            Nothing -> case Text.stripPrefix "toxav_" (methodName m) of
                Just rest -> "toxav" <> pascalCaseHead rest
                Nothing -> case Text.stripPrefix "tox_" (methodName m) of
                    Just rest -> "tox" <> pascalCaseHead rest
                    Nothing   -> camelCase (methodName m)
    in base
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

pascalCase :: Text -> Text
pascalCase = Text.concat . map cap . Text.split (== '_')
  where
    cap s = case Text.uncons (Text.toLower s) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toUpper c) t

--------------------------------------------------------------------------------
-- javah header
--------------------------------------------------------------------------------

-- | The header javah would make from the @*Jni.java@.
renderJavah :: SemanticModel -> Text -> [Text] -> Text
renderJavah model className resourceNames =
    Text.unlines $
        [ "/* DO NOT EDIT THIS FILE - it is machine generated */"
        , "#include <jni.h>"
        , "/* Header for class im_tox_tox4j_impl_jni_" <> className <> " */"
        , ""
        , "#ifndef _Included_im_tox_tox4j_impl_jni_" <> className
        , "#define _Included_im_tox_tox4j_impl_jni_" <> className
        , "#ifdef __cplusplus"
        , "extern \"C\" {"
        , "#endif"
        ]
            ++ concatMap (renderJavahDecl model className) methodList
            ++ headerExtras
            ++ [ "#ifdef __cplusplus"
               , "}"
               , "#endif"
               , "#endif"
               ]
  where
    headerExtras = case jniExtraHeaderDecls className of
        []    -> []
        decls -> "" : decls
    methodList =
        [ (r, m)
        | r <- resources model
        , resourceName r `elem` resourceNames
        , m <- methods r
        , not (skipMethod m)
        ]
    skipMethod m =
        byRole (methodRole m)
            || byName (methodName m)
            || isHiddenFromPublicApi model (methodName m)
    byRole Constructor   = False
    byRole Destructor    = False
    byRole RegistrarRole = True
    byRole StaticRole    = True
    byRole _             = False
    byName n =
        n `elem`
            [ "toxav_audio_iterate"
            , "toxav_audio_iteration_interval"
            , "toxav_video_iterate"
            , "toxav_video_iteration_interval"
            , "toxav_get_tox"
            , "toxav_group_send_audio"
            , "toxav_groupchat_av_enabled"
            , "toxav_groupchat_disable_av"
            ]

renderJavahDecl :: SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderJavahDecl model className (r, m) =
    [ ""
    , "/*"
    , " * Class:     im_tox_tox4j_impl_jni_" <> className
    , " * Method:    " <> jniMethodName m
    , " */"
    , "JNIEXPORT " <> jniReturnTypeC m <> " JNICALL Java_im_tox_tox4j_impl_jni_" <> className <> "_" <> jniMethodName m
    , "  (JNIEnv *, jclass" <> argList <> ");"
    ]
  where
    needsInstance = not (methodRole m == Constructor && parent r == Nothing)
    instanceArg = if needsInstance then ["jint"] else []
    pathArgs = replicate (length (pathIdArgsCount model r (methodRole m /= Constructor))) "jint"
    sigArgs = instanceArg ++ pathArgs ++ map (jniTypeC . paramType) (inputs m)
    argList = if null sigArgs then "" else ", " <> Text.intercalate ", " sigArgs

-- | One @()@ per path-id arg.
pathIdArgsCount :: SemanticModel -> SResource -> Bool -> [()]
pathIdArgsCount model r includeOwn =
    parentArgs ++ (if includeOwn then ownArg else [])
  where
    parentArgs = case parent r of
        Just pname -> case List.find ((== pname) . resourceName) (resources model) of
            Just parentRes -> pathIdArgsCount model parentRes True
            Nothing        -> []
        Nothing -> []
    ownArg = case resourceType r of
        ResId _ -> [()]
        _       -> []

jniTypeC :: SType -> Text
jniTypeC ty = case ty of
    SVoid                     -> "void"
    SBool                     -> "jboolean"
    SInt 8                    -> "jbyte"
    SInt 16                   -> "jint"
    SInt 32                   -> "jint"
    SInt 64                   -> "jlong"
    SInt _                    -> "jint"
    SUInt 8                   -> "jbyte"
    SUInt 16                  -> "jint"
    SUInt 32                  -> "jint"
    SUInt 64                  -> "jlong"
    SUInt _                   -> "jint"
    SSizeT                    -> "jlong"
    SString                   -> "jstring"
    SBytes                    -> "jbyteArray"
    SFixedBytes _ _           -> "jbyteArray"
    SFixedList (SInt 16) _ _  -> "jshortArray"
    SFixedList (SUInt 16) _ _ -> "jshortArray"
    SFixedList (SInt 8) _ _   -> "jbyteArray"
    SFixedList (SUInt 8) _ _  -> "jbyteArray"
    SFixedList (SInt 32) _ _  -> "jintArray"
    SFixedList (SUInt 32) _ _ -> "jintArray"
    SFixedList _ _ _          -> "jobject"
    SEnum _                   -> "jint"
    SHandle "Options"         -> "jbyteArray"
    SHandle _                 -> "jint"
    SCallback _               -> "jobject"
    SResourceId _             -> "jint"
    SList (SInt 16)           -> "jshortArray"
    SList (SUInt 16)          -> "jshortArray"
    SList (SInt 8)            -> "jbyteArray"
    SList (SUInt 8)           -> "jbyteArray"
    SList _                   -> "jintArray"

jniReturnTypeC :: SMethod -> Text
jniReturnTypeC m
    | methodName m
        `elem` [ "tox_iterate"
               , "toxav_iterate"
               , "toxav_audio_iterate"
               , "toxav_video_iterate"
               ] = "jbyteArray"
jniReturnTypeC m = case (output m, methodErrorType m) of
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (ty, _)                                                 -> jniTypeC ty
