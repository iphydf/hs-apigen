{-# LANGUAGE OverloadedStrings #-}

-- | @Tox*Jni.java@ native declarations and their javah headers.
module Apigen.Language.Jvm.Java (generate) where

import           Apigen.Language.Jvm.Conventions (boolReturnsValue,
                                                  hasNoJniShim,
                                                  isHiddenFromPublicApi,
                                                  jniExtraHeaderDecls,
                                                  jniExtraJavaDecls,
                                                  jniExtraJavaImports,
                                                  syntheticExceptionFor)
import           Apigen.Language.Jvm.Model       (avResourceNames,
                                                  coreResourceNames,
                                                  cryptoResourceNames,
                                                  exceptionClassFor,
                                                  isSettingsName,
                                                  libraryMethodName)
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
    [ ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreJni.java", renderJni model "ToxCoreJni" "core" (coreResourceNames model))
    , ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxAvJni.java", renderJni model "ToxAvJni" "av" (avResourceNames model))
    , ("lib/src/main/java/im/tox/tox4j/impl/jni/ToxCryptoJni.java", renderJni model "ToxCryptoJni" "crypto" (cryptoResourceNames model))
    , ("lib/src/main/cpp/ToxCore/generated/im_tox_tox4j_impl_jni_ToxCoreJni.h", renderJavah model "ToxCoreJni" (coreResourceNames model))
    , ("lib/src/main/cpp/ToxAv/generated/im_tox_tox4j_impl_jni_ToxAvJni.h", renderJavah model "ToxAvJni" (avResourceNames model))
    , ("lib/src/main/cpp/ToxCrypto/generated/im_tox_tox4j_impl_jni_ToxCryptoJni.h", renderJavah model "ToxCryptoJni" (cryptoResourceNames model))
    ]


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
        byRole (methodRole m) || hasNoJniShim model m || isHiddenFromPublicApi model (methodName m)
    byRole Constructor   = False
    byRole Destructor    = False
    byRole RegistrarRole = True
    byRole StaticRole    = True
    byRole _             = False

    errorTypes =
        List.sort
            . List.nub
            $ [ exceptionClassName model errTy
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
            args = pathIdArgs model r includeOwn ++ jniInputs model (inputs m)
            argList = Text.intercalate ", " (instanceArg ++ args)
            instanceArg = if needsInstance then ["int instanceNumber"] else []
            ret = jniReturnType model m
            name = jniMethodName model m
            thr = case effectiveErrorType m of
                Just errTy -> " throws " <> exceptionClassName model errTy
                Nothing    -> ""
        in
            "    static native " <> ret <> " " <> name <> "(" <> argList <> ")" <> thr <> ";"

exceptionClassName :: SemanticModel -> Text -> Text
exceptionClassName model = exceptionClassFor model pascalCase

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

jniInputs :: SemanticModel -> [SParameter] -> [Text]
jniInputs model = map (jniParam model)

jniParam :: SemanticModel -> SParameter -> Text
jniParam model p = jniType model (paramType p) <> " " <> camelCase (paramName p)

-- | 16-bit ints widen to @int@, as the hand-written C++ takes @jint@.
jniType :: SemanticModel -> SType -> Text
jniType model ty = case ty of
    SVoid                              -> "void"
    SBool                              -> "boolean"
    SInt 8                             -> "byte"
    SInt 16                            -> "int"
    SInt 32                            -> "int"
    SInt 64                            -> "long"
    SInt _                             -> "int"
    SUInt 8                            -> "byte"
    SUInt 16                           -> "int"
    SUInt 32                           -> "int"
    SUInt 64                           -> "long"
    SUInt _                            -> "int"
    SSizeT                             -> "long"
    SString                            -> "String"
    SBytes                             -> "byte[]"
    SFixedBytes _ _                    -> "byte[]"
    SFixedList (SInt 16) _ _           -> "short[]"
    SFixedList (SUInt 16) _ _          -> "short[]"
    SFixedList (SInt 8) _ _            -> "byte[]"
    SFixedList (SUInt 8) _ _           -> "byte[]"
    SFixedList (SInt 32) _ _           -> "int[]"
    SFixedList (SUInt 32) _ _          -> "int[]"
    SFixedList _ _ _                   -> "Object" -- unsupported
    SEnum _                            -> "int"
    -- A serialized proto @Options@ message.
    SHandle h | isSettingsName model h -> "byte[]"
    SHandle _                          -> "int"
    SCallback _                        -> "Object"
    SResourceId _                      -> "int"
    SList (SInt 16)                    -> "short[]"
    SList (SUInt 16)                   -> "short[]"
    SList (SInt 8)                     -> "byte[]"
    SList (SUInt 8)                    -> "byte[]"
    SList _                            -> "int[]"

jniReturnType :: SemanticModel -> SMethod -> Text
jniReturnType _ m
    | methodRole m == IterateRole = "byte[]"
jniReturnType model m = case (output m, methodErrorType m) of
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (ty, _)                                                 -> jniType model ty

jniMethodName :: SemanticModel -> SMethod -> Text
jniMethodName model m = libraryMethodName model pascalCaseHead camelCase (methodName m)
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
            || hasNoJniShim model m
            || isHiddenFromPublicApi model (methodName m)
    byRole Constructor   = False
    byRole Destructor    = False
    byRole RegistrarRole = True
    byRole StaticRole    = True
    byRole _             = False

renderJavahDecl :: SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderJavahDecl model className (r, m) =
    [ ""
    , "/*"
    , " * Class:     im_tox_tox4j_impl_jni_" <> className
    , " * Method:    " <> jniMethodName model m
    , " */"
    , "JNIEXPORT " <> jniReturnTypeC model m <> " JNICALL Java_im_tox_tox4j_impl_jni_" <> className <> "_" <> jniMethodName model m
    , "  (JNIEnv *, jclass" <> argList <> ");"
    ]
  where
    needsInstance = not (methodRole m == Constructor && parent r == Nothing)
    instanceArg = if needsInstance then ["jint"] else []
    pathArgs = replicate (length (pathIdArgsCount model r (methodRole m /= Constructor))) "jint"
    sigArgs = instanceArg ++ pathArgs ++ map (jniTypeC model . paramType) (inputs m)
    argList = if null sigArgs then "" else ", " <> Text.intercalate ", " sigArgs

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

jniTypeC :: SemanticModel -> SType -> Text
jniTypeC model ty = case ty of
    SVoid                              -> "void"
    SBool                              -> "jboolean"
    SInt 8                             -> "jbyte"
    SInt 16                            -> "jint"
    SInt 32                            -> "jint"
    SInt 64                            -> "jlong"
    SInt _                             -> "jint"
    SUInt 8                            -> "jbyte"
    SUInt 16                           -> "jint"
    SUInt 32                           -> "jint"
    SUInt 64                           -> "jlong"
    SUInt _                            -> "jint"
    SSizeT                             -> "jlong"
    SString                            -> "jstring"
    SBytes                             -> "jbyteArray"
    SFixedBytes _ _                    -> "jbyteArray"
    SFixedList (SInt 16) _ _           -> "jshortArray"
    SFixedList (SUInt 16) _ _          -> "jshortArray"
    SFixedList (SInt 8) _ _            -> "jbyteArray"
    SFixedList (SUInt 8) _ _           -> "jbyteArray"
    SFixedList (SInt 32) _ _           -> "jintArray"
    SFixedList (SUInt 32) _ _          -> "jintArray"
    SFixedList _ _ _                   -> "jobject"
    SEnum _                            -> "jint"
    SHandle h | isSettingsName model h -> "jbyteArray"
    SHandle _                          -> "jint"
    SCallback _                        -> "jobject"
    SResourceId _                      -> "jint"
    SList (SInt 16)                    -> "jshortArray"
    SList (SUInt 16)                   -> "jshortArray"
    SList (SInt 8)                     -> "jbyteArray"
    SList (SUInt 8)                    -> "jbyteArray"
    SList _                            -> "jintArray"

jniReturnTypeC :: SemanticModel -> SMethod -> Text
jniReturnTypeC _ m
    | methodRole m == IterateRole = "jbyteArray"
jniReturnTypeC model m = case (output m, methodErrorType m) of
    (SBool, Just _) | not (boolReturnsValue (methodName m)) -> "void"
    (ty, _)                                                 -> jniTypeC model ty
