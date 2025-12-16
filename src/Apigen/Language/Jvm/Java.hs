{-# LANGUAGE OverloadedStrings #-}

-- | @Tox*Jni.java@ native declarations and their javah headers.
module Apigen.Language.Jvm.Java (generate) where

import           Apigen.Language.Jvm.Conventions (hasNoJniShim,
                                                  isHiddenFromPublicApi,
                                                  jniExtraHeaderDecls,
                                                  jniExtraJavaDecls,
                                                  jniExtraJavaImports,
                                                  syntheticExceptionFor)
import           Apigen.Language.Jvm.Jni         (jniReturnType, jniReturnTypeC,
                                                  jniType, jniTypeC, pathIds)
import           Apigen.Language.Jvm.Model       (Subsystem (..),
                                                  exceptionClassFor,
                                                  jniMethodName, jniPrefix,
                                                  jvmName, subsystems)
import           Apigen.Language.Jvm.Names       (camelCase, pascalCase)
import           Apigen.Semantic                 (SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SResource (..), STargets (..),
                                                  SemanticModel (..))
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model = concat
    [ [ (javaSource model "impl.jni" jni, renderJni model jni (subWord s) ns)
      , (cppSource s (Text.unpack (jniPrefix model <> "impl_jni_" <> jni) <> ".h"), renderJavah model jni ns)
      ]
    | s <- subsystems model
    , let jni = subClass s <> "Jni"
          ns = natives model (subResources s)
    ]

-- | A Java source in a package under the root: @lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreJni.java@.
javaSource :: SemanticModel -> Text -> Text -> FilePath
javaSource model pkg cls =
    "lib/src/main/java/" <> Text.unpack (Text.replace "." "/" (jvmName model pkg)) <> "/" <> Text.unpack cls <> ".java"

-- | A generated file of a subsystem's C++: @lib/src/main/cpp/ToxCore/generated/natives.h@.
cppSource :: Subsystem -> FilePath -> FilePath
cppSource s name = "lib/src/main/cpp/" <> Text.unpack (subClass s) <> "/generated/" <> name

-- | The functions with a native: not callback setters or statics.
natives :: SemanticModel -> [Text] -> [(SResource, SMethod)]
natives model resourceNames =
    [ (r, m)
    | r <- resources model
    , resourceName r `elem` resourceNames
    , m <- methods r
    , methodRole m `notElem` [RegistrarRole, StaticRole]
    , not (hasNoJniShim model m)
    , not (isHiddenFromPublicApi model (methodName m))
    ]

renderJni :: SemanticModel -> Text -> Text -> [(SResource, SMethod)] -> Text
renderJni model className exnPkg methodList =
    Text.unlines $
        [ "package " <> jvmName model "impl.jni" <> ";"
        , ""
        ]
            ++ allImports
            ++ [ ""
               , "@SuppressWarnings({\"checkstyle:emptylineseparator\", \"checkstyle:linelength\"})"
               , "public final class " <> className <> " {"
               , "    static {"
               , "      System.loadLibrary(\"" <> jvmNativeLibrary (targets model) <> "\");"
               , "    }"
               , ""
               ]
            ++ map renderDecl methodList
            ++ jniExtraJavaDecls model className
            ++ ["}"]
  where
    allImports =
        List.sort . List.nub
            $ imports
            ++ map (\i -> "import " <> i <> ";") (jniExtraJavaImports model className)

    errorTypes =
        List.sort
            . List.nub
            $ [ exceptionClassName model errTy
              | (_, m) <- methodList
              , Just errTy <- [effectiveErrorType m]
              ]
    imports = [ "import " <> jvmName model (exnPkg <> ".exceptions." <> ex) <> ";" | ex <- errorTypes ]

    effectiveErrorType m = case methodErrorType m of
        Just errTy -> Just errTy
        Nothing    -> syntheticExceptionFor model (methodName m)

    renderDecl (r, m) =
        let args = [ "int " <> camelCase n | (n, _) <- pathIds model r (methodRole m /= Constructor) ]
                ++ [ jniType model (paramType p) <> " " <> camelCase (paramName p) | p <- inputs m ]
            argList = Text.intercalate ", " (instanceArg r m ++ args)
            thr = case effectiveErrorType m of
                Just errTy -> " throws " <> exceptionClassName model errTy
                Nothing    -> ""
        in
            "    static native " <> jniReturnType model m <> " " <> jniMethodName model (methodName m)
                <> "(" <> argList <> ")" <> thr <> ";"

-- | Root constructors (@tox_new@ etc.) have no instance yet.
instanceArg :: SResource -> SMethod -> [Text]
instanceArg r m
    | methodRole m == Constructor && parent r == Nothing = []
    | otherwise                                          = ["int instanceNumber"]

exceptionClassName :: SemanticModel -> Text -> Text
exceptionClassName model = exceptionClassFor model pascalCase

--------------------------------------------------------------------------------
-- javah header
--------------------------------------------------------------------------------

renderJavah :: SemanticModel -> Text -> [(SResource, SMethod)] -> Text
renderJavah model className methodList =
    Text.unlines $
        [ "/* DO NOT EDIT THIS FILE - it is machine generated */"
        , "#include <jni.h>"
        , "/* Header for class " <> jniClass <> " */"
        , ""
        , "#ifndef _Included_" <> jniClass
        , "#define _Included_" <> jniClass
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
    jniClass = jniPrefix model <> "impl_jni_" <> className
    headerExtras = case jniExtraHeaderDecls model className of
        []    -> []
        decls -> "" : decls

renderJavahDecl :: SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderJavahDecl model className (r, m) =
    [ ""
    , "/*"
    , " * Class:     " <> jniPrefix model <> "impl_jni_" <> className
    , " * Method:    " <> name
    , " */"
    , "JNIEXPORT " <> jniReturnTypeC model m <> " JNICALL Java_" <> jniPrefix model <> "impl_jni_" <> className <> "_" <> name
    , "  (JNIEnv *, jclass" <> argList <> ");"
    ]
  where
    name = jniMethodName model (methodName m)
    sigArgs = map (const "jint") (instanceArg r m)
        ++ map (const "jint") (pathIds model r (methodRole m /= Constructor))
        ++ map (jniTypeC model . paramType) (inputs m)
    argList = Text.concat (map (", " <>) sigArgs)
