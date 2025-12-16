{-# LANGUAGE OverloadedStrings #-}

-- | @Tox*Jni.java@ native declarations and their javah headers.
module Apigen.Language.Jvm.Java (generate) where

import           Apigen.Language.Jvm.Conventions (hasNoJniShim,
                                                  isHiddenFromPublicApi,
                                                  jniExtraJavaImports,
                                                  jniExtraNatives,
                                                  optionsWireFields,
                                                  syntheticExceptionFor)
import           Apigen.Language.Jvm.Jni         (Kind (..), Native (..),
                                                  factNative, javaDecl,
                                                  javahDecl, kind, pathIds,
                                                  returnKind)
import           Apigen.Language.Jvm.Model       (Subsystem (..),
                                                  exceptionClassFor,
                                                  isSettingsName, jniMethodName,
                                                  jniPrefix, jvmName,
                                                  subsystems)
import           Apigen.Language.Jvm.Names       (camelCase, pascalCase)
import           Apigen.Semantic                 (SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SProperty (..),
                                                  SResource (..), STargets (..),
                                                  SType (..),
                                                  SemanticModel (..))
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model = concat
    [ [ (javaSource model "impl.jni" jni, renderJni model jni (subWord s) ms ns)
      , (cppSource s (Text.unpack jniClass <> ".h"), renderJavah jniClass ns)
      ]
    | s <- subsystems model
    , let jni = subClass s <> "Jni"
          jniClass = jniPrefix model <> "impl_jni_" <> jni
          ms = natives model (subResources s)
          ns = map (methodNative model) ms ++ map factNative (jniExtraNatives model jni)
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

renderJni :: SemanticModel -> Text -> Text -> [(SResource, SMethod)] -> [Native] -> Text
renderJni model className exnPkg methodList ns =
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
            ++ map javaDecl ns
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
              , Just errTy <- [effectiveErrorType model m]
              ]
    imports = [ "import " <> jvmName model (exnPkg <> ".exceptions." <> ex) <> ";" | ex <- errorTypes ]

-- | A generated method's native: the instance and the ids naming its
-- resource come before its own arguments.
methodNative :: SemanticModel -> (SResource, SMethod) -> Native
methodNative model (r, m) = Native
    { nativeName = jniMethodName model (methodName m)
    , nativeReturn = returnKind model m
    , nativeParams =
        [ (Int, "instanceNumber") | hasInstance r m ]
        ++ [ (Int, camelCase n) | (n, _) <- pathIds model r (methodRole m /= Constructor) ]
        ++ concatMap param (inputs m)
    , nativeThrows = exceptionClassName model <$> effectiveErrorType model m
    }
  where
    -- The options travel as their fields.
    param p = case paramType p of
        SHandle h | isSettingsName model h ->
            [ (kind model (propType f), camelCase (propName f)) | f <- optionsWireFields model ]
        t -> [(kind model t, camelCase (paramName p))]

effectiveErrorType :: SemanticModel -> SMethod -> Maybe Text
effectiveErrorType model m = case methodErrorType m of
    Just errTy -> Just errTy
    Nothing    -> syntheticExceptionFor model (methodName m)

-- | Root constructors (@tox_new@ etc.) have no instance yet.
hasInstance :: SResource -> SMethod -> Bool
hasInstance r m = not (methodRole m == Constructor && parent r == Nothing)

exceptionClassName :: SemanticModel -> Text -> Text
exceptionClassName model = exceptionClassFor model pascalCase

--------------------------------------------------------------------------------
-- javah header
--------------------------------------------------------------------------------

renderJavah :: Text -> [Native] -> Text
renderJavah jniClass ns =
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
            ++ concatMap (javahDecl jniClass) ns
            ++ [ "#ifdef __cplusplus"
               , "}"
               , "#endif"
               , "#endif"
               ]
