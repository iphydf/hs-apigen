{-# LANGUAGE OverloadedStrings #-}

-- | JVM (jvm-toxcore-c) binding generator.
--
-- Aggregates the per-language sub-modules of the JVM binding:
--
--   * 'Apigen.Language.Jvm.Kotlin' — the Kotlin public API surface
--     (interfaces, data classes, enums, exceptions, callbacks, event
--     dispatch).
--   * 'Apigen.Language.Jvm.Java' — the @*Jni.java@ files that declare
--     the @static native@ methods.
--   * 'Apigen.Language.Jvm.Cpp' — the @lib/src/main/cpp/Tox*/generated/@
--     files and the @JAVA_METHOD@ bodies.
--   * 'Apigen.Language.Jvm.Proto' — the @Core.proto@ / @Av.proto@
--     schema that backs the event-dispatch wire format. (The protobuf
--     middle layer is load-bearing for Android — see
--     @project_jvm_event_layer_protobuf@.)
--
-- All paths produced by 'generate' are relative to the
-- @jvm-toxcore-c@ repository root.
module Apigen.Language.Jvm (generate) where

import qualified Apigen.Language.Jvm.Cpp                  as Cpp
import qualified Apigen.Language.Jvm.Java                 as Java
import qualified Apigen.Language.Jvm.Kotlin               as Kotlin
import qualified Apigen.Language.Jvm.Kotlin.EventDispatch as KotlinEvents
import qualified Apigen.Language.Jvm.Kotlin.Impl          as KotlinImpl
import qualified Apigen.Language.Jvm.Proto                as Proto
import           Apigen.Parser.Docs                       (Docs)
import           Apigen.Semantic                          (SemanticModel)
import           Data.Text                                (Text)

-- | The 'Docs' record bundles function and enum-member kdoc extracted
-- from the source headers. Passed alongside the 'SemanticModel'
-- (rather than embedded in it) so the round-trip check stays
-- meaningful — the C round-trip doesn't preserve comments.
generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model =
    concat
        [ Kotlin.generate docs model
        , KotlinImpl.generate model
        , KotlinEvents.generate model
        , Java.generate model
        , Cpp.generate model
        , Proto.generate model
        ]
