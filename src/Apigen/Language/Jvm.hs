{-# LANGUAGE OverloadedStrings #-}

-- | JVM (jvm-toxcore-c) binding generator. Paths are relative to the
-- jvm-toxcore-c repository root.
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

-- | 'Docs' is separate from the model because the C round-trip check
-- doesn't preserve comments.
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
