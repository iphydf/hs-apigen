{-# LANGUAGE OverloadedStrings #-}
-- | "Apigen.OtherLibrary" through every backend: what a backend knows of
-- Tox shows up in its output.
module Apigen.OtherLibrarySpec (spec) where

import qualified Apigen.Language.C       as C
import qualified Apigen.Language.Cpp     as Cpp
import qualified Apigen.Language.Dart    as Dart
import qualified Apigen.Language.Haskell as Haskell
import qualified Apigen.Language.Jvm     as Jvm
import qualified Apigen.Language.Python  as Python
import qualified Apigen.Language.Rust    as Rust
import           Apigen.OtherLibrary     (model)
import           Apigen.Parser.Docs      (emptyDocs)
import           Control.Exception       (evaluate)
import           Data.List               (isSuffixOf)
import qualified Data.Text               as Text
import           Test.Hspec

spec :: Spec
spec = describe "a library that isn't Tox" $ do
    let check name gen = it ("names no Tox in " <> name) $ do
            files <- gen <$> model
            out <- evaluate (Text.concat (concatMap (\(f, t) -> [Text.pack f, t]) files))
            [ (f, l) | (f, t) <- files, l <- Text.pack f : Text.lines t, "tox" `Text.isInfixOf` Text.toLower l ] `shouldBe` []
            Text.length out `shouldSatisfy` (> 0)
    check "C" (\m -> [("foo.h", C.generate m)])
    check "C++" (Cpp.generate emptyDocs)
    check "Dart" Dart.generate
    check "Haskell" (Haskell.generate emptyDocs)
    check "the JVM" (Jvm.generate emptyDocs)
    check "Python" Python.generate
    check "Rust" (Rust.generate (Rust.Options False))

    it "keeps a unit to its C type's range on the JVM" $ do
        files <- Jvm.generate emptyDocs <$> model
        [ Text.strip l | (f, t) <- files, "/FooWeight.kt" `isSuffixOf` f, l <- Text.lines t, "require" `Text.isInfixOf` l ]
            `shouldBe` ["require(value in 0..0xFFFF) { \"FooWeight must fit in uint16_t, got $value\" }"]

    it "keeps a received unit to its C type's range on the JVM, not throwing" $ do
        files <- Jvm.generate emptyDocs <$> model
        [ Text.strip l | (f, t) <- files, "EventDispatch.kt" `isSuffixOf` f, l <- Text.lines t, "FooWeight(" `Text.isInfixOf` l ]
            `shouldBe` ["FooWeight(event.barWeight.weight.coerceIn(0, 0xFFFF)),"]
