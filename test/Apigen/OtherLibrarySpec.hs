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

    -- Only the binding turns it on, before it makes the instance. The raw
    -- declarations of C's functions name it too.
    describe "C's locking" $ do
        let locking name gen expected = it ("is always on and offered to no one in " <> name) $ do
                files <- gen <$> model
                [ (f, Text.strip l) | (f, t) <- files, l <- Text.lines t
                                    , "thread_safety" `Text.isInfixOf` Text.toLower l ]
                    `shouldBe` expected
        locking "C++" (Cpp.generate emptyDocs)
            [("src/generated/options.cpp", "foo_options_set_experimental_thread_safety(c, true);")]
        locking "Dart" Dart.generate
            [("foo_options.dart", "lib.ffi.foo_options_set_experimental_thread_safety(options, true);")]
        locking "Haskell" (Haskell.generate emptyDocs)
            [ ("FFI/Foo/Foo.hs", "foreign import ccall foo_options_get_experimental_thread_safety")
            , ("FFI/Foo/Foo.hs", "foreign import ccall foo_options_set_experimental_thread_safety")
            , ("Network/Foo/C/Options.hs", "Raw.foo_options_set_experimental_thread_safety ptr (fromBool True)")
            ]
        locking "the JVM" (Jvm.generate emptyDocs)
            [("lib/src/main/cpp/Foo/generated/options.h", "foo_options_set_experimental_thread_safety (opts, true);")]
        locking "Python" Python.generate
            [ ("foo/foo.pxd", "cdef bool foo_options_get_experimental_thread_safety(const Foo_Options* self) noexcept nogil")
            , ("foo/foo.pxd", "cdef void foo_options_set_experimental_thread_safety(Foo_Options* self, bool experimental_thread_safety) noexcept nogil")
            , ("foo/foo.pyx", "foo_options_set_experimental_thread_safety(options._ptr, True)")
            ]
        locking "Rust" (Rust.generate (Rust.Options False))
            [("foo.rs", "ffi::foo_options_set_experimental_thread_safety(options.ptr, true);")]

    -- C may call back from a thread of its own: a callback only adds an
    -- event, which the loop gives once C has returned.
    it "runs no Python but an event in C's callbacks" $ do
        files <- Python.generate <$> model
        let ls ext = [ Text.strip l | (f, t) <- files, ext `isSuffixOf` f, l <- Text.lines t ]
            externs = [ l | l <- ls ".pxd", "cdef " `Text.isPrefixOf` l, "foo_" `Text.isInfixOf` l ]
            handlers = [ l | l <- ls ".pyx", "cdef void handle_" `Text.isPrefixOf` l ]
        externs `shouldSatisfy` (\es -> not (null es) && all ("noexcept nogil" `Text.isSuffixOf`) es)
        handlers `shouldSatisfy` (\hs -> not (null hs) && all ("noexcept with gil:" `Text.isSuffixOf`) hs)
        length (filter ("._events.append(event)" `Text.isSuffixOf`) (ls ".pyx")) `shouldBe` length handlers
        [ f | (f, _) <- files, "events.py" `isSuffixOf` f ] `shouldBe` ["foo/events.py"]

    -- Its loop, AV-like threads and callers share a handle with events.
    it "calls a shared handle's C functions under its lock" $ do
        files <- Python.generate <$> model
        let pyx = [ Text.strip l | (f, t) <- files, "foo.pyx" `isSuffixOf` f, l <- Text.lines t ]
            count x = length (filter (== x) pyx)
        count "self._acquire()" `shouldSatisfy` (> 0)
        count "self._acquire()" `shouldBe` count "self._release()"

    -- The stub, not the extension, is what a type checker reads.
    it "works out a count from its array, but not a picture's size" $ do
        files <- Python.generate <$> model
        let pyi = [ Text.strip l | (f, t) <- files, ".pyi" `isSuffixOf` f, l <- Text.lines t ]
        filter ("def bar_send_" `Text.isPrefixOf`) pyi `shouldBe`
            [ "def bar_send_sound(self, bar_number: BarNumber, pcm: array[int], channels: int) -> bool: ..."
            , "def bar_send_frame(self, bar_number: BarNumber, data: bytes, width: int, height: int) -> bool: ..." ]

    it "raises each error as its own exception" $ do
        files <- Python.generate <$> model
        let pyi = [ Text.strip l | (f, t) <- files, ".pyi" `isSuffixOf` f, l <- Text.lines t ]
        filter ("BarAddError" `Text.isInfixOf`) pyi `shouldBe`
            ["class BarAddError(ApiException):", "code: BarAddError.Code"]

    it "says the word before a getter's verb once" $ do
        files <- Python.generate <$> model
        let pyi = [ Text.strip l | (f, t) <- files, ".pyi" `isSuffixOf` f, l <- Text.lines t ]
        filter ("bar_count" `Text.isInfixOf`) pyi `shouldBe` ["def bar_count(self) -> int: ..."]

    it "types a Python id as its own type in the stub" $ do
        files <- Python.generate <$> model
        let pyi = [ Text.strip l | (f, t) <- files, ".pyi" `isSuffixOf` f, l <- Text.lines t ]
        filter ("NewType(" `Text.isInfixOf`) pyi `shouldBe` ["BarNumber = NewType(\"BarNumber\", int)"]
        filter ("Foo_Bar_Number" `Text.isInfixOf`) pyi `shouldBe` []
        filter (" = ..." `Text.isSuffixOf`) pyi `shouldBe` []

    it "keeps a unit to its C type's range on the JVM" $ do
        files <- Jvm.generate emptyDocs <$> model
        [ Text.strip l | (f, t) <- files, "/FooWeight.kt" `isSuffixOf` f, l <- Text.lines t, "require" `Text.isInfixOf` l ]
            `shouldBe` ["require(value in 0..0xFFFF) { \"FooWeight must fit in uint16_t, got $value\" }"]

    it "reads a received unit as C wrote it on the JVM" $ do
        files <- Jvm.generate emptyDocs <$> model
        [ Text.strip l | (f, t) <- files, "EventReader.kt" `isSuffixOf` f, l <- Text.lines t, "FooWeight(" `Text.isInfixOf` l ]
            `shouldBe` ["FooEvent.BarWeight(FooBarNumber(buf.int), FooWeight(buf.int))"]
