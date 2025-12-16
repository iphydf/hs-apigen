{-# LANGUAGE OverloadedStrings #-}

-- | Rust-specific naming conventions and identifier escaping.
--
-- This module lives separately from 'Apigen.Language.Rust' because
-- everything here is pure text rewriting — language-syntax rules for
-- turning C names into valid Rust identifiers. The keyword escape
-- table is a constraint of the Rust grammar; the casing rewrites
-- mirror the @rust-casing@ convention (snake_case for values,
-- PascalCase for types). Nothing in this module looks at the
-- SemanticModel.
module Apigen.Language.Rust.Naming
    ( -- * Identifier escaping
      safeName
      -- * Casing
    , idToRustSnake
    , idToRustPascal
    , paramSnake
      -- * Error variant naming
    , toxErrorVariant
    ) where

import           Data.Text  (Text)
import qualified Data.Text  as T
import qualified Text.Casing as Casing


-- | Snake-case a C identifier (e.g. type name, method name) for use
-- as a Rust value-position identifier. Collisions with Rust keywords
-- get the raw-identifier escape (@r#@) via 'safeName'.
idToRustSnake :: Text -> Text
idToRustSnake t = safeName (T.toLower (T.pack (Casing.toSnake (Casing.fromAny (T.unpack t)))))

-- | Snake-case a C *parameter* name. Identical to 'idToRustSnake' except that a
-- name colliding with a Rust keyword gets a trailing underscore (@type_@)
-- rather than a raw identifier (@r#type@). Raw identifiers are fine for
-- module/type names but make for an awkward public function signature, so
-- parameters use the conventional trailing-underscore escape instead.
paramSnake :: Text -> Text
paramSnake t =
    let snake = T.toLower (T.pack (Casing.toSnake (Casing.fromAny (T.unpack t))))
    in case T.stripPrefix "r#" (safeName snake) of
        Just kw -> kw <> "_"
        Nothing -> snake

-- | PascalCase a C identifier for use as a Rust type-position identifier.
-- A handful of resource names get bespoke renames: the @Events@ resource
-- maps to @ToxEvents@ to disambiguate from the @core::Events@ struct,
-- and @AV@\/@ToxAV@ both normalise to @ToxAV@ matching the hand-written
-- API's expected casing.
idToRustPascal :: Text -> Text
idToRustPascal "Events" = "ToxEvents"
idToRustPascal "AV"     = "ToxAV"
idToRustPascal "ToxAV"  = "ToxAV"
idToRustPascal t        = T.pack $ Casing.toPascal $ Casing.fromSnake $ T.unpack $ T.toLower t

-- | Maps an error enum's C name to its @ToxError@ variant name, e.g.
-- @Tox_Err_Friend_Add@ -> @FriendAdd@, @Toxav_Err_Call@ -> @AvCall@.
-- The @Av@ prefix on AV-side errors keeps them visually distinct from
-- their core counterparts in the unified @ToxError@ enum.
toxErrorVariant :: Text -> Text
toxErrorVariant cName
    | Just rest <- T.stripPrefix "Toxav_Err_" cName = "Av" <> idToRustPascal rest
    | Just rest <- T.stripPrefix "Tox_Err_" cName   = idToRustPascal rest
    | otherwise                                     = idToRustPascal cName

-- | Escape a Rust reserved-keyword identifier with the raw-identifier
-- @r#@ prefix. Non-keyword identifiers pass through unchanged.
-- @self@ is left as-is so it can serve as a receiver parameter; all
-- other Rust 2018 / 2021 keywords and reserved-for-future-use words
-- get prefixed.
safeName :: Text -> Text
safeName "type"     = "r#type"
safeName "mod"      = "r#mod"
safeName "crate"    = "r#crate"
safeName "self"     = "self" -- self is allowed as first arg
safeName "super"    = "r#super"
safeName "fn"       = "r#fn"
safeName "let"      = "r#let"
safeName "if"       = "r#if"
safeName "else"     = "r#else"
safeName "match"    = "r#match"
safeName "while"    = "r#while"
safeName "for"      = "r#for"
safeName "loop"     = "r#loop"
safeName "break"    = "r#break"
safeName "continue" = "r#continue"
safeName "return"   = "r#return"
safeName "in"       = "r#in"
safeName "ref"      = "r#ref"
safeName "mut"      = "r#mut"
safeName "unsafe"   = "r#unsafe"
safeName "where"    = "r#where"
safeName "pub"      = "r#pub"
safeName "use"      = "r#use"
safeName "trait"    = "r#trait"
safeName "impl"     = "r#impl"
safeName "struct"   = "r#struct"
safeName "enum"     = "r#enum"
safeName "const"    = "r#const"
safeName "static"   = "r#static"
safeName "extern"   = "r#extern"
safeName "as"       = "r#as"
safeName "move"     = "r#move"
safeName "async"    = "r#async"
safeName "await"    = "r#await"
safeName "dyn"      = "r#dyn"
safeName "abstract" = "r#abstract"
safeName "become"   = "r#become"
safeName "box"      = "r#box"
safeName "do"       = "r#do"
safeName "final"    = "r#final"
safeName "macro"    = "r#macro"
safeName "override" = "r#override"
safeName "priv"     = "r#priv"
safeName "typeof"   = "r#typeof"
safeName "unsized"  = "r#unsized"
safeName "virtual"  = "r#virtual"
safeName "yield"    = "r#yield"
safeName "try"      = "r#try"
safeName n          = n
