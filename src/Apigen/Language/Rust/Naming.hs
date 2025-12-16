{-# LANGUAGE OverloadedStrings #-}

-- | Rust naming conventions and identifier escaping.
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

import           Data.Text   (Text)
import qualified Data.Text   as T
import qualified Text.Casing as Casing


-- | Snake-case a C identifier, escaping keywords with @r#@.
idToRustSnake :: Text -> Text
idToRustSnake t = safeName (T.toLower (T.pack (Casing.toSnake (Casing.fromAny (T.unpack t)))))

-- | Like 'idToRustSnake', but keywords get a trailing underscore (@type_@),
-- which reads better than @r#type@ in a public signature.
paramSnake :: Text -> Text
paramSnake t =
    let snake = T.toLower (T.pack (Casing.toSnake (Casing.fromAny (T.unpack t))))
    in case T.stripPrefix "r#" (safeName snake) of
        Just kw -> kw <> "_"
        Nothing -> snake

-- | PascalCase a C identifier. @Events@ becomes @ToxEvents@ so it doesn't
-- clash with the @core::Events@ struct.
idToRustPascal :: Text -> Text
idToRustPascal "Events" = "ToxEvents"
idToRustPascal "AV"     = "ToxAV"
idToRustPascal "ToxAV"  = "ToxAV"
idToRustPascal t        = T.pack $ Casing.toPascal $ Casing.fromSnake $ T.unpack $ T.toLower t

-- | The @ToxError@ variant for an error enum, e.g. @Tox_Err_Friend_Add@ is
-- @FriendAdd@ and @Toxav_Err_Call@ is @AvCall@.
toxErrorVariant :: Text -> Text
toxErrorVariant cName
    | Just rest <- T.stripPrefix "Toxav_Err_" cName = "Av" <> idToRustPascal rest
    | Just rest <- T.stripPrefix "Tox_Err_" cName   = idToRustPascal rest
    | otherwise                                     = idToRustPascal cName

-- | Escape a Rust keyword with @r#@. @self@ stays, as a receiver.
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
