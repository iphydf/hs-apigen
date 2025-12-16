{-# LANGUAGE OverloadedStrings #-}
-- | How a C function crosses JNI: its Java and C types, and the ids before
-- its own arguments.
module Apigen.Language.Jvm.Jni
    ( jniType
    , jniTypeC
    , jniReturnType
    , jniReturnTypeC
    , pathIds
    ) where

import           Apigen.Language.Jvm.Model (isSettingsName)
import           Apigen.Model              (findResource, ignoresReturn)
import           Apigen.Semantic           (SMethod (..), SMethodRole (..),
                                            SResource (..), SResourceType (..),
                                            SType (..), SemanticModel)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

-- | A value's JNI type, which names both its Java and its C type.
data Kind = Void | Boolean | Byte | Short | Int | Long | String | Object
    | Array Kind

-- | Scalars narrower than 64 bits are @int@ but for bytes, as the
-- hand-written C++ takes @jint@.
kind :: SemanticModel -> SType -> Kind
kind model ty = case ty of
    SScalar _ b                        -> kind model b
    SVoid                              -> Void
    SBool                              -> Boolean
    SInt n                             -> scalar n
    SUInt n                            -> scalar n
    SSizeT                             -> Long
    SString                            -> String
    SBytes                             -> Array Byte
    SFixedBytes _ _                    -> Array Byte
    SFixedList t _ _                   -> maybe Object Array (element t)
    SList t                            -> Array (fromMaybe Int (element t))
    SEnum _                            -> Int
    -- A serialized proto @Options@ message.
    SHandle h | isSettingsName model h -> Array Byte
    SHandle _                          -> Int
    SCallback _                        -> Object
    SResourceId _                      -> Int
  where
    scalar 8  = Byte
    scalar 64 = Long
    scalar _  = Int
    element t = case t of
        SInt n  -> Just (width n)
        SUInt n -> Just (width n)
        _       -> Nothing
    width 8  = Byte
    width 16 = Short
    width 64 = Long
    width _  = Int

java :: Kind -> Text
java k = case k of
    Void    -> "void"
    Boolean -> "boolean"
    Byte    -> "byte"
    Short   -> "short"
    Int     -> "int"
    Long    -> "long"
    String  -> "String"
    Object  -> "Object"
    Array e -> java e <> "[]"

c :: Kind -> Text
c k = case k of
    Void    -> "void"
    Array e -> "j" <> Text.toLower (java e) <> "Array"
    _       -> "j" <> Text.toLower (java k)

jniType, jniTypeC :: SemanticModel -> SType -> Text
jniType model = java . kind model
jniTypeC model = c . kind model

-- | Iterating gives back the events, and a @bool@ only saying whether the
-- error is set gives back nothing.
returnKind :: SemanticModel -> SMethod -> Kind
returnKind model m
    | methodRole m == IterateRole = Array Byte
    | output m == SBool, ignoresReturn m = Void
    | otherwise = kind model (output m)

jniReturnType, jniReturnTypeC :: SemanticModel -> SMethod -> Text
jniReturnType model = java . returnKind model
jniReturnTypeC model = c . returnKind model

-- | The ids naming a resource, outermost first, each without its parent's
-- word: @(peer_number, Group_Peer_Number)@ for a group peer.
pathIds :: SemanticModel -> SResource -> Bool -> [(Text, Text)]
pathIds model r includeOwn = parentIds ++ [ own | includeOwn, Just own <- [ownId] ]
  where
    parentIds = maybe [] (\p -> pathIds model p True) (parent r >>= findResource model)
    ownId = case resourceType r of
        ResId (SResourceId idType) -> Just (withoutParent idType, idType)
        _                          -> Nothing
    withoutParent n = fromMaybe n (parent r >>= \p -> Text.stripPrefix (p <> "_") n)
