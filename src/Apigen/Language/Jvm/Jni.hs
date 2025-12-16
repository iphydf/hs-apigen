{-# LANGUAGE OverloadedStrings #-}
-- | How a C function crosses JNI: its Java and C types, and the ids before
-- its own arguments.
module Apigen.Language.Jvm.Jni
    ( Kind (..)
    , kind
    , returnKind
    , jniType
    , jniTypeC
    , jniReturnType
    , jniReturnTypeC
    , pathIds
    , Native (..)
    , factNative
    , javaDecl
    , javahDecl
    ) where

import           Apigen.Model    (findResource, ignoresReturn)
import           Apigen.Semantic (SJvmNative (..), SMethod (..),
                                  SMethodRole (..), SResource (..),
                                  SResourceType (..), SType (..), SemanticModel)
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text

-- | A value's JNI type, which names both its Java and its C type.
data Kind = Void | Boolean | Byte | Short | Int | Long | String | Object
    | Array Kind

-- | Scalars narrower than 64 bits are @int@ but for bytes, as the
-- hand-written C++ takes @jint@.
kind :: SemanticModel -> SType -> Kind
kind model ty = case ty of
    SScalar _ b      -> kind model b
    SVoid            -> Void
    SBool            -> Boolean
    SInt n           -> scalar n
    SUInt n          -> scalar n
    SSizeT           -> Long
    SString          -> String
    SBytes           -> Array Byte
    SFixedBytes _ _  -> Array Byte
    SFixedList t _ _ -> maybe Object Array (element t)
    SList t          -> Array (fromMaybe Int (element t))
    SEnum _          -> Int
    SHandle _        -> Int
    SCallback _      -> Object
    SResourceId _    -> Int
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

-- | The type in a JNI signature: @[B@ for @byte[]@.
descriptor :: Kind -> Text
descriptor k = case k of
    Void    -> "V"
    Boolean -> "Z"
    Byte    -> "B"
    Short   -> "S"
    Int     -> "I"
    Long    -> "J"
    String  -> "Ljava/lang/String;"
    Object  -> "Ljava/lang/Object;"
    Array e -> "[" <> descriptor e

-- | The kind a hand-written native's Java type names.
javaKind :: Text -> Kind
javaKind t = case Text.stripSuffix "[]" t of
    Just e  -> Array (javaKind e)
    Nothing -> case t of
        "void"    -> Void
        "boolean" -> Boolean
        "byte"    -> Byte
        "short"   -> Short
        "int"     -> Int
        "long"    -> Long
        "String"  -> String
        "Object"  -> Object
        _         -> error $ "no JNI kind for the Java type " <> Text.unpack t

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

-- | A native method of a JNI class: what both its Java declaration and its
-- javah header entry say.
data Native = Native
    { nativeName   :: Text
    , nativeReturn :: Kind
    , nativeParams :: [(Kind, Text)]
    , nativeThrows :: Maybe Text
    }

factNative :: SJvmNative -> Native
factNative n = Native
    { nativeName = jnName n
    , nativeReturn = javaKind (jnReturn n)
    , nativeParams = [ (javaKind t, p) | (t, p) <- jnParams n ]
    , nativeThrows = jnThrows n
    }

javaDecl :: Native -> Text
javaDecl n =
    "    static native " <> java (nativeReturn n) <> " " <> nativeName n
        <> "(" <> Text.intercalate ", " [ java k <> " " <> p | (k, p) <- nativeParams n ] <> ")"
        <> maybe "" (" throws " <>) (nativeThrows n) <> ";"

-- | The entry @javac -h@ writes for the native, in the class with this JNI name.
javahDecl :: Text -> Native -> [Text]
javahDecl jniClass n =
    [ "/*"
    , " * Class:     " <> jniClass
    , " * Method:    " <> nativeName n
    , " * Signature: (" <> Text.concat (map (descriptor . fst) (nativeParams n)) <> ")" <> descriptor (nativeReturn n)
    , " */"
    , "JNIEXPORT " <> c (nativeReturn n) <> " JNICALL Java_" <> jniClass <> "_" <> nativeName n
    , "  (JNIEnv *, jclass" <> Text.concat [ ", " <> c k | (k, _) <- nativeParams n ] <> ");"
    , ""
    ]
