{-# LANGUAGE OverloadedStrings #-}
-- | The C++ types of the C API's values, and the conversions between them.
module Apigen.Language.Cpp.Types
    ( CppType (..)
    , render
    , renderIn
    , typeOf
    , enumCppName
    , fromC
    , isArrayType
    ) where

import           Apigen.Language.Cpp.Names
import           Apigen.Model              (arrayTypeOf, findEnum, idModel)
import           Apigen.Namespace          (namespaces)
import           Apigen.Semantic
import           Data.Text                 (Text)

data CppType
    = TVoid
    | TBool
    | TInt Int
    | TUInt Int
    | TSize
    | TString
    | TBytes
    | TVector CppType
    | TEnum Text
    | TId Text        -- ^ @FriendNumber@
    | TUnit Text      -- ^ A typedef'd number: @AvBitRate@
    | TArray Text     -- ^ A fixed-size byte array type: @PublicKey@
    | TFrame Text     -- ^ A move-only frame of several C arguments: @AudioFrame@
    | TOptional CppType
    deriving (Show, Eq)

render :: CppType -> Text
render t = case t of
    TVoid       -> "void"
    TBool       -> "bool"
    TInt n      -> "int" <> showT n <> "_t"
    TUInt n     -> "uint" <> showT n <> "_t"
    TSize       -> "size_t"
    TString     -> "std::string"
    TBytes      -> "std::vector<uint8_t>"
    TVector e   -> "std::vector<" <> render e <> ">"
    TEnum n     -> n
    TId n       -> n
    TUnit n     -> n
    TArray n    -> n
    TFrame n    -> n
    TOptional e -> "std::optional<" <> render e <> ">"
  where
    showT = \n -> case n of
        8  -> "8"
        16 -> "16"
        32 -> "32"
        _  -> "64"

-- | A type in a namespace with these names, which hide @tox@'s: an
-- @event::GroupVoiceState@ has a @::tox::GroupVoiceState@.
renderIn :: Text -> [Text] -> CppType -> Text
renderIn ns hidden t = case t of
    TVector e   -> "std::vector<" <> renderIn ns hidden e <> ">"
    TOptional e -> "std::optional<" <> renderIn ns hidden e <> ">"
    _ | render t `elem` hidden -> "::" <> ns <> "::" <> render t
      | otherwise              -> render t

isArrayType :: CppType -> Bool
isArrayType TArray{} = True
isArrayType _        = False

enumCppName :: SemanticModel -> Text -> Text
enumCppName model name = enumTypeName (namespaces model) $ maybe name enumName (findEnum model name)

typeOf :: SemanticModel -> SType -> Maybe CppType
typeOf model t = case t of
    SScalar u _    -> Just (TUnit (unitTypeName u))
    SVoid          -> Just TVoid
    SBool          -> Just TBool
    SInt n         -> Just (TInt n)
    SUInt n        -> Just (TUInt n)
    SSizeT         -> Just TSize
    SString        -> Just TString
    SBytes         -> Just TBytes
    SFixedBytes size _ -> Just $ case arrayTypeOf model size of
        Just name -> TArray (arrayTypeName name)
        Nothing   -> TBytes
    SFixedList e _ _ -> TVector <$> typeOf model e
    SList e        -> TVector <$> typeOf model e
    SEnum e        -> Just (TEnum (enumCppName model e))
    SResourceId i  -> TId . idTypeName . idName <$> idModel model i
    SHandle _      -> Nothing
    SCallback _    -> Nothing

fromC :: CppType -> Text -> Text
fromC t value = case t of
    TId n   -> n <> "{" <> value <> "}"
    TUnit n -> n <> "{" <> value <> "}"
    TEnum n -> "static_cast<" <> n <> ">(" <> value <> ")"
    _       -> value
