{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module Apigen.Parser.AST
    ( Decl
        ( Typename
        , TypeDecl
        , BuiltinType
        , ConstType
        , PointerType
        , ConstPointerType
        , CallbackType

        , ArrayType
        , ConstArrayType
        , UserArrayType
        , SizedArrayType

        , ClassDecl
        , Namespace

        , CallbackTypeDecl
        , IdTypeDecl
        , ArrayTypeDecl

        , Constructor
        , Destructor
        , Method
        , Property
        , ValueProp
        , ArrayProp

        , EntityDecl
        , Function
        , Define
        , Var

        , Ref
        , IntVal
        , Paren
        , Abs
        , Max
        , Add
        , Sub
        , Mul
        , Div
        , Sizeof

        , EnumMember
        , Enumeration
        )
    , Module (Module, moduleFile, moduleDecls)
    , Model (Model, modelMods, originalDecls)
    ) where

import           Apigen.Types  (BitSize (..), BuiltinType (..), Constness (..),
                                Generated (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Text     (Text)

data Decl lexeme
    = Typename { name :: lexeme }
    | TypeDecl { name :: lexeme }
    | BuiltinType { bty :: BuiltinType }
    | ConstType { ty :: Decl lexeme }
    | PointerType { name :: lexeme }
    | ConstPointerType { name :: lexeme }
    | CallbackType { name :: lexeme }

    | ArrayType { bty :: BuiltinType }
    | ConstArrayType { bty :: BuiltinType }
    | UserArrayType { name :: lexeme }
    | SizedArrayType { memTy :: Decl lexeme, sizer :: Decl lexeme }

    | ClassDecl { name :: lexeme, mems :: [Decl lexeme] }
    | Namespace { ns :: [Text], mems :: [Decl lexeme] }

    | CallbackTypeDecl { name :: lexeme, params :: [Decl lexeme] }
    | IdTypeDecl { name :: lexeme }
    -- | @typedef uint8_t Name[atdSize];@ — a fixed-size byte array type.
    | ArrayTypeDecl { name :: lexeme, atdSize :: Decl lexeme }

    | Constructor { name :: lexeme, params :: [Decl lexeme] }
    | Destructor { name :: lexeme, params :: [Decl lexeme] }
    | Method { constness :: Constness, ty :: Decl lexeme, name :: lexeme, params :: [Decl lexeme] }
    | Property { name :: lexeme, prop :: Decl lexeme }
    | ValueProp
        { valType :: Decl lexeme
        , valGet  :: Maybe (Decl lexeme)
        , valSet  :: Maybe (Decl lexeme)
        }
    | ArrayProp
        { arrType :: Decl lexeme
        , arrGet  :: Maybe (Decl lexeme)
        , arrSet  :: Maybe (Decl lexeme)
        , arrSize :: Maybe (Decl lexeme)
        }

    | EntityDecl { name :: lexeme, idTy :: Decl lexeme, mems :: [Decl lexeme] }
    | Function { retTy :: Decl lexeme, name :: lexeme, params :: [Decl lexeme] }
    | Define { name :: lexeme, value :: Decl lexeme }
    | Var { ty :: Decl lexeme, name :: lexeme }

    | Ref { name :: lexeme }
    | IntVal { val :: lexeme }
    | Paren { expr :: Decl lexeme }
    | Abs { expr :: Decl lexeme }
    | Max { left :: Decl lexeme, right :: Decl lexeme }
    | Add { left :: Decl lexeme, right :: Decl lexeme }
    | Sub { left :: Decl lexeme, right :: Decl lexeme }
    | Mul { left :: Decl lexeme, right :: Decl lexeme }
    | Div { left :: Decl lexeme, right :: Decl lexeme }
    | Sizeof { expr :: Decl lexeme }

    | EnumMember { name :: lexeme }
    | Enumeration { gen :: [Generated], name :: lexeme, mems :: [Decl lexeme] }
    deriving (Show, Functor, Foldable, Traversable, Eq)
$(deriveJSON defaultOptions ''Decl)

data Module lexeme = Module { moduleFile :: FilePath, moduleDecls :: [Decl lexeme] }
    deriving (Show, Functor, Foldable, Traversable)
$(deriveJSON defaultOptions ''Module)

data Model lexeme = Model { modelMods :: [Module lexeme], originalDecls :: [Decl lexeme] }
    deriving (Show, Functor, Foldable, Traversable)
$(deriveJSON defaultOptions ''Model)
