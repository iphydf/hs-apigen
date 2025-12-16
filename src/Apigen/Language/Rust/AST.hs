{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Language.Rust.AST where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data RsModule = RsModule
    { rsModItems :: [RsItem]
    } deriving (Show, Eq, Generic)

data RsItem
    = RsUse RsVis Text -- ^ e.g. "pub use crate::ffi;"
    | RsUseAttr Text Text -- ^ Attribute, Path
    | RsMod RsVis Text (Maybe [RsItem]) -- ^ "pub mod foo;" or "mod foo { ... }"
    | RsItemStruct RsStruct
    | RsItemTupleStruct RsTupleStruct
    | RsItemEnum RsEnum
    | RsItemTrait RsTrait
    | RsItemImpl RsImpl
    | RsItemFn RsFn
    | RsItemTypeAlias RsTypeAlias
    | RsMacroCall Text [Text] -- ^ Macro name and raw args (simplified)
    deriving (Show, Eq, Generic)

data RsStruct = RsStruct
    { structName     :: Text
    , structVis      :: RsVis
    , structGenerics :: [Text]
    , structFields   :: [RsStructField]
    } deriving (Show, Eq, Generic)

data RsTupleStruct = RsTupleStruct
    { tupleStructName     :: Text
    , tupleStructVis      :: RsVis
    , tupleStructGenerics :: [Text]
    , tupleStructFields   :: [RsType] -- Assuming pub for now or add visibility
    } deriving (Show, Eq, Generic)

data RsStructField = RsStructField
    { fieldName :: Text
    , fieldType :: RsType
    , fieldVis  :: RsVis
    } deriving (Show, Eq, Generic)

data RsEnum = RsEnum
    { enumName     :: Text
    , enumVis      :: RsVis
    , enumGenerics :: [Text]
    , enumVariants :: [RsEnumVariant]
    } deriving (Show, Eq, Generic)

data RsEnumVariant = RsEnumVariant
    { variantName   :: Text
    , variantFields :: [RsType] -- ^ Tuple variants for now
    } deriving (Show, Eq, Generic)

data RsTrait = RsTrait
    { traitName    :: Text
    , traitVis     :: RsVis
    , traitMethods :: [RsFn] -- ^ Signatures mostly
    } deriving (Show, Eq, Generic)

data RsImpl = RsImpl
    { implTrait    :: Maybe Text -- ^ "impl Trait for Type"
    , implType     :: RsType
    , implGenerics :: [Text] -- ^ <'a, T>
    , implItems    :: [RsItem] -- ^ Mostly methods
    } deriving (Show, Eq, Generic)

data RsFn = RsFn
    { fnName     :: Text
    , fnVis      :: RsVis
    , fnAbi      :: Maybe Text -- ^ e.g. Just "\"C\""
    , fnGenerics :: [Text]
    , fnArgs     :: [RsArg]
    , fnRet      :: Maybe RsType
    , fnBody     :: Maybe RsBlock -- ^ Nothing for trait declarations
    } deriving (Show, Eq, Generic)

data RsArg
    = RsSelfArg Bool -- ^ &self (false) or &mut self (true)
    | RsSelf -- ^ self (no reference)
    | RsArg Text RsType
    deriving (Show, Eq, Generic)

data RsTypeAlias = RsTypeAlias
    { aliasName :: Text
    , aliasVis  :: RsVis
    , aliasType :: RsType
    } deriving (Show, Eq, Generic)

data RsVis = Pub | PubCrate | Private
    deriving (Show, Eq, Generic)

data RsType
    = TyPath Text -- ^ "u32", "std::ffi::CString"
    | TyRef (Maybe Text) RsType Bool -- ^ &T (false) or &mut T (true), optional lifetime
    | TySlice RsType
    | TyArray RsType Text -- ^ Type, Size expression (e.g. "32" or "ffi::CONST")
    | TyTuple [RsType]
    | TyUnit
    | TyGeneric Text [RsType] -- ^ Vec<u8>, Result<T, E>
    deriving (Show, Eq, Generic)

data RsBlock = RsBlock [RsStmt]
    deriving (Show, Eq, Generic)

data RsStmt
    = StmtLet Text (Maybe RsType) RsExpr
    | StmtExpr RsExpr -- ^ Expression with semicolon (usually)
    | StmtExprNoSemi RsExpr -- ^ Expression without semicolon (implicit return)
    | StmtReturn RsExpr
    | StmtItem RsItem
    deriving (Show, Eq, Generic)

data RsExpr
    = EVar Text
    | ELit RsLit
    | ECall RsExpr [RsExpr]
    | EMethodCall RsExpr Text [RsExpr]
    | EFieldAccess RsExpr Text
    | EStructInit Text [(Text, RsExpr)]
    | EBlock RsBlock
    | EUnsafe RsBlock
    | EIf RsExpr RsBlock (Maybe RsBlock)
    | EMatch RsExpr [(RsPat, RsBlock)]
    | ERef RsExpr Bool -- ^ &x or &mut x
    | ETry RsExpr -- ^ expr?
    | ECast RsExpr RsType -- ^ expr as Type
    | EDeref RsExpr -- ^ *expr
    | ELambda [Text] RsExpr -- ^ |x| expr
    | EMacroCall Text [Text] -- ^ name![args]
    | EParen RsExpr -- ^ (expr)
    | EArrayInit RsExpr RsExpr -- ^ [elem; size]
    | EBinOp Text RsExpr RsExpr -- ^ lhs op rhs
    deriving (Show, Eq, Generic)

data RsPat
    = PVar Text
    | PPath Text -- ^ ffi::Enum::Variant
    | PWildcard
    | PLit RsLit
    | PTupleStruct Text [RsPat] -- ^ Ok(x)
    deriving (Show, Eq, Generic)

data RsLit
    = LInt Int
    | LString Text
    | LBool Bool
    deriving (Show, Eq, Generic)
