-- | A small AST for the subset of Cython we generate.
--
-- This models @.pxd@ (declarations) and @.pyx@ (implementation) files. Types
-- and Python annotations are carried as pre-rendered 'Text': the type-mapping
-- complexity lives in the backend, not here.
module Apigen.Language.Python.AST where

import           Data.Text (Text)

-- | A whole generated file.
data Module = Module
    { modComment :: Text     -- ^ Leading @#@ comment line (no @#@ prefix).
    , modItems   :: [Item]
    }
    deriving (Show, Eq)

-- | Top-level or class-body items.
data Item
    = Blank
    | Comment Text
    | CImport Text [Text]          -- ^ @from MOD cimport a, b@
    | Import Text [Text]           -- ^ @from MOD import a, b@
    | Extern Text [ExternDecl]     -- ^ @cdef extern from "header": ...@
    | ClassDef Class
    | CdefBlock [Func]             -- ^ @cdef:@ block of nested cdef functions
    | FuncDef Func
    | Decl Func                    -- ^ Signature only, no body (@.pxd@ class members).
    | PropertyDef Property
    | Assign Text Text Expr        -- ^ @NAME: annotation = expr@
    | RawItem Text                 -- ^ Escape hatch: a verbatim line.
    deriving (Show, Eq)

-- | Declarations inside a @cdef extern from@ block.
data ExternDecl
    = ExEnum Text [Text]           -- ^ @cpdef enum Name:@ with members
    | ExOpaque Text                -- ^ @ctypedef struct Name@
    | ExAlias Text Text            -- ^ @ctypedef <ctype> Name@
    | ExCallback Text [Param]      -- ^ @ctypedef void Name(params) except *@
    | ExFunc Func                  -- ^ @cdef <ret> name(params)@ (no body)
    deriving (Show, Eq)

data Class = Class
    { clsCdef   :: Bool            -- ^ @cdef class@ vs plain @class@.
    , clsName   :: Text
    , clsBases  :: [Text]
    , clsFields :: [(Text, Text)]  -- ^ @cdef <ctype> name@ fields (.pxd only).
    , clsItems  :: [Item]
    }
    deriving (Show, Eq)

data FuncKind = KindDef | KindCdef | KindCpdef
    deriving (Show, Eq)

-- | How to render a function's parameter list.
data ParamStyle
    = PyStyle  -- ^ @name: Type@ (Python annotation order)
    | CStyle   -- ^ @Type name@ (C declaration order)
    deriving (Show, Eq)

data Func = Func
    { fnKind       :: FuncKind
    , fnName       :: Text
    , fnRet        :: Text         -- ^ Rendered return type/annotation; @""@ = none.
    , fnParams     :: [Param]
    , fnParamStyle :: ParamStyle
    , fnExcept     :: Bool         -- ^ Append @except *@.
    , fnDoc        :: Maybe Text
    , fnDecorators :: [Text]
    , fnBody       :: [Stmt]       -- ^ Empty for declaration-only functions.
    }
    deriving (Show, Eq)

data Param = Param
    { pmName    :: Text
    , pmType    :: Text            -- ^ Rendered; @""@ = untyped.
    , pmDefault :: Maybe Text
    }
    deriving (Show, Eq)

data Property = Property
    { ppName   :: Text
    , ppType   :: Text
    , ppDoc    :: Maybe Text
    , ppGetter :: [Stmt]
    , ppSetter :: Maybe (Text, [Stmt])  -- ^ (value parameter name, body)
    }
    deriving (Show, Eq)

data Stmt
    = SPass
    | SExpr Expr
    | SReturn (Maybe Expr)
    | SRaise Expr
    | SAssign Expr Expr            -- ^ @lhs = rhs@
    | SCdef Text Text (Maybe Expr) -- ^ @cdef <ctype> name [= init]@
    | SIf Expr [Stmt]
    | STryFinally [Stmt] [Stmt]
    | SComment Text
    deriving (Show, Eq)

data Expr
    = EName Text
    | EInt Integer
    | EStr Text
    | ECall Expr [Expr]
    | EAttr Expr Text                       -- ^ @e.attr@
    | ESlice Expr (Maybe Expr) (Maybe Expr) -- ^ @e[a:b]@
    | EIndex Expr Expr                      -- ^ @e[i]@
    | ECast Text Expr                       -- ^ @<ctype> e@
    | EAddr Expr                            -- ^ @&e@
    | EBinOp Text Expr Expr
    | ECond Expr Expr Expr                  -- ^ @a if c else b@
    | ERaw Text
    deriving (Show, Eq)
