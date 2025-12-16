-- | A small AST for the @.pxd@ and @.pyx@ Cython we generate. Types are
-- pre-rendered 'Text'.
module Apigen.Language.Python.AST where

import           Data.Text (Text)

data Module = Module
    { modComment :: Text     -- ^ Leading @#@ comment line (no @#@ prefix).
    , modItems   :: [Item]
    }
    deriving (Show, Eq)

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
    | Assign Text Text Expr        -- ^ @NAME: annotation = expr@, or without one
    | RawItem Text                 -- ^ A verbatim line.
    deriving (Show, Eq)

-- | Declarations inside a @cdef extern from@ block.
data ExternDecl
    = ExEnum Text [Text]           -- ^ @ctypedef enum Name:@ with members
    | ExOpaque Text Text           -- ^ @ctypedef struct Name "CName"@
    | ExAlias Text Text Text       -- ^ @ctypedef <ctype> Name "CName"@
    | ExCallback Text [Param]      -- ^ @ctypedef void Name(params) noexcept@
    | ExFunc Func                  -- ^ @cdef <ret> name(params)@ (no body)
    deriving (Show, Eq)

data Class = Class
    { clsDecorators :: [Text]      -- ^ @\@cython.no_gc_clear@ and so on.
    , clsCdef       :: Bool            -- ^ @cdef class@ vs plain @class@.
    , clsName       :: Text
    , clsBases      :: [Text]
    , clsFields     :: [(Text, Text)]  -- ^ @cdef <ctype> name@ fields (.pxd only).
    , clsItems      :: [Item]
    }
    deriving (Show, Eq)

data FuncKind = KindDef | KindCdef | KindCpdef
    deriving (Show, Eq)

data ParamStyle
    = PyStyle  -- ^ @name: Type@ (Python annotation order)
    | CStyle   -- ^ @Type name@ (C declaration order)
    deriving (Show, Eq)

data Func = Func
    { fnKind       :: FuncKind
    , fnName       :: Text
    , fnRet        :: Text         -- ^ Rendered return type/annotation, or @""@.
    , fnParams     :: [Param]
    , fnParamStyle :: ParamStyle
    , fnExcept     :: Exc          -- ^ How it raises, and what it needs of the GIL.
    , fnDoc        :: Maybe Text
    , fnDecorators :: [Text]
    , fnBody       :: [Stmt]       -- ^ Empty for declaration-only functions.
    }
    deriving (Show, Eq)

-- | What a @cdef@ function says after its parameters.
data Exc
    = ExcDefault       -- ^ Nothing.
    | ExcStar          -- ^ @except *@
    | NoExcept         -- ^ @noexcept@: a C callback, which can't raise.
    | NoExceptNogil    -- ^ @noexcept nogil@: a C function.
    | NoExceptWithGil  -- ^ @noexcept with gil@: a C callback running Python.
    | ExceptNogil Text -- ^ @except -1 nogil@: raises, though it runs without the GIL.
    deriving (Show, Eq)

data Param = Param
    { pmName    :: Text
    , pmType    :: Text            -- ^ Rendered, or @""@ for untyped.
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
    | SAugAssign Text Expr Expr    -- ^ @lhs op= rhs@
    | SIf Expr [Stmt]
    | SIfElse Expr [Stmt] [Stmt]
    | STryFinally [Stmt] [Stmt]
    | STry [Stmt] [(Text, [Stmt])] [Stmt]  -- ^ @try@, @except T@ clauses, @finally@
    | SFor Text Expr [Stmt]
    | SNogil [Stmt]                -- ^ @with nogil:@
    | SGil [Stmt]                  -- ^ @with gil:@
    | SComment Text
    deriving (Show, Eq)

data Expr
    = EName Text
    | EInt Integer
    | EStr Text
    | EList [Expr]
    | EBytes Text                           -- ^ @b"..."@
    | ECall Expr [Expr]
    | EAttr Expr Text                       -- ^ @e.attr@
    | ESlice Expr (Maybe Expr) (Maybe Expr) -- ^ @e[a:b]@
    | EIndex Expr Expr                      -- ^ @e[i]@
    | ECast Text Expr                       -- ^ @<ctype> e@
    | EAddr Expr                            -- ^ @&e@
    | EBinOp Text Expr Expr
    | ECond Expr Expr Expr                  -- ^ @a if c else b@
    | ENot Expr
    | ETuple [Expr]
    | EListComp Expr Text Expr              -- ^ @[e for v in xs]@
    deriving (Show, Eq)
