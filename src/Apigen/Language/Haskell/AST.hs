-- | The Haskell the backend writes: just what the bindings need.
module Apigen.Language.Haskell.AST
    ( Module (..)
    , Export (..)
    , Import (..)
    , Decl (..)
    , Con (..)
    , Field (..)
    , Type (..)
    , Pat (..)
    , Expr (..)
    , Stmt (..)
    , Doc
    ) where

import           Data.Text (Text)

type Doc = Maybe Text

data Module = Module
    { modName    :: Text
    , modDoc     :: Doc
    , modPragmas :: [Text]          -- ^ LANGUAGE extensions
    , modExports :: Maybe [Export]  -- ^ 'Nothing' exports everything
    , modImports :: [Import]
    , modDecls   :: [Decl]
    } deriving (Show, Eq)

data Export
    = ExportVar Text      -- ^ A value, or a type without its constructors
    | ExportAll Text      -- ^ @T (..)@
    | ExportPattern Text  -- ^ @pattern P@
    | ExportModule Text
    | ExportSection Text  -- ^ A Haddock heading
    deriving (Show, Eq)

data Import = Import
    { impModule :: Text
    , impAs     :: Maybe Text    -- ^ Qualified as this
    , impNames  :: Maybe [Text]  -- ^ 'Nothing' imports everything
    } deriving (Show, Eq)

data Decl
    = DataDecl Doc Text [Con] [Text]
      -- ^ A sum type: name, constructors, derived classes
    | RecordDecl Doc Text [Field] [Text]
      -- ^ A record whose constructor has its type's name
    | NewtypeDecl Doc Text Type [Text]
      -- ^ A newtype whose constructor has its type's name
    | TypeAlias Doc Text Type
    | Foreign Text Text Type
      -- ^ C entity (@&f@ for a function pointer, @wrapper@), name, type
    | Signature Doc Text Type
    | Binding Text [Pat] Expr
      -- ^ One clause (a function's clauses follow each other)
    | BindingWhere Text [Pat] Expr [Decl]
      -- ^ A clause with local definitions
    | Instance Text Text [Decl]
      -- ^ Class, type, the methods' bindings
    | PatternSynonym Doc Text Type Expr
    deriving (Show, Eq)

data Con = Con
    { conName :: Text
    , conDoc  :: Doc
    , conArgs :: [Type]
    } deriving (Show, Eq)

data Field = Field
    { fieldName :: Text
    , fieldType :: Type
    , fieldDoc  :: Doc
    } deriving (Show, Eq)

data Type
    = TCon Text
    | TApp Type [Type]
    | TFun [Type] Type
    | TTuple [Type]
    | TUnit
    deriving (Show, Eq)

data Pat
    = PVar Text
    | PCon Text [Pat]
    | PLit Integer
    | PWild
    deriving (Show, Eq)

data Expr
    = Var Text
    | Lit Integer
    | Str Text
    | App Expr [Expr]
    | Op Expr Text Expr
    | Lam [Pat] Expr
    | Do [Stmt]
    | Case Expr [(Pat, Expr)]
    | With [(Expr, [Pat])] Expr
      -- ^ @w1 $ \\p -> w2 $ \\q -> body@, one continuation per line
    deriving (Show, Eq)

data Stmt
    = Bind Pat Expr
    | Run Expr
    deriving (Show, Eq)
