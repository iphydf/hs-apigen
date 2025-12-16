{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Language.Haskell.AST where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data HsModule = HsModule
    { hsModName    :: Text
    , hsModExports :: [Text]
    , hsModImports :: [HsImport]
    , hsModDecls   :: [HsDecl]
    , hsModOptions :: [Text]
    , hsModPragmas :: [Text]
    } deriving (Show, Eq, Generic)

data HsImport = HsImport
    { importModule    :: Text
    , importQualified :: Bool
    , importAs        :: Maybe Text
    , importSpecs     :: Maybe [Text] -- Nothing = implicit all (standard), Just specs = explicit list
    } deriving (Show, Eq, Generic)

data HsDecl
    = HsDataDecl
        { dataName     :: Text
        , dataCons     :: [HsConDecl]
        , dataDeriving :: [Text]
        }
    | HsNewtypeDecl
        { newtypeName     :: Text
        , newtypeCon      :: HsConDecl
        , newtypeDeriving :: [Text]
        }
    | HsDataEnumDecl
        { hsEnumName   :: Text
        , enumMembers  :: [Text]
        , enumDeriving :: [Text]
        }
    | HsTypeAlias
        { aliasName :: Text
        , aliasType :: HsType
        }
    | HsForeignImport
        { foreignCName  :: Text
        , foreignHsName :: Text
        , foreignType   :: HsType
        , foreignPure   :: Bool -- True = no IO in return (unsafe/pure), False = IO
        }
    | HsFunSig
        { funSigName :: Text
        , funSigType :: HsType
        }
    | HsFunBind
        { funBindName :: Text
        , funBindArgs :: [HsPat]
        , funBindBody :: HsExpr
        }
    | HsInstance
        { instClass :: Text
        , instType  :: HsType
        , instDecls :: [HsDecl]
        }
    | HsPragma Text
    | HsComment Text
    | HsRawDecl Text -- Escape hatch for complex things if needed
    deriving (Show, Eq, Generic)

data HsConDecl = HsConDecl { conName :: Text, conArgs :: [HsType] }
    deriving (Show, Eq, Generic)

data HsType
    = TyCon Text
    | TyVar Text
    | TyApp HsType HsType
    | TyFun HsType HsType
    | TyTuple [HsType]
    | TyList HsType
    | TyParen HsType
    | TyUnit
    deriving (Show, Eq, Generic)

data HsPat
    = PVar Text
    | PCon Text [HsPat]
    | PLit HsLit
    | PTuple [HsPat]
    | PWildCard
    deriving (Show, Eq, Generic)

data HsExpr
    = EVar Text
    | ELit HsLit
    | EApp HsExpr HsExpr
    | EInfixApp HsExpr Text HsExpr
    | ELam [HsPat] HsExpr
    | ELet [HsDecl] HsExpr
    | ECase HsExpr [HsAlt]
    | EDo [HsStmt]
    | EIf HsExpr HsExpr HsExpr
    | ETuple [HsExpr]
    | EList [HsExpr]
    | EParen HsExpr
    deriving (Show, Eq, Generic)

data HsLit
    = LInt Int
    | LString Text
    | LChar Char
    deriving (Show, Eq, Generic)

data HsAlt = HsAlt { altPat :: HsPat, altExpr :: HsExpr }
    deriving (Show, Eq, Generic)

data HsStmt
    = SGenerator HsPat HsExpr
    | SExpr HsExpr
    | SLet [HsDecl]
    deriving (Show, Eq, Generic)
