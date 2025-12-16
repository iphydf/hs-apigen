{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Language.Cpp.AST where

import           Apigen.Semantic (SType)
import           Apigen.Types    (Constness)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

data CppType
    = TyVoid
    | TyBool
    | TyInt Int
    | TyUInt Int
    | TySizeT
    | TyString
    | TyBytes
    | TyVector CppType
    | TyArray CppType Text
    | TyOptional CppType
    | TyMonostate
    | TyResult CppType CppType
    | TySharedPtr CppType
    | TyUniquePtr CppType
    | TyPointer CppType
    | TyFunction CppType [CppType]
    | TyUserDefined Text
    | TyReference CppType
    | TyRValueReference CppType
    | TySpan CppType
    | TyConst CppType
    deriving (Show, Eq, Generic)

data AccessSpecifier = Public | Protected | Private | Static AccessSpecifier | Virtual AccessSpecifier
    deriving (Show, Eq, Generic)

data CppDecl
    = Namespace Text [CppDecl]
    | Class Text [CppMember]
    | Enum Text [Text]
    | Function Text CppType [CppParam] [CppStmt]
    | Include Text Bool -- Bool is True for system includes <>, False for ""
    | ForwardDecl Text
    | TypedefDecl CppType Text
    | MethodDef Text Text CppType [CppParam] Constness [CppStmt]
    | ConstructorDef Text [CppParam] [(Text, Text)] [CppStmt]
    | DestructorDef Text [CppStmt]
    | CommentDecl Text
    | HeaderGuard Text [CppDecl]
    | TemplateDecl [Text] CppDecl
    deriving (Show, Eq, Generic)

data CppMember
    = MemberDecl AccessSpecifier CppType Text
    | MethodDecl AccessSpecifier Text CppType [CppParam] Constness (Maybe [CppStmt])
    | TemplateMemberDecl [Text] CppMember
    | ConstructorDecl AccessSpecifier [CppParam] [(Text, Text)] (Maybe [CppStmt]) -- [(Member, Value)]
    | DestructorDecl AccessSpecifier (Maybe [CppStmt])
    | CommentMember Text
    | NestedDecl CppDecl
    deriving (Show, Eq, Generic)

data CppParam = CppParam CppType Text
    deriving (Show, Eq, Generic)

data CppStmt
    = Expr Text
    | Return Text
    | If Text [CppStmt] [CppStmt]
    | Deleted
    deriving (Show, Eq, Generic)
