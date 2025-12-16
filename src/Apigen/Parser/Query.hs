{-# LANGUAGE Strict #-}
module Apigen.Parser.Query (declName) where

import           Apigen.Parser.AST         (Decl (..))
import           Apigen.Parser.SymbolTable (SId, Sym)
import           Language.Cimple           (Lexeme)

declName :: Sym -> Maybe (Lexeme SId)
declName (TypeDecl name)           = Just name
declName (Function _ name _)       = Just name
declName (Method _ _ name _)       = Just name
declName (Enumeration _ name _)    = Just name
declName (CallbackTypeDecl name _) = Just name
declName (IdTypeDecl name)         = Just name
declName (Define name _)           = Just name
declName (Var _ name)              = Just name
declName (ClassDecl name _)        = Just name
declName (EntityDecl name _ _)     = Just name
declName (Constructor name _)      = Just name
declName (Destructor name _)       = Just name
-- ignore properties and namespaces, we don't want to namespace them further
declName Property{}                = Nothing
declName Namespace{}               = Nothing
declName x                         = error $ "unhandled in declName: " <> show x


