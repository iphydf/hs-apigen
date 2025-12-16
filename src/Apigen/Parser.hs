{-# OPTIONS_GHC -Wwarn -fmax-pmcheck-models=100 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Apigen.Parser (parseModel) where

import           Apigen.Parser.AST           (Decl (..), Model (..),
                                              Module (..))
import qualified Apigen.Parser.Query         as Query
import qualified Apigen.Parser.SymbolNumbers as SymbolNumbers
import           Apigen.Parser.SymbolTable   (M, Name, SId, SIdToName, Sym,
                                              mustLookupM, renameM, resolveM)
import           Apigen.Patterns
import           Apigen.Types                (BitSize (..), BuiltinType (..),
                                              Constness (..), Generated (..))
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM, unFix)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..), lexemeText)
import qualified Language.Cimple             as Cimple

type SimplificationMode = Bool

parseModel :: [SymbolNumbers.TranslationUnit Text] -> Model (Lexeme Name)
parseModel tus =
    let (mkDecls, sids) = SymbolNumbers.collect tus
        rawDecls = State.evalState (mapM (uncurry parseRawModule) mkDecls >>= mapM resolveM) sids
        simplifiedMods = State.evalState (mapM (uncurry parseModule) mkDecls >>= mapM resolveM) sids
    in Model simplifiedMods (concatMap moduleDecls (modelMods (Model rawDecls [])))

parseRawModule :: FilePath -> [Node (Lexeme SId)] -> M a (Module (Lexeme SId))
parseRawModule file nodes = Module file . concat <$> mapM (foldFixM (go False)) nodes

parseModule :: FilePath -> [Node (Lexeme SId)] -> M a (Module (Lexeme SId))
parseModule file nodes = Module file . concat <$> mapM (foldFixM (go True)) nodes

go :: SimplificationMode -> NodeF (Lexeme SId) [Sym] -> M a [Sym]
-- {-
go _ (PreprocInclude _) = return []
go _ (VarDecl [] _ []) = return []
go _ (FunctionPrototype [] _ _) = return []

go _ (PreprocIfndef (L _ _ symbol) _ es)
    | symbol == SymbolNumbers.SYM_APIGEN_IGNORE = return es
    | symbol == SymbolNumbers.SYM_TOX_HIDE_DEPRECATED = return es

go _ (FunctionPrototype [ret] name [[BuiltinType Void]]) = return [Function ret name []]
go _ (FunctionPrototype [ret] name params              ) = return [Function ret name (concat params)]

go _ (FunctionDecl Cimple.Global func) = return func
go _ (Enumerator name _) = return [EnumMember name]
go mode (EnumConsts (Just name) enums  ) = mkEnum mode name enums
go mode (EnumDecl         name  enums _) = mkEnum mode name enums

go _ (TyPointer [ConstType (BuiltinType Char)]) = return [BuiltinType String]
go _ (TyPointer [           BuiltinType Char ]) = return [BuiltinType String]
go _ (TyPointer [           Typename ty      ]) = return [     PointerType ty]
go _ (TyPointer [ConstType (Typename ty     )]) = return [ConstPointerType ty]

go _ (DeclSpecArray _ (Just [expr]))                                  = return [SizedArrayType (BuiltinType Void) expr]
go _ (DeclSpecArray _ Nothing)                                        = return [ArrayType Void]
go _ (VarDecl [           BuiltinType ty ] name [[ArrayType Void]]) = return [Var (ArrayType      ty     ) name]
go _ (VarDecl [ConstType (BuiltinType ty)] name [[ArrayType Void]]) = return [Var (ConstArrayType ty     ) name]
go _ (VarDecl [Typename ty               ] name [[ArrayType Void]]) = return [Var (UserArrayType  ty     ) name]
go _ (VarDecl [ty] name [[SizedArrayType (BuiltinType Void) size]]) = return [Var (SizedArrayType ty size) name]

go _ (FunctionCall [Ref (L _ _ sym)] [[expr]  ])
    | sym == SymbolNumbers.SYM_abs = return [Abs expr]
go _ (FunctionCall [Ref (L _ _ sym)] [[a], [b]])
    | sym == SymbolNumbers.SYM_max = return [Max a b]

-- -}

go _ (TyConst [ty])                  = return [ConstType ty]
go _ (TyPointer [BuiltinType Void])  = return [BuiltinType VoidPtr]
go _ (TyPointer [ConstType (BuiltinType Void)]) = return [ConstType (BuiltinType VoidPtr)]
go _ (TyPointer [BuiltinType (UInt bs)]) = return [ArrayType (UInt bs)]
go _ (TyPointer [ConstType (BuiltinType (UInt bs))]) = return [ConstArrayType (UInt bs)]
go _ (TyPointer [BuiltinType (SInt bs)]) = return [ArrayType (SInt bs)]
go _ (TyPointer [ConstType (BuiltinType (SInt bs))]) = return [ConstArrayType (SInt bs)]
go _ (TyPointer [ArrayType bs])      = return [ArrayType bs]
go _ (TyPointer [ConstArrayType bs]) = return [ConstArrayType bs]
go _ (TyPointer [ConstType ty@CallbackType{}]) = return [ty]
go _ (TyPointer ty@[CallbackType{}]) = return ty

go _ (AggregateDecl ty@[TypeDecl _]) = return ty
go _ (Struct ty _)                   = return [TypeDecl ty]
go _ (TyStruct ty)                   = return [Typename ty]
go _ (TyUserDefined ty)              = return [Typename ty]
go _ (TyFunc ty)                     = return [CallbackType ty]
go _ (TyBitwise [ty])                = return [ty]
go _ (TyForce [ty])                  = return [ty]
go _ (TyOwner [ty])                  = return [ty]
go _ (TyNonnull [ty])                = return [ty]
go _ (TyNullable [ty])               = return [ty]
go _ (TypedefFunction [Function (BuiltinType Void) name params]) = return [CallbackTypeDecl name params]
go _ (Typedef [Typename ty@(L _ _ sid)] name _) = do
    (ns, n) <- mustLookupM sid
    let nameStr = Text.intercalate "_" (ns ++ n)
    if nameStr == "uint32_t"
       then return [IdTypeDecl name]
       else return [TypeDecl ty]
go _ (Typedef [BuiltinType (UInt B32)] name _) = return [IdTypeDecl name]
-- typedef uint8_t Name[SIZE]; -- a fixed-size byte array type.
go _ (Typedef [BuiltinType (UInt B8)] name [[SizedArrayType (BuiltinType Void) size]]) =
    return [ArrayTypeDecl name size]
go _ (Typedef _ _ _) = return []

go _ (TyStd (L _ _ ty)) =
    case ty of
        TY_void     -> return [BuiltinType Void]
        TY_char     -> return [BuiltinType Char]
        TY_int      -> return [BuiltinType (SInt B32)]
        TY_unsigned -> return [BuiltinType (UInt B32)]
        TY_bool     -> return [BuiltinType Bool]
        TY_int8_t   -> return [BuiltinType (SInt B8)]
        TY_uint8_t  -> return [BuiltinType (UInt B8)]
        TY_int16_t  -> return [BuiltinType (SInt B16)]
        TY_uint16_t -> return [BuiltinType (UInt B16)]
        TY_int32_t  -> return [BuiltinType (SInt B32)]
        TY_uint32_t -> return [BuiltinType (UInt B32)]
        TY_int64_t  -> return [BuiltinType (SInt B64)]
        TY_uint64_t -> return [BuiltinType (UInt B64)]
        TY_size_t   -> return [BuiltinType SizeT]
        _           -> return [BuiltinType Void]

go _ (PreprocDefineConst name [val]) = return [Define name val]
go _ (PreprocDefineConst _ [])       = return []
go _ (VarDecl [ty] name [])          = return [Var ty name]
go _ (VarExpr name)                  = return [Ref name]
go _ (ParenExpr [x])                 = return [Paren x]
go _ (LiteralExpr Cimple.ConstId name)      = return [Ref name]
go _ (LiteralExpr Cimple.Int val)           = return [IntVal val]
go _ (BinaryExpr [l] Cimple.BopPlus  [r])   = return [Add l r]
go _ (BinaryExpr [l] Cimple.BopMinus [r])   = return [Sub l r]
go _ (BinaryExpr [l] Cimple.BopMul   [r])   = return [Mul l r]
go _ (BinaryExpr [l] Cimple.BopDiv   [r])   = return [Div l r]

go _ BinaryExpr{}                    = return []
go _ CopyrightDecl{}                 = return []
go _ LicenseDecl{}                   = return []
go _ Comment{}                       = return []
go _ CommentSectionEnd{}             = return []
go _ CommentInfo{}                   = return []
go _ PreprocDefine{}                 = return []
go _ (SizeofType [ty]) = return [Sizeof ty]
go _ SizeofType{}                    = return []
-- TODO(iphydf): Create bindings for public structs?
go _ MemberDecl{}                    = return []
-- TODO(iphydf): Create bindings for macros?
go _ MacroBodyFunCall{}              = return []
go _ PreprocDefineMacro{}            = return []
go _ ParenExpr{}                     = return []
go _ FunctionCall{}                  = return []
go _ (PreprocIfndef _ ts es)      = return $ concat ts ++ es
go _ (PreprocElse xs)                = return $ concat xs
go _ (Group xs)                      = return $ concat xs
go _ (Commented _ x)                 = return x
go _ (CommentSection _ xs _)         = return $ concat xs
go _ (ExternC xs)                    = return $ concat xs
go _ Ellipsis                        = return []

go _ x                               = error $ "parse failed: " <> show x

mkEnum :: SimplificationMode -> Lexeme SId -> [[Sym]] -> State (SIdToName, a) [Sym]
mkEnum _ name enums = return [Enumeration [] name (concat enums)]
