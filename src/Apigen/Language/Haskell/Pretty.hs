{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Haskell.Pretty (render, prettyType) where

import           Apigen.Language.Haskell.AST
import           Data.Text                   (Text)
import qualified Data.Text                   as T

render :: HsModule -> Text
render HsModule{..} = T.unlines $
    [
        renderPragmas hsModPragmas
    ,   renderOptions hsModOptions
    ,   if null hsModExports
        then "module " <> hsModName <> " where"
        else "module " <> hsModName <> " ("
    ] ++ (if null hsModExports then [] else renderExports hsModExports ++ [ ") where" ])
    ++ [ "" ] ++ map renderImport hsModImports ++ [ "" ] ++ map renderDecl hsModDecls

renderPragmas :: [Text] -> Text
renderPragmas pragmas = T.unlines $ map (\p -> "{-# LANGUAGE " <> p <> " #-}") pragmas

renderOptions :: [Text] -> Text
renderOptions opts = T.unlines $ map (\o -> "{-# OPTIONS_GHC " <> o <> " #-}") opts

renderExports :: [Text] -> [Text]
renderExports exports =
    let commaSep = map (<> ",") (init exports) ++ [last exports]
    in map ("    " <>) commaSep

renderImport :: HsImport -> Text
renderImport HsImport{..} =
    "import " <> (if importQualified then "qualified " else "") <> importModule <>
    maybe "" (" as " <>) importAs <>
    maybe "" (\specs -> " (" <> T.intercalate ", " specs <> ")") importSpecs

renderDecl :: HsDecl -> Text
renderDecl decl = case decl of
    HsDataDecl{..} ->
        let cons = map renderCon dataCons
            deriv = renderDeriving dataDeriving
            sep = "\n    = "
        in if null dataCons
           then "data " <> dataName <> deriv
           else "data " <> dataName <> sep <> T.intercalate sep cons <> deriv
    HsNewtypeDecl{..} ->
        "newtype " <> newtypeName <> " = " <> renderCon newtypeCon <> renderDeriving newtypeDeriving
    HsDataEnumDecl{..} ->
        let members = map (\m -> m) enumMembers
            sep = "\n    | "
        in "data " <> hsEnumName <> "\n    = " <> T.intercalate sep members <> renderDeriving enumDeriving
    HsTypeAlias{..} ->
        "type " <> aliasName <> " = " <> prettyType aliasType
    HsForeignImport{..} ->
        let safety = "" -- "unsafe " if foreignPure else "" -- simplified
        in "foreign import ccall " <> safety <> "\"" <> foreignCName <> "\" " <> foreignHsName <> " :: " <> prettyType foreignType
    HsFunSig{..} ->
        funSigName <> " :: " <> prettyType funSigType
    HsFunBind{..} ->
        funBindName <> " " <> T.unwords (map prettyPat funBindArgs) <> " = " <> prettyExpr funBindBody
    HsInstance{..} ->
        "instance " <> instClass <> " " <> prettyTypePrec 10 instType <> " where\n"
        <> indent (T.unlines (map renderDecl instDecls))
    HsRawDecl t -> t
    HsPragma t -> "{-# " <> t <> " #-}"
    HsComment t -> "-- " <> t

renderCon :: HsConDecl -> Text
renderCon HsConDecl{..} =
    conName <> if null conArgs then "" else " " <> T.unwords (map (prettyTypePrec 10) conArgs)

renderDeriving :: [Text] -> Text
renderDeriving [] = ""
renderDeriving ds = "\n    deriving (" <> T.intercalate ", " ds <> ")"

prettyType :: HsType -> Text
prettyType = prettyTypePrec 0

prettyTypePrec :: Int -> HsType -> Text
prettyTypePrec p ty = case ty of
    TyCon t -> t
    TyVar t -> t
    TyApp f x -> parens (p > 9) $ prettyTypePrec 9 f <> " " <> prettyTypePrec 10 x
    TyFun a b -> parens (p > 0) $ prettyTypePrec 1 a <> " -> " <> prettyTypePrec 0 b
    TyTuple ts -> "(" <> T.intercalate ", " (map prettyType ts) <> ")"
    TyList t -> "[" <> prettyType t <> "]"
    TyParen t -> "(" <> prettyType t <> ")"
    TyUnit -> "()"

prettyPat :: HsPat -> Text
prettyPat pat = case pat of
    PVar t    -> t
    PCon c ps -> "(" <> c <> " " <> T.unwords (map prettyPat ps) <> ")"
    PLit l    -> prettyLit l
    PTuple ps -> "(" <> T.intercalate ", " (map prettyPat ps) <> ")"
    PWildCard -> "_"

prettyExpr :: HsExpr -> Text
prettyExpr = prettyExprPrec 0

prettyExprPrec :: Int -> HsExpr -> Text
prettyExprPrec p expr = case expr of
    EVar t -> t
    ELit l -> prettyLit l
    EApp f x -> parens (p > 10) $ prettyExprPrec 10 f <> " " <> prettyExprPrec 11 x
    EInfixApp l op r -> parens (p > 0) $ prettyExprPrec 1 l <> " " <> op <> " " <> prettyExprPrec 1 r
    ELam ps body -> parens (p > 0) $ "\\" <> T.unwords (map prettyPat ps) <> " -> " <> prettyExprPrec 0 body
    ELet decls body ->
        let ds = T.unlines (map renderDecl decls)
        in parens (p > 0) $ "\n    let " <> indent ds <> "    in " <> prettyExprPrec 0 body
    ECase e alts ->
        parens (p > 0) $ "\n    case " <> prettyExprPrec 0 e <> " of\n" <>
        indent (indent (T.unlines (map prettyAlt alts)))
    EDo stmts ->
        parens (p > 0) $ "do { " <> T.intercalate "; " (map prettyStmt stmts) <> " }"
    EIf c t e -> parens (p > 0) $ "if " <> prettyExprPrec 0 c <> " then " <> prettyExprPrec 0 t <> " else " <> prettyExprPrec 0 e
    ETuple es -> "(" <> T.intercalate ", " (map prettyExpr es) <> ")"
    EList es -> "[" <> T.intercalate ", " (map prettyExpr es) <> "]"
    EParen e -> "(" <> prettyExpr e <> ")"

prettyLit :: HsLit -> Text
prettyLit l = case l of
    LInt i    -> T.pack (show i)
    LString s -> T.pack (show s)
    LChar c   -> T.pack (show c)

prettyAlt :: HsAlt -> Text
prettyAlt HsAlt{..} = prettyPat altPat <> " -> " <> prettyExpr altExpr

prettyStmt :: HsStmt -> Text
prettyStmt stmt = case stmt of
    SGenerator p e -> prettyPat p <> " <- " <> prettyExpr e
    SExpr e -> prettyExpr e
    SLet decls -> "let { " <> T.intercalate "; " (map renderDecl decls) <> " }"

indent :: Text -> Text
indent t = T.unlines $ map ("    " <>) (T.lines t)

parens :: Bool -> Text -> Text
parens True t  = "(" <> t <> ")"
parens False t = t
