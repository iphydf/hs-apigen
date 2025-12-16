{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Rust.Pretty (render, prettyType) where

import           Apigen.Language.Rust.AST
import           Data.Text                (Text)
import qualified Data.Text                as T

render :: RsModule -> Text
render RsModule{..} = T.unlines $ map renderItem rsModItems

renderItem :: RsItem -> Text
renderItem item = case item of
    RsUse vis path -> renderVis vis <> "use " <> path <> ";"
    RsUseAttr attr path -> "#[" <> attr <> "]\nuse " <> path <> ";"
    RsMod vis name Nothing -> renderVis vis <> "mod " <> name <> ";"
    RsMod vis name (Just items) ->
        renderVis vis <> "mod " <> name <> " {\n" <>
        indent (T.unlines (map renderItem items)) <>
        "}"
    RsItemStruct RsStruct{..} ->
        let vis = renderVis structVis
            gens = renderGenerics structGenerics
            fields = map renderField structFields
        in vis <> "struct " <> structName <> gens <> " {\n" <>
           indent (T.intercalate ",\n" fields) <>
           "\n}"
    RsItemTupleStruct RsTupleStruct{..} ->
        let vis = renderVis tupleStructVis
            gens = renderGenerics tupleStructGenerics
            fields = map (\t -> "pub " <> prettyType t) tupleStructFields -- Hardcoded pub
        in vis <> "struct " <> tupleStructName <> gens <> "(" <> T.intercalate ", " fields <> ");"
    RsItemEnum RsEnum{..} ->
        let vis = renderVis enumVis
            gens = renderGenerics enumGenerics
            variants = map renderVariant enumVariants
        in vis <> "enum " <> enumName <> gens <> " {\n" <>
           indent (T.intercalate ",\n" variants) <>
           "\n}"
    RsItemTrait RsTrait{..} ->
        let vis = renderVis traitVis
            methods = map renderFn traitMethods
        in vis <> "trait " <> traitName <> " {\n" <>
           indent (T.unlines methods) <>
           "}"
    RsItemImpl RsImpl{..} ->
        let tr = maybe "" (<> " for ") implTrait
            gens = renderGenerics implGenerics
            items = map renderItem implItems
        in "impl" <> gens <> " " <> tr <> prettyType implType <> " {\n" <>
           indent (T.unlines items) <>
           "}"
    RsItemFn fn -> renderFn fn
    RsItemTypeAlias RsTypeAlias{..} ->
        renderVis aliasVis <> "type " <> aliasName <> " = " <> prettyType aliasType <> ";"
    RsMacroCall name args ->
        name <> "!(" <> T.intercalate ", " args <> ");"

renderField :: RsStructField -> Text
renderField RsStructField{..} =
    renderVis fieldVis <> fieldName <> ": " <> prettyType fieldType

renderVariant :: RsEnumVariant -> Text
renderVariant RsEnumVariant{..} =
    if null variantFields
    then variantName
    else variantName <> "(" <> T.intercalate ", " (map prettyType variantFields) <> ")"

renderFn :: RsFn -> Text
renderFn RsFn{..} =
    let vis = renderVis fnVis
        abi = maybe "" (\a -> "extern " <> a <> " ") fnAbi
        gens = renderGenerics fnGenerics
        args = T.intercalate ", " (map renderArg fnArgs)
        ret = maybe "" (\t -> " -> " <> prettyType t) fnRet
        sig = vis <> abi <> "fn " <> fnName <> gens <> "(" <> args <> ")" <> ret
    in case fnBody of
        Nothing -> sig <> ";"
        Just b  -> sig <> " " <> renderBlock b

renderArg :: RsArg -> Text
renderArg (RsSelfArg False) = "&self"
renderArg (RsSelfArg True)  = "&mut self"
renderArg RsSelf            = "self"
renderArg (RsArg n t)       = n <> ": " <> prettyType t

renderVis :: RsVis -> Text
renderVis Pub      = "pub "
renderVis PubCrate = "pub(crate) "
renderVis Private  = ""

renderGenerics :: [Text] -> Text
renderGenerics [] = ""
renderGenerics gs = "<" <> T.intercalate ", " gs <> ">"

renderBlock :: RsBlock -> Text
renderBlock (RsBlock stmts) =
    "{\n" <> indent (T.unlines (map renderStmt stmts)) <> "}"

renderStmt :: RsStmt -> Text
renderStmt (StmtLet n t e) =
    let ty = maybe "" (\typ -> ": " <> prettyType typ) t
    in "let " <> n <> ty <> " = " <> prettyExpr e <> ";"
renderStmt (StmtExpr e) =
    -- Rust statements ending in expression without semicolon are return values
    -- But here we handle StmtReturn separately.
    -- If it's a block-like expression (if, match), it doesn't need a semicolon, but usually acceptable.
    prettyExpr e <> ";"
renderStmt (StmtExprNoSemi e) =
    prettyExpr e
renderStmt (StmtReturn e) =
    "return " <> prettyExpr e <> ";"
renderStmt (StmtItem i) = renderItem i

prettyType :: RsType -> Text
prettyType (TyPath t) = t
prettyType (TyRef m t False) = "&" <> maybe "" (<> " ") m <> prettyType t
prettyType (TyRef m t True) = "&mut " <> maybe "" (<> " ") m <> prettyType t
prettyType (TySlice t) = "[" <> prettyType t <> "]"
prettyType (TyArray t n) = "[" <> prettyType t <> "; " <> n <> "]"
prettyType (TyTuple ts) = "(" <> T.intercalate ", " (map prettyType ts) <> ")"
prettyType TyUnit = "()"
prettyType (TyGeneric n ts) = n <> "<" <> T.intercalate ", " (map prettyType ts) <> ">"

prettyExpr :: RsExpr -> Text
prettyExpr (EVar t) = t
prettyExpr (ELit l) = prettyLit l
prettyExpr (ECall f args) =
    prettyExpr f <> "(" <> T.intercalate ", " (map prettyExpr args) <> ")"
prettyExpr (EMethodCall obj method args) =
    prettyExpr obj <> "." <> method <> "(" <> T.intercalate ", " (map prettyExpr args) <> ")"
prettyExpr (EFieldAccess obj field) =
    prettyExpr obj <> "." <> field
prettyExpr (EStructInit name fields) =
    name <> " { " <> T.intercalate ", " (map (\(n,e) -> n <> ": " <> prettyExpr e) fields) <> " }"
prettyExpr (EBlock b) = renderBlock b
prettyExpr (EUnsafe b) = "unsafe " <> renderBlock b
prettyExpr (EIf cond trueBlock falseBlock) =
    "if " <> prettyExpr cond <> " " <> renderBlock trueBlock <>
    maybe "" (\t -> " else " <> renderBlock t) falseBlock
prettyExpr (EMatch e arms) =
    "match " <> prettyExpr e <> " {\n" <>
    indent (T.unlines (map renderArm arms)) <>
    "}"
prettyExpr (ERef e False) = "&" <> prettyExpr e
prettyExpr (ERef e True) = "&mut " <> prettyExpr e
prettyExpr (ETry e) = prettyExpr e <> "?"
prettyExpr (ECast e t) = prettyExpr e <> " as " <> prettyType t
prettyExpr (EDeref e) = "*" <> prettyExpr e
prettyExpr (ELambda args e) = "|" <> T.intercalate ", " args <> "| " <> prettyExpr e
prettyExpr (EMacroCall n args) = n <> "![" <> T.intercalate ", " args <> "]"
prettyExpr (EParen e) = "(" <> prettyExpr e <> ")"
prettyExpr (EArrayInit e size) = "[" <> prettyExpr e <> "; " <> prettyExpr size <> "]"
prettyExpr (EBinOp op lhs rhs) = prettyExpr lhs <> " " <> op <> " " <> prettyExpr rhs

renderArm :: (RsPat, RsBlock) -> Text
renderArm (pat, block) = prettyPat pat <> " => " <> renderBlock block <> ","

prettyPat :: RsPat -> Text
prettyPat (PVar t) = t
prettyPat (PPath t) = t
prettyPat PWildcard = "_"
prettyPat (PLit l) = prettyLit l
prettyPat (PTupleStruct n pats) = n <> "(" <> T.intercalate ", " (map prettyPat pats) <> ")"

prettyLit :: RsLit -> Text
prettyLit (LInt i)    = T.pack (show i)
prettyLit (LString s) = T.pack (show s) -- TODO: Proper escaping
prettyLit (LBool b)   = if b then "true" else "false"

indent :: Text -> Text
indent t = T.unlines $ map ("    " <>) (T.lines t)
