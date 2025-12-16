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
            joined = T.intercalate "\n\n" items <> "\n"
        in "impl" <> gens <> " " <> tr <> prettyType implType <> " {\n" <>
           indent joined <>
           "}"
    RsItemFn fn -> renderFn fn
    RsItemTypeAlias RsTypeAlias{..} ->
        renderVis aliasVis <> "type " <> aliasName <> " = " <> prettyType aliasType <> ";"
    RsMacroCall name args ->
        name <> "!(" <> T.intercalate ", " args <> ");"
    RsConst vis name ty val ->
        renderVis vis <> "const " <> name <> ": " <> prettyType ty <> " = " <> val <> ";"
    RsRaw txt -> txt

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
        unsafeKw = if fnUnsafe then "unsafe " else ""
        gens = renderGenerics fnGenerics
        args = T.intercalate ", " (map renderArg fnArgs)
        ret = maybe "" (\t -> " -> " <> prettyType t) fnRet
        doc = T.concat (map (\l -> "/// " <> l <> "\n") fnDoc)
        sig = doc <> vis <> unsafeKw <> abi <> "fn " <> fnName <> gens <> "(" <> args <> ")" <> ret
    in case fnBody of
        Nothing -> sig <> ";"
        Just b  -> sig <> " " <> renderFnBody b

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

-- | A function body: a trailing @return X;@ becomes a tail expression, for
-- clippy's @needless_return@. Nested blocks keep theirs, as early exits.
renderFnBody :: RsBlock -> Text
renderFnBody (RsBlock stmts) =
    let stmts' = case reverse stmts of
            (StmtReturn e : rest) -> reverse (StmtExprNoSemi e : rest)
            _                     -> stmts
    in "{\n" <> indent (T.unlines (map renderStmt stmts')) <> "}"

renderStmt :: RsStmt -> Text
renderStmt (StmtLet n t e) =
    let ty = maybe "" (\typ -> ": " <> prettyType typ) t
    in "let " <> n <> ty <> " = " <> prettyExpr e <> ";"
renderStmt (StmtExpr e) =
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
prettyExpr e@EMethodCall{}   = prettyChain e
prettyExpr e@EFieldAccess{}  = prettyChain e
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
-- A block-like operand needs parentheses before `as`, or Rust parses the
-- block as a statement.
prettyExpr (ECast e t) = prettyExpr (parenIfBlock e) <> " as " <> prettyType t
prettyExpr (EDeref e) = "*" <> prettyExpr e
prettyExpr (ELambda args e) = "|" <> T.intercalate ", " args <> "| " <> prettyExpr e
prettyExpr (EMacroCall n args) =
    let (open, close) = if n `elem` ["vec", "array"] then ("[", "]") else ("(", ")")
    in n <> "!" <> open <> T.intercalate ", " args <> close
prettyExpr (EParen e) = "(" <> prettyExpr e <> ")"
prettyExpr (EArrayInit e size) = "[" <> prettyExpr e <> "; " <> prettyExpr size <> "]"
prettyExpr (EBinOp op lhs rhs) = prettyExpr lhs <> " " <> op <> " " <> prettyExpr rhs

data ChainSeg
    = SegMethod Text [RsExpr] -- ^ `.method(args)`
    | SegField Text           -- ^ `.field`

-- | The head of a method/field chain, and its segments in order.
unrollChain :: RsExpr -> (RsExpr, [ChainSeg])
unrollChain = go []
  where
    go acc (EMethodCall obj m as) = go (SegMethod m as : acc) obj
    go acc (EFieldAccess obj f)   = go (SegField f : acc) obj
    go acc e                      = (e, acc)

renderSegInline :: ChainSeg -> Text
renderSegInline (SegMethod m as) =
    "." <> m <> "(" <> T.intercalate ", " (map prettyExpr as) <> ")"
renderSegInline (SegField f) = "." <> f

-- | A chain with two or more method calls gets one segment per line.
prettyChain :: RsExpr -> Text
prettyChain e =
    let (h, segs) = unrollChain e
        methodCount = length [() | SegMethod{} <- segs]
    in if methodCount >= 2
       then case segs of
           (first : rest) ->
               prettyExpr h <> renderSegInline first
               <> T.concat ["\n    " <> renderSegInline s | s <- rest]
           [] -> prettyExpr h
       else prettyExpr h <> T.concat (map renderSegInline segs)

parenIfBlock :: RsExpr -> RsExpr
parenIfBlock e = case e of
    EBlock{}  -> EParen e
    EUnsafe{} -> EParen e
    EIf{}     -> EParen e
    EMatch{}  -> EParen e
    _         -> e

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
indent t = T.unlines $ map indentLine (T.lines t)
  where
    indentLine "" = ""
    indentLine l  = "    " <> l
