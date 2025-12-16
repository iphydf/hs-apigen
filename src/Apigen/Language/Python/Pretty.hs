{-# LANGUAGE OverloadedStrings #-}

-- | Renderer for the Cython 'Module' AST.
module Apigen.Language.Python.Pretty (render) where

import           Apigen.Language.Python.AST
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

render :: Module -> Text
render (Module cmt items) =
    Text.unlines $ ("# " <> cmt) : renderSeq 0 2 items

-- | One indentation level.
ind :: Int -> Text
ind n = Text.replicate n "    "

-- | Render a sequence of items, inserting @gap@ blank lines before each
-- \"major\" (block-like) item that is not the first.
renderSeq :: Int -> Int -> [Item] -> [Text]
renderSeq n gap = go True
  where
    go _ [] = []
    go isFirst (it : rest) =
        (if not isFirst && isMajor it then replicate gap "" else [])
        ++ renderItem n it ++ go False rest

isMajor :: Item -> Bool
isMajor ClassDef{}    = True
isMajor FuncDef{}     = True
isMajor PropertyDef{} = True
isMajor CdefBlock{}   = True
isMajor Extern{}      = True
isMajor _             = False

renderItem :: Int -> Item -> [Text]
renderItem _ Blank = [""]
renderItem n (Comment t) = [ind n <> "# " <> t]
renderItem n (CImport m names) =
    [ind n <> "from " <> m <> " cimport " <> Text.intercalate ", " names]
renderItem n (Import m names) =
    [ind n <> "from " <> m <> " import " <> Text.intercalate ", " names]
renderItem n (Extern hdr decls) =
    (ind n <> "cdef extern from \"" <> hdr <> "\":")
    : concatMap (renderExtern (n + 1)) decls
renderItem n (ClassDef cls) = renderClass n cls
renderItem n (CdefBlock funcs) =
    (ind n <> "cdef:") : concatMap (renderFunc (n + 1)) funcs
renderItem n (FuncDef f) = renderFunc n f
renderItem n (Decl f) = [ind n <> renderSignature f]
renderItem n (PropertyDef p) = renderProperty n p
renderItem n (Assign name annot e) =
    [ind n <> name <> ": " <> annot <> " = " <> renderExpr e]
renderItem n (RawItem t) = [ind n <> t]

renderExtern :: Int -> ExternDecl -> [Text]
renderExtern n (ExEnum name members) =
    (ind n <> "cpdef enum " <> name <> ":")
    : map (\m -> ind (n + 1) <> m) members
renderExtern n (ExOpaque name) = [ind n <> "ctypedef struct " <> name]
renderExtern n (ExAlias cty name) = [ind n <> "ctypedef " <> cty <> " " <> name]
renderExtern n (ExCallback name params) =
    [ind n <> "ctypedef void " <> name <> "("
        <> renderParams CStyle params <> ") except *"]
renderExtern n (ExFunc f) = [ind n <> renderSignature f]

renderClass :: Int -> Class -> [Text]
renderClass n (Class cdef name bases fields items) =
    (ind n <> kw <> name <> baseStr <> ":")
    : map (\(cty, fn) -> ind (n + 1) <> "cdef " <> cty <> " " <> fn) fields
    ++ body
  where
    kw = if cdef then "cdef class " else "class "
    baseStr | null bases = ""
            | otherwise  = "(" <> Text.intercalate ", " bases <> ")"
    body | null fields && null items = [ind (n + 1) <> "pass"]
         | otherwise = renderSeq (n + 1) 1 items

renderFunc :: Int -> Func -> [Text]
renderFunc n f =
    map (\d -> ind n <> "@" <> d) (fnDecorators f)
    ++ case (fnDoc f, fnBody f) of
        -- Compact single-line form for trivial stubs.
        (Nothing, [SPass]) -> [ind n <> renderSignature f <> ": pass"]
        _ ->
            (ind n <> renderSignature f <> ":")
            : renderDocBody (n + 1) (fnDoc f) (fnBody f)

-- | Signature without trailing colon (used for both decls and definitions).
renderSignature :: Func -> Text
renderSignature f = case fnKind f of
    KindDef ->
        "def " <> fnName f <> "(" <> params <> ")"
        <> (if Text.null (fnRet f) then "" else " -> " <> fnRet f)
        <> exceptStr
    KindCdef  -> "cdef " <> cRet <> fnName f <> "(" <> params <> ")" <> exceptStr
    KindCpdef -> "cpdef " <> cRet <> fnName f <> "(" <> params <> ")" <> exceptStr
  where
    params = renderParams (fnParamStyle f) (fnParams f)
    cRet = if Text.null (fnRet f) then "" else fnRet f <> " "
    exceptStr = if fnExcept f then " except *" else ""

renderProperty :: Int -> Property -> [Text]
renderProperty n (Property name ty doc getter setter) =
    (ind n <> "@property")
    : (ind n <> "def " <> name <> "(self) -> " <> ty <> ":")
    : renderDocBody (n + 1) doc getter
    ++ case setter of
        Nothing -> []
        Just (val, body) ->
            (ind n <> "@" <> name <> ".setter")
            : (ind n <> "def " <> name <> "(self, " <> val <> ": " <> ty <> "):")
            : renderDocBody (n + 1) Nothing body

-- | Render an optional docstring followed by a statement body; emits @pass@
-- when the body would otherwise be empty.
renderDocBody :: Int -> Maybe Text -> [Stmt] -> [Text]
renderDocBody n doc body =
    docLines ++ stmtLines
  where
    docLines = case doc of
        Nothing -> []
        Just d  -> [ind n <> "\"\"\"" <> d <> "\"\"\""]
    stmtLines
        | null body && null docLines = [ind n <> "pass"]
        | otherwise = concatMap (renderStmt n) body

renderParams :: ParamStyle -> [Param] -> Text
renderParams style = Text.intercalate ", " . map render1
  where
    render1 (Param name ty def) = base <> defStr
      where
        defStr = maybe "" (" = " <>) def
        base = case style of
            PyStyle
                | Text.null ty -> name
                | otherwise    -> name <> ": " <> ty
            CStyle
                | Text.null ty -> name
                | otherwise    -> ty <> " " <> name

renderStmt :: Int -> Stmt -> [Text]
renderStmt n SPass = [ind n <> "pass"]
renderStmt n (SExpr e) = [ind n <> renderExpr e]
renderStmt n (SReturn Nothing) = [ind n <> "return"]
renderStmt n (SReturn (Just e)) = [ind n <> "return " <> renderExpr e]
renderStmt n (SRaise e) = [ind n <> "raise " <> renderExpr e]
renderStmt n (SAssign l r) = [ind n <> renderExpr l <> " = " <> renderExpr r]
renderStmt n (SCdef cty name minit) =
    [ind n <> "cdef " <> cty <> " " <> name
        <> maybe "" (\e -> " = " <> renderExpr e) minit]
renderStmt n (SIf c body) =
    (ind n <> "if " <> renderExpr c <> ":") : renderBlock (n + 1) body
renderStmt n (STryFinally body fin) =
    (ind n <> "try:") : renderBlock (n + 1) body
    ++ (ind n <> "finally:") : renderBlock (n + 1) fin
renderStmt n (SComment t) = [ind n <> "# " <> t]

renderBlock :: Int -> [Stmt] -> [Text]
renderBlock n [] = [ind n <> "pass"]
renderBlock n body = concatMap (renderStmt n) body

renderExpr :: Expr -> Text
renderExpr (EName t) = t
renderExpr (EInt i) = Text.pack (show i)
renderExpr (EStr t) = "\"" <> escapeStr t <> "\""
renderExpr (ECall f args) =
    renderExpr f <> "(" <> Text.intercalate ", " (map renderExpr args) <> ")"
renderExpr (EAttr e a) = renderExpr e <> "." <> a
renderExpr (ESlice e a b) =
    renderExpr e <> "[" <> maybe "" renderExpr a <> ":" <> maybe "" renderExpr b <> "]"
renderExpr (EIndex e i) = renderExpr e <> "[" <> renderExpr i <> "]"
renderExpr (ECast cty e) = "<" <> cty <> "> " <> renderAtom e
renderExpr (EAddr e) = "&" <> renderAtom e
renderExpr (EBinOp op a b) =
    renderExpr a <> " " <> op <> " " <> renderExpr b
renderExpr (ECond c a b) =
    renderExpr a <> " if " <> renderExpr c <> " else " <> renderExpr b
renderExpr (ERaw t) = t

-- | Render an expression, parenthesising compound expressions so it is safe as
-- an operand of a tight-binding operator (cast, address-of).
renderAtom :: Expr -> Text
renderAtom e = case e of
    EBinOp{} -> "(" <> renderExpr e <> ")"
    ECond{}  -> "(" <> renderExpr e <> ")"
    ECast{}  -> "(" <> renderExpr e <> ")"
    _        -> renderExpr e

escapeStr :: Text -> Text
escapeStr = Text.concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc c    = Text.singleton c
