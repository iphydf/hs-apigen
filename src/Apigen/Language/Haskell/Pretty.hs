{-# LANGUAGE OverloadedStrings #-}
-- | Renders the Haskell AST as stylish-haskell lays it out, in 80 columns.
module Apigen.Language.Haskell.Pretty
    ( renderModule
    , renderDecls
    , renderExpr
    ) where

import           Apigen.Language.Haskell.AST
import           Data.List                   (groupBy, nub, sort, sortOn)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Prettyprinter               ((<+>))
import qualified Prettyprinter               as PP
import           Prettyprinter.Render.Text   (renderStrict)

columns :: Int
columns = 80

renderModule :: Module -> Text
renderModule m = Text.unlines $ concat
    [ pragmaLines (modPragmas m)
    , docLines 0 "-- |" (modDoc m)
    , headerLines m
    , if null (modImports m) then [] else "" : importLines (modImports m)
    , renderDecls (modDecls m)
    ]

renderDecls :: [Decl] -> [Text]
renderDecls = concatMap (("" :) . renderGroup) . groupBy together
  where
    together (Signature _ a _) (Binding b _ _)        = a == b
    together (Signature _ a _) (BindingWhere b _ _ _) = a == b
    together (Binding a _ _) (Binding b _ _)          = a == b
    together Foreign{} Foreign{}                      = True
    -- An opaque struct and its pointer, or a callback type and its wrapper.
    together (DataDecl _ _ [] _) TypeAlias{}          = True
    together TypeAlias{} (Foreign "wrapper" _ _)      = True
    together _ _                                      = False

renderExpr :: Expr -> Text
renderExpr = layout . expr 0

pragmaLines :: [Text] -> [Text]
pragmaLines pragmas =
    [ "{-# LANGUAGE " <> padRight width p <> " #-}" | p <- sorted ]
  where
    sorted = nub (sort pragmas)
    width = maximum (0 : map Text.length sorted)

headerLines :: Module -> [Text]
headerLines m = case modExports m of
    Nothing -> ["module " <> modName m <> " where"]
    Just exports ->
        ("module " <> modName m) : exportLines exports ++ ["    ) where"]

exportLines :: [Export] -> [Text]
exportLines = go True
  where
    go _ [] = []
    go first (ExportSection title : rest) =
        (if first then "    ( -- * " else "      -- * ") <> title
            : goAfterSection first rest
    go first (e : rest) =
        (if first then "    ( " else "    , ") <> export e : go False rest

    -- An item after an opening section heading starts the list's items.
    goAfterSection True (e : rest) | not (isSection e) =
        ("      " <> export e) : go False rest
    goAfterSection first rest = go first rest

    isSection ExportSection{} = True
    isSection _               = False

    export (ExportVar n)     = n
    export (ExportAll n)     = n <> " (..)"
    export (ExportPattern n) = "pattern " <> n
    export (ExportModule n)  = "module " <> n
    export (ExportSection n) = n

importLines :: [Import] -> [Text]
importLines imports = concatMap line (sortOn impModule imports)
  where
    modWidth = maximum (map (Text.length . impModule) imports)

    line i =
        -- Room for @qualified@ even if no import is qualified.
        let qualified
                | impAs i /= Nothing = "import qualified "
                | otherwise          = "import           "
            name = padRight modWidth (impModule i)
            alias = maybe "" (" as " <>) (impAs i)
            prefix = qualified <> name <> alias
        in case impNames i of
            Nothing    -> [Text.stripEnd prefix]
            Just names -> packList (prefix <> " ") (sort names)

    -- Greedily as many names per line as fit, aligned after the paren.
    packList prefix names =
        let indent = Text.replicate (Text.length prefix + 1) " "
            items = zipWith3 item [0 :: Int ..] names (map (const ()) names)
            item n name () = name <> (if n == length names - 1 then ")" else ",")
            fill current [] = [current]
            fill current (x : xs)
                | Text.length current + 1 + Text.length x <= columns =
                    fill (current <> " " <> x) xs
                | otherwise = current : fill (indent <> x) xs
        in case items of
            []       -> [prefix <> "()"]
            (x : xs) -> fill (prefix <> "(" <> x) xs

renderGroup :: [Decl] -> [Text]
renderGroup decls@(Binding name _ _ : _) | all isClause decls = clauses 0 decls
  where
    isClause (Binding n _ _) = n == name
    isClause _               = False
renderGroup decls = concatMap renderDecl decls

renderDecl :: Decl -> [Text]
renderDecl decl = case decl of
    DataDecl doc name cons derivs ->
        docLines 0 "-- |" doc
        ++ ["data " <> name]
        ++ concat (zipWith constructor ("= " : repeat "| ") cons)
        ++ deriving' derivs
    RecordDecl doc name fields derivs ->
        docLines 0 "-- |" doc
        ++ ["data " <> name <> " = " <> name]
        ++ recordFields fields
        ++ deriving' derivs
    NewtypeDecl doc name ty derivs ->
        docLines 0 "-- |" doc
        ++ ["newtype " <> name <> " = " <> name <> " " <> layout (typeDoc 11 ty)]
        ++ deriving' derivs
    TypeAlias doc name ty ->
        docLines 0 "-- |" doc
        ++ [layout (PP.group (PP.pretty ("type " <> name <> " =")
                <> PP.nest 4 (PP.line <> arrows "" ty)))]
    Foreign entity name ty ->
        -- The C name can go when it's the Haskell name.
        let entity' = if entity == name then "" else quote entity <> " "
        in [layout (signature ("foreign import ccall " <> entity' <> name) ty)]
    Signature doc name ty ->
        docLines 0 "-- |" doc ++ [layout (signature name ty)]
    Binding{} -> clauses 0 [decl]
    BindingWhere{} -> Text.lines (layout (clauseDoc decl))
    Instance cls ty methods ->
        ("instance " <> cls <> " " <> ty <> " where")
        : concat (intersperseBlank (map (clauses 4) (groupBy sameName methods)))
    PatternSynonym doc name ty value ->
        docLines 0 "-- |" doc
        ++ [ layout (signature ("pattern " <> name) ty)
           , "pattern " <> name <> " = " <> renderExpr value
           ]
  where
    sameName (Binding a _ _) (Binding b _ _) = a == b
    sameName _ _                             = False
    intersperseBlank (x : xs) = x : map ("" :) xs
    intersperseBlank []       = []
    quote t = "\"" <> t <> "\""

constructor :: Text -> Con -> [Text]
constructor lead c =
    ("    " <> lead <> layout (pattern' (PCon (conName c) (map typePat (conArgs c)))))
    : docLines 6 "-- ^" (conDoc c)
  where
    -- Constructor arguments print like patterns: atoms, else parenthesised.
    typePat t = PVar (layout (typeDoc 11 t))

recordFields :: [Field] -> [Text]
recordFields fields =
    concat (zipWith field ("{ " : repeat ", ") fields) ++ ["    }"]
  where
    width = maximum (map (Text.length . fieldName) fields)
    field lead f =
        ("    " <> lead <> padRight width (fieldName f) <> " :: " <> layout (typeDoc 0 (fieldType f)))
        : docLines 6 "-- ^" (fieldDoc f)

deriving' :: [Text] -> [Text]
deriving' []     = []
deriving' derivs = ["    deriving (" <> Text.intercalate ", " derivs <> ")"]

-- | A function's clauses, their @=@ aligned where each fits on one line.
clauses :: Int -> [Decl] -> [Text]
clauses indent bindings
    | all fitsFlat flat = map alignFlat flat
    | otherwise = concatMap (map (pad <>) . Text.lines . layoutAt indent . clauseDoc) bindings
  where
    pad = Text.replicate indent " "
    flat = [ (lhs name pats, layout (expr 0 body)) | Binding name pats body <- bindings ]
    lhsWidth = maximum (map (Text.length . fst) flat)
    fitsFlat (_, r) =
        not (Text.any (== '\n') r)
            && indent + lhsWidth + 3 + Text.length r <= columns
    alignFlat (l, r) = pad <> padRight lhsWidth l <> " = " <> r
    lhs name pats = Text.unwords (name : map (layout . patternArg) pats)

clauseDoc :: Decl -> PP.Doc ()
clauseDoc (Binding name pats body) =
    let lhs = PP.nest 4 (PP.fillSep (PP.pretty name : map patternArg pats)) <+> "="
    in case body of
        -- @f x = do@, the statements below.
        Do stmts -> lhs <+> doBlock stmts
        _        -> PP.group (lhs <> PP.nest 4 (PP.line <> expr 0 body))
clauseDoc (BindingWhere name pats body locals) =
    clauseDoc (Binding name pats body)
        <> PP.nest 2 (PP.hardline <> "where"
            <> PP.nest 2 (PP.hardline <> PP.concatWith (\a b -> a <> PP.hardline <> b) (map clauseDoc locals)))
clauseDoc _ = mempty

-- | @name :: type@: on one line, else the type on the next, else an
-- argument per line.
signature :: Text -> Type -> PP.Doc ()
signature name ty =
    PP.group (PP.pretty name <> PP.nest 4 (PP.line <> arrows "::" ty))

-- | A type's arguments and result, an arrow before each but the first,
-- which @lead@ precedes.
arrows :: Text -> Type -> PP.Doc ()
arrows lead ty = PP.group $ PP.vsep $ zipWith prefix (lead : repeat "->") parts
  where
    parts = case ty of
        TFun args ret -> map (typeDoc 1) (args ++ [ret])
        _             -> [typeDoc 0 ty]
    prefix l d = if Text.null l then d else PP.pretty l <+> d

typeDoc :: Int -> Type -> PP.Doc ()
typeDoc p ty = case ty of
    TCon n -> PP.pretty n
    TUnit -> "()"
    TTuple ts -> PP.tupled (map (typeDoc 0) ts)
    TApp (TCon "[]") [e] -> PP.brackets (typeDoc 0 e)
    TApp f args ->
        parensIf (p > 10) (PP.hsep (typeDoc 10 f : map (typeDoc 11) args))
    TFun args ret ->
        parensIf (p > 0) $ PP.group $ PP.align $ PP.vsep $
            zipWith (\lead t -> lead <> typeDoc 1 t) ("" : repeat "-> ") (args ++ [ret])

patternArg :: Pat -> PP.Doc ()
patternArg p = case p of
    PCon _ (_ : _) -> PP.parens (pattern' p)
    _              -> pattern' p

pattern' :: Pat -> PP.Doc ()
pattern' p = case p of
    PVar v    -> PP.pretty v
    PCon c ps -> PP.hsep (PP.pretty c : map patternArg ps)
    PLit n    -> if n < 0 then PP.parens (PP.pretty n) else PP.pretty n
    PWild     -> "_"

data Assoc = AssocLeft | AssocRight | AssocNone

fixity :: Text -> (Int, Assoc)
fixity op = case op of
    "$"     -> (0, AssocRight)
    ">>="   -> (1, AssocLeft)
    ">>"    -> (1, AssocLeft)
    "=<<"   -> (1, AssocRight)
    "<$>"   -> (4, AssocLeft)
    "<$"    -> (4, AssocLeft)
    "<*>"   -> (4, AssocLeft)
    "=="    -> (4, AssocNone)
    "<"     -> (4, AssocNone)
    "+"     -> (6, AssocLeft)
    "-"     -> (6, AssocLeft)
    "*"     -> (7, AssocLeft)
    "`div`" -> (7, AssocLeft)
    "."     -> (9, AssocRight)
    _       -> (9, AssocLeft)

expr :: Int -> Expr -> PP.Doc ()
expr p e = case e of
    Var v -> PP.pretty v
    Lit n -> if n < 0 then PP.parens (PP.pretty n) else PP.pretty n
    Str s -> PP.pretty (show (Text.unpack s))
    -- @f $ g x@ rather than @f (g x)@, where nothing follows.
    App f [a] | p == 0, isApplication a -> expr 0 (Op f "$" a)
    -- Plain arguments fill lines. Compound ones get a line each, after the
    -- plain ones that lead.
    App f args ->
        let (plain, rest) = span isAtom args
            lead = PP.hsep (expr 10 f : map (expr 11) plain)
        in parensIf (p > 10) $ case rest of
            [] -> PP.nest 4 (PP.fillSep (expr 10 f : map (expr 11) args))
            _  -> PP.group (lead <> PP.nest 4 (PP.line <> PP.vsep (map (expr 11) rest)))
    -- @f <$> a <*> b@ with more than one @<*>@: an operand per line.
    Op _ op _ | op `elem` ["<$>", "<*>"], (first, rest@(_ : _ : _)) <- applicativeChain e ->
        parensIf (p > 4) $ PP.group $ PP.nest 4 $
            PP.vsep (expr 5 first : [ PP.pretty o <+> PP.align (expr 5 x) | (o, x) <- rest ])
    Op l op r ->
        let (n, assoc) = fixity op
            lp = case assoc of AssocLeft -> n; _ -> n + 1
            rp = case assoc of AssocRight -> n; _ -> n + 1
        in parensIf (p > n) $ case op of
            -- What @$@ applies to may go on the next line.
            "$" -> PP.group (expr lp l <+> "$" <> PP.nest 4 (PP.line <> expr rp r))
            -- A bind chain may go on the next line, the operator first.
            _ | op `elem` [">>=", "=<<", "`finally`", "<$>", "<*>"] ->
                PP.group (expr lp l <> PP.nest 4 (PP.line <> PP.pretty op <+> expr rp r))
            _   -> expr lp l <+> PP.pretty op <+> expr rp r
    Lam pats body ->
        let lead = "\\" <> PP.hsep (map patternArg pats) <+> "->"
        in parensIf (p > 0) $ case body of
            Do stmts -> lead <+> doBlock stmts
            _        -> PP.group (lead <> PP.nest 4 (PP.line <> expr 0 body))
    Do stmts -> parensIf (p > 0) (doBlock stmts)
    Case scrutinee alts ->
        parensIf (p > 0) $
            "case" <+> expr 0 scrutinee <+> "of"
                <> PP.nest 4 (PP.hardline <> alternatives alts)
    With wrappers body ->
        parensIf (p > 0) $
            PP.concatWith (\a b -> a <> PP.hardline <> b)
                [ expr 1 w <+> "$" <+> "\\" <> PP.hsep (map patternArg pats) <+> "->"
                | (w, pats) <- wrappers ]
            <> case body of
                -- The last continuation's @-> do@, the statements below.
                Do stmts -> " " <> doBlock stmts
                _        -> PP.nest 4 (PP.hardline <> expr 0 body)

-- | A left-nested chain of @<$>@ and @<*>@: its first operand, and each
-- operator with the operand after it.
applicativeChain :: Expr -> (Expr, [(Text, Expr)])
applicativeChain (Op l op r)
    | op `elem` ["<$>", "<*>"] =
        let (first, rest) = applicativeChain l in (first, rest ++ [(op, r)])
applicativeChain e = (e, [])

doBlock :: [Stmt] -> PP.Doc ()
doBlock stmts =
    "do" <> PP.nest 4 (PP.hardline <> PP.concatWith (\a b -> a <> PP.hardline <> b) (map stmt stmts))

isAtom :: Expr -> Bool
isAtom e = case e of
    Var _ -> True
    Lit _ -> True
    Str _ -> True
    _     -> False

isApplication :: Expr -> Bool
isApplication e = case e of
    App{} -> True
    Lam{} -> True
    _     -> False

stmt :: Stmt -> PP.Doc ()
stmt (Bind pat e) = pattern' pat <+> "<-" <+> expr 0 e
stmt (Run e)      = expr 0 e

-- | Case alternatives, their arrows aligned where each fits on one line.
alternatives :: [(Pat, Expr)] -> PP.Doc ()
alternatives alts = PP.nesting $ \indent ->
    if all (fits indent) flat
        then PP.vsep [ PP.pretty (padRight width l) <+> "->" <+> PP.pretty r | (l, r) <- flat ]
        else PP.vsep [ PP.group (pattern' pat <+> "->" <> PP.nest 4 (PP.line <> expr 0 body))
                     | (pat, body) <- alts ]
  where
    flat = [ (layout (pattern' pat), layout (expr 0 body)) | (pat, body) <- alts ]
    width = maximum (map (Text.length . fst) flat)
    fits indent (_, r) =
        not (Text.any (== '\n') r) && indent + width + 4 + Text.length r <= columns

parensIf :: Bool -> PP.Doc () -> PP.Doc ()
parensIf True  = PP.parens
parensIf False = id

layout :: PP.Doc () -> Text
layout = layoutAt 0

layoutAt :: Int -> PP.Doc () -> Text
layoutAt indent =
    renderStrict . PP.layoutPretty (PP.LayoutOptions (PP.AvailablePerLine (columns - indent) 1))

padRight :: Int -> Text -> Text
padRight width t = t <> Text.replicate (width - Text.length t) " "

-- | A Haddock comment, @marker@ (@-- |@ or @-- ^@) on its first line,
-- wrapped to fit after @indent@ columns.
docLines :: Int -> Text -> Doc -> [Text]
docLines _ _ Nothing = []
docLines indent marker (Just doc) =
    case concat (zipWith paragraph [0 :: Int ..] (Text.splitOn "\n\n" doc)) of
        []            -> []
        (first : rest) -> (pad <> marker <> suffix first) : map ((pad <> "--") <>) (map suffix rest)
  where
    pad = Text.replicate indent " "
    suffix l = if Text.null l then "" else " " <> l
    width = columns - indent - Text.length marker - 1

    paragraph n p = (if n == 0 then [] else [""]) ++ block (Text.lines p)

    block ls
        | all (Text.isPrefixOf "> ") ls = ls
        | any (Text.isPrefixOf "* ") ls = concatMap bullet (items ls)
        | otherwise                     = wrap width "" (Text.unwords ls)

    items [] = []
    items (l : ls) =
        let (cont, rest) = break (Text.isPrefixOf "* ") ls
        in Text.unwords (l : cont) : items rest

    bullet item = case wrap (width - 2) "" (Text.drop 2 item) of
        (first : rest) -> ("* " <> first) : map ("  " <>) rest
        []             -> []

wrap :: Int -> Text -> Text -> [Text]
wrap width lead text = go lead (Text.words text)
  where
    go current [] = [current | not (Text.null current)]
    go current (w : ws)
        | Text.null current                          = go w ws
        | Text.length current + 1 + Text.length w <= width = go (current <> " " <> w) ws
        | otherwise                                  = current : go w ws
