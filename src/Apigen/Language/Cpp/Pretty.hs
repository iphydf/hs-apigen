{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Language.Cpp.Pretty (render, plain, renderAST) where

import           Apigen.Language.Cpp.AST
import           Apigen.Types                  (Constness (..))
import qualified Apigen.Types                  as T
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as Term

indentWidth :: Int
indentWidth = 2

plain :: Doc ann -> Doc xxx
plain = unAnnotate

renderSmart :: Float -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderSmart ribbonFraction widthPerLine
    = layoutSmart LayoutOptions
        { layoutPageWidth = AvailablePerLine widthPerLine (realToFrac ribbonFraction) }

render :: Doc AnsiStyle -> Text
render doc = (TL.toStrict . Term.renderLazy . renderSmart 1 120) doc <> "\n"

asterisk :: Doc AnsiStyle
asterisk = pretty ('*' :: Char)

ampersand :: Doc AnsiStyle
ampersand = pretty ('&' :: Char)

ppDecl :: CppDecl -> Doc AnsiStyle
ppDecl = \case
    Namespace name decls ->
        pretty ("namespace" :: Text) <+> pretty name <+> lbrace <> line <>
        indent indentWidth (vsep (map ppDecl decls)) <> line <>
        rbrace <+> pretty ("// namespace" :: Text) <+> pretty name
    Class name members ->
        pretty ("class" :: Text) <+> pretty name <+> lbrace <> line <>
        ppMembers name members <>
        rbrace <> semi
    Enum name members ->
        pretty ("enum class" :: Text) <+> pretty name <+> lbrace <> line <>
        indent indentWidth (vsep (punctuate comma (map (pretty :: Text -> Doc AnsiStyle) members))) <> line <>
        rbrace <> semi
    Function name ret params body ->
        ppType ret <+> pretty name <> parens (ppParams params) <+> lbrace <> line <>
        indent indentWidth (vsep (map ppStmt body)) <> line <>
        rbrace
    TypedefDecl ty name ->
        pretty ("using" :: Text) <+> pretty name <+> equals <+> ppType ty <> semi
    MethodDef className name ret params cns body ->
        ppType ret <+> pretty className <> pretty ("::" :: Text) <> pretty name <> parens (ppParams params) <>
        (if cns == ConstThis then space <> pretty ("const" :: Text) else mempty) <+> lbrace <> line <>
        indent indentWidth (vsep (map ppStmt body)) <> line <>
        rbrace
    ConstructorDef className params initList body ->
        pretty className <> pretty ("::" :: Text) <> pretty className <> parens (ppParams params) <+>
        (if null initList then mempty else colon <+> hsep (punctuate comma (map (\(m, v) -> pretty m <> parens (pretty v)) initList))) <+> lbrace <> line <>
        indent indentWidth (vsep (map ppStmt body)) <> line <>
        rbrace
    DestructorDef className body ->
        pretty className <> pretty ("::" :: Text) <> pretty ("~" :: Text) <> pretty className <> pretty ("()" :: Text) <+> lbrace <> line <>
        indent indentWidth (vsep (map ppStmt body)) <> line <>
        rbrace
    Include name system ->
        if system then pretty ("#include" :: Text) <+> angles (pretty name)
                  else pretty ("#include" :: Text) <+> dquotes (pretty name)
    ForwardDecl name ->
        pretty ("class" :: Text) <+> pretty name <> semi
    CommentDecl c ->
        pretty ("//" :: Text) <+> pretty c
    HeaderGuard guard decls ->
        pretty ("#ifndef" :: Text) <+> pretty guard <> line <>
        pretty ("#define" :: Text) <+> pretty guard <> line <> line <>
        vsep (map ppDecl decls) <> line <> line <>
        pretty ("#endif //" :: Text) <+> pretty guard
    TemplateDecl params decl ->
        pretty ("template" :: Text) <+> angles (hsep (punctuate comma (map pretty params))) <> line <>
        ppDecl decl

ppMembers :: Text -> [CppMember] -> Doc AnsiStyle
ppMembers className members =
    vsep $ map (ppGroup className members) [Public, Protected, Private]

ppGroup :: Text -> [CppMember] -> AccessSpecifier -> Doc AnsiStyle
ppGroup className allMembers spec =
    let filtered = filter (isSpec spec) allMembers
    in if null filtered then mempty
       else ppSpec spec <> colon <> line <>
            indent indentWidth (vsep (map (ppMember className) filtered))

isSpec :: AccessSpecifier -> CppMember -> Bool
isSpec s m = baseSpec (memberSpec m) == s
  where
    memberSpec = \case
        MemberDecl s' _ _ -> s'
        MethodDecl s' _ _ _ _ _ -> s'
        ConstructorDecl s' _ _ _ -> s'
        DestructorDecl s' _ -> s'
        CommentMember _ -> Private -- Doesn't matter
        NestedDecl _ -> Public -- Assuming public for now, or need to extend NestedDecl
        TemplateMemberDecl _ m' -> memberSpec m'

    baseSpec = \case
        Public -> Public
        Protected -> Protected
        Private -> Private
        Static s' -> baseSpec s'
        Virtual s' -> baseSpec s'

isStatic :: AccessSpecifier -> Bool
isStatic (Static _) = True
isStatic _          = False

isVirtual :: AccessSpecifier -> Bool
isVirtual (Virtual _) = True
isVirtual _           = False

ppSpec :: AccessSpecifier -> Doc AnsiStyle
ppSpec = \case
    Public -> pretty ("public" :: Text)
    Protected -> pretty ("protected" :: Text)
    Private -> pretty ("private" :: Text)
    Static s -> ppSpec s -- Don't print static here, handled in member
    Virtual s -> ppSpec s -- Don't print virtual here, handled in member

ppMember :: Text -> CppMember -> Doc AnsiStyle
ppMember className = \case
    MemberDecl spec ty name ->
        (if isStatic spec then pretty ("static" :: Text) <+> ppType ty else ppType ty)
        <+> pretty name <> semi
    MethodDecl spec name ret params cns body ->
        if "friend " `Text.isPrefixOf` name
        then pretty name <> semi
        else (if isStatic spec then pretty ("static" :: Text) <+> ppType ret
              else if isVirtual spec then pretty ("virtual" :: Text) <+> ppType ret
              else if "operator " `Text.isPrefixOf` name && name /= "operator=" then mempty
              else ppType ret)
             <+> pretty name <> parens (ppParams params) <+>
             (if cns == ConstThis && not (isStatic spec) then pretty ("const" :: Text) else mempty) <>
             case body of
                 Nothing -> semi
                 Just [Deleted] -> space <> pretty ("= delete" :: Text) <> semi
                 Just b -> space <> lbrace <> line <>
                           indent indentWidth (vsep (map ppStmt b)) <> line <>
                           rbrace
    ConstructorDecl _ params initList body ->
        pretty className <> parens (ppParams params) <+>
        (if null initList then mempty else colon <+> hsep (punctuate comma (map (\(m, v) -> pretty m <> parens (pretty v)) initList))) <>
        case body of
            Nothing -> semi
            Just [Deleted] -> space <> pretty ("= delete" :: Text) <> semi
            Just b -> space <> lbrace <> line <>
                      indent indentWidth (vsep (map ppStmt b)) <> line <>
                      rbrace
    DestructorDecl spec body ->
        (if isVirtual spec then pretty ("virtual" :: Text) <> space else mempty) <>
        pretty ("~" :: Text) <> pretty className <> pretty ("()" :: Text) <>
        case body of
            Nothing -> semi
            Just b -> space <> lbrace <> line <>
                      indent indentWidth (vsep (map ppStmt b)) <> line <>
                      rbrace
    CommentMember c ->
        pretty ("//" :: Text) <+> pretty c <> line
    NestedDecl decl ->
        ppDecl decl
    TemplateMemberDecl params member ->
        pretty ("template" :: Text) <+> angles (hsep (punctuate comma (map pretty params))) <> line <>
        ppMember className member

ppType :: CppType -> Doc AnsiStyle
ppType = \case
    TyVoid -> pretty ("void" :: Text)
    TyBool -> pretty ("bool" :: Text)
    TyInt b -> pretty ("int" :: Text) <> pretty b <> pretty ("_t" :: Text)
    TyUInt b -> pretty ("uint" :: Text) <> pretty b <> pretty ("_t" :: Text)
    TySizeT -> pretty ("size_t" :: Text)
    TyString -> pretty ("std::string" :: Text)
    TyBytes -> pretty ("std::vector<uint8_t>" :: Text)
    TyVector t -> pretty ("std::vector" :: Text) <> angles (ppType t)
    TyArray t s -> pretty ("std::array" :: Text) <> angles (ppType t <> comma <+> pretty s)
    TyOptional t -> pretty ("std::optional" :: Text) <> angles (ppType t)
    TyMonostate -> pretty ("std::monostate" :: Text)
    TyResult t e -> pretty ("Result" :: Text) <> angles (ppType t <> comma <+> ppType e)
    TySharedPtr t -> pretty ("std::shared_ptr" :: Text) <> angles (ppType t)
    TyUniquePtr t -> pretty ("std::unique_ptr" :: Text) <> angles (ppType t)
    TyPointer (TyFunction ret args) -> ppType ret <+> parens (asterisk) <> parens (hsep (punctuate comma (map ppType args)))
    TyPointer t -> ppType t <> asterisk
    TyFunction ret args -> pretty ("std::function" :: Text) <> angles (ppType ret <> parens (hsep (punctuate comma (map ppType args))))
    TyUserDefined t -> pretty t
    TyReference t -> ppType t <> ampersand
    TyRValueReference t -> ppType t <> ampersand <> ampersand
    TySpan t -> pretty ("std::span" :: Text) <> angles (ppType t)
    TyConst t -> pretty ("const" :: Text) <+> ppType t

ppParams :: [CppParam] -> Doc AnsiStyle
ppParams params = hsep $ punctuate comma $ map (\(CppParam ty name) -> ppType ty <+> pretty name) params

ppStmt :: CppStmt -> Doc AnsiStyle
ppStmt = \case
    Expr t -> pretty t <> semi
    Return t -> pretty ("return" :: Text) <+> pretty t <> semi
    If cond true false ->
        pretty ("if" :: Text) <+> parens (pretty cond) <+> lbrace <> line <>
        indent indentWidth (vsep (map ppStmt true)) <> line <>
        rbrace <> (if null false then mempty else space <> pretty ("else" :: Text) <+> lbrace <> line <> indent indentWidth (vsep (map ppStmt false)) <> line <> rbrace)
    Deleted -> pretty ("= delete" :: Text) <> semi

renderAST :: [CppDecl] -> Doc AnsiStyle
renderAST decls = vsep (map ppDecl decls)
