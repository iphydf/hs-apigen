{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The doxygen comments on C declarations, as text for the emitters.
module Apigen.Parser.Docs
    ( Docs (..)
    , emptyDocs
    , extractDocs
    ) where

import qualified Apigen.Parser.SymbolNumbers as SymbolNumbers
import           Data.Fix                    (Fix (..), foldFix)
import           Data.Foldable               (toList)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Comment, CommentF (..),
                                              CommentStyle (..), Lexeme, Node,
                                              NodeF (..), lexemeText)

-- | Docs by C function name, by (enum typedef name, member name), and by
-- (struct name, member name).
data Docs = Docs
    { funcDocs         :: Map Text Text
    , enumMemberDocs   :: Map (Text, Text) Text
    , structMemberDocs :: Map (Text, Text) Text
    } deriving (Show, Eq)

emptyDocs :: Docs
emptyDocs = Docs Map.empty Map.empty Map.empty

unionDocs :: Docs -> Docs -> Docs
unionDocs a b = Docs
    { funcDocs         = funcDocs a         `Map.union` funcDocs b
    , enumMemberDocs   = enumMemberDocs a   `Map.union` enumMemberDocs b
    , structMemberDocs = structMemberDocs a `Map.union` structMemberDocs b
    }

unionsDocs :: [Docs] -> Docs
unionsDocs = foldr unionDocs emptyDocs

extractDocs :: [SymbolNumbers.TranslationUnit Text] -> Docs
extractDocs = unionsDocs . concatMap (map walkRoot . snd)

walkRoot :: Node (Lexeme Text) -> Docs
walkRoot (Fix n) = walkNode n

walkNode :: NodeF (Lexeme Text) (Node (Lexeme Text)) -> Docs
walkNode (Commented commentNode declNode) =
    let captured = case (extractCommentText commentNode, extractFunctionName declNode) of
            (Just cmt, Just fname) ->
                emptyDocs { funcDocs = Map.singleton fname cmt }
            _                      -> emptyDocs
    in captured `unionDocs` walkRoot declNode
walkNode (EnumDecl _tag members aliasLex) =
    -- Keyed by the typedef alias.
    walkEnumMembers (lexemeText aliasLex) members
walkNode (EnumConsts (Just tagLex) members) =
    -- The open enums (@Tox_File_Kind@) have no typedef.
    walkEnumMembers (lexemeText tagLex) members
walkNode (Struct nameLex members) =
    walkStructMembers (lexemeText nameLex) members
walkNode other =
    unionsDocs (map walkRoot (toList other))

walkEnumMembers :: Text -> [Node (Lexeme Text)] -> Docs
walkEnumMembers enumName = unionsDocs . map go
  where
    go :: Node (Lexeme Text) -> Docs
    go (Fix (Commented c (Fix (Enumerator nameLex _)))) =
        case extractCommentText c of
            Just cmt ->
                emptyDocs { enumMemberDocs = Map.singleton (enumName, lexemeText nameLex) cmt }
            Nothing  -> emptyDocs
    go _ = emptyDocs

walkStructMembers :: Text -> [Node (Lexeme Text)] -> Docs
walkStructMembers structName = unionsDocs . map go
  where
    go :: Node (Lexeme Text) -> Docs
    go (Fix (Commented c (Fix (MemberDecl (Fix (VarDecl _ nameLex _)) _)))) =
        case extractCommentText c of
            Just cmt ->
                emptyDocs { structMemberDocs = Map.singleton (structName, lexemeText nameLex) cmt }
            Nothing  -> emptyDocs
    go other = walkRoot other


-- | Enum-member comments stay raw: Cimple's tree parser doesn't reach them.
extractCommentText :: Node (Lexeme Text) -> Maybe Text
extractCommentText (Fix (CommentInfo c)) = Just (renderCommentInfo c)
extractCommentText (Fix (Comment Doxygen _open body _close)) =
    -- The lexer already drops the per-line @ * @ markers.
    Just . normalizeWhitespace $ Text.concat (map lexemeText body)
extractCommentText _ = Nothing

-- | Collapse each line's runs of spaces, which Cimple keeps from indentation.
normalizeWhitespace :: Text -> Text
normalizeWhitespace =
      Text.intercalate "\n"
    . map collapseSpaces
    . Text.splitOn "\n"
  where
    collapseSpaces = Text.unwords . Text.words

-- | Fold a structured doxygen comment back to its text, @\@param@ and all.
renderCommentInfo :: Comment (Lexeme Text) -> Text
renderCommentInfo = foldFix go
  where
    go :: CommentF (Lexeme Text) Text -> Text
    go = \case
        DocComment lines_   -> Text.intercalate "\n" lines_
        DocLine pieces      -> Text.concat pieces
        DocWord lex_        -> lexemeText lex_
        DocParam mAttr name ->
            let attr = maybe "" (\l -> "[" <> lexemeText l <> "] ") mAttr
            in "@param " <> attr <> lexemeText name
        DocBrief            -> "@brief"
        DocReturn           -> "@return"
        DocRetval           -> "@retval"
        DocAttention        -> "@attention"
        DocDeprecated       -> "@deprecated"
        DocFile             -> "@file"
        DocNote             -> "@note"
        DocPrivate          -> "@private"
        DocSee n            -> "@see "        <> lexemeText n
        DocRef n            -> lexemeText n
        DocP n              -> "@p "          <> lexemeText n
        DocExtends l        -> "@extends "    <> lexemeText l
        DocImplements l     -> "@implements " <> lexemeText l
        DocSection t        -> "@section "    <> lexemeText t
        DocSubsection t     -> "@subsection " <> lexemeText t
        DocThread r         -> "@thread "     <> lexemeText r
        DocGuardedBy l      -> "@guarded_by " <> lexemeText l
        DocRequires p f     -> "@requires(" <> lexemeText p <> ", " <> lexemeText f <> ")"
        DocSecurityRank kw mp rank ->
            let mparam = maybe "" (\l -> ", " <> lexemeText l) mp
            in "@security_rank(" <> lexemeText kw <> mparam <> ", " <> lexemeText rank <> ")"
        DocCode _ code _    ->
            "```\n" <> Text.intercalate "\n" code <> "\n```"

extractFunctionName :: Node (Lexeme Text) -> Maybe Text
extractFunctionName (Fix (FunctionDecl _ inner))          = extractFunctionName inner
extractFunctionName (Fix (FunctionDefn _ proto _))        = extractFunctionName proto
extractFunctionName (Fix (TypedefFunction inner))         = extractFunctionName inner
extractFunctionName (Fix (FunctionPrototype _ nameLex _)) = Just (lexemeText nameLex)
extractFunctionName (Fix (PreprocDefineConst nameLex _))  = Just (lexemeText nameLex)
extractFunctionName _                                     = Nothing
