{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Walk the parsed Cimple AST to extract doxygen comments attached to
-- C function declarations. The result is a map keyed by C function
-- name, holding the raw comment text. Downstream language emitters
-- apply their own transformations on top.
--
-- Cimple's TreeParser pass converts every raw @\/** ... *\/@ comment
-- that decorates a function into a structured 'CommentInfo' tree —
-- the 'CommentF' AST. To preserve the original prose we walk that
-- tree and re-flatten it back to text. The Cimple parser splits each
-- comment line into a list of word\/space\/punctuation lexemes (so
-- @"the address"@ becomes @[DocWord "the", DocWord " ", DocWord
-- "address"]@); we concatenate them verbatim.
module Apigen.Parser.Docs
    ( Docs (..)
    , emptyDocs
    , extractDocs
    ) where

import           Data.Fix                    (Fix (..), foldFix)
import           Data.Foldable               (toList)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Comment, CommentF (..),
                                              CommentStyle (..), Lexeme, Node,
                                              NodeF (..), lexemeText)
import qualified Apigen.Parser.SymbolNumbers as SymbolNumbers

-- | All doxygen comments harvested from a set of translation units.
--
-- * 'funcDocs' is keyed by C function name and holds the comment
--   attached to a top-level function declaration.
-- * 'enumMemberDocs' is keyed by @(enumTypedefName, memberName)@ and
--   holds the comment attached to an enumerator within a typedef'd
--   enum. The enum *name* is the typedef's lexeme (e.g.
--   @Tox_Err_Friend_Add@), not the @TOX_ERR_FRIEND_ADD_@ macro
--   prefix.
data Docs = Docs
    { funcDocs       :: Map Text Text
    , enumMemberDocs :: Map (Text, Text) Text
    } deriving (Show, Eq)

emptyDocs :: Docs
emptyDocs = Docs Map.empty Map.empty

unionDocs :: Docs -> Docs -> Docs
unionDocs a b = Docs
    { funcDocs       = funcDocs a       `Map.union` funcDocs b
    , enumMemberDocs = enumMemberDocs a `Map.union` enumMemberDocs b
    }

unionsDocs :: [Docs] -> Docs
unionsDocs = foldr unionDocs emptyDocs

-- | Pair every doxygen-style comment with the C declaration it
-- decorates. Functions are top-level; enum members are nested inside
-- an @EnumDecl@/@EnumConsts@ and need their parent enum's typedef
-- name passed in for the map key.
extractDocs :: [SymbolNumbers.TranslationUnit Text] -> Docs
extractDocs = unionsDocs . concatMap (map walkRoot . snd)

walkRoot :: Node (Lexeme Text) -> Docs
walkRoot (Fix n) = walkNode n

walkNode :: NodeF (Lexeme Text) (Node (Lexeme Text)) -> Docs
walkNode (Commented commentNode declNode) =
    let captured = case (extractCommentText commentNode, extractFunctionName declNode) of
            (Just cmt, Just fname) ->
                Docs (Map.singleton fname cmt) Map.empty
            _                      -> emptyDocs
    in captured `unionDocs` walkRoot declNode
walkNode (EnumDecl _tag members aliasLex) =
    -- @typedef enum Tag { … } Alias;@ in C. The third lexeme is the
    -- typedef alias — what downstream code keys by — so we use that
    -- as the enum name for each commented member.
    walkEnumMembers (lexemeText aliasLex) members
walkNode (EnumConsts (Just tagLex) members) =
    -- Plain @enum Tag { … };@ without a typedef — the form the open
    -- enumerations (@Tox_File_Kind@, @Toxav_Friend_Call_State@) use.
    -- Downstream keys by the tag name.
    walkEnumMembers (lexemeText tagLex) members
walkNode other =
    -- NodeF is Foldable, so 'toList' yields every child Node. This
    -- catch-all keeps the walker robust as new container shapes appear
    -- (Group, ExternC, Preproc*, …): we always descend, only the
    -- explicit @Commented@ case captures.
    unionsDocs (map walkRoot (toList other))

-- | Walk an enum's member list capturing the per-member doxygen
-- comments, keyed by @(enumName, memberName)@.
walkEnumMembers :: Text -> [Node (Lexeme Text)] -> Docs
walkEnumMembers enumName = unionsDocs . map go
  where
    go :: Node (Lexeme Text) -> Docs
    go (Fix (Commented c (Fix (Enumerator nameLex _)))) =
        case extractCommentText c of
            Just cmt ->
                Docs Map.empty (Map.singleton (enumName, lexemeText nameLex) cmt)
            Nothing  -> emptyDocs
    go _ = emptyDocs


-- | Two shapes of comment node reach us. After Cimple's tree-parsing
-- pass, function-level doxygen comments are replaced by a
-- 'CommentInfo' wrapping a structured 'CommentF' tree. Enum-member
-- comments live nested inside an @EnumDecl@ and the tree parser
-- doesn't recurse into them, so they keep the raw @Comment Doxygen@
-- shape. We handle both.
extractCommentText :: Node (Lexeme Text) -> Maybe Text
extractCommentText (Fix (CommentInfo c)) = Just (renderCommentInfo c)
extractCommentText (Fix (Comment Doxygen _open body _close)) =
    -- The lexer keeps per-line @ * @ markers out of @body@ (it
    -- consumes them in the @cmtNewlineSC@ start condition). Body
    -- lexemes are individual tokens — words, spaces, and newlines —
    -- so concatenating their text gives the original comment prose.
    Just . normalizeWhitespace $ Text.concat (map lexemeText body)
extractCommentText _ = Nothing

-- | Collapse runs of spaces to a single space and drop leading\/trailing
-- whitespace on every line. Cimple's tokenizer can emit a CmtSpace
-- token for any non-zero run of spaces (e.g. the indentation after
-- @ \\*@ on a continuation line), which appears as repeated spaces
-- when we concatenate body tokens verbatim.
normalizeWhitespace :: Text -> Text
normalizeWhitespace =
      Text.intercalate "\n"
    . map collapseSpaces
    . Text.splitOn "\n"
  where
    collapseSpaces = Text.unwords . Text.words

-- | Fold a structured doxygen comment back to plain text. Each
-- 'DocLine' is a list of word\/space lexemes; concatenating them
-- preserves the original spacing. Markup like @\@param@ is rendered
-- back as the original at-prefixed token so downstream emitters can
-- match on it.
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
        DocSecurityRank kw mp rank ->
            let mparam = maybe "" (\l -> ", " <> lexemeText l) mp
            in "@security_rank(" <> lexemeText kw <> mparam <> ", " <> lexemeText rank <> ")"
        DocCode _ code _    ->
            "```\n" <> Text.intercalate "\n" code <> "\n```"

-- | Drop the per-line @ * @ marker that Cimple keeps in each body
-- lexeme of a raw 'Comment'. Used only for raw-comment fallback;
-- @CommentInfo@ comments are already split into structured lines.
stripCommentMarkers :: Text -> Text
stripCommentMarkers = Text.intercalate "\n" . map stripLine . Text.splitOn "\n"
  where
    stripLine l =
        let stripped = Text.dropWhile (== ' ') l
        in case Text.stripPrefix "* " stripped of
            Just rest -> rest
            Nothing -> case Text.stripPrefix "*" stripped of
                Just rest -> rest
                Nothing   -> l

-- | Pull the declared name out of a top-level node. Handles
-- @FunctionDecl@/@FunctionDefn@/@TypedefFunction@ (functions),
-- @FunctionPrototype@ directly, and @PreprocDefineConst@ (named
-- @#define X val@ constants). Used to key 'funcDocs' so docs apply
-- to the right declaration regardless of which kind it is.
extractFunctionName :: Node (Lexeme Text) -> Maybe Text
extractFunctionName (Fix (FunctionDecl _ inner))          = extractFunctionName inner
extractFunctionName (Fix (FunctionDefn _ proto _))        = extractFunctionName proto
extractFunctionName (Fix (TypedefFunction inner))         = extractFunctionName inner
extractFunctionName (Fix (FunctionPrototype _ nameLex _)) = Just (lexemeText nameLex)
extractFunctionName (Fix (PreprocDefineConst nameLex _))  = Just (lexemeText nameLex)
extractFunctionName _                                     = Nothing
