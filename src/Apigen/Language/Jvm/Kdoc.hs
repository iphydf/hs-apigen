{-# LANGUAGE OverloadedStrings #-}

-- | Text rewrites turning C doxygen into Kotlin kdoc.
module Apigen.Language.Jvm.Kdoc
    ( transformKdoc
    , renderMemberKdoc
    , kdocBlock
    ) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.DocText           (normalizeLines,
                                                    rewriteIdentifiers)
import           Apigen.Language.Jvm.Conventions   (isCryptoConstant,
                                                    isHiddenFromPublicApi)
import           Apigen.Language.Jvm.Kotlin.Common (kotlinMemberName)
import           Apigen.Language.Jvm.Model         (Subsystem (..),
                                                    rootSubsystem, selfPrefix,
                                                    standalonePrefix,
                                                    subsystems)
import           Apigen.Language.Jvm.Names         (camelCase)
import           Apigen.Semantic                   (SResourceKind (..),
                                                    SemanticModel)
import           Data.Char                         (isAlphaNum)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

transformKdoc :: SemanticModel -> Text -> Text
transformKdoc model =
      normalizeLines
    . camelCaseParamNames
    -- Before 'camelCaseParamNames', which renames @foo_length@.
    . dropLengthParam
    . reflowParagraphs
    . dropBriefMarker
    . dropStaleReturnSentinel
    . unquoteLinks
    . rewriteFunctionRefs model
    . rewriteConstants model
    . rewriteCLanguageConstants

-- | Drop the C error sentinel: on the JVM, errors throw.
dropStaleReturnSentinel :: Text -> Text
dropStaleReturnSentinel =
      Text.replace " * This function will return UINT64_MAX on error.\n * \n" ""
    . Text.replace " * This function will return UINT64_MAX on error.\n"      ""

camelCaseParamNames :: Text -> Text
camelCaseParamNames =
      Text.intercalate "\n"
    . map rewriteLine
    . Text.splitOn "\n"
  where
    rewriteLine l =
        let (lead, rest) = Text.break (/= ' ') l
        in case Text.stripPrefix "@param " rest of
            Just rest' ->
                let (attr, afterAttr) = case Text.stripPrefix "[" rest' of
                        Just t ->
                            let (a, b) = Text.break (== ']') t
                            in case Text.stripPrefix "] " b of
                                Just b' -> ("[" <> a <> "] ", b')
                                Nothing -> ("", rest')
                        Nothing -> ("", rest')
                    (pname, body) = Text.break (== ' ') afterAttr
                in lead <> "@param " <> attr <> camelCase pname <> body
            Nothing -> l

reflowParagraphs :: Text -> Text
reflowParagraphs = Text.intercalate "\n\n" . map joinSoftBreaks . splitParagraphs
  where
    splitParagraphs t =
        filter (not . Text.null) (Text.splitOn "\n\n" t)

    joinSoftBreaks p =
        let ls = Text.splitOn "\n" p
            blocks = groupBlocks ls
        in Text.intercalate "\n" (map collapseBlock blocks)

    groupBlocks :: [Text] -> [[Text]]
    groupBlocks []     = []
    groupBlocks (l:ls) =
        let (cont, rest) = break startsWithTag ls
        in (l : cont) : groupBlocks rest

    collapseBlock = Text.unwords . filter (not . Text.null) . map Text.strip

    startsWithTag l = case Text.uncons (Text.dropWhile (== ' ') l) of
        Just ('@', _) -> True
        _             -> False

-- | Drop the docs of length params, which the JVM API doesn't have.
dropLengthParam :: Text -> Text
dropLengthParam =
      Text.intercalate "\n"
    . filter (not . isLengthParamLine)
    . Text.splitOn "\n"
  where
    isLengthParamLine l = case Text.stripPrefix "@param " (Text.dropWhile (== ' ') l) of
        Nothing   -> False
        Just rest ->
            let (pname, _) = Text.break (== ' ') rest
            in pname == "length" || "_length" `Text.isSuffixOf` pname

-- | Dokka takes the first paragraph as the summary.
dropBriefMarker :: Text -> Text
dropBriefMarker = Text.replace "@brief " ""

-- | @TOX_FOO_BAR@ becomes @[ToxCoreConstants.FOO_BAR]@, or
-- @[ToxCryptoConstants...]@ if 'isCryptoConstant'.
rewriteConstants :: SemanticModel -> Text -> Text
rewriteConstants model = rewriteIdentifiers replaceConst
  where
    replaceConst ident
        | not (Text.all isConstChar ident) = Nothing
        | Just c <- Text.stripPrefix (Text.toUpper (standalonePrefix model)) ident = Just (link standalone c)
        | Just c <- Text.stripPrefix (Text.toUpper (K.rootLibrary model) <> "_") ident = Just $
            if isCryptoConstant model ident then link standalone c else link root c
        | otherwise = Nothing
    link s c = "[" <> subClass s <> "Constants." <> c <> "]"
    root = rootSubsystem model
    standalone = case [ s | s <- subsystems model, subKind s == StandaloneHandle ] of
        s : _ -> s
        []    -> root
    isConstChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | @tox_foo_bar@ becomes the link @[fooBar]@.
rewriteFunctionRefs :: SemanticModel -> Text -> Text
rewriteFunctionRefs model = rewriteIdentifiers replaceFn
  where
    replaceFn ident
        -- A hidden method has nothing to link to. A size getter links to its data
        -- getter, so a dead link if that's hidden too.
        | isHiddenFromPublicApi model ident
        , not ("_size" `Text.isSuffixOf` ident)
            = Just ("`" <> ident <> "`")
        | any (`Text.isPrefixOf` ident) prefixes = Just ("[" <> kotlinMemberName model (dataGetter ident) <> "]")
        | otherwise = Nothing
    prefixes = selfPrefix model : [ w <> "_" | w <- K.libraryWords model ]
    -- A size getter's link is its data's: the JVM API has no size getters.
    dataGetter ident = fromMaybe ident (Text.stripSuffix "_size" ident)

-- | A link in a code span is shown as code, brackets and all: @`[fooBar]`@
-- becomes @[fooBar]@.
unquoteLinks :: Text -> Text
unquoteLinks t = case Text.breakOn "`[" t of
    (before, rest)
        | Text.null rest -> t
        | (target, after) <- Text.breakOn "]`" (Text.drop 2 rest)
        , not (Text.null after)
        , Text.all isLinkChar target ->
            before <> "[" <> target <> "]" <> unquoteLinks (Text.drop 2 after)
        | otherwise -> before <> "`" <> unquoteLinks (Text.drop 1 rest)
  where
    isLinkChar c = isAlphaNum c || c == '.' || c == '_'

rewriteCLanguageConstants :: Text -> Text
rewriteCLanguageConstants =
      Text.replace "INT32_MAX"  "[Int.MAX_VALUE]"
    . Text.replace "UINT32_MAX" "[UInt.MAX_VALUE]"
    . Text.replace "INT64_MAX"  "[Long.MAX_VALUE]"
    . Text.replace "UINT64_MAX" "[ULong.MAX_VALUE]"
    . Text.replace " NULL"      " null"

-- | One line if it fits there.
renderMemberKdoc :: Int -> Text -> [Text]
renderMemberKdoc col body = case Text.lines (Text.strip body) of
    []     -> []
    [line] -> [Text.replicate col " " <> "/** " <> line <> " */"]
    ls     -> kdocBlock col ls

kdocBlock :: Int -> [Text] -> [Text]
kdocBlock col ls =
    [indent <> "/**"]
        ++ [ if Text.null l then indent <> " *" else indent <> " * " <> l | l <- ls ]
        ++ [indent <> " */"]
  where
    indent = Text.replicate col " "
