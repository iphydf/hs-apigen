{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Text rewrites turning C doxygen into Kotlin kdoc.
module Apigen.Language.Jvm.Kdoc
    ( transformKdoc
    ) where

import           Apigen.Language.Jvm.Conventions (isHiddenFromPublicApi)
import           Apigen.Semantic                 (SemanticModel)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

transformKdoc :: SemanticModel -> Text -> Text
transformKdoc model =
      normalizeLines
    . camelCaseParamNames
    -- Before 'camelCaseParamNames', which renames @foo_length@.
    . dropLengthParam
    . reflowParagraphs
    . dropBriefMarker
    . dropStaleReturnSentinel
    . rewriteFunctionRefs model
    . rewriteConstants
    . rewriteCLanguageConstants

-- | Drop the C error sentinel: on the JVM, errors throw.
dropStaleReturnSentinel :: Text -> Text
dropStaleReturnSentinel =
      Text.replace " * This function will return UINT64_MAX on error.\n * \n" ""
    . Text.replace " * This function will return UINT64_MAX on error.\n"      ""

-- | @\@param foo_bar@ becomes @\@param fooBar@.
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
                in lead <> "@param " <> attr <> camelize pname <> body
            Nothing -> l
    camelize = camelFromSnake
    camelFromSnake t = case Text.splitOn "_" t of
        []       -> ""
        (h : tl) -> h <> Text.concat (map capFirst tl)
    capFirst s = case Text.uncons s of
        Nothing      -> ""
        Just (c, cs) -> Text.cons (toUpperAscii c) cs
    toUpperAscii c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise            = c

-- | Join each paragraph's lines, keeping each @\@tag@ on its own line.
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

-- | Trim each line's leading space (which Cimple keeps after @*@) and
-- the blank lines at either end.
normalizeLines :: Text -> Text
normalizeLines =
      Text.intercalate "\n"
    . dropWhileTrailing Text.null
    . dropWhile Text.null
    . map trimLeadingSpace
    . Text.splitOn "\n"
  where
    trimLeadingSpace = Text.dropWhile (== ' ')
    dropWhileTrailing p = reverse . dropWhile p . reverse

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

-- | @TOX_FOO_BAR@ becomes @[ToxCoreConstants.FOO_BAR]@.
rewriteConstants :: Text -> Text
rewriteConstants = rewriteIdentifiers replaceConst
  where
    replaceConst ident
        | "TOX_" `Text.isPrefixOf` ident && Text.all isConstChar ident
            = Just ("[ToxCoreConstants." <> Text.drop 4 ident <> "]")
        | otherwise = Nothing
    isConstChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | @tox_foo_bar@ becomes the link @[fooBar]@, named as in
-- 'Apigen.Language.Jvm.Kotlin.Common.kotlinMethodName'.
rewriteFunctionRefs :: SemanticModel -> Text -> Text
rewriteFunctionRefs model = rewriteIdentifiers replaceFn
  where
    replaceFn ident
        -- A hidden method has nothing to link to. A size getter links to
        -- its data getter, which is a dead link if that is hidden too.
        | isHiddenFromPublicApi model ident
        , not ("_size" `Text.isSuffixOf` ident)
            = Just ("`" <> ident <> "`")
        | "tox_self_" `Text.isPrefixOf` ident
            = Just (link (Text.drop 9 ident))
        | "toxav_"    `Text.isPrefixOf` ident
            = Just (link (Text.drop 6 ident))
        | "tox_"      `Text.isPrefixOf` ident
            = Just (link (Text.drop 4 ident))
        | otherwise = Nothing
    link rest =
        let camel = camelCaseFn rest
            -- As in 'kotlinPropertyName'.
            resolved = case Text.stripSuffix "Size" camel of
                Just base -> stripGetPrefix base
                Nothing   -> stripGetPrefix camel
        in "[" <> resolved <> "]"
    stripGetPrefix t = case Text.stripPrefix "get" t of
        Just (Text.uncons -> Just (c, cs))
            | c >= 'A' && c <= 'Z' ->
                Text.cons (toEnum (fromEnum c + 32)) cs
        _ -> t
    camelCaseFn t = case Text.splitOn "_" t of
        []       -> ""
        (h : tl) -> h <> Text.concat (map capFirst tl)
    capFirst s = case Text.uncons s of
        Nothing      -> ""
        Just (c, cs) -> Text.cons (toUpper c) cs
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise            = c

rewriteCLanguageConstants :: Text -> Text
rewriteCLanguageConstants =
      Text.replace "INT32_MAX"  "[Int.MAX_VALUE]"
    . Text.replace "UINT32_MAX" "[UInt.MAX_VALUE]"
    . Text.replace "INT64_MAX"  "[Long.MAX_VALUE]"
    . Text.replace "UINT64_MAX" "[ULong.MAX_VALUE]"
    . Text.replace " NULL"      " null"

-- | Replace the identifiers @f@ gives a replacement for.
rewriteIdentifiers :: (Text -> Maybe Text) -> Text -> Text
rewriteIdentifiers f = go
  where
    go t = case Text.break isIdentStart t of
        (lead, rest) | Text.null rest -> lead
                     | otherwise ->
                         let (ident, after) = Text.span isIdentChar rest
                             repl = case f ident of
                                 Just r  -> r
                                 Nothing -> ident
                         in lead <> repl <> go after
    isIdentStart c = isIdentChar c && not (c >= '0' && c <= '9')
    isIdentChar c =
        (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c == '_'
