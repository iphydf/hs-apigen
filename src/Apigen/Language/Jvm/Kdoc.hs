{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Mechanical C-doxygen → Kotlin-kdoc transforms. These are
-- rendering-time text rewrites — the comment structure isn't parsed,
-- and the substitutions are deterministic per-line / per-paragraph.
--
-- Lives separately from "Apigen.Language.Jvm.Conventions" because
-- nothing here is convention-as-policy — it's just text munging that
-- the renderer applies to every kdoc body. Conventions.hs holds
-- semantic policy (what's hidden, how wrappers derive); Kdoc.hs holds
-- the text-pipeline.
module Apigen.Language.Jvm.Kdoc
    ( transformKdoc
    ) where

import           Apigen.Language.Jvm.Conventions (isHiddenFromPublicApi)
import           Apigen.Semantic                 (SemanticModel)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

-- | Apply the mechanical C-doxygen → Kotlin-kdoc transforms. We do
-- *not* parse the comment structure — Cimple has a structured comment
-- AST, but for the first slice we treat the comment as raw text and
-- run a fixed set of substitutions. Hand-written kdoc in
-- jvm-toxcore-c does further rewriting (NULL handling, undefined
-- behaviour wording, etc.) that this pass deliberately preserves
-- as-is rather than approximate badly.
transformKdoc :: SemanticModel -> Text -> Text
transformKdoc model =
      normalizeLines
    . camelCaseParamNames
    -- Drops match @\@param length@ and @\@param foo_length@; run
    -- *before* camelCaseParamNames so the snake_case match still works.
    . dropLengthParam
    . reflowParagraphs
    . dropBriefMarker
    . dropStaleReturnSentinel
    . rewriteFunctionRefs model
    . rewriteConstants
    . rewriteCLanguageConstants

-- | Strip sentences like \"This function will return UINT64_MAX on
-- error.\" — they describe the C-API failure protocol (return a
-- sentinel + set the error param), but on the JVM surface errors
-- always throw the matching @Tox*Exception@, so the sentinel never
-- reaches the caller. Today only @tox_friend_get_last_online@ has
-- this phrasing; extend if more land in c-toxcore.
dropStaleReturnSentinel :: Text -> Text
dropStaleReturnSentinel =
      Text.replace " * This function will return UINT64_MAX on error.\n * \n" ""
    . Text.replace " * This function will return UINT64_MAX on error.\n"      ""

-- | @\@param foo_bar Description@ becomes @\@param fooBar Description@.
-- The C source uses snake_case for param names; the Kotlin signature
-- uses camelCase. Rewrite for consistency.
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
                -- Optional @[in]@ / @[out]@ attribute.
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

-- | Collapse soft line breaks within paragraphs. C source typically
-- wraps comments at ~80 chars by inserting @\\n@ mid-sentence; Kotlin
-- conventions wrap wider, so we join those into single lines and let
-- the consumer pick its own width. Paragraph breaks (blank lines)
-- get one @\\n\\n@; otherwise lines are joined.
--
-- Doxygen tags (@\@param@, @\@return@, …) start a new logical block;
-- their *continuation* lines (the indented text on the next source
-- line) collapse into the same block, but they don't merge with the
-- preceding or following @\@tag@ block.
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

-- | Trim each line's leading space (Cimple's word-tokenisation keeps
-- the space between @\ *@ and the first word) and drop blank lines at
-- the head and tail. Internal blank paragraphs are preserved.
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

-- | Drop @\@param length …@ and @\@param foo_length …@ lines. C exposes
-- the buffer length as a separate parameter; the JVM API uses
-- @ByteArray.size@ so the documentation is redundant noise.
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

-- | @\@brief X@ becomes a free-standing leading sentence by dropping
-- the marker. Kotlin/Dokka treats the first paragraph as the summary
-- automatically.
dropBriefMarker :: Text -> Text
dropBriefMarker = Text.replace "@brief " ""

-- | @TOX_FOO_BAR@ → @[ToxCoreConstants.FOO_BAR]@. The match is
-- conservative: an all-uppercase identifier starting with @TOX_@,
-- not followed by another identifier character. Single-bracket form
-- is Kotlin\/Dokka kdoc; the double-bracket Scaladoc syntax that
-- predated this generator did not resolve as a link.
rewriteConstants :: Text -> Text
rewriteConstants = rewriteIdentifiers replaceConst
  where
    replaceConst ident
        | "TOX_" `Text.isPrefixOf` ident && Text.all isConstChar ident
            = Just ("[ToxCoreConstants." <> Text.drop 4 ident <> "]")
        | otherwise = Nothing
    isConstChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | @tox_foo_bar@ / @toxav_foo_bar@ as a free-standing identifier
-- becomes a Kotlin link @[fooBar]@. Strips subsystem prefixes the
-- same way 'Apigen.Language.Jvm.Kotlin.Common.kotlinMethodName' does
-- (@tox_self_@ before @tox_@), so @tox_self_get_address@ links to
-- @[address]@ rather than @[selfGetAddress]@. Identifiers used as
-- type names (PascalCase @Tox_Foo@) are left alone.
rewriteFunctionRefs :: SemanticModel -> Text -> Text
rewriteFunctionRefs model = rewriteIdentifiers replaceFn
  where
    replaceFn ident
        -- Hidden methods (in 'isHiddenFromPublicApi') have no Kotlin
        -- counterpart, so a Dokka @[ref]@ link would render as plain
        -- text in the docs and confuse readers ("what is this
        -- thing?"). Fall back to inline code formatting for the
        -- C name instead, which at least signals "this is a c-toxcore
        -- name with no JVM equivalent".
        --
        -- Size-getters (@tox_X_get_Y_size@) are deliberately skipped
        -- by this filter so the @link@ branch below rewrites them to
        -- the data-accessor name (@[Y]@). This is only *safely*
        -- correct when the resulting accessor is itself exposed —
        -- e.g. @tox_conference_get_title_size@ → @[conferenceGetTitle]@
        -- which is a real Kotlin method. When the data accessor is
        -- ALSO hidden (e.g. @tox_friend_get_name_size@ →
        -- @[friendGetName]@, both hidden) the link is dead.
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
            -- @get_address@ becomes @address@ to match the property
            -- naming, mirroring 'kotlinPropertyName'. Size getters
            -- (@get_name_size@, etc.) re-link to the companion data
            -- accessor so the reference still resolves.
            resolved = case Text.stripSuffix "Size" camel of
                Just base -> stripGetPrefix base
                Nothing -> stripGetPrefix camel
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

-- | A small set of C-language constants get Kotlin equivalents.
-- Anything not in the table passes through unchanged.
rewriteCLanguageConstants :: Text -> Text
rewriteCLanguageConstants =
      Text.replace "INT32_MAX"  "[Int.MAX_VALUE]"
    . Text.replace "UINT32_MAX" "[UInt.MAX_VALUE]"
    . Text.replace "INT64_MAX"  "[Long.MAX_VALUE]"
    . Text.replace "UINT64_MAX" "[ULong.MAX_VALUE]"
    . Text.replace " NULL"      " null"

-- | Walk the text token by token (identifier-or-other), applying @f@
-- to identifier tokens. Non-identifier characters and identifiers
-- where @f@ returns @Nothing@ pass through unchanged.
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
