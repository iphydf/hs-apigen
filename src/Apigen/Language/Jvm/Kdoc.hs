{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Text rewrites turning C doxygen into Kotlin kdoc.
module Apigen.Language.Jvm.Kdoc
    ( transformKdoc
    ) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.DocText         (normalizeLines,
                                                  rewriteIdentifiers)
import           Apigen.Language.Jvm.Conventions (isCryptoConstant,
                                                  isHiddenFromPublicApi)
import           Apigen.Language.Jvm.Model       (selfPrefix, standalonePrefix)
import           Apigen.Semantic                 (SemanticModel)
import qualified Data.List                       as List
import           Data.Maybe                      (mapMaybe)
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
        | Just c <- Text.stripPrefix (Text.toUpper (standalonePrefix model)) ident = Just ("[ToxCryptoConstants." <> c <> "]")
        | Just c <- Text.stripPrefix (Text.toUpper (K.rootLibrary model) <> "_") ident = Just $
            if isCryptoConstant model ident then "[ToxCryptoConstants." <> c <> "]"
            else "[ToxCoreConstants." <> c <> "]"
        | otherwise = Nothing
    isConstChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | @tox_foo_bar@ becomes the link @[fooBar]@, named as in
-- 'Apigen.Language.Jvm.Kotlin.Common.kotlinMethodName'.
rewriteFunctionRefs :: SemanticModel -> Text -> Text
rewriteFunctionRefs model = rewriteIdentifiers replaceFn
  where
    replaceFn ident
        -- A hidden method has nothing to link to. A size getter links to its data
        -- getter, so a dead link if that's hidden too.
        | isHiddenFromPublicApi model ident
        , not ("_size" `Text.isSuffixOf` ident)
            = Just ("`" <> ident <> "`")
        | rest : _ <- mapMaybe (`Text.stripPrefix` ident) prefixes
            = Just (link rest)
        | otherwise = Nothing
    -- As in 'kotlinMethodName'.
    prefixes = selfPrefix model : List.sortOn (negate . Text.length) [ w <> "_" | w <- K.libraryWords model ]
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
