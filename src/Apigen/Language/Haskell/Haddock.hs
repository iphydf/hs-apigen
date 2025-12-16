{-# LANGUAGE OverloadedStrings #-}
-- | C doxygen as Haddock: paragraphs of prose, bullet lists (@* @) or code
-- (@> @), as 'Apigen.Language.Haskell.Pretty' wraps them.
module Apigen.Language.Haskell.Haddock
    ( DocContext (..)
    , haddock
    ) where

import           Apigen.Language.DocText (isIdentChar, normalizeLines)
import           Data.Maybe              (mapMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as Text

data DocContext = DocContext
    { docParams :: [(Text, Text)]
      -- ^ The C parameters the Haskell function takes, and its names for them
    , docLink   :: Text -> Maybe Text
      -- ^ The Haskell name of a C function or constant, to link to
    }

data Block
    = Prose Text
    | Param Text Text
    | Code [Text]
    | Dropped

haddock :: DocContext -> Text -> Text
haddock ctx =
      Text.intercalate "\n\n"
    . filter (not . Text.null)
    . map (render ctx)
    . groupParams
    . concatMap blocks
    . paragraphs
    . Text.lines
    . normalizeLines

paragraphs :: [Text] -> [Either [Text] [Text]]
paragraphs [] = []
paragraphs ("```" : rest) =
    let (code, after) = break (== "```") rest
    in Right code : paragraphs (drop 1 after)
paragraphs (l : rest) | Text.null l = paragraphs rest
paragraphs ls =
    let (para, rest) = break (\l -> Text.null l || l == "```") ls
    in Left para : paragraphs rest

-- | A paragraph's blocks: each block tag starts one.
blocks :: Either [Text] [Text] -> [Block]
blocks (Right code) = [Code code]
blocks (Left ls) = map block (splitTags ls)
  where
    splitTags [] = []
    splitTags (l : rest) =
        let (cont, after) = break isTagLine rest
        in Text.unwords (l : cont) : splitTags after

    block t
        | Just rest <- Text.stripPrefix "@param " t =
            let (name, desc) = Text.break (== ' ') (stripAttr rest)
            in Param name (Text.strip desc)
        | any (`Text.isPrefixOf` t) ["@return", "@retval"] = Dropped
        -- Errors are 'Left's, not sentinels.
        | t `elem` staleSentences = Dropped
        -- Drop this (the Haskell function sizes its buffer itself).
        | "Call tox_" `Text.isPrefixOf` t && "_size to " `Text.isInfixOf` t = Dropped
        | Just rest <- Text.stripPrefix "@see " t = Prose ("See " <> sentence rest)
        | Just rest <- Text.stripPrefix "@note " t = Prose ("Note: " <> rest)
        | Just rest <- Text.stripPrefix "@deprecated " t = Prose ("Deprecated: " <> rest)
        | Just rest <- Text.stripPrefix "@attention " t = Prose ("Attention: " <> rest)
        | Just rest <- Text.stripPrefix "@brief " t = Prose rest
        | otherwise = Prose t

    sentence t = if "." `Text.isSuffixOf` t then t else t <> "."

    -- @\@param[in] name@ has a direction attribute.
    stripAttr t = case Text.stripPrefix "[" t of
        Just r  -> Text.strip (Text.drop 1 (Text.dropWhile (/= ']') r))
        Nothing -> t

staleSentences :: [Text]
staleSentences = ["This function will return UINT64_MAX on error."]

isTagLine :: Text -> Bool
isTagLine l = any (`Text.isPrefixOf` l)
    ["@param ", "@return", "@retval", "@see ", "@note ", "@deprecated ", "@attention ", "@brief "]

groupParams :: [Block] -> [[Block]]
groupParams [] = []
groupParams (b@Param{} : rest) =
    let (params, after) = span isParam rest
    in (b : params) : groupParams after
  where
    isParam Param{} = True
    isParam _       = False
groupParams (b : rest) = [b] : groupParams rest

render :: DocContext -> [Block] -> Text
render ctx bs = case bs of
    [Prose t] -> inline ctx t
    [Code ls] -> Text.intercalate "\n" (map ("> " <>) ls)
    [Dropped] -> ""
    params    -> Text.intercalate "\n" (mapMaybe param params)
  where
    param (Param name desc) = do
        hsName <- lookup name (docParams ctx)
        return ("* @" <> hsName <> "@: " <> inline ctx desc)
    param _ = Nothing

-- | Prose with identifiers linked and Haddock's markup characters escaped.
inline :: DocContext -> Text -> Text
inline ctx = go ' '
  where
    go prev t = case Text.uncons t of
        Nothing -> ""
        Just ('`', rest)
            | (code, after) <- Text.breakOn "`" rest, not (Text.null after) ->
                codeSpan code <> go '`' (Text.drop 1 after)
        Just ('@', rest)
            | Just r <- Text.stripPrefix "p " rest ->
                let (name, after) = Text.span isIdentChar r
                in "@" <> maybe name id (lookup name (docParams ctx)) <> "@" <> go 'x' after
        Just (c, _)
            | isIdentChar c && not (c >= '0' && c <= '9') ->
                let (ident, after) = Text.span isIdentChar t
                in case docLink ctx ident of
                    Just hs -> "'" <> hs <> "'" <> go '\'' (dropCall after)
                    Nothing -> ident <> go 'x' after
        Just (c, rest)
            | c `elem` ['-', '*'] && prev == ' ' && startsWord rest -> "\\" <> Text.singleton c <> go c rest
            | otherwise -> escape c <> go c rest

    startsWord r = Text.null r || Text.head r == ' '

    -- @tox_iterate()@ links to the function, without the call.
    dropCall after = maybe after id (Text.stripPrefix "()" after)

    codeSpan code = case docLink ctx code of
        Just hs -> "'" <> hs <> "'"
        Nothing -> "@" <> Text.concatMap escape code <> "@"

escape :: Char -> Text
escape c
    | c `elem` ['\\', '/', '@', '<', '#', '`'] = Text.pack ['\\', c]
    | otherwise                                = Text.singleton c
