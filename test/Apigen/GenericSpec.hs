{-# LANGUAGE OverloadedStrings #-}
-- | The language backends know nothing about Tox: what the C API doesn't
-- say through its patterns is knowledge before them. A word of a name in
-- Tox's headers that "Apigen.OtherLibrary" doesn't also use is Tox's
-- vocabulary, and a backend's string literal using it is a place to fix.
-- Each count here may only go down.
module Apigen.GenericSpec (spec) where

import qualified Apigen.OtherLibrary    as Other
import           Apigen.Parser          (parseModel)
import qualified Apigen.Parser.Semantic as Semantic
import           Apigen.Semantic
import           Control.Monad          (filterM, (<=<))
import           Data.Char              (isAlphaNum, isDigit, isLower, isUpper,
                                         toLower)
import           Data.List              (isPrefixOf, isSuffixOf, sort)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Language.Cimple.IO     (parseFile)
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        ((</>))
import           System.IO              (IOMode (ReadMode), hGetContents,
                                         hSetEncoding, openFile, utf8)
import           Test.Hspec

-- | Literal words of Tox's vocabulary, per backend file.
allowed :: [(FilePath, Int)]
allowed =
    [ ("Dart.hs", 3)
    ]

-- | Words Tox uses that aren't Tox's: the target languages' (@None@,
-- @#[derive(Debug)]@, @__exit__@, @src/@, C++'s @friend class@), English,
-- and our standards' that "Apigen.OtherLibrary" happens not to use (@_Id@,
-- the binding's @SecretBytes@).
languageWords :: Set String
languageWords = Set.fromList
    [ "abs", "accept", "action", "all", "by", "bytes", "call", "calling"
    , "control", "copy", "core", "count", "create", "debug", "delete"
    , "derive", "dispatch", "empty", "end", "exit", "failed", "file"
    , "format", "friend", "func", "hash", "id", "ignore", "in", "init"
    , "kind", "last", "leave", "len", "level", "load", "local", "lock", "many"
    , "message", "mod", "multiple", "no", "none", "own", "part", "pass"
    , "payload", "private", "proxy", "public", "read", "receive", "safety"
    , "secret", "send", "show", "source", "src", "start", "state", "sync"
    , "system", "thread", "typing", "unknown", "value", "with"
    ]

-- | From data = ["//c-toxcore:public_headers"].
toxHeaders :: [FilePath]
toxHeaders =
    [ "c-toxcore/tox/tox.h"
    , "c-toxcore/tox/tox_event.h"
    , "c-toxcore/tox/tox_events.h"
    , "c-toxcore/tox/tox_log_level.h"
    , "c-toxcore/tox/tox_options.h"
    , "c-toxcore/tox/toxav.h"
    , "c-toxcore/tox/toxencryptsave.h"
    ]

spec :: Spec
spec = describe "language backends" $
    it "use no more of Tox's vocabulary than they did" $ do
        -- From data = glob(["src/**/*.hs"]); cabal runs in the package.
        roots <- filterM doesDirectoryExist
            ["hs-apigen/src/Apigen/Language", "src/Apigen/Language"]
        missing <- filterM (fmap not . doesFileExist) toxHeaders
        case roots of
            root : _ | null missing -> do
                tus <- mapM (either fail return <=< parseFile) toxHeaders
                other <- Other.model
                let vocab = (vocabulary (Semantic.toSemantic False (parseModel tus))
                             `Set.difference` vocabulary other) `Set.difference` languageWords
                files <- sourcesUnder root
                counts <- mapM (\f -> (,) (drop (length root + 1) f) . uses vocab <$> readUtf8 f) files
                let found = [ (f, length ws) | (f, ws) <- counts, not (null ws) ]
                sort found `shouldBe` sort allowed
            _ -> pendingWith "no backend sources or Tox headers"

-- | The words of every name a model declares.
vocabulary :: SemanticModel -> Set String
vocabulary m = Set.fromList (concatMap identWords (strings (show declared)))
  where
    declared = m { diagnostics = [], knowledge = emptyKnowledge, targets = emptyTargets }
    strings s = case lex s of
        [("", _)]               -> []
        [(tok@('"' : _), rest)] -> read tok : strings rest
        [(_, rest)]             -> strings rest
        _                       -> []

-- | @Tox_Err_Friend_Add@, @toxEventsIterate@: lower-case words, less
-- numbers and single letters.
identWords :: String -> [String]
identWords = filter keep . map (map toLower) . concatMap camel . split
  where
    keep w = length w > 1 && not (all isDigit w)
    split s = case dropWhile (not . isAlphaNum) s of
        "" -> []
        s' -> let (w, rest) = span isAlphaNum s' in w : split rest
    camel s = case s of
        [] -> []
        c : _
            | isDigit c -> let (d, rest) = span isDigit s in d : camel rest
            | isUpper c -> case span isUpper s of
                (ups, rest@(l : _)) | isLower l ->
                    let (low, rest') = span isLower rest
                    in [ init ups | length ups > 1 ] ++ (last ups : low) : camel rest'
                (ups, rest) -> ups : camel rest
            | otherwise -> let (low, rest) = span isLower s in low : camel rest

-- | Each literal's vocabulary words, outside comments.
uses :: Set String -> String -> [(String, String)]
uses vocab src =
    [ (lit, w) | lit <- concatMap literals (lines src), w <- Set.toList (Set.fromList (identWords lit)), w `Set.member` vocab ]
  where
    literals l
        | "--" `isPrefixOf` dropWhile (== ' ') l = []
        | otherwise = go l
    go s = case break (== '"') s of
        (_, '"' : rest) -> let (lit, rest') = string rest in lit : go rest'
        _               -> []
    string ('\\' : c : rest) = let (l, r) = string rest in ('\\' : c : l, r)
    string ('"' : rest)      = ("", rest)
    string (c : rest)        = let (l, r) = string rest in (c : l, r)
    string []                = ("", "")

readUtf8 :: FilePath -> IO String
readUtf8 f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    hGetContents h

sourcesUnder :: FilePath -> IO [FilePath]
sourcesUnder dir = do
    entries <- map (dir </>) . sort <$> listDirectory dir
    concat <$> mapM (\e -> do
        isDir <- doesDirectoryExist e
        isFile <- doesFileExist e
        if isDir then sourcesUnder e
        else return [ e | isFile, ".hs" `isSuffixOf` e ]) entries
