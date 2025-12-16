{-# LANGUAGE OverloadedStrings #-}
-- | Haskell bindings for hs-toxcore-c: raw FFI modules under @FFI.Tox@, and
-- the safe API under @Network.Tox.C@.
module Apigen.Language.Haskell
    ( generate
    ) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Callbacks (callbackDecls)
import           Apigen.Language.Haskell.Events    (eventsModule)
import           Apigen.Language.Haskell.Haddock   (DocContext (..), haddock)
import           Apigen.Language.Haskell.Imports   (Entry (..), Scope,
                                                    baseScope, declNames,
                                                    importsFor, scopeOf)
import           Apigen.Language.Haskell.Model     (callbackType, handleName,
                                                    handleResources)
import           Apigen.Language.Haskell.Options   (optionsModule)
import           Apigen.Language.Haskell.Pretty    (renderModule)
import           Apigen.Language.Haskell.Raw       (rawFunctions, rawModules)
import           Apigen.Language.Haskell.Safe      (SafeFunction (..),
                                                    safeDecls, safeFunctions)
import           Apigen.Language.Haskell.Types     (constantNames,
                                                    constantsModule,
                                                    typesConstructors,
                                                    typesExports, typesModule)
import           Apigen.Parser.Docs                (Docs (..))
import           Apigen.Semantic
import           Data.List                         (find, nub)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (listToMaybe, mapMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model = [ (modulePath m, renderModule m) | m <- modules ]
  where
    functions = safeFunctions model

    -- What a doc in a module links to, qualified where it's in another.
    links here name = qualify <$> lookup name targets
      where
        qualify (m, n) = if m == here then n else m <> "." <> n
    targets =
        [ (sfCName f, (sfModule f, sfName f)) | f <- functions ]
        ++ [ (c, ("Network.Tox.C.Constants", n)) | (c, n) <- constantNames model ]
    docIn here fn params =
        haddock (DocContext params (links here)) <$> Map.lookup fn (funcDocs docs)
    memberDoc enum member =
        haddock (DocContext [] (links "Network.Tox.C.Types")) <$> Map.lookup (enum, member) (enumMemberDocs docs)
    optionDoc member =
        haddock (DocContext [] (links "Network.Tox.C.Options")) <$> (settingsName >>= \n -> Map.lookup (n, member) (structMemberDocs docs))
    settingsName = cName <$> listToMaybe (K.handlesOfKind model SettingsHandle)

    av = find (K.keepsHandler model) (resources model)
    avCallbacks = maybe Nothing (callbackDecls (docIn "Network.Tox.C.Av") model) av

    functionModules = nub (map sfModule functions)
    modules =
        rawModules model
        ++ [ constantsModule (\fn -> docIn "Network.Tox.C.Constants" fn []) model
           , typesModule (\fn -> docIn "Network.Tox.C.Types" fn [], memberDoc) model
           ]
        ++ [ moduleFrom model name (lookup name moduleDocs) (decls ++ extraDecls) (exports ++ extraExports)
           | name <- functionModules
           , let own = filter ((== name) . sfModule) functions
                 decls = concatMap (\f -> safeDecls (\fn -> docIn name fn (sfParams f)) f) own
                 exports = map (ExportVar . sfName) own
                 (extraDecls, extraExports) = case avCallbacks of
                     Just (ds, names) | name == "Network.Tox.C.Av" -> (ds, map ExportVar names)
                     _                                         -> ([], [])
           ]
        ++ [ moduleFrom model "Network.Tox.C.Options" (lookup "Network.Tox.C.Options" moduleDocs) ds es
           | Just (ds, es) <- [optionsModule optionDoc model] ]
        ++ [ moduleFrom model "Network.Tox.C.Events" (lookup "Network.Tox.C.Events" moduleDocs) ds es
           | Just (ds, es) <- [eventsModule (docIn "Network.Tox.C.Events") model] ]
        ++ [ topModule (functionModules ++ ["Network.Tox.C.Options", "Network.Tox.C.Events"]) ]

moduleFrom :: SemanticModel -> Text -> Maybe Text -> [Decl] -> [Export] -> Module
moduleFrom model name doc decls exports = Module
    { modName    = name
    , modDoc     = doc
    , modPragmas = []
    , modExports = Just exports
    , modImports = importsFor (safeScope model) (rawImports model decls) decls
    , modDecls   = decls
    }

-- | @Network.Tox.C@, re-exporting the safe API.
topModule :: [Text] -> Module
topModule safeModules = Module
    { modName    = "Network.Tox.C"
    , modDoc     = Just "The Tox API, safe to use from Haskell. Start with 'withTox'."
    , modPragmas = []
    , modExports = Just $
        map ExportModule reexported
        ++ [ExportVar "FixedBytes (fixedSize, toBytes)", ExportVar "fromBytes"]
    , modImports =
        [ Import m Nothing Nothing | m <- reexported ]
        ++ [Import "Network.Tox.C.Marshal" Nothing (Just ["FixedBytes (fixedSize, toBytes)", "fromBytes"])]
    , modDecls   = []
    }
  where
    reexported =
        ["Network.Tox.C.Constants", "Network.Tox.C.Types", "Network.Tox.C.Lifecycle"]
        ++ safeModules

safeScope :: SemanticModel -> Scope
safeScope model =
    baseScope
    <> Map.fromList (concatMap entry (typesExports model))
    <> Map.fromList [ (h, Entry types h (h <> " (..)")) | h <- handles ]
    <> Map.fromList [ (c, Entry types (t <> " (..)") (t <> " (..)")) | (c, t) <- typesConstructors model ]
    <> scopeOf "Network.Tox.C.Constants" (map snd (constantNames model))
    <> scopeOf "FFI.Tox.Types" [ handleName r <> "Ptr" | r <- resources model, resourceType r == ResHandle ]
  where
    entry e = case e of
        ExportAll n     -> [(n, Entry types n (n <> " (..)"))]
        ExportVar n     -> [(n, Entry types n n)]
        ExportPattern n -> [(n, Entry types ("pattern " <> n) ("pattern " <> n))]
        _               -> []
    types = "Network.Tox.C.Types"
    handles = [ handleName r | r <- resources model, resourceName r `elem` handleResources model ]

rawImports :: SemanticModel -> [Decl] -> [Text]
rawImports model decls =
    nub (mapMaybe (`lookup` index) (mapMaybe (Text.stripPrefix "Raw." . snd) (concatMap declNames decls)))
  where
    index = [ (hsName d, m) | (m, d) <- rawFunctions model ]
        ++ [ ("wrap" <> callbackType (cbName cb), "FFI.Tox.Types") | cb <- callbacks model ]
    hsName (Foreign _ n _) = n
    hsName _               = ""

moduleDocs :: [(Text, Text)]
moduleDocs =
    [ ("Network.Tox.C.Tox", "A Tox instance: its own address, name and status, its connections, and its save data.")
    , ("Network.Tox.C.Friend", "Friends: adding and removing them, what they say about themselves, and messages and packets to them.")
    , ("Network.Tox.C.File", "File transfers with friends.")
    , ("Network.Tox.C.Conference", "Conferences, the older group chats, and their peers.")
    , ("Network.Tox.C.Group", "Group chats and their peers.")
    , ("Network.Tox.C.Av", "Audio and video calls with friends.")
    , ("Network.Tox.C.EncryptSave", "Encrypting and decrypting data, such as save data, with a passphrase.")
    , ("Network.Tox.C.Options", "The options a Tox instance starts with.")
    , ("Network.Tox.C.Events", "What happened since the last iteration, as Tox records it.")
    ]

modulePath :: Module -> FilePath
modulePath m = Text.unpack (Text.replace "." "/" (modName m)) <> ".hs"
