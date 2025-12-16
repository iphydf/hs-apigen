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
                                                    handleResources, rawModule,
                                                    safeModule)
import           Apigen.Language.Haskell.Options   (optionsModule)
import           Apigen.Language.Haskell.Pretty    (renderModule)
import           Apigen.Language.Haskell.Raw       (rawFunctions, rawModules)
import           Apigen.Language.Haskell.Safe      (SafeFunction (..),
                                                    safeDecls, safeFunctions,
                                                    safeModuleOf)
import           Apigen.Language.Haskell.Types     (constantNames,
                                                    constantsModule,
                                                    typesConstructors,
                                                    typesExports, typesModule)
import           Apigen.Parser.Docs                (Docs (..))
import           Apigen.Semantic
import           Data.List                         (find, nub)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (mapMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model = [ (modulePath m, renderModule m) | m <- modules ]
  where
    functions = safeFunctions model

    -- What a doc in a module links to, qualified where it's in another.
    links here name = qualify <$> lookup name linkTargets
      where
        qualify (m, n) = if m == here then n else m <> "." <> n
    linkTargets =
        [ (sfCName f, (sfModule f, sfName f)) | f <- functions ]
        ++ [ (c, ((safeModule model "Constants"), n)) | (c, n) <- constantNames model ]
    docIn here fn params =
        haddock (DocContext params (links here)) <$> Map.lookup fn (funcDocs docs)
    memberDoc enum member =
        haddock (DocContext [] (links (safeModule model "Types"))) <$> Map.lookup (enum, member) (enumMemberDocs docs)
    optionDoc member =
        haddock (DocContext [] (links (safeModule model "Options"))) <$> (settingsName >>= \n -> Map.lookup (n, member) (structMemberDocs docs))
    settingsName = cName <$> K.rootSettings model

    av = find (K.keepsHandler model) (resources model)
    avCallbacks = (\r -> callbackDecls (docIn (safeModuleOf model r)) model r) =<< av

    functionModules = nub (map sfModule functions)
    modules =
        rawModules model
        ++ [ constantsModule (\fn -> docIn (safeModule model "Constants") fn []) model
           , typesModule (\fn -> docIn (safeModule model "Types") fn [], memberDoc) model
           ]
        ++ [ moduleFrom model name (moduleDoc model name) (decls ++ extraDecls) (exports ++ extraExports)
           | name <- functionModules
           , let own = filter ((== name) . sfModule) functions
                 decls = concatMap (\f -> safeDecls (\fn -> docIn name fn (sfParams f)) f) own
                 exports = map (ExportVar . sfName) own
                 (extraDecls, extraExports) = case avCallbacks of
                     Just (ds, names) | Just name == fmap (safeModuleOf model) av -> (ds, map ExportVar names)
                     _ -> ([], [])
           ]
        ++ [ moduleFrom model (safeModule model "Options") (moduleDoc model "Options") ds es
           | Just (ds, es) <- [optionsModule optionDoc model] ]
        ++ [ moduleFrom model (safeModule model "Events") (moduleDoc model "Events") ds es
           | Just (ds, es) <- [eventsModule (docIn (safeModule model "Events")) model] ]
        ++ [ topModule model (functionModules ++ [safeModule model "Options", safeModule model "Events"]) ]

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
topModule :: SemanticModel -> [Text] -> Module
topModule model safeModules = Module
    { modName    = safeModule model ""
    , modDoc     = Just ("The " <> root <> " API, safe to use from Haskell. Start with 'with" <> root <> "'.")
    , modPragmas = []
    , modExports = Just $
        map ExportModule reexported
        ++ [ExportVar "FixedBytes (fixedSize, toBytes)", ExportVar "fromBytes"]
    , modImports =
        [ Import m Nothing Nothing | m <- reexported ]
        ++ [Import (safeModule model "Marshal") Nothing (Just ["FixedBytes (fixedSize, toBytes)", "fromBytes"])]
    , modDecls   = []
    }
  where
    root = maybe "" handleName (find isRoot (resources model))
    reexported =
        [(safeModule model "Constants"), (safeModule model "Types"), (safeModule model "Lifecycle")]
        ++ safeModules

safeScope :: SemanticModel -> Scope
safeScope model =
    baseScope (safeModule model "Marshal")
    <> Map.fromList (concatMap entry (typesExports model))
    <> Map.fromList [ (h, Entry types h (h <> " (..)")) | h <- handles ]
    <> Map.fromList [ (c, Entry types (t <> " (..)") (t <> " (..)")) | (c, t) <- typesConstructors model ]
    <> scopeOf (safeModule model "Constants") (map snd (constantNames model))
    <> scopeOf (rawModule model "Types") [ handleName r <> "Ptr" | r <- resources model, resourceType r == ResHandle ]
  where
    entry e = case e of
        ExportAll n     -> [(n, Entry types n (n <> " (..)"))]
        ExportVar n     -> [(n, Entry types n n)]
        ExportPattern n -> [(n, Entry types ("pattern " <> n) ("pattern " <> n))]
        _               -> []
    types = (safeModule model "Types")
    handles = [ handleName r | r <- resources model, resourceName r `elem` handleResources model ]

rawImports :: SemanticModel -> [Decl] -> [Text]
rawImports model decls =
    nub (mapMaybe (`lookup` index) (mapMaybe (Text.stripPrefix "Raw." . snd) (concatMap declNames decls)))
  where
    index = [ (hsName d, m) | (m, d) <- rawFunctions model ]
        ++ [ ("wrap" <> callbackType (cbName cb), (rawModule model "Types")) | cb <- callbacks model ]
    hsName (Foreign _ n _) = n
    hsName _               = ""

-- | What a safe module is for.
moduleDoc :: SemanticModel -> Text -> Maybe Text
moduleDoc model name = lookup (Text.takeWhileEnd (/= '.') name) (haskellModuleDocs (targets model))

modulePath :: Module -> FilePath
modulePath m = Text.unpack (Text.replace "." "/" (modName m)) <> ".hs"
