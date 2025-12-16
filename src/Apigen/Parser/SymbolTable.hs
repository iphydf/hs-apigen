{-# LANGUAGE OverloadedStrings #-}
module Apigen.Parser.SymbolTable where

import           Apigen.Parser.AST          (Decl)
import           Control.Arrow              (Arrow (first))
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Bifunctor             (Bifunctor (bimap))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Tuple.Extra           (both)
import           Language.Cimple            (Lexeme)
import           Language.Cimple            (Lexeme)

type Name = ([Text], [Text])
type SId = Int
type Sym = Decl (Lexeme SId)
type NameToSId = HashMap Name SId
type SIdToName = HashMap SId Name

type M s a = State (SIdToName, s) a

displayWithin :: [Text] -> Name -> String
displayWithin curNs = addNamespace . both Text.unpack . bimap (Text.intercalate "_" . stripCurrent) (Text.intercalate "_")
  where
    addNamespace ([], []  ) = "this"
    addNamespace ([], name) = name
    addNamespace (ns, []  ) = ns <> "::this"
    addNamespace (ns, name) = ns <> "::" <> name

    stripCurrent ns = Maybe.fromMaybe ns (List.stripPrefix curNs ns)

display :: Name -> String
display = displayWithin []

mustLookup :: SIdToName -> SId -> Name
mustLookup syms sym =
    case HashMap.lookup sym syms of
        Nothing   -> error $ "symbol lookup failed: " <> show sym
        Just name -> name

mustLookupM :: SId -> M s Name
mustLookupM = (<$> (fst <$> State.get)) . flip mustLookup

resolve :: Traversable t => SIdToName -> t (Lexeme SId) -> t (Lexeme Name)
resolve st = (fmap . fmap) $ mustLookup st

resolveM :: Traversable t => t (Lexeme SId) -> State (SIdToName, s) (t (Lexeme Name))
resolveM = (<$> (fst <$> State.get)) . flip resolve

renameM :: (Name -> Name) -> SId -> M s ()
renameM f nm = do
    bs <- mustLookupM nm
    State.modify $ first $ HashMap.insert nm (f bs)

insert :: Name -> M s SId
insert name = do
    st <- fst <$> State.get
    let num = HashMap.size st
    State.modify $ first $ HashMap.insert num name
    return num
