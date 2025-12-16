{-# LANGUAGE OverloadedStrings #-}
-- | The imports a module's declarations need, from the names they use.
module Apigen.Language.Haskell.Imports
    ( Scope
    , Entry (..)
    , baseScope
    , scopeOf
    , importsFor
    , declNames
    ) where

import           Apigen.Language.Haskell.AST
import           Data.List                   (nub)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

-- | Where a name comes from, and how to import it as a type or as a value
-- (a constructor, which needs @(..)@).
data Entry = Entry
    { entryModule :: Text
    , asType      :: Text
    , asValue     :: Text
    }

type Scope = Map Text Entry

plainIn :: Text -> Text -> (Text, Entry)
plainIn m n = (n, Entry m n n)

-- | A type whose constructors FFI or the code needs.
withConstructors :: Text -> Text -> (Text, Entry)
withConstructors m n = (n, Entry m (n <> " (..)") (n <> " (..)"))

baseScope :: Scope
baseScope = Map.fromList $
    map (plainIn "Data.Int") ["Int8", "Int16", "Int32", "Int64"]
    ++ map (plainIn "Data.Word") ["Word8", "Word16", "Word32", "Word64"]
    -- FFI marshals these newtypes through their constructors.
    ++ map (withConstructors "Foreign.C.Types") ["CBool", "CInt", "CSize"]
    ++ map (plainIn "Foreign.C.String") ["CString", "withCString", "peekCString"]
    ++ map (plainIn "Foreign.Ptr") ["Ptr", "FunPtr", "nullPtr", "freeHaskellFunPtr"]
    ++ map (plainIn "Foreign.ForeignPtr") ["ForeignPtr", "newForeignPtr", "withForeignPtr"]
    ++ map (plainIn "Foreign.Marshal.Utils") ["fromBool", "toBool"]
    ++ map (plainIn "Control.Exception") ["bracket", "finally"]
    ++ map (plainIn "Control.Monad") ["void"]
    ++ [ plainIn "Data.ByteString" "ByteString"
       , plainIn "Data.Vector.Storable" "Vector"
       , withConstructors "Foreign.C.Enum" "CEnum"
       , withConstructors "Network.Tox.C.Marshal" "FixedBytes"
       ]
    ++ map (plainIn "Foreign.C.Enum")
        ["CErr", "callErrFun", "callErrFun_", "fromCEnum", "toCEnum", "unknownCEnum"]
    ++ map (plainIn "Network.Tox.C.Marshal")
        [ "withFixedBytes", "withMaybeFixedBytes", "getFixedBytes", "getFixedBytesErr", "getFixedBytesMaybe"
        , "peekFixedBytes", "withBytesLen", "withSizedBytes", "withSizedVector"
        , "getSized", "getSizedErr", "getBytesErr", "peekBytes", "peekSizedBytes"
        , "peekSizedString", "peekNullableString", "peekSizedVector", "getList"
        , "iterateList", "withNew"
        , "CallbackRegistry", "registerCallback"
        ]

scopeOf :: Text -> [Text] -> Scope
scopeOf m names = Map.fromList (map (plainIn m) names)

importsFor :: Scope -> [Text] -> [Decl] -> [Import]
importsFor scope rawModules decls =
    [ Import m Nothing (Just (dropCovered (nub items))) | (m, items) <- Map.toList byModule ]
    ++ [ Import "Data.ByteString" (Just "BS") Nothing | any (Text.isPrefixOf "BS." . snd) uses ]
    ++ [ Import m (Just "Raw") Nothing | m <- nub rawModules ]
  where
    uses = nub (concatMap declNames decls)
    byModule = Map.fromListWith (flip (++))
        [ (entryModule e, [item])
        | (isValue, n) <- uses
        , Just e <- [Map.lookup n scope]
        , let item = if isValue then asValue e else asType e ]
    -- @T@ goes when @T (..)@ is imported too.
    dropCovered items = filter (\i -> (i <> " (..)") `notElem` items) items

declNames :: Decl -> [(Bool, Text)]
declNames d = case d of
    DataDecl _ _ cons _       -> concatMap (concatMap types . conArgs) cons
    RecordDecl _ _ fields _   -> concatMap (types . fieldType) fields
    NewtypeDecl _ _ ty _      -> types ty
    TypeAlias _ _ ty          -> types ty
    Foreign _ _ ty            -> types ty
    Signature _ _ ty          -> types ty
    Binding _ pats body       -> concatMap pats' pats ++ values body
    BindingWhere _ pats body locals ->
        concatMap pats' pats ++ values body ++ concatMap declNames locals
    Instance cls ty methods   -> (False, cls) : (False, ty) : concatMap declNames methods
    PatternSynonym _ _ ty val -> types ty ++ values val
  where
    types = map ((,) False) . typeNames
    pats' = map ((,) True) . patNames
    values = map ((,) True) . exprNames

typeNames :: Type -> [Text]
typeNames t = case t of
    TCon n      -> [n]
    TApp f args -> typeNames f ++ concatMap typeNames args
    TFun args r -> concatMap typeNames args ++ typeNames r
    TTuple ts   -> concatMap typeNames ts
    TUnit       -> []

patNames :: Pat -> [Text]
patNames p = case p of
    PCon c ps -> c : concatMap patNames ps
    _         -> []

exprNames :: Expr -> [Text]
exprNames e = case e of
    Var v       -> [v]
    Lit _       -> []
    Str _       -> []
    App f args  -> exprNames f ++ concatMap exprNames args
    Op l op r   -> exprNames l ++ [op] ++ exprNames r
    Lam pats b  -> concatMap patNames pats ++ exprNames b
    Do stmts    -> concatMap stmtNames stmts
    Case s alts -> exprNames s ++ concatMap (\(p, b) -> patNames p ++ exprNames b) alts
    With ws b   -> concatMap (\(w, ps) -> exprNames w ++ concatMap patNames ps) ws ++ exprNames b
  where
    stmtNames (Bind p x) = patNames p ++ exprNames x
    stmtNames (Run x)    = exprNames x
