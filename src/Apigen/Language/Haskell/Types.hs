{-# LANGUAGE OverloadedStrings #-}
-- | @Network.Tox.C.Types@ and @Network.Tox.C.Constants@: the types and
-- constants the safe API shares.
module Apigen.Language.Haskell.Types
    ( typesModule
    , constantsModule
    , typesExports
    , typesConstructors
    , constantNames
    ) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Imports (baseScope, importsFor,
                                                  scopeOf)
import           Apigen.Language.Haskell.Model
import           Apigen.Language.Haskell.Names   (typeName, valueName)
import           Apigen.Semantic
import           Data.List                       (partition)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

-- | Doc comments by C name, and enum member docs by enum and member.
type Docs' = (Text -> Maybe Text, Text -> Text -> Maybe Text)

constantNames :: SemanticModel -> [(Text, Text)]
constantNames model = [ (constantName c, valueName (constantName c)) | c <- constants model ]

constantsModule :: (Text -> Maybe Text) -> SemanticModel -> Module
constantsModule doc model = Module
    { modName    = "Network.Tox.C.Constants"
    , modDoc     = Just "The C API's numeric constants."
    , modPragmas = []
    , modExports = Just [ ExportVar n | (_, n) <- constantNames model ]
    , modImports = []
    , modDecls   = concat
        [ [ Signature (doc (constantName c)) n (TCon "Int")
          , Binding n [] (Lit (fromIntegral (constantValue c)))
          ]
        | c <- constants model, let n = valueName (constantName c) ]
    }

typesConstructors :: SemanticModel -> [(Text, Text)]
typesConstructors model =
    [ (conName c, n) | DataDecl _ n cons _ <- typesDecls (const Nothing, \_ _ -> Nothing) model, c <- cons ]

typesExports :: SemanticModel -> [Export]
typesExports = concatMap snd . exportGroups

exportGroups :: SemanticModel -> [(Text, [Export])]
exportGroups model =
    [ ("Enumerations", [ ExportAll (typeName (enumSemanticName e)) | e <- closed ])
    , ("Open enumerations", [ ExportPattern (typeName (enumSemanticName e) <> typeName (enumMemberName m))
                            | e <- open, m <- enumMembers e ])
    , ("Numbers", [ ExportAll (typeName (idName i)) | i <- idTypes model ])
    , ("Fixed-size byte strings", [ ExportVar (typeName name) | (_, name) <- arrayTypes model ])
    , ("Handles", [ ExportAll (handleName r) | r <- safeHandles model ])
    ]
  where
    (open, closed) = partition (isOpenEnum model) (enums model)

typesModule :: Docs' -> SemanticModel -> Module
typesModule docs model = Module
    { modName    = "Network.Tox.C.Types"
    , modDoc     = Just "The types the safe API shares: enums, numbers naming friends, groups and the like, fixed-size byte strings, and handles."
    , modPragmas = ["PatternSynonyms"]
    , modExports = Just (concat [ ExportSection title : items | (title, items) <- exportGroups model, not (null items) ])
    , modImports = importsFor scope [] decls
    , modDecls   = decls
    }
  where
    decls = typesDecls docs model
    scope = baseScope
        <> scopeOf "FFI.Tox.Types" [ handleName r <> s | r <- resources model, s <- ["Ptr", "Struct"] ]
        <> scopeOf "Network.Tox.C.Constants" (map snd (constantNames model))

safeHandles :: SemanticModel -> [SResource]
safeHandles model = [ r | r <- resources model, resourceName r `elem` handleResources model ]

typesDecls :: Docs' -> SemanticModel -> [Decl]
typesDecls (doc, memberDoc) model =
    concatMap enum (enums model)
    ++ concatMap idNewtypeDecl (idTypes model)
    ++ concatMap fixedBytes (arrayTypes model)
    ++ map handle (safeHandles model)
  where
    enum e
        | isOpenEnum model e =
            [ PatternSynonym (memberDoc (enumName e) (enumMemberCName mem))
                (constructor e mem) (TCon "Word32") (Lit (fromIntegral (enumMemberValue mem)))
            | mem <- enumMembers e ]
        | otherwise =
            let members = filter (not . isOk) (enumMembers e)
                name = typeName (enumSemanticName e)
            in [ DataDecl Nothing name
                    [ Con (constructor e mem) (memberDoc (enumName e) (enumMemberCName mem)) []
                    | mem <- members ]
                    ["Eq", "Ord", "Enum", "Bounded", "Read", "Show"]
               , Instance "CEnum" name $
                    [ Binding "toCEnum" [PCon (constructor e mem) []] (Lit (fromIntegral (enumMemberValue mem)))
                    | mem <- members ]
                    ++ [ Binding "fromCEnum" [PLit (fromIntegral (enumMemberValue mem))] (Var (constructor e mem))
                       | mem <- members ]
                    ++ [ Binding "fromCEnum" [PVar "n"]
                            (App (Var "unknownCEnum") [Str (enumName e), Var "n"]) ]
               ]
      where
        isOk mem = isErrorEnum e && enumMemberValue mem == 0

    constructor e mem = typeName (enumSemanticName e) <> typeName (enumMemberName mem)

    idNewtypeDecl i =
        [ NewtypeDecl Nothing (typeName (idName i)) (underlying (idType i)) ["Eq", "Ord", "Read", "Show"] ]
    underlying t = case t of
        SUInt n -> TCon ("Word" <> Text.pack (show n))
        SInt n  -> TCon ("Int" <> Text.pack (show n))
        _       -> TCon "Word32"

    fixedBytes (size, name) =
        let ty = typeName name
            sizeName = valueName size
        in [ NewtypeDecl (Just ("'" <> sizeName <> "' bytes; 'Network.Tox.C.Marshal.fromBytes' makes one."))
                ty (TCon "ByteString") ["Eq", "Ord", "Read", "Show"]
           , Instance "FixedBytes" ty
                [ Binding "fixedSize" [PWild] (Var sizeName)
                , Binding "toBytes" [PCon ty [PVar "bytes"]] (Var "bytes")
                , Binding "unsafeFromBytes" [] (Var ty)
                ]
           ]

    -- A handle keeping a handler holds its callbacks, and a standalone one
    -- is freed by a finalizer.
    handle r
        | K.keepsHandler model r = DataDecl (constructorDoc r) (handleName r)
            [Con (handleName r) Nothing [TCon (handleName r <> "Ptr"), TCon "CallbackRegistry"]] []
        | resourceName r `elem` foreignPtrHandles model =
            NewtypeDecl Nothing (handleName r)
                (TApp (TCon "ForeignPtr") [TCon (handleName r <> "Struct")]) []
        | otherwise = NewtypeDecl Nothing (handleName r) (TCon (handleName r <> "Ptr")) []
    constructorDoc r = case [ m | m <- methods r, methodRole m == Constructor ] of
        m : _ -> doc (methodName m)
        []    -> Nothing

