{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Rust (generate, byteArrayConstants, Options (..)) where

import           Apigen.Inference            (deriveHandleName, getHierarchy,
                                              inferCFunctionMapping)
import qualified Apigen.Inference            as I
import qualified Apigen.Knowledge            as K
import           Apigen.Language.Rust.AST
import           Apigen.Language.Rust.Crate  (HandleGlue (..))
import qualified Apigen.Language.Rust.Crate  as Crate
import           Apigen.Language.Rust.Naming (errorVariant, idToRustPascal,
                                              idToRustSnake, paramSnake,
                                              safeName)
import           Apigen.Language.Rust.Pretty (prettyType, render)
import           Apigen.Model                (eventFieldType, findEnum)
import           Apigen.Namespace            (namespaces, rootNamespace,
                                              stripNamespace)
import           Apigen.Semantic
import qualified Apigen.Semantic             as S
import           Apigen.SizeExpr             (SizeExpr (..), parseSizeExpr,
                                              sizeExprVars)
import           Apigen.Types                (Constness (..))
import           Control.Applicative         ((<|>))
import           Data.Char                   (isAlphaNum, isDigit, isLower,
                                              isPunctuation, isUpper, toUpper)
import           Data.List                   (find, findIndex, groupBy, last,
                                              lookup, nub, partition, sort,
                                              sortOn)
import           Data.Maybe                  (catMaybes, fromMaybe, isJust,
                                              isNothing, mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.FilePath             (takeBaseName)
import qualified Text.Casing                 as Casing

data Options = Options
    { typesOnly :: Bool
    }

data ModuleKind = MainModule | ExtensionModule Text deriving (Eq, Show)

sizeToType :: SemanticModel -> [(Text, Text)]
sizeToType model =
    [ (sizeConst, idToRustPascal semName)
    | (sizeConst, semName) <- S.arrayTypes model
    ]

byteArrayConstants :: SemanticModel -> [Text]
byteArrayConstants = map fst . S.arrayTypes

-- | The trait a handle's callbacks go to: @ToxAVHandler@.
handlerTrait :: SResource -> Text
handlerTrait res = idToRustPascal (resourceName res) <> "Handler"

-- | A handle struct's parameters: @['a, H: ToxAVHandler]@.
handleGenerics :: SemanticModel -> SResource -> [Text]
handleGenerics model res =
    [ "'a" | hasLifetime model res ] ++ [ "H: " <> handlerTrait res | K.keepsHandler model res ]

-- | A handle struct's type: @ToxAV<'a, H>@.
structType :: SemanticModel -> SResource -> RsType
structType model res = case [ "'a" | hasLifetime model res ] ++ [ "H" | K.keepsHandler model res ] of
    [] -> TyPath name
    ps -> TyGeneric name (map TyPath ps)
  where
    name = idToRustPascal (resourceName res)

-- | The marker of what a handle borrows.
markerType :: SemanticModel -> SResource -> RsType
markerType model res = TyGeneric "std::marker::PhantomData" [TyRef (Just "'a") borrowed False]
  where
    borrowed = maybe TyUnit (TyPath . idToRustPascal) (K.borrowed model res)

-- | The root resource's name: @Tox@.
rootName :: SemanticModel -> Text
rootName model = maybe (error "no root resource") resourceName (find isRoot (S.resources model))

-- | A view, or a handle made from what it can't outlive.
hasLifetime :: SemanticModel -> SResource -> Bool
hasLifetime model res = K.isView model res || isJust (K.borrowed model res)

generate :: Options -> SemanticModel -> [(FilePath, Text)]
generate opts model =
    let
        allItems = concatMap (resourceItems model) (S.resources model)

        modules = mapMaybe (generateModule opts model allItems) (structModules model)

        -- `dispatch` isn't a resource module, but `core` re-exports its traits.
        moduleNames = map (\(fp, _) -> T.pack (takeBaseName fp)) modules
                   ++ ["dispatch"]
        modRs = generateAggregateModule "mod" moduleNames

        -- Not in mod.rs: the hand-written `src/types.rs` includes it with
        -- `#[path]`.
        typesRs = generateTypesModule model

        safeModules = generateSafeModules model allItems

        dispatchModule = generateDispatchModule model

        resourceModules = generateResourceWrappers model allItems

    in modules ++ [modRs, typesRs] ++ safeModules ++ [dispatchModule] ++ resourceModules

-- | A resource, and the root's functions another resource's prefix names,
-- like @tox_file_control@: they're that resource's, on the root's struct.
resourceItems :: SemanticModel -> SResource -> [(ModuleKind, SResource)]
resourceItems model res
    | isRoot res = map item (groupOn (K.namedFor model . S.methodName) (S.methods res))
    | otherwise  = [(MainModule, res)]
  where
    item (Nothing, ms) = (MainModule, res { methods = ms })
    item (Just r, ms)  = (ExtensionModule (resourceName res), res { resourceName = r, methods = ms })
    groupOn f xs = [ (f x, g) | g@(x : _) <- groupBy (\a b -> f a == f b) (sortOn f xs) ]

-- | The modules, one per struct with a lifecycle of its own.
structModules :: SemanticModel -> [Text]
structModules model = sort (nub (map (structModule model . resourceName) (S.resources model)))

-- | A struct's module. A handle with a lifecycle of its own has its own. A
-- struct only handed out by another is in that one's: a variant's members in
-- the variant's, a list's elements in the list's, anything else in its
-- parent's.
structModule :: SemanticModel -> Text -> Text
structModule model = go
  where
    allRes = S.resources model
    go n = case find ((== n) . resourceName) allRes of
        Nothing -> idToRustSnake n
        Just r
            | ownLifecycle r         -> idToRustSnake n
            | h : _ <- handedOutBy r -> go h
            | otherwise              -> go (rootName model)
    ownLifecycle r = resourceType r == ResHandle
        && any ((`elem` [Constructor, Destructor]) . methodRole) (S.methods r)
    handedOutBy r = maybe [] pure (K.handedOutBy model (resourceName r)) ++ maybe [] pure (parent r)

-- | The module an item's functions are in: their struct's.
itemModule :: SemanticModel -> ModuleKind -> SResource -> Text
itemModule model kind res = structModule model (itemOwner model kind res)

-- | The struct an item's functions are methods of.
itemOwner :: SemanticModel -> ModuleKind -> SResource -> Text
itemOwner _ (ExtensionModule base) _ = base
itemOwner model MainModule res       = findHandleAncestor (S.resources model) res

findHandleAncestor :: [SResource] -> SResource -> Text
findHandleAncestor allRes res =
    case resourceType res of
        ResHandle -> resourceName res
        ResId _ -> case parent res of
            Just pName -> case find ((== pName) . resourceName) allRes of
                Just pRes -> findHandleAncestor allRes pRes
                Nothing   -> root
            Nothing -> root
  where
    root = maybe "" resourceName (find isRoot allRes)

generateAggregateModule :: Text -> [Text] -> (FilePath, Text)
generateAggregateModule name modules =
    let
        mkItems m =
            let modItem = RsMod Pub m Nothing
                useItem = RsUse Pub ("self::" <> m <> "::*")
                extra = [ RsUse Pub ("self::" <> m <> " as " <> alias) | Just alias <- [lookup m Crate.moduleAliases] ]
            in [modItem, useItem] ++ extra

        items = concatMap mkItems modules
        rsMod = RsModule items
        content = "#![allow(unused_imports)]\n" <> render rsMod
    in (T.unpack name <> ".rs", content)

generateTypesModule :: SemanticModel -> (FilePath, Text)
generateTypesModule model =
    let
        -- The byte-array macro expands to `hex::encode`, from the hand-written
        -- `src/types.rs`.
        header = RsRaw $ T.unlines
            [ "// Generated by apigen. Do not edit."
            , "use crate::ffi;"
            , "use crate::types::hex;"
            , "use serde::{Deserialize, Serialize};"
            , "use std::{error, ffi as std_ffi, fmt};"
            ]

        sizeConstants =
            [ RsConst Pub (stripRootPrefix model (constantName c))
                      (TyPath "usize")
                      ("ffi::" <> constantName c <> " as usize")
            | c <- constants model
            , isSizeConstant (constantName c)
            ]

        newtypeNames = map (idToRustPascal . idName) (idTypes model) ++ Crate.extraNewtypes
        newtypeInvocations =
            [ RsMacroCall "impl_safe_newtype" [n] | n <- nub newtypeNames ]

        byteArrayInvocations =
            [ RsMacroCall "impl_byte_array_type" [ty, stripRootPrefix model sizeConst]
            | (sizeConst, ty) <- sizeToType model
            ]

        modelEnumPairs = [ (S.enumName e, map S.enumMemberCName (S.enumMembers e)) | e <- enums model ]
        allEnumPairs = sortOn fst (modelEnumPairs ++ K.privateEnums model)
        enumInvocations = map (uncurry mkEnumInvocation) allEnumPairs

        errorTypeRs = RsRaw (renderErrorType model)

    in ( "types.rs"
       , render (RsModule
           ( [header]
          ++ sizeConstants
          ++ newtypeInvocations
          ++ byteArrayInvocations
          ++ enumInvocations
          ++ [errorTypeRs]
           ))
       )

isSizeConstant :: Text -> Bool
isSizeConstant n = "_SIZE" `T.isSuffixOf` n || "_LENGTH" `T.isSuffixOf` n

-- | A constant without the root library's word: @PUBLIC_KEY_SIZE@.
stripRootPrefix :: SemanticModel -> Text -> Text
stripRootPrefix model n = fromMaybe n (T.stripPrefix (T.toUpper (rootNamespace model) <> "_") n)

mkEnumInvocation :: Text -> [Text] -> RsItem
mkEnumInvocation cName members =
    let safe = fromMaybe cName (safeEnumName cName)
        block = "{\n" <> T.concat ["    " <> m <> ",\n" | m <- members] <> "}"
    in RsMacroCall Crate.enumMacro [safe, "ffi::" <> cName, block]

errorEnumCName :: SemanticModel -> Text -> Maybe Text
errorEnumCName model sem =
    fmap S.enumName (find (\e -> S.enumName e == sem || S.enumSemanticName e == sem) (enums model))

renderErrorType :: SemanticModel -> Text
renderErrorType model =
    let
        methodErrEnums = nub $ mapMaybe (errorEnumCName model)
            (concatMap (mapMaybe methodErrorType . methods) (resources model))
        privateErrEnums = [ c | (c, _) <- K.privateEnums model, "_Err_" `T.isInfixOf` c ]
        errEnums = sortOn (errorVariant (namespaces model)) (nub (methodErrEnums ++ privateErrEnums))

        variants = [ "    " <> errorVariant (namespaces model) c <> "(" <> c <> ")," | c <- errEnums ]
                ++ [ "    AvGroupError,"
                   , "    InvalidString(std_ffi::NulError),"
                   ]
    in T.unlines
        ( [ "// --- Safe Results ---"
          , ""
          , "#[derive(Debug, Clone, PartialEq, Eq)]"
          , "pub enum " <> Crate.errorType <> " {"
          ]
       ++ variants
       ++ [ "}"
          , ""
          , "impl error::Error for " <> Crate.errorType <> " {}"
          , "impl fmt::Display for " <> Crate.errorType <> " {"
          , "    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {"
          , "        write!(f, \"{:?}\", self)"
          , "    }"
          , "}"
          , ""
          , "pub type Result<T> = std::result::Result<T, " <> Crate.errorType <> ">;"
          ] )
-- | A module: its structs, then one impl per struct.
generateModule :: Options -> SemanticModel -> [(ModuleKind, SResource)] -> Text -> Maybe (FilePath, Text)
generateModule _opts model allItems modName
    | null body = Nothing
    | otherwise = Just (T.unpack modName <> ".rs", "#![allow(unused_imports)]\n" <> render (RsModule (imports ++ body)))
  where
    allRes = S.resources model
    homeOf = structModule model . resourceName
    homes = [ res | (MainModule, res) <- allItems, homeOf res == modName ]
    contributing = [ (kind, res) | (kind, res) <- allItems, itemModule model kind res == modName ]

    structs = concatMap (generateResourceStructs model MainModule) homes
    impls = mergeImpls (concat [ fst (generateResourceMethods model kind res) | (kind, res) <- contributing ])
    traitItems = concat [ concatMap (generateTraitItems model res) (traits res) | res <- homes ]
    variantsRs = concat [ generateVariant model v | v <- S.variants model, structModule model (S.variantName v) == modName ]
    body = structs ++ impls ++ traitItems ++ variantsRs

    usedHandles = [ h | (_, res) <- contributing, m <- S.methods res, p <- S.inputs m, S.SHandle h <- [S.paramType p] ]
    -- A handle struct borrowing another has a marker of it, like ToxAV's Tox,
    -- and a handle's functions take its parent, like the events' Tox.
    borrowed = [ p | res <- homes, resourceType res == ResHandle, Just p <- [lifecycleParent res] ]
    parents = [ p | (_, res) <- contributing, resourceType res == ResHandle, Just p <- [S.parent res] ]
    elsewhere = nub [ (m, idToRustPascal name)
                    | name <- [ itemOwner model k r | (k, r) <- contributing ] ++ usedHandles ++ borrowed ++ parents
                    , Just r <- [find ((== name) . resourceName) allRes]
                    , let m = homeOf r, m /= modName ]
    handlers = nub [ handlerTrait r | r <- allRes, K.keepsHandler model r
                   , homeOf r == modName || resourceName r `elem` usedHandles ]
    imports = nub $ [RsUse Private "crate::ffi", RsUseAttr "allow(unused_imports)" "crate::types::*"]
        ++ [ RsUse Private ("super::" <> m <> "::" <> n) | (m, n) <- elsewhere ]
        ++ [ RsUse Private ("crate::core::dispatch::" <> h) | h <- handlers ]

-- | One inherent impl per type, where its first was.
mergeImpls :: [RsItem] -> [RsItem]
mergeImpls = \case
    [] -> []
    RsItemImpl i@RsImpl{implTrait = Nothing} : rest ->
        let (same, others) = partition (sameAs i) rest
        in RsItemImpl i { implItems = implItems i ++ concat [ implItems j | RsItemImpl j <- same ] } : mergeImpls others
    x : rest -> x : mergeImpls rest
  where
    sameAs i (RsItemImpl j) = isNothing (implTrait j) && implType j == implType i && implGenerics j == implGenerics i
    sameAs _ _              = False

generateVariant :: SemanticModel -> SVariant -> [RsItem]
generateVariant model var =
    let
        name = idToRustPascal (S.variantName var)
        -- Its members are views of what C holds.
        isEvent = any (K.isView model) [ r | r <- S.resources model, resourceName r `elem` map memberType (variantMembers var) ]
        generics = if isEvent then ["'a"] else []

        mkVariant mem =
            let vName = idToRustPascal (memberName mem)
                vTy = idToRustPascal (memberType mem)
                ty = if isEvent then TyPath (vTy <> "<'a>") else TyPath vTy
            in RsEnumVariant vName [ty]

        -- `Unknown`: the linked c-toxcore may have events these bindings don't.
        variants' = map mkVariant (variantMembers var)
                 ++ [RsEnumVariant "Unknown" [] | isEvent]

        rsEnum = RsItemEnum RsEnum
            { enumName = name
            , enumVis = Pub
            , enumGenerics = generics
            , enumVariants = variants'
            }

        implType = if isEvent then TyGeneric name [TyPath "'a"] else TyPath name

        ptrTy = case find (\r -> resourceName r == S.variantName var) (S.resources model) of
            Just r  -> "*const ffi::" <> cName r
            Nothing -> "*const std::ffi::c_void"

        mkArm mem =
            let vName = idToRustPascal (memberName mem)
                getter = "ffi::" <> memberGetter mem
                vTy = idToRustPascal (memberType mem)

                enumMod = find (\e -> S.enumSemanticName e == S.variantTypeEnum var) (S.enums model)
                cVariantName = case enumMod of
                    Just e -> fromMaybe (memberName mem) $ lookup (memberName mem) (map (\m -> (S.enumMemberName m, S.enumMemberCName m)) (S.enumMembers e))
                    Nothing -> memberName mem

                enumTypeName = fromMaybe (idToRustPascal (S.variantTypeEnum var)) (fmap S.enumName enumMod)
                enumVariant = "ffi::" <> enumTypeName <> "::" <> cVariantName

                -- The deref must be inside the unsafe block.
                innerCall = ECall (EVar getter) [EVar "ptr"]
                wrapped = ECall (EVar vTy)
                    [EUnsafe (RsBlock [StmtExprNoSemi (ERef (EDeref innerCall) False)])]
            in (PPath enumVariant, RsBlock [StmtExprNoSemi (ECall (EVar (name <> "::" <> vName)) [wrapped])])

        fromPtrBody = RsBlock
            [ StmtLet "type_" Nothing (EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar ("ffi::" <> T.toLower (S.commonPrefix model) <> "_" <> idToRustSnake (S.variantName var) <> "_get_type")) [EVar "ptr"])]))
            , StmtExprNoSemi (EMatch (EVar "type_")
                (map mkArm (variantMembers var) ++
                 [ if isEvent
                   then (PWildcard, RsBlock [StmtExprNoSemi (EVar (name <> "::Unknown"))])
                   else (PWildcard, RsBlock [StmtExpr (ECall (EVar "panic!") [ELit (LString "Unknown variant type")])]) ]))
            ]

        -- Dereferences a raw pointer, so it's `unsafe fn` (clippy's
        -- `not_unsafe_ptr_arg_deref`).
        fromPtrFn = RsItemFn RsFn
            { fnName = "from_ptr"
            , fnVis = Pub
            , fnAbi = Nothing
            , fnGenerics = []
            , fnArgs = [RsArg "ptr" (TyPath ptrTy)]
            , fnRet = Just implType
            , fnBody = Just fromPtrBody
            , fnUnsafe = True
            , fnDoc = ["# Safety", "", "`ptr` must be a valid, non-null pointer to a live event", "object that stays alive for the lifetime `'a`."]
            }

        rsImpl = RsItemImpl RsImpl
            { implTrait = Nothing
            , implType = implType
            , implGenerics = generics
            , implItems = [fromPtrFn]
            }

    in [rsEnum, rsImpl]

generateTraitItems :: SemanticModel -> SResource -> SResourceTrait -> [RsItem]
generateTraitItems model res (Iterable elemTy sizeMethod accessor) =
    let name = idToRustPascal (resourceName res)
        isLife = hasLifetime model res

        stripCommon n = fromMaybe n $ T.stripPrefix (T.toLower (S.commonPrefix model) <> "_") (T.toLower n)

        stripMethodName n =
            let nameLower = T.toLower n
                stripped = case resourceType res of
                    ResHandle -> fromMaybe (stripCommon nameLower) $ T.stripPrefix (T.toLower (S.cPrefix res)) nameLower
                    ResId _ -> stripCommon nameLower
            in idToRustSnake stripped

        accName = stripMethodName (T.toLower (S.cPrefix res) <> accessor)
        sizeName = stripMethodName (T.toLower (S.cPrefix res) <> sizeMethod)

        -- A `_Nullable` accessor already returns `Option<T>`.
        accNullable = any (\m -> T.toLower (S.methodName m)
                                   == T.toLower (S.cPrefix res <> accessor)
                                 && methodReturnNullable m) (S.methods res)

        itemTy = toSafeRetType model res Nothing elemTy
        itemTyLife = case elemTy of
            SHandle h ->
                case find (\r -> resourceName r == h) (S.resources model) of
                    Just r | hasLifetime model r -> TyGeneric (idToRustPascal h) [TyPath "'a"]
                    _ -> itemTy
            _ -> itemTy

        iterName = name <> "Iter"

        iterStruct = RsItemStruct RsStruct
            { structName = iterName
            , structVis = Pub
            , structGenerics = if isLife then ["'a"] else []
            , structFields = [ RsStructField "parent" (TyRef (if isLife then Just "'a" else Nothing) (if isLife then TyGeneric name [TyPath "'a"] else TyPath name) False) Private
                             , RsStructField "index" (TyPath "u32") Private
                             , RsStructField "count" (TyPath "u32") Private
                             ] ++ [ RsStructField "_marker" (TyGeneric "std::marker::PhantomData" [TyRef (Just "'a") (TyUnit) False]) Private | isLife && not (K.isView model res) ]
            }

        nextBody = RsBlock
            [ StmtExpr (EIf (EBinOp ">=" (EFieldAccess (EVar "self") "index") (EFieldAccess (EVar "self") "count")) (RsBlock [StmtReturn (EVar "None")]) Nothing)
            , StmtLet "item" Nothing (EMethodCall (EFieldAccess (EVar "self") "parent") accName [EFieldAccess (EVar "self") "index"])
            , StmtExpr (EBinOp "+=" (EFieldAccess (EVar "self") "index") (ELit (LInt 1)))
            , StmtExprNoSemi (if accNullable
                              then EVar "item"
                              else ECall (EVar "Some") [EVar "item"])
            ]


        iterImpl = RsItemImpl RsImpl
            { implTrait = Just "Iterator"
            , implType = TyGeneric iterName (if isLife then [TyPath "'a"] else [])
            , implGenerics = if isLife then ["'a"] else []
            , implItems = [
                RsItemTypeAlias RsTypeAlias { aliasName = "Item", aliasVis = Private, aliasType = itemTyLife }
              , RsItemFn RsFn { fnName = "next", fnVis = Private, fnAbi = Nothing, fnGenerics = [], fnArgs = [RsSelfArg True], fnRet = Just (TyGeneric "Option" [itemTyLife]), fnBody = Just nextBody, fnUnsafe = False, fnDoc = [] }
              ]
            }

        intoIterImpl = RsItemImpl RsImpl
            { implTrait = Just "IntoIterator"
            , implType = TyRef (if isLife then Just "'a" else Nothing) (if isLife then TyGeneric name [TyPath "'a"] else TyPath name) False
            , implGenerics = if isLife then ["'a"] else []
            , implItems = [
                RsItemTypeAlias RsTypeAlias { aliasName = "Item", aliasVis = Private, aliasType = itemTyLife }
              , RsItemTypeAlias RsTypeAlias { aliasName = "IntoIter", aliasVis = Private, aliasType = TyGeneric iterName (if isLife then [TyPath "'a"] else []) }
              , RsItemFn RsFn { fnName = "into_iter", fnVis = Private, fnAbi = Nothing, fnGenerics = [], fnArgs = [RsSelf], fnRet = Just (TyGeneric iterName (if isLife then [TyPath "'a"] else [])), fnBody = Just (RsBlock [
                    StmtExprNoSemi (EStructInit iterName ([
                        ("parent", EVar "self")
                      , ("index", ELit (LInt 0))
                      , ("count", EMethodCall (EVar "self") sizeName [])
                    ] ++ [("_marker", EVar "std::marker::PhantomData") | isLife && not (K.isView model res)]))
                ]), fnUnsafe = False, fnDoc = [] }
              ]
            }

    in [iterStruct, iterImpl, intoIterImpl]

generateResourceMethods :: SemanticModel -> ModuleKind -> SResource -> ([RsItem], [RsItem])
generateResourceMethods model kind res =
    let structBaseName = case kind of
            MainModule -> case resourceType res of
                ResHandle -> idToRustPascal (resourceName res)
                ResId _ -> idToRustPascal (findHandleAncestor (S.resources model) res)
            ExtensionModule base ->
                let baseRes = find (\r -> resourceName r == base) (S.resources model)
                in case baseRes of
                    Just br -> idToRustPascal (findHandleAncestor (S.resources model) br)
                    Nothing -> idToRustPascal base

        structRes = find (\r -> idToRustPascal (resourceName r) == structBaseName) (S.resources model)
        gens = maybe [] (handleGenerics model) structRes
        structTy = maybe (TyPath structBaseName) (structType model) structRes

        notHandWritten m = S.methodName m `notElem` Crate.handWrittenCore
        isEventResource = K.isView model res
        refineEventOutput m
            | isEventResource = m { output = eventFieldType model res m }
            | otherwise       = m
        -- The destructor is the `Drop` impl. A method too would double-free.
        resMethods = map refineEventOutput
            (filter (\m -> notHandWritten m && methodRole m /= Destructor) (S.methods res))

        (staticMethods, instanceMethods') = partition (\m -> methodRole m == Constructor || methodRole m == StaticRole) resMethods
        instanceMethods = filter (not . takesRawPointers) instanceMethods'

        (realStatic, ctors) = partition (\m -> methodRole m == StaticRole) staticMethods

        methodToItem = generateMethod model kind res

        -- The element accessor takes an unchecked index and may return null, so
        -- only the bounds-checked iterator may call it.
        iterAccessorCNames =
            [ T.toLower (S.cPrefix res <> acc) | Iterable _ _ acc <- traits res ]
        genMethodItems m = map privatize (methodToItem m)
          where privatize (RsItemFn f)
                    | T.toLower (S.methodName m) `elem` iterAccessorCNames =
                        RsItemFn f { fnVis = Private }
                privatize item = item

        implItems = if (null instanceMethods && null ctors && null realStatic) || (kind == MainModule && isVariantResource model res)
                    then []
                    else [RsItemImpl (RsImpl Nothing structTy gens (concatMap genMethodItems (ctors ++ instanceMethods ++ realStatic)))]

        staticItems = []
    in (implItems, staticItems)

-- | A C callback or a @void *@ can't be safe Rust: the crate wraps such
-- functions by hand, like the log callback.
takesRawPointers :: SMethod -> Bool
takesRawPointers m = any K.isRawPointer (output m : map paramType (S.inputs m))

isVariantResource :: SemanticModel -> SResource -> Bool
isVariantResource model res = any (\v -> S.variantName v == resourceName res) (S.variants model)

generateResourceStructs :: SemanticModel -> ModuleKind -> SResource -> [RsItem]
generateResourceStructs model kind res =
    case kind of
        ExtensionModule _ -> []
        MainModule ->
            if isVariantResource model res then [] else
            let name = idToRustPascal (resourceName res)
            in case resourceType res of
                -- Its id type is in crate::types.
                ResId _ -> []
                ResHandle ->
                    let isRef = K.isView model res
                        isLife = hasLifetime model res

                        structItem = if isRef
                            then RsItemTupleStruct RsTupleStruct
                                { tupleStructName = name
                                , tupleStructVis = Pub
                                , tupleStructGenerics = handleGenerics model res
                                , tupleStructFields = [ TyRef (Just "'a") (TyPath ("ffi::" <> cName res)) False ]
                                }
                            else RsItemStruct RsStruct
                                { structName = name
                                , structVis = Pub
                                , structGenerics = handleGenerics model res
                                , structFields = [ RsStructField "ptr" (TyPath ("*mut ffi::" <> cName res)) PubCrate ]
                                                 ++ [ RsStructField "handler" (TyGeneric "Box" [TyPath "H"]) Pub | K.keepsHandler model res ]
                                                 ++ [ RsStructField "_marker" (markerType model res) Private | isLife ]
                                }
                        -- A handle made from its parent is dropped before it:
                        -- toxav asserts that its Tox is still alive.
                        dropImpl = if isRef then [] else generateDropImpl model res
                    in [structItem] ++ dropImpl

generateDropImpl :: SemanticModel -> SResource -> [RsItem]
generateDropImpl model res =
    case find (\m -> methodRole m == Destructor) (methods res) of
        Just destructor ->
            let mapping = case methodMapping destructor of
                    StandardMapping -> inferCFunctionMapping (resources model) res destructor
                    CustomMapping m -> m
                funcName = "ffi::" <> cFunctionName mapping
                body = StmtExprNoSemi (EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar funcName) [EFieldAccess (EVar "self") "ptr"])]))

            in [RsItemImpl (RsImpl (Just "Drop") (structType model res) (handleGenerics model res) [RsItemFn (RsFn "drop" Private Nothing [] [RsSelfArg True] Nothing (Just (RsBlock [body])) False [])])]
        Nothing -> []

generateMethod :: SemanticModel -> ModuleKind -> SResource -> SMethod -> [RsItem]
generateMethod model kind res method =
    let
        mapping = case methodMapping method of
            StandardMapping -> inferCFunctionMapping (resources model) res method
            CustomMapping m -> m

        -- Handles keeping a handler that the method takes or gives.
        handlersOf (SHandle h) = [ r | r <- S.resources model, resourceName r == h, K.keepsHandler model r ]
        handlersOf (SList t)   = handlersOf t
        handlersOf _           = []
        usedHandlers = nub (concatMap (handlersOf . paramType) (S.inputs method) ++ handlersOf (output method))

        structName = case kind of
            MainModule -> case resourceType res of
                ResHandle -> idToRustPascal (resourceName res)
                ResId _   -> fromMaybe (rootName model) (S.parent res)
            ExtensionModule base -> idToRustPascal base

        foreignHandlers = [ r | r <- usedHandlers, idToRustPascal (resourceName r) /= structName ]
        needsH = not (null foreignHandlers)

        hasUserData = methodHasUserData method
        generics = (if needsH then ["'a"] else []) ++ (if hasUserData then ["T"] else []) ++ [ "H: " <> handlerTrait r | r <- foreignHandlers ]

        isRootStruct = structName == idToRustPascal (rootName model)

        originalName = S.methodName method
        methodName =
            let
                nameLower = T.toLower originalName
                commonPrefix' = T.toLower (S.commonPrefix model) <> "_"

                stripCommon n = fromMaybe n $ T.stripPrefix commonPrefix' n

                stripped = case kind of
                    MainModule ->
                        case resourceType res of
                            ResHandle -> fromMaybe (stripCommon nameLower) $ T.stripPrefix (T.toLower (S.cPrefix res)) nameLower
                            ResId _ -> stripCommon nameLower
                    ExtensionModule _ -> stripCommon nameLower

                stripped' = stripNamespace (namespaces model) stripped

                isEventRes = K.isView model res
                strippedEvent = fromMaybe stripped' (T.stripPrefix "get_" stripped')

                -- Name the enum accessor after its enum (`message_type`, `conference_type`).
                eventTypeName = case output method of
                    SEnum n ->
                        T.intercalate "_"
                             (filter (`notElem` namespaces model)
                               (map T.toLower (T.splitOn "_" (enumCName model n))))
                    _ -> "type"
                stripped'' = if isEventRes && strippedEvent == "type"
                             then eventTypeName
                             else strippedEvent
            in idToRustSnake (if isEventRes then stripped'' else stripped')

        methodName' = if K.isSettings model res
                      then fromMaybe methodName (T.stripPrefix "set_" methodName)
                      else methodName

        (args, retType, body) = generateWrapperBody model kind res method mapping

        isStatic = case kind of
            MainModule ->
                case resourceType res of
                    ResHandle -> methodRole method == Constructor || methodRole method == StaticRole
                    ResId _ -> methodRole method == StaticRole
            ExtensionModule _ -> methodRole method == StaticRole

        finalArgs = if isStatic
                    then args
                    else
                        -- The root and handles keeping a handler hold a raw
                        -- pointer, so a shared borrow is enough.
                        let ownsHandler = any (\r -> K.keepsHandler model r && idToRustPascal (resourceName r) == structName) (S.resources model)
                            constness = if isRootStruct || ownsHandler then ConstThis else methodConstness method
                            selfArg = if constness == ConstThis then RsSelfArg False else RsSelfArg True
                        in [selfArg] ++ args

    in [ RsItemFn RsFn
            { fnName = methodName'
            , fnVis = Pub
            , fnAbi = Nothing
            , fnGenerics = generics
            , fnUnsafe = False
            , fnDoc = []
            , fnArgs = finalArgs
            , fnRet = retType
            , fnBody = Just body
            }
       ]

generateWrapperBody :: SemanticModel -> ModuleKind -> SResource -> SMethod -> CFunctionMapping -> ([RsArg], Maybe RsType, RsBlock)
generateWrapperBody model kind res method mapping =
    let
        cFunc = cFunctionName mapping
        cArgs = argMapping mapping
        semParams = cSemParams mapping

        (contextArgs, ctxNames) = generateContextArgs model kind res (methodConstness method) method cArgs

        safeArgs = mapMaybe (toSafeArg model semParams) (zip cArgs [0..])

        finalSafeArgs = safeArgs

        hasUserData = methodHasUserData method
        -- A handle keeping a handler is made with it.
        takesHandler = K.keepsHandler model res && methodRole method == Constructor
        userDataArg = if takesHandler
                      then RsArg "handler" (TyPath "H")
                      else RsArg "user_data" (TyPath "&mut T")

        finalArgs = contextArgs ++ finalSafeArgs ++ (if hasUserData || takesHandler then [userDataArg] else [])

        (argExprs, stmts) = resolveArguments model semParams ctxNames cArgs

        -- A `size_t` size function needs no `as usize` cast (event `_length`
        -- accessors return `uint32_t` and do).
        sizeFuncReturnsUsize = case cSizeFunctionName mapping of
            Just sf -> any (\(r, m) ->
                              cFunctionName (inferCFunctionMapping (S.resources model) r m) == sf
                              && output m == SSizeT)
                           [ (r, m) | r <- S.resources model, m <- S.methods r ]
            Nothing -> False

        -- A buffer sized by the inputs, like `plaintext_len + EXTRA`.
        computedSize = case output method of
            SFixedBytes size _
                | Just e <- parseSizeExpr size
                , any (T.any isLower) (sizeExprVars e) -> Just e
            _ -> Nothing

        isSBytesNoSize = case output method of SBytes -> isNothing (cSizeFunctionName mapping); _ -> False

        -- Event structs borrow the C event, so their bytes can be a borrowed
        -- slice.
        isEventRes = K.isView model res
        isEventBytes = isEventRes && output method == SBytes
                       && isJust (cSizeFunctionName mapping)

        hasErrorPtr = any (\(a) -> case a of ErrorPtr -> True; _ -> False) cArgs

        rawRetType = if isJust computedSize
                     then TyGeneric "Vec" [TyPath "u8"]
                     else if isSBytesNoSize then TyPath "*const u8"
                     else if isEventBytes then TyRef (Just "'a") (TySlice (TyPath "u8")) False
                     else if output method == SVoid then TyUnit else toSafeRetType model res (Just (methodRole method)) (output method)

        errEnumName = fromMaybe (error "generateWrapperBody: no error type") (methodErrorType method)
        screaming = T.toUpper $ T.pack $ Casing.toSnake $ Casing.fromAny $ T.unpack errEnumName
        okVariant = "ffi::" <> errEnumName <> "::" <> screaming <> "_OK"

        rawRetType' = case methodResultStrategy method of
                        IgnoreReturn -> TyUnit
                        _            -> rawRetType

        hasBufferPtr = any (\(a) -> case a of BufferPtr _ -> True; _ -> False) cArgs

        -- An instance getter returning a `bool` presence flag, with no error
        -- enum, returns `Option` (static helpers like tox_hash don't).
        isOptionFixedBytes = case output method of
            SFixedBytes _ _ -> not hasErrorPtr && hasBufferPtr
                               && cReturnType mapping == SBool
                               && methodRole method /= StaticRole
                               && methodRole method /= Constructor
            _               -> False

        isNullableHandle = methodReturnNullable method && not hasErrorPtr
            && case output method of SHandle _ -> True; _ -> False
        finalRetType = if hasErrorPtr
                       then Just (TyGeneric "std::result::Result" [rawRetType', TyPath errEnumName])
                       else if isOptionFixedBytes || isNullableHandle
                       then Just (TyGeneric "Option" [rawRetType])
                       else if rawRetType == TyUnit then Nothing else Just rawRetType
        macroArgs = filterArgExprs cArgs argExprs

        bodyStmt = case computedSize of
          Just size -> generateComputedBufferBody model cFunc cArgs semParams ctxNames okVariant size
          Nothing -> case output method of
                SFixedBytes sizeConst _ ->
                    generateFixedBytesBody model method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr isOptionFixedBytes okVariant sizeConst
                -- A `const char *` accessor is a `&str`, even with a `_length`
                -- function.
                SString -> generateStringAccessBody (methodReturnNullable method) cFunc macroArgs
                _ -> case cSizeFunctionName mapping of
                    Just sizeFunc ->
                        if hasBufferPtr then
                             generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant
                        else if isEventBytes then
                             generateEventBytesBody cFunc macroArgs sizeFunc
                        else
                             generateAccessBody (methodReturnNullable method) sizeFuncReturnsUsize cFunc macroArgs sizeFunc
                    Nothing ->
                         generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant

    in (finalArgs, finalRetType, RsBlock (stmts ++ [bodyStmt]))

-- | A function writing a buffer sized by its inputs: C checks them, so a
-- size below zero is zero, and C's error says why.
generateComputedBufferBody :: SemanticModel -> Text -> [CArgSource] -> [SParameter] -> [Text] -> Text -> SizeExpr -> RsStmt
generateComputedBufferBody model cFunc cArgs semParams ctxNames okVariant size =
    let
        (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs

        lenExpr e = case e of
            SizeNum n -> T.pack (show n)
            SizeVar v
                | T.all (\c -> isUpper c || isDigit c || c == '_') v -> "ffi::" <> v <> " as usize"
                | Just p <- K.lengthOf model cFunc v -> idToRustSnake p <> ".len()"
                | otherwise -> idToRustSnake v
            SizeOp '-' x y -> operand x <> ".saturating_sub(" <> lenExpr y <> ")"
            SizeOp op x y  -> operand x <> " " <> T.singleton op <> " " <> operand y
            SizeCall f xs  -> "std::cmp::" <> f <> "(" <> T.intercalate ", " (map lenExpr xs) <> ")"
        operand x = case x of
            SizeOp{} -> "(" <> lenExpr x <> ")"
            _        -> lenExpr x

        fixedArgs = zipWith (\src expr -> case src of
            BufferPtr _ -> EMethodCall (EVar "buf") "as_mut_ptr" []
            ErrorPtr    -> EVar "&mut err"
            _           -> expr) cArgs allArgExprs

        stmts =
            [ StmtLet "len" Nothing (EVar (lenExpr size))
            , StmtLet "mut buf" Nothing (EMacroCall "vec" ["0u8; len"])
            , StmtLet "mut err" Nothing (EVar okVariant)
            , StmtExpr (ECall (EVar ("ffi::" <> cFunc)) fixedArgs)
            , StmtExpr (EIf (EVar ("err != " <> okVariant))
                   (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "err.into()"])])
                   Nothing)
            , StmtExprNoSemi (ECall (EVar "Ok") [EVar "buf"])
            ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateFixedBytesBody :: SemanticModel -> SMethod -> Text -> [CArgSource] -> [SParameter] -> [Text] -> Bool -> Bool -> Bool -> Text -> Text -> RsStmt
generateFixedBytesBody model method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr isOption okVariant sizeConst =
    let (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs
        macroArgs = filterArgExprs cArgs allArgExprs
        c' = stripRootPrefix model sizeConst

        wrapType' t expr = case t of
            SFixedBytes sc _ ->
                case lookup sc (sizeToType model) of
                    Just ty -> ECall (EVar ty) [expr]
                    Nothing -> expr
            _ -> expr

    in if hasBufferPtr && hasErrorPtr then
        let macroCall = ECall (EVar "ffi_get_array!") ([EVar cFunc, EVar okVariant, EVar c'] ++ macroArgs)
        in case lookup sizeConst (sizeToType model) of
            Just ty -> StmtExprNoSemi (EMethodCall macroCall "map" [EVar ty])
            Nothing -> StmtExprNoSemi macroCall
    else if hasBufferPtr && isOption then
        let
            sizeExpr = constSizeExpr model sizeConst
            bufInit = EArrayInit (ELit (LInt 0)) sizeExpr

            fixedArgs = zipWith (\(src) expr -> case src of
                BufferPtr _ -> EMethodCall (EVar "buf") "as_mut_ptr" []
                _           -> expr) cArgs allArgExprs

            okExpr = ECall (EVar "Some") [wrapType' (output method) (EVar "buf")]
            stmts = [ StmtLet "mut buf" Nothing bufInit
                    , StmtExprNoSemi (EIf (ECall (EVar ("ffi::" <> cFunc)) fixedArgs)
                          (RsBlock [StmtExprNoSemi okExpr])
                          (Just (RsBlock [StmtExprNoSemi (EVar "None")])))
                    ]
        in StmtExprNoSemi (EUnsafe (RsBlock stmts))
    else if hasBufferPtr then
        let
            sizeExpr = constSizeExpr model sizeConst
            bufInit = EArrayInit (ELit (LInt 0)) sizeExpr

            fixedArgs = zipWith (\(src) expr -> case src of
                BufferPtr _ -> EMethodCall (EVar "buf") "as_mut_ptr" []
                ErrorPtr    -> EVar "&mut err"
                _           -> expr) cArgs allArgExprs

            stmts = [ StmtLet "mut buf" Nothing bufInit ]
                    ++ (if hasErrorPtr
                        then [ StmtLet "mut err" Nothing (EVar okVariant)
                             , StmtExpr (ECall (EVar ("ffi::" <> cFunc)) (fixedArgs))
                             , StmtExpr (EIf (EVar ("err != " <> okVariant))
                                    (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "err.into()"])])
                                    Nothing)
                             ]
                        else [ StmtExpr (ECall (EVar ("ffi::" <> cFunc)) fixedArgs) ])

            finalStmts = if hasErrorPtr
                         then stmts ++ [ StmtExprNoSemi (ECall (EVar "Ok") [wrapType' (output method) (EVar "buf")]) ]
                         else stmts ++ [ StmtExprNoSemi (wrapType' (output method) (EVar "buf")) ]


        in StmtExprNoSemi (EUnsafe (RsBlock finalStmts))
    else
        let
            callPtr = ECall (EVar ("ffi::" <> cFunc)) allArgExprs
            sizeExpr = constSizeExpr model sizeConst

            stmts = [ StmtLet "ptr" Nothing callPtr
                    , StmtLet "slice" Nothing (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr])
                    , StmtExprNoSemi (wrapType' (output method) (EMethodCall (EMethodCall (EVar "slice") "try_into" []) "unwrap" []))
                    ]
        in StmtExprNoSemi (EUnsafe (RsBlock stmts))

-- | A `const char *` with a `_length` function isn't nul-terminated. A
-- nullable one may be null, which `CStr::from_ptr` is UB on.
generateStringAccessBody :: Bool -> Text -> [RsExpr] -> RsStmt
generateStringAccessBody nullable cFunc macroArgs =
    let
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs
        cstr = ECall (EVar "std::ffi::CStr::from_ptr") [EVar "ptr"]
        toStr = EMethodCall (EMethodCall cstr "to_str" []) "unwrap_or" [ELit (LString "")]
        stmts = [ StmtLet "ptr" Nothing (ECast callPtr (TyPath "*const std::os::raw::c_char"))
                , if nullable
                    then StmtExprNoSemi (EIf (EMethodCall (EVar "ptr") "is_null" [])
                        (RsBlock [StmtExprNoSemi (ELit (LString ""))])
                        (Just (RsBlock [StmtExprNoSemi toStr])))
                    else StmtExprNoSemi toStr
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateAccessBody :: Bool -> Bool -> Text -> [RsExpr] -> Text -> RsStmt
generateAccessBody nullable sizeIsUsize cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        -- Clippy rejects a reflexive `as usize`.
        sizeExpr = if sizeIsUsize then EVar "size" else ECast (EVar "size") (TyPath "usize")

        -- An empty C buffer may be null, and `slice::from_raw_parts` is UB on
        -- null even for length 0.
        copy = EMethodCall (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr]) "to_vec" []
        accessStmts = [ StmtLet "size" Nothing callSize
                , StmtLet "ptr" Nothing callPtr
                , if nullable
                    then StmtExprNoSemi (EIf (EMethodCall (EVar "ptr") "is_null" [])
                        (RsBlock [StmtExprNoSemi (ECall (EVar "Vec::new") [])])
                        (Just (RsBlock [StmtExprNoSemi copy])))
                    else StmtExprNoSemi copy
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

generateEventBytesBody :: Text -> [RsExpr] -> Text -> RsStmt
generateEventBytesBody cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        fromRaw = ECall (EVar "std::slice::from_raw_parts")
                        [EVar "ptr", ECast (EVar "size") (TyPath "usize")]
        accessStmts = [ StmtLet "size" Nothing callSize
                , StmtLet "ptr" Nothing callPtr
                , StmtExprNoSemi (EIf (EBinOp "==" (EVar "size") (ELit (LInt 0)))
                      (RsBlock [StmtExprNoSemi (EVar "&[]")])
                      (Just (RsBlock [StmtExprNoSemi fromRaw])))
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

generateStandardBody :: SemanticModel -> SResource -> SMethod -> CFunctionMapping -> Text -> [CArgSource] -> [RsExpr] -> Bool -> Bool -> RsType -> Text -> RsStmt
generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType okVariant =
    let
        macroArgs = filterArgExprs cArgs argExprs

        (rawElemTyName, mapResult) = case output method of
            SList (SResourceId resName) ->
                let idName = idToRustPascal resName
                    wrapper = idName
                in ("u32", Just wrapper)
            SList t ->
                 case toSafeRsType model t of
                    TyPath p -> (p, Nothing)
                    _        -> ("u8", Nothing)
            _ -> ("u8", Nothing)

        mkMacroCall name args =
            let
                callExpr = ECall (EVar (name <> "!")) args
                mappedExpr = case mapResult of
                    Just wrapper ->
                        EMethodCall
                            (EMethodCall (EMethodCall callExpr "into_iter" []) "map" [EVar wrapper])
                            "collect" []
                    Nothing ->
                        case output method of
                            SResourceId resName ->
                                let idName = idToRustPascal resName
                                    wrapper = idName
                                in EMethodCall callExpr "map" [EVar wrapper]
                            t | isSafeEnum model t ->
                                EMethodCall callExpr "map" [EVar "Into::into"]
                            t ->
                                if any (\r -> resourceName r == case t of SHandle h -> h; _ -> "") (S.resources model)
                                   || case t of SFixedBytes sc _ -> isJust (lookup sc (sizeToType model)); _ -> False
                                then EMethodCall callExpr "map" [ELambda ["val"] (wrapType model res (Just (methodRole method)) t (EVar "val"))]
                                else callExpr
            in StmtExprNoSemi mappedExpr

    in case cSizeFunctionName mapping of
        Just sizeFunc ->
             case methodErrorType method of
                Just _ ->
                    mkMacroCall "ffi_get_vec" ([EVar cFunc, EVar sizeFunc, EVar okVariant] ++ macroArgs)
                Nothing ->
                    mkMacroCall "ffi_get_vec_simple" ([EVar cFunc, EVar sizeFunc, EVar rawElemTyName] ++ macroArgs)
        Nothing ->
            if hasBufferPtr then
                StmtExpr (ECall (EVar "unimplemented!") [ELit (LString "BufferPtr without size function")])
            else if hasErrorPtr
            then
                if rawRetType == TyUnit
                then mkMacroCall "ffi_call_unit" ([EVar cFunc, EVar okVariant] ++ macroArgs)
                else mkMacroCall "ffi_call" ([EVar cFunc, EVar okVariant] ++ macroArgs)
            else
                if rawRetType == TyPath "bool"
                then mkMacroCall "ffi_bool" ([EVar cFunc] ++ filterArgExprs cArgs argExprs)
                else
                    if output method == SString
                    then
                         let call = ECall (EVar ("ffi::" <> cFunc)) argExprs
                             cstr = ECall (EVar "std::ffi::CStr::from_ptr") [call]
                             toStr = EMethodCall (EMethodCall cstr "to_str" []) "unwrap_or" [ELit (LString "")]
                         in StmtReturn (EUnsafe (RsBlock [StmtExprNoSemi toStr]))
                    else if methodReturnNullable method
                              && (case output method of SHandle _ -> True; _ -> False)
                    then
                         let call = ECall (EVar ("ffi::" <> cFunc)) argExprs
                             wrapped = wrapType model res (Just (methodRole method))
                                                (output method) (EVar "raw")
                             -- The body is already in an `unsafe` block, so
                             -- drop the one `wrapType` may add.
                             wrappedBare = case wrapped of
                                 EUnsafe (RsBlock [StmtExprNoSemi e]) -> e
                                 _                                    -> wrapped
                         in StmtExprNoSemi (EUnsafe (RsBlock
                             [ StmtLet "raw" Nothing call
                             , StmtExprNoSemi (EIf
                                 (EMethodCall (EVar "raw") "is_null" [])
                                 (RsBlock [StmtExprNoSemi (EVar "None")])
                                 (Just (RsBlock [StmtExprNoSemi
                                     (ECall (EVar "Some") [wrappedBare])]))) ]))
                    else
                         let call = ECall (EVar ("ffi::" <> cFunc)) argExprs
                             callBlock = EUnsafe (RsBlock [StmtExprNoSemi call])
                             -- A `size_t` C return is already `usize`, and clippy
                             -- rejects the cast `wrapType` adds.
                             wrapped = if output method == SSizeT
                                          && cReturnType mapping == SSizeT
                                       then callBlock
                                       else wrapType model res (Just (methodRole method)) (output method) callBlock
                         in StmtReturn wrapped

constSizeExpr :: SemanticModel -> Text -> RsExpr
constSizeExpr model c =
    let c' = stripRootPrefix model c
    in if T.any isUpper c'
       then EVar c' -- Constant (already usize)
       else ECast (ECall (EVar c') []) (TyPath "usize") -- Function call

resolveArguments :: SemanticModel -> [SParameter] -> [Text] -> [CArgSource] -> ([RsExpr], [RsStmt])
resolveArguments model semParams ctxNames cArgs =
    let (exprs, (stmts, _)) = mapAccumL (resolveOne model semParams ctxNames) ([], -1) cArgs
    in (exprs, stmts)

-- | Accumulator: (Statements, Last Semantic Param Index)
resolveOne :: SemanticModel -> [SParameter] -> [Text] -> ([RsStmt], Int) -> CArgSource -> (([RsStmt], Int), RsExpr)
resolveOne model semParams _ (stmts, _) (SemanticArg i) =
    let param = semParams !! i
        name = paramSnake (paramName param)
        isMutable = paramConstness param == MutableThis
        ptrMethod = if isMutable then "as_mut_ptr" else "as_ptr"

        cstrName = name <> "_cstr"
        extraStmts = case paramType param of
            SString -> [ StmtLet cstrName Nothing (EMacroCall "c_str" [name]) ]
            -- A plain `uint8_t x[CONST]` is read for `CONST` bytes, so a
            -- shorter slice would be an out-of-bounds read.
            SFixedBytes sizeConst _
                | isNothing (lookup sizeConst (sizeToType model))
                , not (T.null sizeConst)
                , T.all (\c -> isAlphaNum c || c == '_') sizeConst ->
                let c' = stripRootPrefix model sizeConst
                in [ StmtExpr (EMacroCall "assert"
                        [ name <> ".len() >= " <> c' ]) ]
            -- One sized by other arguments, like a video plane: C reads that
            -- many.
            SFixedBytes sz _ | isNothing (lookup sz (sizeToType model)) -> sizedBy sz
            SFixedList _ sz _ -> sizedBy sz
            _ -> []
        sizedBy sz =
            [ StmtExpr (EMacroCall "assert" [ name <> ".len() as i64 >= " <> sizerToRust sz ]) ]

        expr = case paramType param of
            SString -> EMethodCall (EVar cstrName) "as_ptr" []
            SBytes          -> EMethodCall (EVar name) ptrMethod []
            SList _         -> EMethodCall (EVar name) ptrMethod []
            SFixedList {}   -> EMethodCall (EVar name) ptrMethod []
            SFixedBytes sizeConst _ ->
                if isJust (lookup sizeConst (sizeToType model))
                then
                    if paramNullable param
                    then EMethodCall (EVar name) "map_or" [EVar "std::ptr::null()", ELambda ["v"] (EMethodCall (EFieldAccess (EVar "v") "0") ptrMethod [])]
                    else EMethodCall (EFieldAccess (EVar name) "0") ptrMethod []
                else EMethodCall (EVar name) ptrMethod []
            SResourceId _   -> EFieldAccess (EVar name) "0"
            SHandle _       -> EFieldAccess (EVar name) "ptr"
            -- Plain enums convert to the `ffi::` enum. Error enums have no safe
            -- wrapper, and clippy rejects a no-op `.into()`.
            t@(SEnum _) | isSafeEnum model t -> EMethodCall (EVar name) "into" []
                        | otherwise          -> EVar name
            _               -> EVar name
    in ((stmts ++ extraStmts, i), expr)

resolveOne _ semParams _ (stmts, lastIdx) BufferSize =
    let expr = if lastIdx >= 0 && lastIdx < length semParams then
            let param = semParams !! lastIdx
                name = paramSnake (paramName param)
            in EMethodCall (EVar name) "len" []
        else ELit (LInt 0)
    in ((stmts, lastIdx), expr)

resolveOne _ _ ctxNames (stmts, lastIdx) (PathObject n _) =
    let name = if n < length ctxNames then ctxNames !! n else error "resolveOne: no such path object"
    in ((stmts, lastIdx), EVar name)

resolveOne _ _ ctxNames (stmts, lastIdx) (PathId n) =
    let idx = n
        name = if idx < length ctxNames then ctxNames !! idx else "unknown_id"
    in ((stmts, lastIdx), EFieldAccess (EVar name) "0")

resolveOne _ _ _ (stmts, lastIdx) ErrorPtr =
    ((stmts, lastIdx), EVar "&mut err")

resolveOne _ _ _ (stmts, lastIdx) (BufferPtr _) =
    ((stmts, lastIdx), EVar "std::ptr::null_mut()") -- Placeholder

resolveOne _ _ ctxNames (stmts, lastIdx) (ThisObject _) =
    ((stmts, lastIdx), EVar (last ctxNames))

resolveOne _ _ _ (stmts, lastIdx) UserData =
    ((stmts, lastIdx), ECast (ECast (EVar "user_data") (TyPath "*mut T")) (TyPath "*mut std::ffi::c_void"))

resolveOne _ _ _ _ arg =
    error $ "Apigen.Language.Rust: Unhandled CArgSource: " ++ show arg


generateContextArgs :: SemanticModel -> ModuleKind -> SResource -> Constness -> SMethod -> [CArgSource] -> ([RsArg], [Text])
generateContextArgs model kind res cns method cArgs =
    let
        path = getHierarchy (resources model) res
        rootRes = case path of
            (top:_) -> find (\r -> resourceName r == top) (resources model)
            []      -> Nothing
        rootIsHandle = case rootRes of
            Just r  -> resourceType r == ResHandle
            Nothing -> False
        fullArgs = case path of
            [] -> []
            (top:rest) ->
                let
                    rootCName = case rootRes of
                        Just r -> cName r
                        Nothing -> error ("generateContextArgs: no resource " <> T.unpack top)

                    ptrType = if cns == MutableThis then "*mut ffi::" <> rootCName else "*const ffi::" <> rootCName
                    rootArg = if rootIsHandle
                              then RsArg (idToRustSnake top) (TyRef Nothing (TyPath (idToRustPascal top)) False)
                              else RsArg (idToRustSnake top) (TyPath ptrType)

                    idArgs = map (toContextArg model cns) rest
                in [rootArg] ++ idArgs

        isCoveredBySelf idx =
            not (methodRole method == StaticRole) &&
            (case kind of
                ExtensionModule _ -> idx == 0 -- self is the base (Root)
                MainModule -> case resourceType res of
                    ResHandle -> idx == length path - 1 -- self is the resource itself
                    ResId _   -> idx == 0 -- self is the parent (Root)
            )

        structRes = case kind of
            MainModule -> case resourceType res of
                ResHandle -> Just res
                ResId _ -> find (\r -> resourceName r == findHandleAncestor (S.resources model) res) (S.resources model)
            ExtensionModule base -> find (\r -> resourceName r == base) (S.resources model)

        selfName = case structRes of
            Just r  -> if K.isView model r then "self.0" else "self.ptr"
            Nothing -> "self.ptr"

        ctxNames = [ if isCoveredBySelf i then selfName
                     else if i == 0 && rootIsHandle then argName (fullArgs !! i) <> ".ptr"
                     else argName (fullArgs !! i)
                   | i <- [0 .. length fullArgs - 1] ]

        isUsed idx = any (matches idx) cArgs
          where
            matches i (PathObject j _) = i == j
            matches i (ThisObject _)   = i == length path - 1
            matches i (PathId j)       = i == j
            matches _ _                = False

        finalArgs = [ arg | (i, arg) <- zip [0..] fullArgs, isUsed i, not (isCoveredBySelf i) ]

    in (finalArgs, ctxNames)

toContextArg :: SemanticModel -> Constness -> Text -> RsArg
toContextArg model cns resName =
    case find (\(r) -> resourceName r == resName) (resources model) of
        Just r -> case resourceType r of
            ResId _ -> toIdArg resName
            ResHandle ->
                let ptrType = case cns of
                        MutableThis -> "*mut ffi::" <> cName r
                        ConstThis   -> "*const ffi::" <> cName r
                in RsArg (idToRustSnake resName) (TyPath ptrType)
        Nothing -> toIdArg resName

argName :: RsArg -> Text
argName (RsArg n _)   = n
argName (RsSelfArg _) = "self"
argName RsSelf        = "self"

toIdArg :: Text -> RsArg
toIdArg resName =
    RsArg (idToRustSnake resName <> "_number") (TyPath (idToRustPascal resName <> "Number"))

toSafeArg :: SemanticModel -> [SParameter] -> (CArgSource, Int) -> Maybe RsArg
toSafeArg model semParams (SemanticArg i, _) =
    let param = semParams !! i
        name = paramSnake (paramName param)
        baseTy = toSafeRsType model (paramType param)
        ty = if paramConstness param == MutableThis
             then case baseTy of
                 TyPath p -> if "*const" `T.isPrefixOf` p
                             then TyPath ("*mut" <> T.drop 6 p)
                             else if "&[u8]" == p
                             then TyPath "&mut [u8]"
                             else if T.drop 1 p `elem` map snd (sizeToType model)
                             then TyPath ("&mut " <> T.drop 1 p)
                             else baseTy
                 _ -> baseTy
             else baseTy

        safeTy = case ty of
            TyPath _ | paramNullable param -> TyGeneric "Option" [ty]
            TySlice _                      -> TyRef Nothing ty False
            _                              -> ty
    in Just (RsArg name safeTy)
toSafeArg _ _ _ = Nothing

toSafeRetType :: SemanticModel -> SResource -> Maybe SMethodRole -> SType -> RsType
toSafeRetType model res mRole t =
    case t of
        SHandle n ->
            let name = idToRustPascal n
                isLife = any (\r -> resourceName r == n && hasLifetime model r) (S.resources model)
                resName = if isLife then name <> "<'a>" else name
            in if n == "void" then TyPath "*mut std::ffi::c_void"
               else if n `elem` ["uint8_t", "char", "unsigned char"] then TyPath "u8"
               else if n == resourceName res && mRole == Just Constructor
               then TyPath "Self"
               else if any (\r -> resourceName r == n) (S.resources model)
               then TyPath resName
               else error ("toSafeRetType: no handle " <> T.unpack n)
        SString -> if mRole == Just StaticRole then TyPath "&'static str" else TyPath "&str"
        SBytes  -> TyGeneric "Vec" [TyPath "u8"]
        SFixedBytes sizeConst _ ->
            case lookup sizeConst (sizeToType model) of
                Just ty -> TyPath ty
                Nothing ->
                    if not (T.null sizeConst) && T.all (\c -> isAlphaNum c || c == '_') sizeConst
                    then
                        let c' = stripRootPrefix model sizeConst
                        in TyArray (TyPath "u8") c'
                    else TyGeneric "Vec" [TyPath "u8"]
        SList inner -> TyGeneric "Vec" [toSafeRetType model res mRole inner]
        _ -> toSafeRsType model t

toSafeRsType :: SemanticModel -> SType -> RsType
toSafeRsType _ (SInt 8)   = TyPath "i8"
toSafeRsType _ (SInt 16)  = TyPath "i16"
toSafeRsType _ (SInt 32)  = TyPath "i32"
toSafeRsType _ (SInt 64)  = TyPath "i64"
toSafeRsType _ (SUInt 8)  = TyPath "u8"
toSafeRsType _ (SUInt 16) = TyPath "u16"
toSafeRsType _ (SUInt 32) = TyPath "u32"
toSafeRsType _ (SUInt 64) = TyPath "u64"
toSafeRsType _ SSizeT     = TyPath "usize"
toSafeRsType _ SBool      = TyPath "bool"
toSafeRsType _ SString            = TyPath "&str"
toSafeRsType _ SBytes             = TyPath "&[u8]"
toSafeRsType model (SFixedBytes sizeConst _) =
    case lookup sizeConst (sizeToType model) of
        Just ty -> TyPath ("&" <> ty)
        Nothing -> TyPath "&[u8]"

toSafeRsType model (SFixedList t _ _) = TySlice (toSafeRsType model t)
toSafeRsType model (SList t)          = TySlice (toSafeRsType model t)
toSafeRsType _ SVoid = TyUnit
toSafeRsType model (SEnum n) =
    let cName = enumCName model n
    in case safeEnumName cName of
        Just safe -> TyPath safe
        Nothing   -> TyPath ("ffi::" <> cName)
toSafeRsType _ (SResourceId n) =
    TyPath (idResourceType n)
toSafeRsType model (SHandle n) =
    if n == "void" then TyPath "*mut std::ffi::c_void"
    else
        case find (\r -> resourceName r == n) (resources model) of
            Just r  ->
                let name = idToRustPascal n
                    isLife = hasLifetime model r
                    params = [ "'a" | isLife ] ++ [ "H" | K.keepsHandler model r ]
                    resName = if null params then name else name <> "<" <> T.intercalate ", " params <> ">"
                in TyRef Nothing (TyPath resName) False
            Nothing -> error ("toSafeRsType: no handle " <> T.unpack n)
toSafeRsType model (SCallback n) =
    let cName = case find (\(cb) -> S.cbName cb == n) (S.callbacks model) of
            Just cb -> S.cbCName cb
            Nothing -> error ("toSafeRsType: no callback " <> T.unpack n)
    in TyPath ("ffi::" <> cName)
toSafeRsType _ _ = TyPath "()"

-- | Error enums keep their C name.
safeEnumName :: Text -> Maybe Text
safeEnumName cName
    | "_Err_" `T.isInfixOf` cName = Nothing
    | otherwise                   = Just (idToRustPascal cName)

isSafeEnum :: SemanticModel -> SType -> Bool
isSafeEnum model (SEnum n) =
    case find (\e -> S.enumSemanticName e == n) (S.enums model) of
        Just e  -> isJust (safeEnumName (S.enumName e))
        Nothing -> False
isSafeEnum _ _ = False

filterArgExprs :: [CArgSource] -> [RsExpr] -> [RsExpr]
filterArgExprs sources exprs =
    let zipped = zip sources exprs
        filtered = filter (\(s, _) -> case s of
            ErrorPtr    -> False
            BufferPtr _ -> False -- Filter buffer for macros that handle it
            _           -> True) zipped
    in map snd filtered

wrapType :: SemanticModel -> SResource -> Maybe SMethodRole -> SType -> RsExpr -> RsExpr
wrapType model res mRole t expr = case t of
    SHandle h ->
        case find (\r -> resourceName r == h) (S.resources model) of
            Just r ->
                let structName = if h == resourceName res && mRole == Just Constructor
                                 then "Self"
                                 else idToRustPascal h
                in if isVariantResource model r
                   -- `from_ptr` is `unsafe`, but the arg already has its own
                   -- `unsafe` block, which would nest (clippy `unused_unsafe`).
                   then let inner = case expr of EUnsafe (RsBlock [StmtExprNoSemi e]) -> e
                                                 _ -> expr
                        in EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar (structName <> "::from_ptr")) [inner])])
                   else if K.isView model r
                   then ECall (EVar structName) [ERef (EDeref expr) False]
                   else EStructInit structName $
                        [("ptr", expr)]
                        ++ [ ("handler", ECall (EVar "Box::new") [EVar "handler"]) | K.keepsHandler model r, mRole == Just Constructor ]
                        ++ [ ("_marker", EVar "std::marker::PhantomData") | hasLifetime model r ]
            Nothing -> expr
    SFixedBytes sizeConst _ ->
        case lookup sizeConst (sizeToType model) of
            Just ty -> ECall (EVar ty) [expr]
            Nothing -> expr
    SEnum _ | isSafeEnum model t -> EMethodCall expr "into" []
    SResourceId n ->
        let name = idToRustPascal n
            wrapper = name
        in ECall (EVar wrapper) [expr]
    SSizeT -> ECast expr (TyPath "usize")
    _ -> expr


-- ---------------------------------------------------------------------------
-- Safe wrappers on `tox::Tox`, `tox::ToxAV` and `tox::Options`.
-- ---------------------------------------------------------------------------

safeMethodName :: SemanticModel -> SResource -> Text -> Text
safeMethodName model res core = fromMaybe mechanical (lookup core Crate.methodRenames)
  where
    mechanical
        | K.isSettings model res =
            if "set_" `T.isPrefixOf` core then core else "set_" <> core
        | Just r <- T.stripPrefix "self_get_" core = r
        | Just r <- T.stripPrefix "self_set_" core = "set_" <> r
        | Just r <- T.stripPrefix "get_" core      = r
        | otherwise                                = core

safeInScope :: SemanticModel -> SResource -> SMethod -> Bool
safeInScope model res m =
    methodRole m `notElem` [Constructor, Destructor, StaticRole]
    && not (methodHasUserData m)
    && S.methodName m `notElem` Crate.handWrittenSafe
    && not (returnsHandle (output m))
    && not (takesRawPointers m)
    && (not (K.isSettings model res) || "_set_" `T.isInfixOf` S.methodName m)
  where
    returnsHandle (SHandle _)             = True
    returnsHandle (SList (SResourceId _)) = True
    returnsHandle _                       = False

-- | @prefix@ is the path from `self` to `core`.
safeWrapper :: SemanticModel -> SResource -> Text -> SMethod -> Maybe RsItem
safeWrapper model res prefix method
    | not (safeInScope model res method) = Nothing
    | otherwise = case generateMethod model MainModule res method of
        (RsItemFn core : _) -> Just (mkWrapper core)
        _                   -> Nothing
  where
    mkWrapper core =
        let coreName = fnName core
            recv     = [ a | a@RsSelfArg{} <- fnArgs core ]
            valArgs  = [ a | a@RsArg{} <- fnArgs core ]
            argExprs = [ EVar n | RsArg n _ <- valArgs ]
            target   = foldl EFieldAccess (EVar "self") (T.splitOn "." prefix)
            call     = EMethodCall target coreName argExprs
            (safeRet, body) = case fnRet core of
                Just (TyGeneric "std::result::Result" [t, errTy]) ->
                    ( Just (TyGeneric "Result" [t])
                    , EMethodCall call "map_err"
                        [EVar (Crate.errorType <> "::" <> errorVariant (namespaces model) (errName errTy))] )
                other -> (other, call)
        in RsItemFn RsFn
            { fnName     = safeMethodName model res coreName
            , fnVis      = Pub
            , fnAbi      = Nothing
            , fnGenerics = []
            , fnArgs     = recv ++ valArgs
            , fnRet      = safeRet
            , fnBody     = Just (RsBlock [StmtExprNoSemi body])
            , fnUnsafe   = False
            , fnDoc      = []
            }
    errName (TyPath p) = p
    errName t          = error ("safeWrapper: error type " <> show t)

-- | Handles with a safe-wrapper module: `Tox`, `AV`, `Options`, `Pass_Key`.
safeHandleResources :: SemanticModel -> [SResource]
safeHandleResources model =
    [ r
    | r <- S.resources model
    , resourceType r == ResHandle
    , any ((== Constructor) . methodRole) (S.methods r)
    -- A list of views, like the events, has a hand-written safe layer.
    , not (K.isViewList model r)
    ]

-- | The `Tox`, `ToxAV` and `Options` structs are hand-written.
generateSafeModules :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateSafeModules model items =
    [ safeFileFor handle res
    | handle <- safeHandleResources model
    , let name = idToRustPascal (resourceName handle)
    , Just res <- [mainModuleResource name]
    ]
  where
    mainModuleResource name =
        fmap snd (find isTarget items)
      where
        isTarget (MainModule, r) = idToRustPascal (resourceName r) == name
        isTarget _               = False

    safeFileFor handle res =
        case lookup (idToRustPascal (resourceName handle)) Crate.handleGlue of
            Just glue -> generateFlatHandle model res glue
            Nothing   -> generatePassKeyModule model res

generateFlatHandle :: SemanticModel -> SResource -> HandleGlue -> (FilePath, Text)
generateFlatHandle model res glue =
    mkSafeFile (hgFile glue)
        (hgImports glue)
        (RsItemImpl (RsImpl Nothing (hgImplType glue) (hgGenerics glue)
            (mapMaybe (safeWrapper model res (hgField glue)) (S.methods res))))

-- | Static helpers become module functions, except the `*_length` ones.
generatePassKeyModule :: SemanticModel -> SResource -> (FilePath, Text)
generatePassKeyModule model res =
    ( "safe_" <> T.unpack (idToRustSnake (resourceName res)) <> ".rs"
    , T.unlines
        ( [ "// Generated by apigen. Do not edit."
          , "use crate::core;"
          , "use crate::ffi;"
          , "use crate::types::*;"
          , ""
          , "pub struct " <> typeName <> "(core::" <> typeName <> ");"
          , ""
          , "impl " <> typeName <> " {"
          ]
       ++ indentNonEmpty (T.intercalate "\n" (ctorFns ++ instanceFns))
       ++ [ "}" ]
       ++ ("" : T.lines (T.intercalate "\n" moduleFns)) ) )
  where
    typeName = idToRustPascal (resourceName res)

    coreFn m = case generateMethod model MainModule res m of
        (RsItemFn fn : _) -> Just fn
        _                 -> Nothing

    argText (RsArg n ty)      = n <> ": " <> prettyType ty
    argText (RsSelfArg False) = "&self"
    argText (RsSelfArg True)  = "&mut self"
    argText RsSelf            = "self"

    valArgs fn  = [ a | a@RsArg{} <- fnArgs fn ]
    argExprs fn = [ n | RsArg n _ <- valArgs fn ]

    adapt rewrapHandle fn =
        case fnRet fn of
            Just (TyGeneric "std::result::Result" [t, TyPath errTy]) ->
                let t'   = if rewrapHandle then TyPath typeName else t
                    mapH = if rewrapHandle then ".map(" <> typeName <> ")" else ""
                in ( Just (prettyType (TyGeneric "Result" [t']))
                   , mapH <> ".map_err(" <> Crate.errorType <> "::" <> errorVariant (namespaces model) errTy <> ")" )
            other -> (fmap prettyType other, "")

    renderFn recv receiver rename rewrapHandle m = do
        fn <- coreFn m
        let (ret, suffix) = adapt rewrapHandle fn
            args = recv ++ map argText (valArgs fn)
            call = receiver <> fnName fn
                <> "(" <> T.intercalate ", " (argExprs fn) <> ")" <> suffix
        Just $ T.unlines
            [ "pub fn " <> rename (fnName fn)
                <> "(" <> T.intercalate ", " args <> ")"
                <> maybe "" (" -> " <>) ret <> " {"
            , "    " <> call
            , "}"
            ]

    methodsOf p = filter p (S.methods res)

    ctorFns = mapMaybe
        (renderFn [] ("core::" <> typeName <> "::") id True)
        (methodsOf ((== Constructor) . methodRole))

    instanceFns = mapMaybe
        (renderFn ["&self"] "self.0." id False)
        (methodsOf ((== ActionRole) . methodRole))

    rawModuleFns = methodsOf (\m -> methodRole m == StaticRole && not (null (S.inputs m)))
    isStringizer m = "to_string" `T.isSuffixOf` S.methodName m
    (stringizers, helpers) = partition isStringizer rawModuleFns
    moduleFns = mapMaybe
        (renderFn [] ("core::" <> typeName <> "::") stripPassPrefix False)
        (stringizers ++ helpers)

    stripPassPrefix n = fromMaybe n (T.stripPrefix "pass_" n)

indentNonEmpty :: Text -> [Text]
indentNonEmpty = map indentLine . T.lines
  where
    indentLine "" = ""
    indentLine l  = "    " <> l

mkSafeFile :: FilePath -> [Text] -> RsItem -> (FilePath, Text)
mkSafeFile name imports item =
    ( name
    , T.unlines ("// Generated by apigen. Do not edit." : imports)
      <> "\n"
      <> render (RsModule [item])
    )

-- ---------------------------------------------------------------------------
-- Callback dispatch: handler traits, trampolines and registration.
-- ---------------------------------------------------------------------------

enumCName :: SemanticModel -> Text -> Text
enumCName model sem =
    case findEnum model sem of
        Just e  -> S.enumName e
        Nothing -> error ("enumCName: no enum " <> T.unpack sem)

handlerArgType :: SemanticModel -> SParameter -> Text
handlerArgType model p = case paramType p of
    SResourceId n           -> idResourceType n
    SBytes                  -> "&[u8]"
    SString                 -> "&[u8]"
    SBool                   -> "bool"
    SSizeT                  -> "usize"
    SInt b                  -> "i" <> T.pack (show b)
    SUInt b                 -> "u" <> T.pack (show b)
    SEnum n                 -> fromMaybe (enumCName model n) (safeEnumName (enumCName model n))
    SFixedBytes sz _
        | Just ty <- lookup sz (sizeToType model) -> ty
        | otherwise                               -> "&[u8]"
    SFixedList el _ _       -> "&[" <> scalarRsType el <> "]"
    _                       -> "u32"

-- | An id type's Rust type, from @crate::types@: @FriendNumber@.
idResourceType :: Text -> Text
idResourceType = idToRustPascal

scalarRsType :: SType -> Text
scalarRsType (SInt b)  = "i" <> T.pack (show b)
scalarRsType (SUInt b) = "u" <> T.pack (show b)
scalarRsType _         = "u8"

-- | Adds back the `size_t` the model drops after each byte array.
rawCArgs :: SemanticModel -> SParameter -> [(Text, Text)]
rawCArgs model p =
    -- Keywords get `type_`, not `r#type`, as in the safe wrappers.
    let nm = paramSnake (paramName p)
    in case paramType p of
        SResourceId _     -> [(nm, "u32")]
        SBytes            -> [(nm, "*const u8"), (nm <> "_len", "usize")]
        SString           -> [(nm, "*const u8"), (nm <> "_len", "usize")]
        SBool             -> [(nm, "bool")]
        SSizeT            -> [(nm, "usize")]
        SInt b            -> [(nm, "i" <> T.pack (show b))]
        SUInt b           -> [(nm, "u" <> T.pack (show b))]
        SEnum n           -> [(nm, "ffi::" <> enumCName model n)]
        SFixedBytes _ _   -> [(nm, "*const u8")]
        SFixedList el _ _ -> [(nm, "*const " <> scalarRsType el)]
        _                 -> [(nm, "u32")]

convertArg :: SemanticModel -> SParameter -> ([Text], Text)
convertArg model p =
    -- Must match the names 'rawCArgs' uses.
    let nm = paramSnake (paramName p)
    in case paramType p of
        SResourceId n   -> ([], "crate::types::" <> idResourceType n <> "(" <> nm <> ")")
        SBytes          -> ([ "let " <> nm <> "_slice = unsafe { safe_slice(" <> nm <> ", " <> nm <> "_len) };" ]
                           , nm <> "_slice")
        SString         -> ([ "let " <> nm <> "_slice = unsafe { safe_slice(" <> nm <> ", " <> nm <> "_len) };" ]
                           , nm <> "_slice")
        SBool           -> ([], nm)
        SSizeT          -> ([], nm)
        SInt _          -> ([], nm)
        SUInt _         -> ([], nm)
        SEnum _         -> ([], nm <> ".into()")
        SFixedBytes sz _
            | Just ty <- lookup sz (sizeToType model) ->
                let szConst = stripRootPrefix model sz
                in ( [ "let mut " <> nm <> "_arr = [0u8; " <> szConst <> "];"
                     , "let " <> nm <> "_src = unsafe { slice::from_raw_parts(" <> nm
                         <> ", " <> szConst <> ") };"
                     , nm <> "_arr.copy_from_slice(" <> nm <> "_src);" ]
                   , "crate::types::" <> ty <> "(" <> nm <> "_arr)" )
            | otherwise ->
                ( [ "let " <> nm <> "_size = (" <> sizerToRust sz <> ") as usize;"
                  , "let " <> nm <> "_slice = unsafe { slice::from_raw_parts(" <> nm
                      <> ", " <> nm <> "_size) };" ]
                , nm <> "_slice" )
        SFixedList _ sz _ ->
            ( [ "let " <> nm <> "_count = (" <> sizerToRust sz <> ") as usize;"
              , "let " <> nm <> "_slice = unsafe { slice::from_raw_parts(" <> nm
                  <> ", " <> nm <> "_count) };" ]
            , nm <> "_slice" )
        _               -> ([], nm)

-- | Operands are cast to `i64` so nothing overflows before the final
-- `as usize`.
sizerToRust :: Text -> Text
sizerToRust t = maybe (error ("sizerToRust: can't parse " <> T.unpack t)) expr (parseSizeExpr t)
  where
    expr e = case e of
        -- Clippy rejects `as i64` on a literal (`unnecessary_cast`).
        SizeNum n         -> T.pack (show n) <> "_i64"
        SizeVar v         -> v <> " as i64"
        SizeOp op x y     -> operand x <> " " <> T.singleton op <> " " <> operand y
        SizeCall "abs" [x] -> "(" <> expr x <> ").abs()"
        SizeCall f xs     -> "std::cmp::" <> f <> "(" <> T.intercalate ", " (map expr xs) <> ")"
    operand x = case x of
        SizeOp{} -> "(" <> expr x <> ")"
        _        -> expr x

renderTrampoline :: SemanticModel -> Text -> SEvent -> Text
renderTrampoline model macroName ev =
    let fnName = "dispatch_" <> eventName ev
        rawArgs = concatMap (rawCArgs model) (eventParams ev)
        rawSig  = T.intercalate ", " [ n <> ": " <> ty | (n, ty) <- rawArgs ]
        -- Size-only parameters get `let`s, for the sizers that use them, but
        -- aren't passed on.
        conv p  = (handlerVisible ev p, convertArg model p)
        conversions = map conv (eventParams ev)
        lets = concatMap (fst . snd) conversions
        callArgs = T.intercalate ", "
            [ snd c | (visible, c) <- conversions, visible ]
    in T.unlines
        ( [ macroName <> "!("
          , "    " <> fnName <> ","
          , "    (" <> rawSig <> "),"
          , "    |handler: &mut H| {"
          ]
       ++ [ "        " <> l | l <- lets ]
       ++ [ "        handler.on_" <> eventName ev <> "(" <> callArgs <> ");"
          , "    }"
          , ");"
          ] )

-- | Identifiers used in a sibling array's sizer, e.g. @sample_count@ in
-- @pcm[sample_count * channels]@. The slice already has that length.
sizerIdentifiers :: SEvent -> [Text]
sizerIdentifiers ev = nub
    [ ident
    | p <- eventParams ev
    , sz <- case paramType p of
                SFixedList _ s _ -> [s]
                SFixedBytes s _  -> [s]
                _                -> []
    , Just e <- [parseSizeExpr sz]
    , ident <- sizeExprVars e
    ]

handlerVisible :: SEvent -> SParameter -> Bool
handlerVisible ev p = case paramType p of
    SSizeT -> paramSnake (paramName p) `notElem` sizerIdentifiers ev
    _      -> True

renderTraitMethod :: SemanticModel -> SEvent -> Text
renderTraitMethod model ev =
    let -- `_type` needs no `r#` escape.
        plainSnake = T.toLower . T.pack . Casing.toSnake . Casing.fromAny . T.unpack
        params = [ "_" <> plainSnake (paramName p) <> ": "
                     <> handlerArgType model p
                 | p <- eventParams ev, handlerVisible ev p ]
        allParams = "&mut self" : params
    in "    fn on_" <> eventName ev <> "(" <> T.intercalate ", " allParams <> ") {}"

-- | Handles with callbacks: the root, which takes its handler when it
-- iterates, then those keeping one.
callbackHandles :: SemanticModel -> [SResource]
callbackHandles model =
    [ r | r <- S.resources model, isRoot r, not (null (events r)) ]
    ++ filter (K.keepsHandler model) (S.resources model)

-- | The macro defining a handle's trampolines: @define_av_dispatch@, and the
-- root's @define_dispatch@.
dispatchMacroName :: SResource -> Text
dispatchMacroName r
    | isRoot r  = "define_dispatch"
    | otherwise = "define_" <> idToRustSnake (resourceName r) <> "_dispatch"

generateDispatchModule :: SemanticModel -> (FilePath, Text)
generateDispatchModule model =
    let handles = callbackHandles model
        root = find isRoot (S.resources model)

        handlerTraitFor r = T.unlines
            ( ("pub trait " <> handlerTrait r <> " {")
            : map (renderTraitMethod model) (events r)
           ++ ["}"] )

        -- The root registers its callbacks each time it iterates, with the
        -- handler the iteration takes.
        iterateFn r = case find ((== IterateRole) . methodRole) (S.methods r) of
            Nothing -> ""
            Just it ->
                let var = idToRustSnake (resourceName r)
                in T.unlines
                    ( [ "pub fn " <> S.methodName it <> "<H: " <> handlerTrait r <> ">(" <> var <> ": &" <> idToRustPascal (resourceName r) <> ", handler: &mut H) {"
                      , "    let " <> var <> "_ptr = " <> var <> ".ptr;"
                      , "    unsafe {" ]
                   ++ [ "        ffi::" <> cPrefix r <> "callback_" <> eventName ev <> "(" <> var <> "_ptr, Some(dispatch_"
                          <> eventName ev <> "::<H>));"
                      | ev <- events r ]
                   ++ [ "        ffi::" <> S.methodName it <> "(" <> var <> "_ptr, handler as *mut H as *mut c_void);"
                      , "    }"
                      , "}" ] )

        -- A handle keeping its handler registers its callbacks once.
        registerFn r =
            let var = idToRustSnake (resourceName r)
            in T.unlines
                ( [ "pub(crate) unsafe fn register_" <> var <> "_callbacks<H: " <> handlerTrait r <> ">("
                  , "    " <> var <> "_ptr: *mut ffi::" <> cName r <> ","
                  , "    handler: &mut H,"
                  , ") {"
                  , "    unsafe {" ]
               ++ concat
                  [ [ "        ffi::" <> cPrefix r <> "callback_" <> eventName ev <> "("
                    , "            " <> var <> "_ptr,"
                    , "            Some(dispatch_" <> eventName ev <> "::<H>),"
                    , "            handler as *mut H as *mut c_void,"
                    , "        );" ]
                  | ev <- events r ]
               ++ [ "    }"
                  , "}" ] )

        registration r = if isRoot r then iterateFn r else registerFn r

        trampolines r = T.intercalate "\n"
            (map (renderTrampoline model (dispatchMacroName r)) (events r))

        content = T.unlines $
            [ "// Generated by apigen. Do not edit."
            , "//"
            , "// The callback dispatch layer: a handler trait per handle with"
            , "// callbacks, one `extern \"C\"` trampoline per event and the"
            , "// callback-registration entry points."
            , "#![allow(unused_imports)]"
            , "#![allow(clippy::too_many_arguments)]"
            ]
            ++ [ "use super::" <> structModule model (resourceName r) <> "::" <> idToRustPascal (resourceName r) <> ";" | Just r <- [root] ]
            ++
            [ "use crate::ffi;"
            , "use crate::types::*;"
            , "use std::os::raw::c_void;"
            , "use std::slice;"
            , ""
            , "/// Builds a slice from a raw pointer + length, treating a zero length as"
            , "/// an empty slice so a null/dangling pointer is never dereferenced."
            , "unsafe fn safe_slice<'a, T>(ptr: *const T, len: usize) -> &'a [T] {"
            , "    if len == 0 {"
            , "        &[]"
            , "    } else {"
            , "        unsafe { slice::from_raw_parts(ptr, len) }"
            , "    }"
            , "}"
            , ""
            ]
            ++ map handlerTraitFor handles
            ++ map registration handles
            ++ [ T.intercalate "\n" (map dispatchMacro handles) ]
            ++ drop 1 (concat [ ["", trampolines r] | r <- handles ])
    in ("dispatch.rs", content)

-- | A panic in the handler must not unwind into C.
dispatchMacro :: SResource -> Text
dispatchMacro r = T.unlines
    [ "macro_rules! " <> dispatchMacroName r <> " {"
    , "    ($func_name:ident, ($($arg_name:ident: $arg_type:ty),*), $body:expr) => {"
    , "        unsafe extern \"C\" fn $func_name<H: " <> handlerTrait r <> ">("
    , "            _" <> idToRustSnake (resourceName r) <> ": *mut ffi::" <> cName r <> ","
    , "            $($arg_name: $arg_type),*,"
    , "            user_data: *mut c_void,"
    , "        ) {"
    , "            if !user_data.is_null() {"
    , "                let handler = unsafe { &mut *(user_data as *mut H) };"
    , "                // AssertUnwindSafe: a panic could leave `&mut H` inconsistent,"
    , "                // but unwinding across the FFI boundary is UB, so the panic"
    , "                // must be caught here regardless."
    , "                let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {"
    , "                    $body(handler)"
    , "                }));"
    , "            }"
    , "        }"
    , "    }"
    , "}"
    ]

-- ---------------------------------------------------------------------------
-- Resource wrappers: `Friend`, `Group`, `File` and `Conference`.
-- ---------------------------------------------------------------------------

data ResWrapper = ResWrapper
    { rwName      :: Text       -- ^ Wrapper struct name (e.g. @Friend@).
    , rwIdType    :: Text       -- ^ Primary id newtype (e.g. @FriendNumber@).
    , rwExtraId   :: Maybe (Text, Text) -- ^ Extra id field @(name, type)@ (File only).
    , rwResources :: [Text]     -- ^ Model resource names contributing methods.
    }

-- | Ids a wrapper carries besides its own: a `File` is addressed by friend
-- and file number, but the model keys it on the file number alone.

resWrappers :: SemanticModel -> [ResWrapper]
resWrappers model =
    [ ResWrapper
        { rwName      = idToRustPascal (resourceName r)
        , rwIdType    = idToRustPascal idTypeName
        , rwExtraId   = K.idScope model (resourceName r)
        , rwResources = resourceName r : foldedSubResources r
        }
    | r <- S.resources model
    , Just idTypeName <- [idResourceIdType r]
    , parent r == Just rootResourceName
    ]
  where
    rootResourceName =
        rootName model

    idResourceIdType res = case resourceType res of
        ResId (SResourceId n) -> Just n
        _                     -> Nothing

    foldedSubResources wrapper =
        map resourceName $
        sortOn (\r -> (length (T.splitOn "_" (resourceName r)), resourceName r))
            [ r
            | r <- S.resources model
            , parent r == Just (resourceName wrapper)
            , isJust (idResourceIdType r)
            , not (null (S.methods r))
            ]

resWrapperMethodName :: SemanticModel -> ResWrapper -> SMethod -> Text -> Text
resWrapperMethodName model rw method cfn = isPrefixed (paramSnake (deGet stripped))
  where
    bare = fromMaybe cfn (T.stripPrefix (rootNamespace model <> "_") cfn)
    -- Only a leading `self_`: `tox_group_self_get_name` keeps its.
    deSelf = fromMaybe bare (T.stripPrefix "self_" bare)
    -- `tox_group_peer_*` keeps its `peer_`.
    primaryPrefix = idToRustSnake (rwName rw) <> "_"
    stripped = fromMaybe deSelf (T.stripPrefix primaryPrefix deSelf)
    isGetter = "get_" `T.isPrefixOf` stripped || "_get_" `T.isInfixOf` stripped
    deGet n
        | Just r <- T.stripPrefix "get_" n = r
        | "_get_" `T.isInfixOf` n          = T.replace "_get_" "_" n
        | otherwise                        = n
    isPrefixed n
        | isGetter
        , output method == SBool
        , not ("is_" `T.isPrefixOf` n)
        , not ("has_" `T.isPrefixOf` n)
        = "is_" <> n
        | otherwise = n

resWrapper :: SemanticModel -> ResWrapper -> SResource -> SMethod -> Maybe RsItem
resWrapper model rw res method
    | methodRole method `elem` [Constructor, Destructor, StaticRole] = Nothing
    | methodHasUserData method = Nothing
    | S.methodName method `elem` Crate.handWrittenWrappers = Nothing
    | otherwise = case generateMethod model MainModule res method of
        (RsItemFn core : _) -> Just (mkWrapper core)
        _                   -> Nothing
  where
    cfn = S.methodName method
    idFields = (rwIdType rw, "self.number")
             : maybe [] (\(n, t) -> [(t, "self." <> n)]) (rwExtraId rw)

    mkWrapper core =
        let valArgs = [ a | a@RsArg{} <- fnArgs core ]
            classify a@(RsArg _ ty) = case ty of
                TyPath p | Just selfExpr <- lookup p idFields -> Left selfExpr
                _                                             -> Right a
            classify a = Right a
            classified = map classify valArgs
            -- A kept parameter loses the wrapper's prefix (`conference_peer_number`
            -- on `Conference`).
            wrapperPrefix = idToRustSnake (rwName rw) <> "_"
            stripPrefix (RsArg n ty)
                | Just bare <- T.stripPrefix wrapperPrefix n = RsArg bare ty
            stripPrefix a = a
            keptArgs   = [ stripPrefix a | Right a <- classified ]
            callArgs   = [ either EVar argVar (fmap stripPrefix c) | c <- classified ]
            argVar a = case a of
                RsArg n _ -> EVar n
                _         -> error "resWrapperMethod: not a plain argument"
            recv = if K.endsId model cfn then RsSelf else RsSelfArg False
            target = foldl EFieldAccess (EVar "self") Crate.wrapperCore
            call = EMethodCall target (fnName core) callArgs
            (safeRet, body) = case fnRet core of
                Just (TyGeneric "std::result::Result" [t, errTy]) ->
                    ( Just (TyGeneric "Result" [t])
                    , EMethodCall call "map_err"
                        [EVar (Crate.errorType <> "::" <> errorVariant (namespaces model) (errPath errTy))] )
                other -> (other, call)
        in RsItemFn RsFn
            { fnName     = resWrapperMethodName model rw method cfn
            , fnVis      = Pub
            , fnAbi      = Nothing
            , fnGenerics = []
            , fnArgs     = recv : keptArgs
            , fnRet      = safeRet
            , fnBody     = Just (RsBlock [StmtExprNoSemi body])
            , fnUnsafe   = False
            , fnDoc      = []
            }
    errPath (TyPath p) = p
    errPath t          = error ("resWrapper: error type " <> show t)

sizeCompanionFunctions :: SemanticModel -> [Text]
sizeCompanionFunctions model = nub
    [ sf
    | r <- S.resources model
    , m <- S.methods r
    , CustomMapping cm <- [methodMapping m]
    , Just sf <- [cSizeFunctionName cm]
    ]

resWrapperMethods :: SemanticModel -> ResWrapper -> [RsItem]
resWrapperMethods model rw =
    let sizeCompanions = sizeCompanionFunctions model
        ownMethods =
            [ (r, m)
            | rn <- rwResources rw
            , r  <- maybe [] (:[]) (find ((== rn) . resourceName) (S.resources model))
            , m  <- S.methods r
            , S.methodName m `notElem` sizeCompanions
            ]
        rootRes = find isRoot (S.resources model)
        adopted =
            [ (tr, m)
            | (w, c) <- K.functionOwners model, idToRustPascal w == rwName rw
            , tr <- maybe [] (:[]) rootRes
            , m  <- S.methods tr, S.methodName m == c
            ]
    in mapMaybe (\(r, m) -> resWrapper model rw r m) (ownMethods ++ adopted)

resWrapperGetters :: ResWrapper -> [RsItem]
resWrapperGetters rw = [ getter n t f | (n, t, f) <- Crate.wrapperGetters (rwName rw) (rwIdType rw) ]
  where
    getter name retTy field = RsItemFn RsFn
        { fnName     = name
        , fnVis      = Pub
        , fnAbi      = Nothing
        , fnGenerics = []
        , fnArgs     = [RsSelfArg False]
        , fnRet      = Just (TyPath retTy)
        , fnBody     = Just (RsBlock [StmtExprNoSemi (EFieldAccess (EVar "self") field)])
        , fnUnsafe   = False
        , fnDoc      = []
        }

resWrapperHeader :: SemanticModel -> ResWrapper -> Text
resWrapperHeader model rw =
    let n = rwName rw
        fields = case rwExtraId rw of
            Just (_, ft) -> [ "    pub(crate) friend: " <> ft <> ","
                             , "    pub(crate) number: " <> rwIdType rw <> ","
                             ]
            Nothing       -> [ "    pub(crate) number: " <> rwIdType rw <> "," ]
        dbgFields = case rwExtraId rw of
            Just _ -> [ "            .field(\"friend\", &self.friend)"
                            , "            .field(\"number\", &self.number)"
                            ]
            Nothing      -> [ "            .field(\"number\", &self.number)" ]
        ctxLetter = T.toLower (T.take 1 n)
        fromImpls = case rwExtraId rw of
            -- File carries two ids, so has no `From`.
            Just _  -> ""
            Nothing -> T.unlines
                [ "impl<'a> From<" <> n <> "<'a>> for " <> rwIdType rw <> " {"
                , "    fn from(" <> ctxLetter <> ": " <> n <> "<'a>) -> Self {"
                , "        " <> ctxLetter <> ".number"
                , "    }"
                , "}"
                , ""
                , "impl<'a> From<&" <> n <> "<'a>> for " <> rwIdType rw <> " {"
                , "    fn from(" <> ctxLetter <> ": &" <> n <> "<'a>) -> Self {"
                , "        " <> ctxLetter <> ".number"
                , "    }"
                , "}"
                , ""
                ]
    in T.unlines
        ( [ "#[derive(Clone, Copy)]"
          , "pub struct " <> n <> "<'a> {"
          , "    pub(crate) " <> idToRustSnake (rootName model) <> ": &'a " <> idToRustPascal (rootName model) <> ","
          ]
       ++ fields
       ++ [ "}"
          , ""
          , "impl<'a> std::fmt::Debug for " <> n <> "<'a> {"
          , "    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {"
          , "        f.debug_struct(\"" <> n <> "\")"
          ]
       ++ dbgFields
       ++ [ "            .finish()"
          , "    }"
          , "}"
          , ""
          ] )
       <> fromImpls

nonGeneratableNote :: Text -> Text
nonGeneratableNote w = fromMaybe "" (lookup w Crate.wrapperNotes)

generateResourceWrapperFile :: SemanticModel -> ResWrapper -> (FilePath, Text)
generateResourceWrapperFile model rw =
    let n = rwName rw
        fileName = "safe_" <> T.toLower n <> ".rs"
        items = resWrapperGetters rw ++ resWrapperMethods model rw
        implBody = render (RsModule [RsItemImpl (RsImpl Nothing
                       (TyGeneric n [TyPath "'a"]) ["'a"] items)])
        content = "// Generated by apigen. Do not edit.\n"
               <> "use " <> Crate.safeModule <> "::" <> idToRustPascal (rootName model) <> ";\n"
               <> "use crate::types::*;\n\n"
               <> nonGeneratableNote n
               <> resWrapperHeader model rw
               <> implBody
    in (T.unpack fileName, content)

generateResourceWrappers :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateResourceWrappers model _items =
    map (generateResourceWrapperFile model) (resWrappers model)


mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> ([y], acc)
mapAccumL _ z [] = ([], z)
mapAccumL f z (x:xs) =
    let (z', y) = f z x
        (ys, z'') = mapAccumL f z' xs
    in (y:ys, z'')
