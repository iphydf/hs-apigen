{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Rust (generate, byteArrayConstants, Options (..)) where

import           Apigen.Inference            (getHierarchy,
                                              inferCFunctionMapping)
import qualified Apigen.Knowledge            as K
import           Apigen.Language.Cpp.Names   (enumTypeName, enumeratorName)
import           Apigen.Language.Rust.AST
import           Apigen.Language.Rust.Naming (idToRustPascal, idToRustSnake,
                                              paramSnake)
import           Apigen.Language.Rust.Pretty (prettyType, render, renderStmt)
import           Apigen.Model                (declaredElsewhere, eventFieldType,
                                              findEnum, mappingOf,
                                              returnsBorrowed)
import           Apigen.Namespace            (namespaces, resourceNamespace,
                                              rootNamespace, stripNamespace)
import           Apigen.Semantic
import qualified Apigen.Semantic             as S
import           Apigen.SizeExpr             (SizeExpr (..), sizeExprVars,
                                              sizeName)
import           Apigen.Types                (Constness (..))
import           Data.Char                   (isDigit, isLower, isUpper)
import           Data.List                   (find, groupBy, mapAccumL, nub,
                                              partition, sort, sortOn)
import           Data.Maybe                  (fromMaybe, isJust, isNothing,
                                              mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.FilePath             (takeBaseName)
import qualified Text.Casing                 as Casing

data Options = Options
    { typesOnly :: Bool
    }

data ModuleKind = MainModule | ExtensionModule Text deriving (Eq, Show)

-- | The Rust type a size names: @PublicKey@ for @TOX_PUBLIC_KEY_SIZE@.
sizeTypeOf :: SemanticModel -> SizeExpr -> Maybe Text
sizeTypeOf model size = sizeName size >>= (`lookup` sizeToType model)

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

rootCName :: SemanticModel -> Text
rootCName model = maybe (error "no root resource") cName (find isRoot (S.resources model))

-- | The crate's error type: @ToxError@.
errorType :: SemanticModel -> Text
errorType model = idToRustPascal (rootName model) <> "Error"

-- | How a hand-written handle struct's safe layer reaches its core struct.
-- @hgField@ is the path from `self` to `core`.
data HandleGlue = HandleGlue
    { hgImports  :: [Text]
    , hgImplType :: RsType
    , hgGenerics :: [Text]
    , hgField    :: Text
    }

-- | How the safe layer reaches a hand-written handle struct's core: the
-- root's, another library's and the settings'. A standalone handle's safe
-- layer is generated whole.
handleGlue :: SemanticModel -> SResource -> Maybe HandleGlue
handleGlue model r = do
    path <- lookup (cName r) (rustCorePaths (S.targets model))
    let name = idToRustPascal (resourceName r)
        library = fromMaybe (rootNamespace model) (resourceNamespace (namespaces model) r)
        use n = "use crate::" <> library <> "::" <> n <> ";"
    return HandleGlue
        { hgImports  = use name : [ use (handlerTrait r) | K.keepsHandler model r ] ++ ["use crate::types::*;"]
        , hgImplType = structType model r
        , hgGenerics = handleGenerics model r
        , hgField    = path
        }

-- | A view, or a handle made from what it can't outlive.
hasLifetime :: SemanticModel -> SResource -> Bool
hasLifetime model res = K.isView model res || isJust (K.borrowed model res)

generate :: Options -> SemanticModel -> [(FilePath, Text)]
generate opts = generateTyped opts . withEnumParams

-- | The integers holding an open enum, typed as it: @tox_file_send@'s @kind@
-- is a @FileKind@. Its type converts from and to @u32@ like the others do
-- from and to their C enum.
withEnumParams :: SemanticModel -> SemanticModel
withEnumParams model = model
    { resources = map resource (S.resources model)
    , callbacks = map callback (S.callbacks model)
    }
  where
    retype fn p = case K.enumParam model (fn, paramName p) >>= findEnum model of
        Just e | SUInt _ <- paramType p -> p { paramType = SEnum (S.enumSemanticName e) }
        _                               -> p
    resource r = r { methods = map method (methods r), events = map event (events r) }
    method m = m
        { inputs = map (retype (methodName m)) (inputs m)
        , methodMapping = case methodMapping m of
            CustomMapping cm -> CustomMapping cm { cSemParams = map (retype (cFunctionName cm)) (cSemParams cm) }
            standard         -> standard
        }
    event e = e { eventParams = map (retype (cCallback e)) (eventParams e) }
    callback c = c { cbParams = map (retype (cbCName c)) (cbParams c) }

generateTyped :: Options -> SemanticModel -> [(FilePath, Text)]
generateTyped opts model =
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
            in [modItem, useItem]

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
                      (ECast (EVar ("ffi::" <> constantName c)) usize)
            | c <- constants model
            , isSizeConstant (constantName c)
            ]

        -- Ids and units: a name, and the number it holds.
        newtypes = nub $
            [ (idToRustPascal (idName i), numberType (idType i)) | i <- idTypes model ]
            ++ [ (idToRustPascal (S.scalarName u), numberType (S.scalarType u)) | u <- S.scalarTypes model ]
        numberType t = case t of
            SSizeT  -> "usize"
            SInt b  -> "i" <> T.pack (show b)
            SUInt b -> "u" <> T.pack (show b)
            _       -> "u32"
        newtypeInvocations =
            [ RsMacroCall "impl_safe_newtype" [n, t] | (n, t) <- newtypes ]

        byteArrayInvocations =
            [ RsMacroCall "impl_byte_array_type" [ty, stripRootPrefix model sizeConst]
            | (sizeConst, ty) <- sizeToType model
            ]

        enumInvocations = map (mkEnumInvocation model) (sortOn (\(c, _, _) -> c)
            [ (S.enumName e, S.enumKind e, [ (S.enumMemberCName m, S.enumMemberName m) | m <- S.enumMembers e ])
            | e <- enums model ++ privateEnums model ])

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

-- | Each value with the C constant it is: @Tcp = TOX_CONNECTION_TCP@. An
-- open enum's values are constants of a @u32@ newtype (@AVATAR@), and a
-- set of flags' can be combined.
mkEnumInvocation :: SemanticModel -> (Text, S.SEnumKind, [(Text, Text)]) -> RsItem
mkEnumInvocation model (cName, kind, members) =
    RsMacroCall macro [rustEnumName model cName, "ffi::" <> cName, block]
  where
    (macro, name) = case kind of
        S.OpenEnum  -> (crateMacro "open_enum", T.toUpper)
        S.FlagsEnum -> (crateMacro "flags", T.toUpper)
        _           -> (crateMacro "enum", variant)
    -- The crate's macros: @impl_tox_enum@.
    crateMacro kind' = "impl_" <> rootNamespace model <> "_" <> kind'
    block = "{\n" <> T.concat [ "    " <> name n <> " = " <> c <> ",\n" | (c, n) <- members ] <> "}"
    variant m = case enumeratorName m of
        -- @TOX_ERR_GROUP_KICK_PEER_SELF@: the target is ourselves.
        "Self" -> "Ourselves"
        v      -> v

errorEnumCName :: SemanticModel -> Text -> Maybe Text
errorEnumCName model sem =
    fmap S.enumName (find (\e -> S.enumName e == sem || S.enumSemanticName e == sem) (enums model))

-- | The context enums the crate's own code uses.
privateEnums :: SemanticModel -> [S.SEnumModel]
privateEnums model =
    [ e | e <- S.contextEnums (S.context model), S.enumName e `elem` rustPrivateEnums (S.targets model) ]

renderErrorType :: SemanticModel -> Text
renderErrorType model =
    let
        methodErrEnums = nub $ mapMaybe (errorEnumCName model)
            (concatMap (mapMaybe methodErrorType . methods) (resources model))
        privateErrEnums = [ S.enumName e | e <- privateEnums model, S.enumKind e == S.ErrorEnum ]
        errTypes = sortOn errorVariant (map (rustEnumName model) (nub (methodErrEnums ++ privateErrEnums)))

        variants = [ "    " <> errorVariant e <> "(" <> e <> ")," | e <- errTypes ]
                -- A C call failed without saying why.
                ++ [ "    Failed,"
                   , "    InvalidString(std_ffi::NulError),"
                   ]
    in T.unlines
        ( [ "// --- Safe Results ---"
          , ""
          , "#[derive(Debug, Clone, PartialEq, Eq)]"
          , "pub enum " <> errorType model <> " {"
          ]
       ++ variants
       ++ [ "}"
          , ""
          , "impl error::Error for " <> errorType model <> " {}"
          , "impl fmt::Display for " <> errorType model <> " {"
          , "    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {"
          , "        write!(f, \"{:?}\", self)"
          , "    }"
          , "}"
          , ""
          , "pub type Result<T> = std::result::Result<T, " <> errorType model <> ">;"
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
            Just r  -> TyPtr False (TyPath ("ffi::" <> cName r))
            Nothing -> TyPtr False (TyPath "std::ffi::c_void")

        mkArm mem =
            let vName = idToRustPascal (memberName mem)
                getter = "ffi::" <> memberGetter mem
                vTy = idToRustPascal (memberType mem)

                enumMod = find (\e -> S.enumSemanticName e == S.variantTypeEnum var) (S.enums model)
                cVariantName = case enumMod of
                    Just e -> fromMaybe (memberName mem) $ lookup (memberName mem) (map (\m -> (S.enumMemberName m, S.enumMemberCName m)) (S.enumMembers e))
                    Nothing -> memberName mem

                variantEnum = fromMaybe (idToRustPascal (S.variantTypeEnum var)) (fmap S.enumName enumMod)
                enumVariant = "ffi::" <> variantEnum <> "::" <> cVariantName

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
                   else (PWildcard, RsBlock [StmtExpr (EMacroCall "panic" [ELit (LString "Unknown variant type")])]) ]))
            ]

        -- Dereferences a raw pointer, so it's `unsafe fn` (clippy's
        -- `not_unsafe_ptr_arg_deref`).
        fromPtrFn = RsItemFn RsFn
            { fnName = "from_ptr"
            , fnVis = Pub
            , fnAbi = Nothing
            , fnGenerics = []
            , fnArgs = [RsArg "ptr" ptrTy]
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
        sizeMethodName = stripMethodName (T.toLower (S.cPrefix res) <> sizeMethod)

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
                      , ("count", EMethodCall (EVar "self") sizeMethodName [])
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

        notHandWritten m = not (handWrittenCore model res m)
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
                                , tupleStructFields = [ (Pub, TyRef (Just "'a") (TyPath ("ffi::" <> cName res)) False) ]
                                }
                            else RsItemStruct RsStruct
                                { structName = name
                                , structVis = Pub
                                , structGenerics = handleGenerics model res
                                , structFields = [ RsStructField "ptr" (TyPtr True (TyPath ("ffi::" <> cName res))) PubCrate ]
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

-- | The C call a wrapper makes, as its body needs it.
data CCall = CCall
    { callFunc      :: Text
    , callArgs      :: [CArgSource]
    , callParams    :: [SParameter]
    , callContext   :: [RsExpr]  -- ^ The objects on the method's path.
    , callOk        :: Text      -- ^ Its error enum's OK value.
    , callHasError  :: Bool      -- ^ It takes an error pointer.
    , callHasBuffer :: Bool      -- ^ It writes to a buffer.
    }

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
                      else RsArg "user_data" (TyRef Nothing (TyPath "T") True)

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
            SFixedBytes e _ | any (T.any isLower) (sizeExprVars e) -> Just e
            _                                                      -> Nothing

        isSBytesNoSize = case output method of SBytes -> isNothing (cSizeFunctionName mapping); _ -> False

        -- Event structs borrow the C event, so their bytes can be a borrowed
        -- slice.
        isEventRes = K.isView model res
        isEventBytes = isEventRes && output method == SBytes
                       && isJust (cSizeFunctionName mapping)

        hasErrorPtr = any (\(a) -> case a of ErrorPtr -> True; _ -> False) cArgs

        rawRetType
            | isJust computedSize    = TyGeneric "Vec" [u8]
            | isSBytesNoSize         = TyPtr False u8
            | isEventBytes           = TyRef (Just "'a") (TySlice u8) False
            | output method == SVoid = TyUnit
            | otherwise              = toSafeRetType model res (Just (methodRole method)) (output method)

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
        finalRetType
            | hasErrorPtr                         = Just (TyGeneric "std::result::Result" [rawRetType', TyPath (rustEnumName model errEnumName)])
            | isOptionFixedBytes || isNullableHandle = Just (TyGeneric "Option" [rawRetType])
            | rawRetType == TyUnit                = Nothing
            | otherwise                           = Just rawRetType
        macroArgs = filterArgExprs cArgs argExprs

        call = CCall
            { callFunc      = cFunc
            , callArgs      = cArgs
            , callParams    = semParams
            , callContext   = ctxNames
            , callOk        = okVariant
            , callHasError  = hasErrorPtr
            , callHasBuffer = hasBufferPtr
            }

        bodyStmt = case computedSize of
          Just size -> generateComputedBufferBody model call size
          Nothing -> case output method of
                SFixedBytes size _ | Just sizeConst <- sizeName size ->
                    generateFixedBytesBody model method call isOptionFixedBytes sizeConst
                -- A `const char *` accessor is a `&str`, even with a `_length`
                -- function.
                SString -> generateStringAccessBody (methodReturnNullable method) cFunc macroArgs
                _ -> case cSizeFunctionName mapping of
                    Just sizeFunc ->
                        if hasBufferPtr then
                             generateStandardBody model res method mapping call argExprs rawRetType'
                        else if isEventBytes then
                             generateEventBytesBody cFunc macroArgs sizeFunc
                        else
                             generateAccessBody (methodReturnNullable method) sizeFuncReturnsUsize cFunc macroArgs sizeFunc
                    Nothing ->
                         generateStandardBody model res method mapping call argExprs rawRetType'

    in (finalArgs, finalRetType, RsBlock (stmts ++ [bodyStmt]))

-- | A function writing a buffer sized by its inputs: C checks them, so a
-- size below zero is zero, and C's error says why.
generateComputedBufferBody :: SemanticModel -> CCall -> SizeExpr -> RsStmt
generateComputedBufferBody model CCall { callFunc = cFunc, callArgs = cArgs, callParams = semParams, callContext = ctxNames, callOk = okVariant } size =
    let
        (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs

        lenExpr e = case e of
            SizeNum n -> ELit (LInt n)
            SizeVar v
                | T.all (\c -> isUpper c || isDigit c || c == '_') v -> ECast (EVar ("ffi::" <> v)) usize
                | Just p <- K.lengthOf model cFunc v -> EMethodCall (EVar (idToRustSnake p)) "len" []
                | otherwise -> EVar (idToRustSnake v)
            SizeOp '-' x y -> EMethodCall (operand x) "saturating_sub" [lenExpr y]
            SizeOp op x y  -> EBinOp (T.singleton op) (operand x) (operand y)
            SizeCall f xs  -> ECall (EVar ("std::cmp::" <> f)) (map lenExpr xs)
        operand x = case x of
            SizeOp{} -> EParen (lenExpr x)
            _        -> lenExpr x

        fixedArgs = zipWith (\src expr -> case src of
            BufferPtr _ -> EMethodCall (EVar "buf") "as_mut_ptr" []
            ErrorPtr    -> errRef
            _           -> expr) cArgs allArgExprs

        stmts =
            [ StmtLet "len" Nothing (lenExpr size)
            , StmtLetMut "buf" Nothing (EVecOf (ELit (LIntOf 0 "u8")) (EVar "len"))
            , StmtLetMut "err" Nothing (EVar okVariant)
            , StmtExpr (ECall (EVar ("ffi::" <> cFunc)) fixedArgs)
            , errCheck okVariant
            , StmtExprNoSemi (ECall (EVar "Ok") [EVar "buf"])
            ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

-- | @isOption@: a @bool@ says whether it filled the buffer.
generateFixedBytesBody :: SemanticModel -> SMethod -> CCall -> Bool -> Text -> RsStmt
generateFixedBytesBody model method call isOption sizeConst =
    let CCall { callFunc = cFunc, callArgs = cArgs, callParams = semParams, callContext = ctxNames
              , callOk = okVariant, callHasError = hasErrorPtr, callHasBuffer = hasBufferPtr } = call in
    let (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs
        macroArgs = filterArgExprs cArgs allArgExprs
        c' = stripRootPrefix model sizeConst

        wrapType' t expr = case t of
            SFixedBytes sc _ ->
                case sizeTypeOf model sc of
                    Just ty -> ECall (EVar ty) [expr]
                    Nothing -> expr
            _ -> expr

    in if hasBufferPtr && hasErrorPtr then
        let macroCall = EMacroCall "ffi_get_array" ([EVar cFunc, EVar okVariant, EVar c'] ++ macroArgs)
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
            stmts = [ StmtLetMut "buf" Nothing bufInit
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
                ErrorPtr    -> errRef
                _           -> expr) cArgs allArgExprs

            stmts = [ StmtLetMut "buf" Nothing bufInit ]
                    ++ (if hasErrorPtr
                        then [ StmtLetMut "err" Nothing (EVar okVariant)
                             , StmtExpr (ECall (EVar ("ffi::" <> cFunc)) fixedArgs)
                             , errCheck okVariant
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
        stmts = [ StmtLet "ptr" Nothing (ECast callPtr (TyPtr False (TyPath "std::os::raw::c_char")))
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
                      (RsBlock [StmtExprNoSemi (ERef (EArray []) False)])
                      (Just (RsBlock [StmtExprNoSemi fromRaw])))
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

generateStandardBody :: SemanticModel -> SResource -> SMethod -> CFunctionMapping -> CCall -> [RsExpr] -> RsType -> RsStmt
generateStandardBody model res method mapping cCall argExprs rawRetType =
    let CCall { callFunc = cFunc, callArgs = cArgs, callOk = okVariant
              , callHasError = hasErrorPtr, callHasBuffer = hasBufferPtr } = cCall in
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
                callExpr = EMacroCall name args
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
                            SScalar n _ -> EMethodCall callExpr "map" [EVar (idToRustPascal n)]
                            t | isSafeEnum model t ->
                                EMethodCall callExpr "map" [EVar "Into::into"]
                            t ->
                                if any (\r -> resourceName r == case t of SHandle h -> h; _ -> "") (S.resources model)
                                   || case t of SFixedBytes sc _ -> isJust (sizeTypeOf model sc); _ -> False
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
                StmtExpr (EMacroCall "unimplemented" [ELit (LString "BufferPtr without size function")])
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

-- | The @n@th object on the method's path.
pathObject :: [RsExpr] -> Int -> RsExpr
pathObject ctx n
    | n < length ctx = ctx !! n
    | otherwise      = error ("pathObject: no object " <> show n)

resolveArguments :: SemanticModel -> [SParameter] -> [RsExpr] -> [CArgSource] -> ([RsExpr], [RsStmt])
resolveArguments model semParams ctxNames cArgs =
    let (_, resolved) = mapAccumL step Nothing cArgs
        units = [ paramSnake (paramName p) | p <- semParams, SScalar _ _ <- [paramType p] ]
        -- A size is the parameter before it's.
        step lastParam arg = case arg of
            SemanticArg i -> let p = semParams !! i in (Just p, resolveParam model units p)
            _             -> (lastParam, ([], resolveOther ctxNames lastParam arg))
    in (map snd resolved, concatMap fst resolved)

-- | The statements a parameter needs first, and what C gets. Sizes read
-- the parameters holding units (@units@) as their numbers.
resolveParam :: SemanticModel -> [Text] -> SParameter -> ([RsStmt], RsExpr)
resolveParam model units param =
    let name = paramSnake (paramName param)
        isMutable = paramConstness param == MutableThis
        ptrMethod = if isMutable then "as_mut_ptr" else "as_ptr"

        cstrName = name <> "_cstr"
        extraStmts = case paramType param of
            SString
                | paramNullable param ->
                    [ StmtLet cstrName Nothing (EMethodCall (EVar name) "map" [ELambda ["s"] (EMacroCall "c_str" [EVar "s"])]) ]
                | otherwise -> [ StmtLet cstrName Nothing (EMacroCall "c_str" [EVar name]) ]
            -- A plain `uint8_t x[CONST]` is read for `CONST` bytes, so a
            -- shorter slice would be an out-of-bounds read.
            SFixedBytes size _
                | isNothing (sizeTypeOf model size)
                , Just sizeConst <- sizeName size ->
                let c' = stripRootPrefix model sizeConst
                in [ StmtExpr (EMacroCall "assert" [ EBinOp ">=" len (EVar c') ]) ]
            -- One sized by other arguments, like a video plane: C reads that
            -- many.
            SFixedBytes sz _ | isNothing (sizeTypeOf model sz) -> sizedBy sz
            SFixedList _ sz _ -> sizedBy sz
            _ -> []
        sizedBy sz =
            [ StmtExpr (EMacroCall "assert" [ EBinOp ">=" (ECast len (TyPath "i64")) (sizerExprOf units sz) ]) ]
        len = EMethodCall (EVar name) "len" []

        expr = case paramType param of
            -- C reads no string from NULL.
            SString
                | paramNullable param ->
                    EMethodCall (EMethodCall (EVar cstrName) "as_ref" []) "map_or" [ECall (EVar "std::ptr::null") [], ELambda ["c"] (EMethodCall (EVar "c") "as_ptr" [])]
                | otherwise -> EMethodCall (EVar cstrName) "as_ptr" []
            SBytes          -> EMethodCall (EVar name) ptrMethod []
            SList _         -> EMethodCall (EVar name) ptrMethod []
            SFixedList {}   -> EMethodCall (EVar name) ptrMethod []
            SFixedBytes size _ ->
                if isJust (sizeTypeOf model size)
                then
                    if paramNullable param
                    then EMethodCall (EVar name) "map_or" [ECall (EVar "std::ptr::null") [], ELambda ["v"] (EMethodCall (EFieldAccess (EVar "v") "0") ptrMethod [])]
                    else EMethodCall (EFieldAccess (EVar name) "0") ptrMethod []
                else EMethodCall (EVar name) ptrMethod []
            SResourceId _   -> EFieldAccess (EVar name) "0"
            SScalar _ _     -> EFieldAccess (EVar name) "0"
            SHandle _       -> EFieldAccess (EVar name) "ptr"
            -- Plain enums convert to the `ffi::` enum. Error enums have no safe
            -- wrapper, and clippy rejects a no-op `.into()`.
            t@(SEnum _) | isSafeEnum model t -> EMethodCall (EVar name) "into" []
                        | otherwise          -> EVar name
            _               -> EVar name
    in (extraStmts, expr)

-- | What C gets for an argument that isn't a parameter.
resolveOther :: [RsExpr] -> Maybe SParameter -> CArgSource -> RsExpr
resolveOther ctxNames lastParam arg = case arg of
    BufferSize    -> maybe (ELit (LInt 0)) (\p -> EMethodCall (EVar (paramSnake (paramName p))) "len" []) lastParam
    PathObject n _ -> pathObject ctxNames n
    PathId n      -> EFieldAccess (pathObject ctxNames n) "0"
    ErrorPtr      -> errRef
    BufferPtr _   -> ECall (EVar "std::ptr::null_mut") []
    ThisObject _  -> last ctxNames
    UserData      -> ECast (ECast (EVar "user_data") (TyPtr True (TyPath "T"))) voidPtr
    _             -> error ("Apigen.Language.Rust: Unhandled CArgSource: " ++ show arg)


generateContextArgs :: SemanticModel -> ModuleKind -> SResource -> Constness -> SMethod -> [CArgSource] -> ([RsArg], [RsExpr])
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
                    topCName = case rootRes of
                        Just r -> cName r
                        Nothing -> error ("generateContextArgs: no resource " <> T.unpack top)

                    ptrType = TyPtr (cns == MutableThis) (TyPath ("ffi::" <> topCName))
                    rootArg = if rootIsHandle
                              then RsArg (idToRustSnake top) (TyRef Nothing (TyPath (idToRustPascal top)) False)
                              else RsArg (idToRustSnake top) ptrType

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

        -- A view holds its C value, a handle a pointer.
        selfExpr = EFieldAccess (EVar "self") $ case structRes of
            Just r | K.isView model r -> "0"
            _                         -> "ptr"

        ctxExprs = [ if isCoveredBySelf i then selfExpr
                     else if i == 0 && rootIsHandle then EFieldAccess (EVar (argName arg)) "ptr"
                     else EVar (argName arg)
                   | (i, arg) <- zip [0 ..] fullArgs ]

        isUsed idx = any (matches idx) cArgs
          where
            matches i (PathObject j _) = i == j
            matches i (ThisObject _)   = i == length path - 1
            matches i (PathId j)       = i == j
            matches _ _                = False

        finalArgs = [ arg | (i, arg) <- zip [0..] fullArgs, isUsed i, not (isCoveredBySelf i) ]

    in (finalArgs, ctxExprs)

toContextArg :: SemanticModel -> Constness -> Text -> RsArg
toContextArg model cns resName =
    case find (\(r) -> resourceName r == resName) (resources model) of
        Just r -> case resourceType r of
            ResId _ -> toIdArg resName
            ResHandle ->
                RsArg (idToRustSnake resName) (TyPtr (cns == MutableThis) (TyPath ("ffi::" <> cName r)))
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
        -- A buffer C writes to.
        ty = case (paramType param, baseTy) of
            (SBytes, TyRef l t _)         | mutable -> TyRef l t True
            (SFixedBytes{}, TyRef l t _)  | mutable -> TyRef l t True
            _                                       -> baseTy
        mutable = paramConstness param == MutableThis

        -- A nullable handle stays a reference.
        safeTy = case (paramType param, ty) of
            (_, TySlice _)               -> TyRef Nothing ty False
            (SHandle n, _) | n /= "void" -> ty
            _ | paramNullable param      -> TyGeneric "Option" [ty]
            _                            -> ty
    in Just (RsArg name safeTy)
toSafeArg _ _ _ = Nothing

toSafeRetType :: SemanticModel -> SResource -> Maybe SMethodRole -> SType -> RsType
toSafeRetType model res mRole t =
    case t of
        SHandle n ->
            let name = idToRustPascal n
                isLife = any (\r -> resourceName r == n && hasLifetime model r) (S.resources model)
                resName = if isLife then name <> "<'a>" else name
            in if n == "void" then voidPtr
               else if n `elem` ["uint8_t", "char", "unsigned char"] then TyPath "u8"
               else if n == resourceName res && mRole == Just Constructor
               then TyPath "Self"
               else if any (\r -> resourceName r == n) (S.resources model)
               then TyPath resName
               else error ("toSafeRetType: no handle " <> T.unpack n)
        SString -> TyRef (if mRole == Just StaticRole then Just "'static" else Nothing) (TyPath "str") False
        SBytes  -> TyGeneric "Vec" [TyPath "u8"]
        SFixedBytes size _
            | Just ty <- sizeTypeOf model size -> TyPath ty
            | Just c <- sizeName size          -> TyArray u8 (stripRootPrefix model c)
            | otherwise                        -> TyGeneric "Vec" [u8]
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
toSafeRsType _ SString            = TyRef Nothing (TyPath "str") False
toSafeRsType _ SBytes             = TyRef Nothing (TySlice u8) False
toSafeRsType model (SFixedBytes size _) =
    case sizeTypeOf model size of
        Just ty -> TyRef Nothing (TyPath ty) False
        Nothing -> TyRef Nothing (TySlice u8) False

toSafeRsType model (SFixedList t _ _) = TySlice (toSafeRsType model t)
toSafeRsType model (SList t)          = TySlice (toSafeRsType model t)
toSafeRsType _ SVoid = TyUnit
toSafeRsType model (SEnum n) =
    let cName = enumCName model n
    in TyPath (rustEnumName model cName)
toSafeRsType _ (SResourceId n) =
    TyPath (idResourceType n)
toSafeRsType _ (SScalar n _) =
    TyPath (idToRustPascal n)
toSafeRsType model (SHandle n) =
    if n == "void" then voidPtr
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
-- | A C enum's Rust name: @Connection@, @FriendAddError@, @AvCallControl@.
rustEnumName :: SemanticModel -> Text -> Text
rustEnumName model = enumTypeName (namespaces model)

-- | The @ToxError@ variant holding an error enum: @FriendAdd@ for
-- @FriendAddError@.
errorVariant :: Text -> Text
errorVariant e = fromMaybe e (T.stripSuffix "Error" e)

-- | An enum, which crosses to C with @into@.
isSafeEnum :: SemanticModel -> SType -> Bool
isSafeEnum model (SEnum n) = isJust (find (\e -> S.enumSemanticName e == n) (S.enums model))
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
    SFixedBytes size _ ->
        case sizeTypeOf model size of
            Just ty -> ECall (EVar ty) [expr]
            Nothing -> expr
    SEnum _ | isSafeEnum model t -> EMethodCall expr "into" []
    SResourceId n ->
        let name = idToRustPascal n
            wrapper = name
        in ECall (EVar wrapper) [expr]
    SScalar n _ -> ECall (EVar (idToRustPascal n)) [expr]
    SSizeT -> ECast expr (TyPath "usize")
    _ -> expr


-- ---------------------------------------------------------------------------
-- Safe wrappers on `tox::Tox`, `tox::ToxAV` and `tox::Options`.
-- ---------------------------------------------------------------------------

safeMethodName :: SemanticModel -> SResource -> Text -> Text
safeMethodName model res core = mechanical
  where
    mechanical
        | K.isSettings model res =
            if "set_" `T.isPrefixOf` core then core else "set_" <> core
        | Just r <- T.stripPrefix "self_get_" core = r
        | Just r <- T.stripPrefix "self_set_" core = "set_" <> r
        | Just r <- T.stripPrefix "get_" core      = r
        | otherwise                                = core

-- | What the crate's core writes itself: another library's calls taking a
-- callback, the loops running a handler's callbacks, a handle someone else
-- owns, and the crate's own choices.
handWrittenCore :: SemanticModel -> SResource -> SMethod -> Bool
handWrittenCore model res m =
    (declaredElsewhere res m && methodRole m == RegistrarRole)
    || (K.isLoopFunction model name && output m == SVoid && K.keepsHandler model res)
    || ownedElsewhere
    || name `elem` fst (rustHandWritten (S.targets model))
  where
    name = S.methodName m
    ownedElsewhere = returnsBorrowed res m && case output m of
        SHandle h -> any (\r -> resourceName r == h && K.resourceKind model r `elem` [RootHandle, LibraryHandle]) (S.resources model)
        _         -> False

-- | What the crate's safe layer writes itself: the loops, which take
-- handlers, settings setters whose @bool@ only says it had memory, and the
-- crate's own choices.
handWrittenSafe :: SemanticModel -> SResource -> SMethod -> Bool
handWrittenSafe model res m =
    (K.isLoopFunction model name && not (isInteger (output m)))
    || (K.isSettings model res && cReturnType (mappingOf model res m) == SBool)
    || name `elem` snd (rustHandWritten (S.targets model))
  where
    name = S.methodName m
    isInteger t = case t of
        SUInt _ -> True
        SInt _  -> True
        _       -> False

safeInScope :: SemanticModel -> SResource -> SMethod -> Bool
safeInScope model res m =
    methodRole m `notElem` [Constructor, Destructor, StaticRole]
    && not (methodHasUserData m)
    && not (handWrittenSafe model res m)
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
                        [EVar (errorType model <> "::" <> errorVariant (errName errTy))] )
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
        case handleGlue model handle of
            Just glue -> generateFlatHandle model res glue
            Nothing   -> generatePassKeyModule model res

generateFlatHandle :: SemanticModel -> SResource -> HandleGlue -> (FilePath, Text)
generateFlatHandle model res glue =
    mkSafeFile ("safe_" <> T.unpack (idToRustSnake (resourceName res)) <> ".rs")
        (hgImports glue)
        (RsItemImpl (RsImpl Nothing (hgImplType glue) (hgGenerics glue)
            (mapMaybe (safeWrapper model res (hgField glue)) (S.methods res))))

-- | Static helpers become module functions, except the `*_length` ones.
generatePassKeyModule :: SemanticModel -> SResource -> (FilePath, Text)
generatePassKeyModule model res =
    ( "safe_" <> T.unpack (idToRustSnake (resourceName res)) <> ".rs"
    , T.unlines
        ( [ "// Generated by apigen. Do not edit."
          , "use crate::core;" ]
       ++ [ "use crate::ffi;" | "ffi::" `T.isInfixOf` body ]
       ++ [ "use crate::types::*;" ] )
      <> "\n" <> body )
  where
    body = render $ RsModule $
        [ RsItemTupleStruct RsTupleStruct
            { tupleStructName     = typeName
            , tupleStructVis      = Pub
            , tupleStructGenerics = []
            , tupleStructFields   = [(Private, TyPath ("core::" <> typeName))]
            }
        , RsItemImpl (RsImpl Nothing (TyPath typeName) [] (map RsItemFn (ctorFns ++ instanceFns)))
        ] ++ map RsItemFn moduleFns

    typeName = idToRustPascal (resourceName res)

    coreFn m = case generateMethod model MainModule res m of
        (RsItemFn fn : _) -> Just fn
        _                 -> Nothing

    valArgs fn = [ a | a@RsArg{} <- fnArgs fn ]

    -- Errors become the crate's, and a new core value this type.
    adapt rewrapHandle fn = case fnRet fn of
        Just (TyGeneric "std::result::Result" [t, TyPath errTy]) ->
            ( Just (TyGeneric "Result" [if rewrapHandle then TyPath typeName else t])
            , \call -> EMethodCall
                (if rewrapHandle then EMethodCall call "map" [EVar typeName] else call)
                "map_err" [EVar (errorType model <> "::" <> errorVariant errTy)] )
        other -> (other, id)

    forward recv callee rename rewrapHandle m = do
        fn <- coreFn m
        let (ret, wrap) = adapt rewrapHandle fn
            call = callee (fnName fn) [ EVar n | RsArg n _ <- valArgs fn ]
        Just (plainFn (rename (fnName fn)) Pub (recv ++ valArgs fn) [StmtExprNoSemi (wrap call)])
            { fnRet = ret }

    coreCall name = ECall (EVar ("core::" <> typeName <> "::" <> name))
    selfCall = EMethodCall (EFieldAccess (EVar "self") "0")

    methodsOf p = filter p (S.methods res)

    ctorFns = mapMaybe (forward [] coreCall id True) (methodsOf ((== Constructor) . methodRole))

    instanceFns = mapMaybe (forward [RsSelfArg False] selfCall id False) (methodsOf ((== ActionRole) . methodRole))

    rawModuleFns = methodsOf (\m -> methodRole m == StaticRole && not (null (S.inputs m)))
    isStringizer m = "to_string" `T.isSuffixOf` S.methodName m
    (stringizers, helpers) = partition isStringizer rawModuleFns
    moduleFns = mapMaybe (forward [] coreCall stripPassPrefix False) (stringizers ++ helpers)

    stripPassPrefix n = fromMaybe n (T.stripPrefix "pass_" n)

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

handlerArgType :: SemanticModel -> SParameter -> RsType
handlerArgType model p = case paramType p of
    SResourceId n           -> TyPath (idResourceType n)
    SScalar n _             -> TyPath (idToRustPascal n)
    SBytes                  -> bytes
    SString                 -> bytes
    SBool                   -> TyPath "bool"
    SSizeT                  -> usize
    t@SInt{}                -> scalarRsType t
    t@SUInt{}               -> scalarRsType t
    SEnum n                 -> TyPath (rustEnumName model (enumCName model n))
    SFixedBytes sz _
        | Just ty <- sizeTypeOf model sz -> TyPath ty
        | otherwise                               -> bytes
    SFixedList el _ _       -> TyRef Nothing (TySlice (scalarRsType el)) False
    _                       -> u32
  where
    bytes = TyRef Nothing (TySlice u8) False

-- | An id type's Rust type, from @crate::types@: @FriendNumber@.
idResourceType :: Text -> Text
idResourceType = idToRustPascal

scalarRsType :: SType -> RsType
scalarRsType (SInt b)  = TyPath ("i" <> T.pack (show b))
scalarRsType (SUInt b) = TyPath ("u" <> T.pack (show b))
scalarRsType _         = u8

-- | Adds back the `size_t` the model drops after each byte array.
rawCArgs :: SemanticModel -> SParameter -> [(Text, RsType)]
rawCArgs model p =
    -- Keywords get `type_`, not `r#type`, as in the safe wrappers.
    let nm = paramSnake (paramName p)
    in case paramType p of
        SResourceId _     -> [(nm, u32)]
        SScalar _ t       -> rawCArgs model p { paramType = t }
        SBytes            -> [(nm, TyPtr False u8), (nm <> "_len", usize)]
        SString           -> [(nm, TyPtr False u8), (nm <> "_len", usize)]
        SBool             -> [(nm, TyPath "bool")]
        SSizeT            -> [(nm, usize)]
        t@SInt{}          -> [(nm, scalarRsType t)]
        t@SUInt{}         -> [(nm, scalarRsType t)]
        -- An open enum's value may be one bindgen's enum doesn't have.
        SEnum n
            | Just e <- findEnum model n, S.enumKind e `elem` [S.OpenEnum, S.FlagsEnum]
                          -> [(nm, u32)]
            | otherwise   -> [(nm, TyPath ("ffi::" <> enumCName model n))]
        SFixedBytes _ _   -> [(nm, TyPtr False u8)]
        SFixedList el _ _ -> [(nm, TyPtr False (scalarRsType el))]
        _                 -> [(nm, u32)]

-- | The statements a trampoline needs for a parameter, and what it passes
-- the handler.
convertArg :: SemanticModel -> SParameter -> ([RsStmt], RsExpr)
convertArg model p =
    -- Must match the names 'rawCArgs' uses.
    let nm = paramSnake (paramName p)
        var = EVar . (nm <>)
        unsafeCall f args = EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar f) args)])
        fromRaw len = unsafeCall "slice::from_raw_parts" [EVar nm, len]
        sized suffix sz =
            ( [ StmtLet (nm <> suffix) Nothing (ECast (EParen (sizerExpr sz)) usize)
              , StmtLet (nm <> "_slice") Nothing (fromRaw (var suffix)) ]
            , var "_slice" )
    in case paramType p of
        SResourceId n   -> ([], ECall (EVar ("crate::types::" <> idResourceType n)) [EVar nm])
        SScalar n _     -> ([], ECall (EVar ("crate::types::" <> idToRustPascal n)) [EVar nm])
        SBytes          -> safeSlice nm
        SString         -> safeSlice nm
        SEnum _         -> ([], EMethodCall (EVar nm) "into" [])
        SFixedBytes sz _
            | Just ty <- sizeTypeOf model sz, Just c <- sizeName sz ->
                let szConst = EVar (stripRootPrefix model c)
                in ( [ StmtLetMut (nm <> "_arr") Nothing (EArrayInit (ELit (LIntOf 0 "u8")) szConst)
                     , StmtLet (nm <> "_src") Nothing (fromRaw szConst)
                     , StmtExpr (EMethodCall (var "_arr") "copy_from_slice" [var "_src"]) ]
                   , ECall (EVar ("crate::types::" <> ty)) [var "_arr"] )
            | otherwise -> sized "_size" sz
        SFixedList _ sz _ -> sized "_count" sz
        _               -> ([], EVar nm)
  where
    safeSlice nm =
        ( [ StmtLet (nm <> "_slice") Nothing
              (EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar "safe_slice") [EVar nm, EVar (nm <> "_len")])])) ]
        , EVar (nm <> "_slice") )

-- | Operands are cast to `i64` so nothing overflows before the final
-- `as usize`.
sizerExpr :: SizeExpr -> RsExpr
sizerExpr = sizerExprOf []

-- | With these variables units, whose number is their @.0@.
sizerExprOf :: [Text] -> SizeExpr -> RsExpr
sizerExprOf units = expr
  where
    expr e = case e of
        -- Clippy rejects `as i64` on a literal (`unnecessary_cast`).
        SizeNum n          -> ELit (LIntOf n "_i64")
        SizeVar v
            | v `elem` units -> ECast (EFieldAccess (EVar v) "0") (TyPath "i64")
            | otherwise      -> ECast (EVar v) (TyPath "i64")
        SizeOp op x y      -> EBinOp (T.singleton op) (operand x) (operand y)
        SizeCall "abs" [x] -> EMethodCall (EParen (expr x)) "abs" []
        SizeCall f xs      -> ECall (EVar ("std::cmp::" <> f)) (map expr xs)
    operand x = case x of
        SizeOp{} -> EParen (expr x)
        _        -> expr x

-- | @&mut err@
errRef :: RsExpr
errRef = ERef (EVar "err") True

-- | Returns C's error unless it is @ok@.
errCheck :: Text -> RsStmt
errCheck ok = StmtExpr (EIf (EBinOp "!=" (EVar "err") (EVar ok))
    (RsBlock [StmtReturn (ECall (EVar "Err") [EMethodCall (EVar "err") "into" []])])
    Nothing)

u8, u32, usize, voidPtr :: RsType
u8 = TyPath "u8"
u32 = TyPath "u32"
usize = TyPath "usize"
voidPtr = TyPtr True (TyPath "std::ffi::c_void")

renderTrampoline :: SemanticModel -> Text -> SEvent -> Text
renderTrampoline model macroName ev =
    let fnName = "dispatch_" <> eventName ev
        rawArgs = concatMap (rawCArgs model) (eventParams ev)
        rawSig  = T.intercalate ", " [ n <> ": " <> prettyType ty | (n, ty) <- rawArgs ]
        -- Size-only parameters get `let`s, for the sizers that use them, but
        -- aren't passed on.
        conv p  = (handlerVisible ev p, convertArg model p)
        conversions = map conv (eventParams ev)
        lets = concatMap (fst . snd) conversions
        call = StmtExpr (EMethodCall (EVar "handler") ("on_" <> eventName ev)
            [ snd c | (visible, c) <- conversions, visible ])
    -- rustfmt leaves a macro call like this one as it is.
    in T.unlines
        ( [ macroName <> "!("
          , "    " <> fnName <> ","
          , "    (" <> rawSig <> "),"
          , "    |handler: &mut H| {"
          ]
       ++ [ "        " <> l | st <- lets ++ [call], l <- T.lines (renderStmt st) ]
       ++ [ "    }"
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
    , ident <- sizeExprVars sz
    ]

handlerVisible :: SEvent -> SParameter -> Bool
handlerVisible ev p = case paramType p of
    SSizeT -> paramSnake (paramName p) `notElem` sizerIdentifiers ev
    _      -> True

traitMethod :: SemanticModel -> SEvent -> RsFn
traitMethod model ev =
    let -- `_type` needs no `r#` escape.
        plainSnake = T.toLower . T.pack . Casing.toSnake . Casing.fromAny . T.unpack
    in plainFn ("on_" <> eventName ev) Private
        (RsSelfArg True : [ RsArg ("_" <> plainSnake (paramName p)) (handlerArgType model p)
                          | p <- eventParams ev, handlerVisible ev p ])
        []

-- | A function without generics, return value or docs.
plainFn :: Text -> RsVis -> [RsArg] -> [RsStmt] -> RsFn
plainFn name vis args body = RsFn
    { fnName     = name
    , fnVis      = vis
    , fnAbi      = Nothing
    , fnGenerics = []
    , fnArgs     = args
    , fnRet      = Nothing
    , fnBody     = Just (RsBlock body)
    , fnUnsafe   = False
    , fnDoc      = []
    }

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

        handlerTraitFor r = render (RsModule [RsItemTrait RsTrait
            { traitName    = handlerTrait r
            , traitVis     = Pub
            , traitMethods = map (traitMethod model) (events r)
            }])

        var r = idToRustSnake (resourceName r)
        ptr r = var r <> "_ptr"
        handlerArg = RsArg "handler" (TyRef Nothing (TyPath "H") True)
        handlerPtr = ECast (ECast (EVar "handler") (TyPtr True (TyPath "H"))) (TyPtr True (TyPath "c_void"))
        dispatcher ev = ECall (EVar "Some") [EVar ("dispatch_" <> eventName ev <> "::<H>")]
        setCallback ev args = StmtExpr (ECall (EVar ("ffi::" <> eventRegistrar ev)) args)
        generic r fn = fn { fnGenerics = ["H: " <> handlerTrait r] }

        -- The root registers its callbacks each time it iterates, with the
        -- handler the iteration takes.
        iterateFn r it = generic r $ plainFn (fromMaybe (S.methodName it) (T.stripPrefix (cPrefix r) (S.methodName it))) Pub
            [RsArg (var r) (TyRef Nothing (TyPath (idToRustPascal (resourceName r))) False), handlerArg]
            [ StmtLet (ptr r) Nothing (EFieldAccess (EVar (var r)) "ptr")
            , StmtExprNoSemi (EUnsafe (RsBlock
                ( [ setCallback ev [EVar (ptr r), dispatcher ev] | ev <- events r ]
               ++ [ StmtExpr (ECall (EVar ("ffi::" <> S.methodName it)) [EVar (ptr r), handlerPtr]) ] )))
            ]

        -- A handle keeping its handler registers its callbacks once.
        registerFn r = (generic r $ plainFn ("register_" <> var r <> "_callbacks") PubCrate
            [RsArg (ptr r) (TyPtr True (TyPath ("ffi::" <> cName r))), handlerArg]
            [ StmtExprNoSemi (EUnsafe (RsBlock
                [ setCallback ev [EVar (ptr r), dispatcher ev, handlerPtr] | ev <- events r ])) ])
            { fnUnsafe = True }

        registration r
            | isRoot r  = maybe "" (render . RsModule . pure . RsItemFn . iterateFn r)
                              (find ((== IterateRole) . methodRole) (S.methods r))
            | otherwise = render (RsModule [RsItemFn (registerFn r)])

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

resWrappers :: SemanticModel -> [ResWrapper]
resWrappers model =
    [ ResWrapper
        { rwName      = idToRustPascal (resourceName r)
        , rwIdType    = idToRustPascal idTypeName
        , rwExtraId   = (\(o, i) -> (idToRustSnake o, idToRustPascal i)) <$> K.idScope model (resourceName r)
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
    | S.methodName method `elem` rustWrappers (S.targets model) = Nothing
    | otherwise = case generateMethod model MainModule res method of
        (RsItemFn core : _) -> Just (mkWrapper core)
        _                   -> Nothing
  where
    cfn = S.methodName method
    idFields = (rwIdType rw, "self.number")
             : maybe [] (\(n, t) -> [(t, "self." <> n)]) (rwExtraId rw)

    mkWrapper core =
        let valArgs = [ a | a@RsArg{} <- fnArgs core ]
            -- Each argument as the wrapper takes it, if it does, and what the
            -- core gets: its own id is `self`'s, and another wrapper's id is
            -- that wrapper (`friend: &Friend`, then `friend.number()`).
            classify a = case a of
                RsArg _ (TyPath p) | Just selfExpr <- lookup p idFields -> (Nothing, EVar selfExpr)
                RsArg n (TyPath p) | Just w <- find ((== p) . rwIdType) others ->
                    let n' = fromMaybe n (T.stripSuffix (idSuffix w) n)
                    in (Just (RsArg n' (TyRef Nothing (TyPath (rwName w)) False)), EMethodCall (EVar n') "number" [])
                RsArg n ty -> let a' = stripPrefix n ty in (Just a', EVar (keptName a'))
                _          -> error "resWrapperMethod: not a plain argument"
            classified = map classify valArgs
            -- A kept parameter loses the wrapper's prefix (`conference_peer_number`
            -- on `Conference`).
            wrapperPrefix = idToRustSnake (rwName rw) <> "_"
            stripPrefix n ty = RsArg (fromMaybe n (T.stripPrefix wrapperPrefix n)) ty
            keptName a' = case a' of
                RsArg n _ -> n
                _         -> error "resWrapperMethod: not a plain argument"
            keptArgs   = [ a' | (Just a', _) <- classified ]
            callArgs   = map snd classified
            recv = if K.endsId model cfn then RsSelf else RsSelfArg False
            -- The wrapper's root, then its path to the core.
            target = foldl EFieldAccess (EVar "self")
                (idToRustSnake (rootName model) : maybe [] (T.splitOn ".") (lookup (rootCName model) (rustCorePaths (S.targets model))))
            call = EMethodCall target (fnName core) callArgs
            (safeRet, body) = case fnRet core of
                Just (TyGeneric "std::result::Result" [t, errTy]) ->
                    ( Just (TyGeneric "Result" [t])
                    , EMethodCall call "map_err"
                        [EVar (errorType model <> "::" <> errorVariant (errPath errTy))] )
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
    -- The wrappers other functions can take: those an id alone names.
    others = [ w | w <- resWrappers model, rwName w /= rwName rw, isNothing (rwExtraId w) ]
    -- What an id's name adds to its wrapper's: @_number@ for a @FriendNumber@.
    idSuffix w = "_" <> idToRustSnake (fromMaybe (rwIdType w) (T.stripPrefix (rwName w) (rwIdType w)))

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
resWrapperGetters rw =
    getter "number" (rwIdType rw) "number"
    : [ getter (f <> "_number") t f | Just (f, t) <- [rwExtraId rw] ]
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
        ids = [ (f, t) | Just (f, t) <- [rwExtraId rw] ] ++ [("number", rwIdType rw)]
        fields = [ "    pub(crate) " <> f <> ": " <> t <> "," | (f, t) <- ids ]
        dbgFields = [ "            .field(\"" <> f <> "\", &self." <> f <> ")" | (f, _) <- ids ]
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

nonGeneratableNote :: SemanticModel -> Text -> Text
nonGeneratableNote model w = maybe "" T.unlines (lookup w (rustWrapperNotes (S.targets model)))

generateResourceWrapperFile :: SemanticModel -> ResWrapper -> (FilePath, Text)
generateResourceWrapperFile model rw =
    let n = rwName rw
        fileName = "safe_" <> T.toLower n <> ".rs"
        items = resWrapperGetters rw ++ resWrapperMethods model rw
        implBody = render (RsModule [RsItemImpl (RsImpl Nothing
                       (TyGeneric n [TyPath "'a"]) ["'a"] items)])
        -- The root, and the wrappers its methods take: @Friend@ on a @Group@.
        taken = [ w | RsItemFn f <- items, RsArg _ (TyRef _ (TyPath w) _) <- fnArgs f, w `elem` map rwName (resWrappers model) ]
        uses = [ "use crate::" <> rootNamespace model <> "::" <> t <> ";\n" | t <- sort (nub (idToRustPascal (rootName model) : taken)) ]
        content = "// Generated by apigen. Do not edit.\n"
               <> T.concat uses
               <> "use crate::types::*;\n\n"
               <> nonGeneratableNote model n
               <> resWrapperHeader model rw
               <> implBody
    in (T.unpack fileName, content)

generateResourceWrappers :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateResourceWrappers model _items =
    map (generateResourceWrapperFile model) (resWrappers model)
