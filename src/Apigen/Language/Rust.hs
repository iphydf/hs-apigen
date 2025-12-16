{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Rust (generate, Options (..)) where

import           Apigen.Inference            (deriveHandleName, getHierarchy,
                                              inferCFunctionMapping)
import qualified Apigen.Inference            as I
import           Apigen.Language.Rust.AST
import           Apigen.Language.Rust.Pretty (render)
import           Apigen.Semantic
import qualified Apigen.Semantic             as S
import           Apigen.Types                (Constness (..))
import           Data.Char                   (isAlphaNum, isPunctuation,
                                              isUpper, toUpper)
import           Data.List                   (find, findIndex, groupBy, last,
                                              lookup, nub, partition, sortOn)
import           Data.Maybe                  (catMaybes, fromMaybe, isJust,
                                              isNothing, mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Debug.Trace                 (trace)
import           System.FilePath             (takeBaseName)
import qualified Text.Casing                 as Casing

data Options = Options
    { typesOnly :: Bool
    }

data ModuleKind = MainModule | ExtensionModule Text deriving (Eq, Show)

knownSubsystems :: [Text]
knownSubsystems = ["friend", "file", "conference", "group", "pass_key", "pass", "options", "events", "av", "toxav"]

sizeToType :: [(Text, Text)]
sizeToType =
    [ ("TOX_ADDRESS_SIZE", "Address")
    , ("TOX_PUBLIC_KEY_SIZE", "PublicKey")
    , ("TOX_FILE_ID_LENGTH", "FileId")
    , ("TOX_CONFERENCE_ID_SIZE", "ConferenceId")
    ]

isReferenceResource :: SResource -> Bool
isReferenceResource res =
    let name = resourceName res
    in "Event" == name || "Event_" `T.isPrefixOf` name || "Conference_Offline_Peer" == name || "Conference_Peer" == name || "Group_Peer" == name

hasLifetime :: SResource -> Bool
hasLifetime res = isReferenceResource res || resourceName res `elem` ["Events", "AV", "ToxAV"]

generate :: Options -> SemanticModel -> [(FilePath, Text)]
generate opts model =
    let
        resources = S.resources model

        mkItems res =
            let hAncestor = findHandleAncestor resources res
                mainItem = (MainModule, res)
            in if resourceName res == "Tox"
               then splitToxResource opts model res
               else if resourceName res == hAncestor || isResId (resourceType res)
                    then [mainItem]
                    else [mainItem, (ExtensionModule hAncestor, res)]
          where isResId (ResId _) = True
                isResId _         = False

        allItems = concatMap mkItems resources

        -- Group by module name
        groupedItems = groupByModuleName allItems

        modules = map (generateModule opts model) groupedItems

        -- Generate mod.rs
        moduleNames = nub $ map (\(fp, _) -> T.pack (takeBaseName fp)) modules
        modRs = generateAggregateModule "mod" moduleNames

    in modules ++ [modRs]

splitToxResource :: Options -> SemanticModel -> SResource -> [(ModuleKind, SResource)]
splitToxResource _ _ tox =
    let
        allMethods = methods tox
        grouped = trace ("Splitting Tox, methods=" ++ show (length allMethods)) $ groupMethodsBySubsystem allMethods

        mkItem (subsys, subMethods) =
            let kind = if subsys == "tox" then MainModule else ExtensionModule "Tox"
                resName = if subsys == "tox" then "Tox" else T.pack (Casing.toPascal (Casing.fromAny (T.unpack subsys)))
                res = tox { resourceName = resName
                          , methods = subMethods
                          }
            in (kind, res)

    in map mkItem grouped

groupMethodsBySubsystem :: [SMethod] -> [(Text, [SMethod])]
groupMethodsBySubsystem methods =
    let
        classify m =
            let name = S.methodName m
                nameWithoutTox = if "tox_" `T.isPrefixOf` name then T.drop 4 name else name

                -- Find longest matching subsystem prefix
                match = find (\s -> (s <> "_") `T.isPrefixOf` nameWithoutTox) sortedSubsystems
            in case match of
                Just s -> s
                Nothing -> if "toxav_" `T.isPrefixOf` name then "toxav" else "tox"

        sortedSubsystems = sortOn (\s -> negate (T.length s)) knownSubsystems

        grouped = groupBy (\a b -> classify a == classify b) (sortOn classify methods)
    in mapMaybe (\g -> case g of
                            (m:_) -> Just (classify m, g)
                            []    -> Nothing
                        ) grouped

groupByModuleName :: [(ModuleKind, SResource)] -> [[(ModuleKind, SResource)]]
groupByModuleName items =
    groupBy (\a b -> resourceToModuleName a == resourceToModuleName b) (sortOn resourceToModuleName items)

resourceToModName :: Text -> Text
resourceToModName name =
    let nameSnake = idToRustSnake name
    in if "event" `T.isPrefixOf` nameSnake then "events"
       else if nameSnake `elem` ["av", "toxav"] then "av"
       else
            let match = find (\s -> s == nameSnake || (s <> "_") `T.isPrefixOf` nameSnake) sortedSubsystems
            in fromMaybe nameSnake match
  where
    sortedSubsystems = sortOn (\s -> negate (T.length s)) knownSubsystems


resourceToModuleName :: (ModuleKind, SResource) -> Text
resourceToModuleName (ExtensionModule base, _) | base == "Tox" = "tox"
resourceToModuleName (ExtensionModule base, _) | base /= "Tox" =
    resourceToModName base
resourceToModuleName (_, r) =
    let modName = resourceToModName (resourceName r)
    in if modName `elem` ["friend", "group", "file", "conference", "events"]
       then "tox"
       else modName

findHandleAncestor :: [SResource] -> SResource -> Text
findHandleAncestor allRes res =
    case resourceType res of
        ResHandle -> resourceName res
        ResId _ -> case parent res of
            Just pName -> case find ((== pName) . resourceName) allRes of
                Just pRes -> findHandleAncestor allRes pRes
                Nothing   -> "Tox"
            Nothing -> "Tox"

generateAggregateModule :: Text -> [Text] -> (FilePath, Text)
generateAggregateModule name modules =
    let
        mkItems m =
            let modItem = RsMod Pub m Nothing
                useItem = RsUse Pub ("self::" <> m <> "::*")
                extra = if m == "av"
                        then [RsUse Pub "self::av as toxav"]
                        else []
            in [modItem, useItem] ++ extra

        items = concatMap mkItems modules
        rsMod = RsModule items
        content = "#![allow(unused_imports)]\n" <> render rsMod
    in (T.unpack name <> ".rs", content)
generateModule :: Options -> SemanticModel -> [(ModuleKind, SResource)] -> (FilePath, Text)
generateModule _opts model items =
    let
        -- Assume all items in group map to same filename
        (firstKind, firstRes) = case items of
            (x:_) -> x
            []    -> error "generateModule: empty items"

        modName = resourceToModuleName (firstKind, firstRes)
        fileName = T.unpack modName <> ".rs"

        collectImports (ExtensionModule base) =
            let modName' = resourceToModName base
                structName = idToRustPascal base
            in if modName' == modName
               then []
               else [RsUse Private ("super::" <> modName' <> "::" <> structName)]
        collectImports _ = []

        -- Collect used handle types in methods
        usedHandles = nub $ concatMap (concatMap (mapMaybe usedHandle . S.inputs) . S.methods . snd) items
          where usedHandle p = case S.paramType p of
                    S.SHandle h -> if h `elem` [resourceName r | (_, r) <- items] then Nothing else Just h
                    _ -> Nothing

        handleImports = mapMaybe mkHandleImport usedHandles
          where mkHandleImport h =
                    let m = resourceToModName h
                        s = idToRustPascal h
                    in if m == modName || h == "void" || h `elem` ["uint8_t", "char"] then Nothing
                       else Just (RsUse Private ("super::" <> m <> "::" <> s))

        extraImports = nub $ concatMap (collectImports . fst) items ++ handleImports

        toxImport = if modName == "tox" then [] else [RsUse Private "super::tox::Tox"]
        needsAVHandler = modName == "av" || any (\h -> h == "AV" || h == "ToxAV") usedHandles
        imports = nub $ [RsUse Private "crate::ffi", RsUse Private "crate::types", RsUseAttr "allow(unused_imports)" "crate::types::*"] ++ extraImports ++ toxImport ++ [RsUse Private "crate::core::av_dispatch::ToxAVHandler" | needsAVHandler]

        genItem (kind, res) = generateResourceItems model kind res

        itemsRs = concatMap genItem items
        -- Generate variants that belong to this module
        matchedVariants = filter (\v ->
            let base = if S.variantName v == "Event" then "events" else idToRustSnake (S.variantName v)
                mapped = if base `elem` ["friend", "group", "file", "conference", "events"] then "tox" else base
            in mapped == modName) (S.variants model)
        variantsRs = concatMap (generateVariant model) matchedVariants

        extraItems = if modName == "options"
                     then [generateToxLogger, generateToxLoggerProxy]
                     else []

        rsMod = RsModule (imports ++ itemsRs ++ variantsRs ++ extraItems)
        content = "#![allow(unused_imports)]\n" <> render rsMod
    in (fileName, content)

generateToxLogger :: RsItem
generateToxLogger = RsItemTrait RsTrait
    { traitName = "ToxLogger"
    , traitVis = Pub
    , traitMethods = [ RsFn
        { fnName = "log"
        , fnVis = Private
        , fnAbi = Nothing
        , fnGenerics = []
        , fnArgs = [ RsSelfArg True
                   , RsArg "level" (TyPath "ToxLogLevel")
                   , RsArg "file" (TyPath "&str")
                   , RsArg "line" (TyPath "u32")
                   , RsArg "func" (TyPath "&str")
                   , RsArg "message" (TyPath "&str")
                   ]
        , fnRet = Nothing
        , fnBody = Nothing
        }
    ]
    }

generateToxLoggerProxy :: RsItem
generateToxLoggerProxy = RsItemFn RsFn
    { fnName = "tox_log_handler"
    , fnVis = Private
    , fnAbi = Just "\"C\""
    , fnGenerics = []
    , fnArgs = [ RsArg "_tox" (TyPath "*mut ffi::Tox")
               , RsArg "level" (TyPath "ffi::Tox_Log_Level")
               , RsArg "file" (TyPath "*const std::os::raw::c_char")
               , RsArg "line" (TyPath "u32")
               , RsArg "func" (TyPath "*const std::os::raw::c_char")
               , RsArg "message" (TyPath "*const std::os::raw::c_char")
               , RsArg "user_data" (TyPath "*mut std::ffi::c_void")
               ]
    , fnRet = Nothing
    , fnBody = Just $ RsBlock
        [ StmtExprNoSemi $ EUnsafe $ RsBlock
            [ StmtLet "logger" Nothing (ERef (EDeref (EParen (ECast (EVar "user_data") (TyPath "*mut Box<dyn ToxLogger>")))) True)
            , StmtLet "file" Nothing (EMethodCall (EMethodCall (ECall (EVar "std::ffi::CStr::from_ptr") [EVar "file"]) "to_str" []) "unwrap_or" [ELit (LString "")])
            , StmtLet "func" Nothing (EMethodCall (EMethodCall (ECall (EVar "std::ffi::CStr::from_ptr") [EVar "func"]) "to_str" []) "unwrap_or" [ELit (LString "")])
            , StmtLet "message" Nothing (EMethodCall (EMethodCall (ECall (EVar "std::ffi::CStr::from_ptr") [EVar "message"]) "to_str" []) "unwrap_or" [ELit (LString "")])
            , StmtExprNoSemi $ EMethodCall (EVar "logger") "log" [EVar "level", EVar "file", EVar "line", EVar "func", EVar "message"]
            ]
        ]
    }

generateOptionsSetLogger :: RsItem
generateOptionsSetLogger = RsItemFn RsFn
    { fnName = "set_logger"
    , fnVis = Pub
    , fnAbi = Nothing
    , fnGenerics = ["T: ToxLogger + 'static"]
    , fnArgs = [ RsSelfArg True
               , RsArg "logger" (TyPath "T")
               ]
    , fnRet = Nothing
    , fnBody = Just $ RsBlock
        [ StmtLet "boxed" Nothing (ECall (EVar "Box::new") [ECast (ECall (EVar "Box::new") [EVar "logger"]) (TyPath "Box<dyn ToxLogger>")])
        , StmtLet "ptr" Nothing (ECall (EVar "Box::into_raw") [EVar "boxed"])
        , StmtExpr $ EUnsafe $ RsBlock
            [ StmtExpr $ ECall (EVar "ffi::tox_options_set_log_callback") [EFieldAccess (EVar "self") "ptr", ECall (EVar "Some") [EVar "tox_log_handler"]]
            , StmtExprNoSemi $ ECall (EVar "ffi::tox_options_set_log_user_data") [EFieldAccess (EVar "self") "ptr", ECast (EVar "ptr") (TyPath "*mut std::ffi::c_void")]
            ]
        ]
    }

generateVariant :: SemanticModel -> SVariant -> [RsItem]
generateVariant model var =
    let
        name = idToRustPascal (S.variantName var)
        isEvent = name == "Event"
        generics = if isEvent then ["'a"] else []

        mkVariant mem =
            let vName = idToRustPascal (memberName mem)
                vTy = idToRustPascal (memberType mem)
                ty = if isEvent then TyPath (vTy <> "<'a>") else TyPath vTy
            in RsEnumVariant vName [ty]

        variants' = map mkVariant (variantMembers var)

        rsEnum = RsItemEnum RsEnum
            { enumName = name
            , enumVis = Pub
            , enumGenerics = generics
            , enumVariants = variants'
            }

        -- Implementation with from_ptr
        implType = if isEvent then TyGeneric name [TyPath "'a"] else TyPath name

        ptrTy = case find (\r -> resourceName r == S.variantName var) (S.resources model) of
            Just r  -> "*const ffi::" <> cName r
            Nothing -> "*const std::ffi::c_void"

        mkArm mem =
            let vName = idToRustPascal (memberName mem)
                getter = "ffi::" <> memberGetter mem
                vTy = idToRustPascal (memberType mem)

                -- Find the C name from the enum model
                enumMod = find (\e -> S.enumSemanticName e == S.variantTypeEnum var) (S.enums model)
                cVariantName = case enumMod of
                    Just e -> fromMaybe (memberName mem) $ lookup (memberName mem) (map (\(c, s) -> (s, c)) (S.enumMembers e))
                    Nothing -> memberName mem

                enumTypeName = fromMaybe (idToRustPascal (S.variantTypeEnum var)) (fmap S.enumName enumMod)
                enumVariant = "ffi::" <> enumTypeName <> "::" <> cVariantName

                innerCall = EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar getter) [EVar "ptr"])])
                wrapped = ECall (EVar vTy) [ERef (EDeref innerCall) False]
            in (PPath enumVariant, RsBlock [StmtExprNoSemi (ECall (EVar (name <> "::" <> vName)) [wrapped])])

        fromPtrBody = RsBlock
            [ StmtLet "type_" Nothing (EUnsafe (RsBlock [StmtExprNoSemi (ECall (EVar ("ffi::" <> T.toLower (S.commonPrefix model) <> "_" <> idToRustSnake (S.variantName var) <> "_get_type")) [EVar "ptr"])]))
            , StmtExprNoSemi (EMatch (EVar "type_") (map mkArm (variantMembers var) ++ [(PWildcard, RsBlock [StmtExpr (ECall (EVar "panic!") [ELit (LString "Unknown variant type")])])]))
            ]

        fromPtrFn = RsItemFn RsFn
            { fnName = "from_ptr"
            , fnVis = Pub
            , fnAbi = Nothing
            , fnGenerics = []
            , fnArgs = [RsArg "ptr" (TyPath ptrTy)]
            , fnRet = Just implType
            , fnBody = Just fromPtrBody
            }

        rsImpl = RsItemImpl RsImpl
            { implTrait = Nothing
            , implType = implType
            , implGenerics = generics
            , implItems = [fromPtrFn]
            }

    in [rsEnum, rsImpl]

generateResourceItems :: SemanticModel -> ModuleKind -> SResource -> [RsItem]
generateResourceItems model kind res =
    let
        structs = generateResourceStructs model kind res
        (implItems, staticItems) = generateResourceMethods model kind res
        traitItems = case kind of
            MainModule -> concatMap (generateTraitItems model res) (traits res)
            _ -> []
    in structs ++ implItems ++ staticItems ++ traitItems

generateTraitItems :: SemanticModel -> SResource -> SResourceTrait -> [RsItem]
generateTraitItems model res (Iterable elemTy sizeMethod accessor) =
    let name = idToRustPascal (resourceName res)
        isLife = hasLifetime res

        stripCommon n = fromMaybe n $ T.stripPrefix (T.toLower (S.commonPrefix model) <> "_") (T.toLower n)

        stripMethodName n =
            let nameLower = T.toLower n
                stripped = case resourceType res of
                    ResHandle -> fromMaybe (stripCommon nameLower) $ T.stripPrefix (T.toLower (S.cPrefix res)) nameLower
                    ResId _ -> stripCommon nameLower
            in idToRustSnake stripped

        accName = stripMethodName (T.toLower (S.cPrefix res) <> accessor)
        sizeName = stripMethodName (T.toLower (S.cPrefix res) <> sizeMethod)

        itemTy = toSafeRetType model res Nothing elemTy
        itemTyLife = case elemTy of
            SHandle h ->
                case find (\r -> resourceName r == h) (S.resources model) of
                    Just r | hasLifetime r -> TyGeneric (idToRustPascal h) [TyPath "'a"]
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
                             ] ++ [ RsStructField "_marker" (TyGeneric "std::marker::PhantomData" [TyRef (Just "'a") (TyUnit) False]) Private | isLife && not (isReferenceResource res) ]
            }

        nextBody = RsBlock
            [ StmtExpr (EIf (EBinOp ">=" (EFieldAccess (EVar "self") "index") (EFieldAccess (EVar "self") "count")) (RsBlock [StmtReturn (EVar "None")]) Nothing)
            , StmtLet "item" Nothing (EMethodCall (EFieldAccess (EVar "self") "parent") accName [EFieldAccess (EVar "self") "index"])
            , StmtExpr (EBinOp "=" (EFieldAccess (EVar "self") "index") (EBinOp "+" (EFieldAccess (EVar "self") "index") (ELit (LInt 1))))
            , StmtExprNoSemi (ECall (EVar "Some") [EVar "item"])
            ]


        iterImpl = RsItemImpl RsImpl
            { implTrait = Just "Iterator"
            , implType = TyGeneric iterName (if isLife then [TyPath "'a"] else [])
            , implGenerics = if isLife then ["'a"] else []
            , implItems = [
                RsItemTypeAlias RsTypeAlias { aliasName = "Item", aliasVis = Private, aliasType = itemTyLife }
              , RsItemFn RsFn { fnName = "next", fnVis = Private, fnAbi = Nothing, fnGenerics = [], fnArgs = [RsSelfArg True], fnRet = Just (TyGeneric "Option" [itemTyLife]), fnBody = Just nextBody }
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
                    ] ++ [("_marker", EVar "std::marker::PhantomData") | isLife && not (isReferenceResource res)]))
                ]) }
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

        -- Determine if the target struct itself has a lifetime
        structHasLife = case find (\r -> idToRustPascal (resourceName r) == structBaseName) (S.resources model) of
            Just r  -> hasLifetime r
            Nothing -> False

        isLife = structHasLife
        gens = if structBaseName == "ToxAV" then ["'a", "H: ToxAVHandler"]
               else if isLife then ["'a"] else []
        structTy = if structBaseName == "ToxAV" then TyGeneric structBaseName [TyPath "'a", TyPath "H"]
                   else if isLife then TyGeneric structBaseName [TyPath "'a"] else TyPath structBaseName

        (staticMethods, instanceMethods') = partition (\m -> methodRole m == Constructor || methodRole m == StaticRole) (S.methods res)
        instanceMethods = if resourceName res == "Options"
                          then filter (\m -> S.methodName m `notElem` ["tox_options_set_log_callback", "tox_options_set_log_user_data", "tox_options_get_log_callback", "tox_options_get_log_user_data"]) instanceMethods'
                          else instanceMethods'

        (realStatic, ctors) = partition (\m -> methodRole m == StaticRole) staticMethods

        methodToItem = generateMethod model kind res

        implItems = if (null instanceMethods && null ctors && null realStatic) || (kind == MainModule && isVariantResource model res)
                    then trace ("Skipping " ++ show (resourceName res) ++ " kind=" ++ show kind ++ " isVar=" ++ show (isVariantResource model res)) []
                    else trace ("Generating " ++ show (resourceName res) ++ " kind=" ++ show kind) [RsItemImpl (RsImpl Nothing structTy gens (concatMap methodToItem (ctors ++ instanceMethods ++ realStatic) ++ extraImplItems))]
          where extraImplItems = if resourceName res == "Options" && kind == MainModule
                                 then [generateOptionsSetLogger]
                                 else []

        staticItems = []
    in (implItems, staticItems)

isVariantResource :: SemanticModel -> SResource -> Bool
isVariantResource model res = any (\v -> S.variantName v == resourceName res) (S.variants model) && resourceName res /= "Events"

generateResourceStructs :: SemanticModel -> ModuleKind -> SResource -> [RsItem]
generateResourceStructs model kind res =
    case kind of
        ExtensionModule _ -> []
        MainModule ->
            if isVariantResource model res then [] else
            let name = idToRustPascal (resourceName res)
            in case resourceType res of
                ResId _ ->
                    let name' = if "Number" `T.isSuffixOf` name then name else name <> "Number"
                    in if name' `elem` knownTypes
                       then [] -- Provided by crate::types
                       else [ RsItemTupleStruct RsTupleStruct
                                { tupleStructName = name'
                                , tupleStructVis = Pub
                                , tupleStructGenerics = []
                                , tupleStructFields = [ TyPath "u32" ]
                                }
                            ]
                ResHandle ->
                    if name == "ToxAV" then
                        [ RsItemStruct RsStruct
                            { structName = "ToxAV"
                            , structVis = Pub
                            , structGenerics = ["'a", "H: ToxAVHandler"]
                            , structFields = [ RsStructField "ptr" (TyPath "*mut ffi::ToxAV") PubCrate
                                             , RsStructField "handler" (TyGeneric "Box" [TyPath "H"]) Pub
                                             , RsStructField "_tox" (TyGeneric "std::marker::PhantomData" [TyRef (Just "'a") (TyPath "Tox") False]) Private
                                             ]
                            }
                        ]
                    else
                        let isRef = isReferenceResource res
                            isLife = hasLifetime res
                            gens = if isLife then ["'a"] else []

                            structItem = if isRef
                                then RsItemTupleStruct RsTupleStruct
                                    { tupleStructName = name
                                    , tupleStructVis = Pub
                                    , tupleStructGenerics = gens
                                    , tupleStructFields = [ TyRef (Just "'a") (TyPath ("ffi::" <> cName res)) False ]
                                    }
                                else RsItemStruct RsStruct
                                    { structName = name
                                    , structVis = Pub
                                    , structGenerics = gens
                                    , structFields = [ RsStructField "ptr" (TyPath ("*mut ffi::" <> cName res)) PubCrate ]
                                                     ++ [ RsStructField "_marker" (TyGeneric "std::marker::PhantomData" [TyRef (Just "'a") (TyUnit) False]) Private | isLife ]
                                    }
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

                name = idToRustPascal (resourceName res)
                isLife = hasLifetime res
                structTy = if isLife then TyGeneric name [TyPath "'a"] else TyPath name
                gens = if isLife then ["'a"] else []

            in [RsItemImpl (RsImpl (Just "Drop") structTy gens [RsItemFn (RsFn "drop" Private Nothing [] [RsSelfArg True] Nothing (Just (RsBlock [body])))])]
        Nothing -> []

knownTypes :: [Text]
knownTypes =
    [
 "FriendNumber"
    , "GroupNumber"
    , "FileNumber"
    , "ConferenceNumber"
    , "ConferencePeerNumber"
    , "MessageId"
    , "Address"
    , "PublicKey"
    , "SecretKey"
    , "SharedKey"
    , "FileId"
    , "ConferenceId"
    , "AvNumber"
    , "FriendMessageId"
    , "GroupMessageId"
    , "GroupPeerNumber"
    , "ToxEvents"
    ]

generateMethod :: SemanticModel -> ModuleKind -> SResource -> SMethod -> [RsItem]
generateMethod model kind res method =
    let
        mapping = case methodMapping method of
            StandardMapping -> inferCFunctionMapping (resources model) res method
            CustomMapping m -> m

        isToxAVType (SHandle "AV")    = True
        isToxAVType (SHandle "ToxAV") = True
        isToxAVType (SList t)         = isToxAVType t
        isToxAVType _                 = False

        usesToxAV = any (isToxAVType . paramType) (S.inputs method) || isToxAVType (output method)

        structName = case kind of
            MainModule -> case resourceType res of
                ResHandle -> idToRustPascal (resourceName res)
                ResId _   -> fromMaybe "Tox" (S.parent res)
            ExtensionModule base -> idToRustPascal base

        needsH = usesToxAV && structName /= "ToxAV"

        hasUserData = methodHasUserData method
        generics = (if needsH then ["'a"] else []) ++ (if hasUserData then ["T"] else []) ++ (if needsH then ["H: ToxAVHandler"] else [])

        isTox = structName == "Tox"

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
                    ExtensionModule "Tox" -> stripCommon nameLower
                    _ -> nameLower

                -- Special case for toxav_ which doesn't match the common prefix tox_
                stripped' = if "toxav_" `T.isPrefixOf` stripped
                            then T.drop 6 stripped
                            else if "toxav" `T.isPrefixOf` stripped
                            then T.drop 5 stripped
                            else stripped
            in idToRustSnake stripped'

        modName = resourceToModuleName (kind, res)
        methodName' = if modName == "options"
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
                        let constness = if isTox then ConstThis else methodConstness method
                            selfArg = if constness == ConstThis then RsSelfArg False else RsSelfArg True
                        in [selfArg] ++ args

    in [ RsItemFn RsFn
            { fnName = methodName'
            , fnVis = Pub
            , fnAbi = Nothing
            , fnGenerics = generics
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

        modName = resourceToModuleName (kind, res)
        originalName = S.methodName method

        -- 1. Context Arguments
        (contextArgs, ctxNames) = generateContextArgs model kind res (methodConstness method) method cArgs

        -- 2. Safe Arguments
        safeArgs = mapMaybe (toSafeArg model semParams) (zip cArgs [0..])

        finalSafeArgs = safeArgs

        hasUserData = methodHasUserData method
        isToxAVNew = modName == "av" && originalName == "toxav_new"
        userDataArg = if isToxAVNew
                      then RsArg "handler" (TyPath "H")
                      else RsArg "user_data" (TyPath "&mut T")

        finalArgs = contextArgs ++ finalSafeArgs ++ (if hasUserData || isToxAVNew then [userDataArg] else [])

        -- 3. Resolve C Arguments
        (argExprs, stmts) = resolveArguments semParams ctxNames cArgs

        -- 4. Return Type Logic
        isPassFunc = "tox_pass_" `T.isPrefixOf` cFunc
        isEncrypt = "encrypt" `T.isSuffixOf` cFunc
        isDecrypt = "decrypt" `T.isSuffixOf` cFunc
        isCrypto = isPassFunc && (isEncrypt || isDecrypt)
        isGetSalt = cFunc == "tox_get_salt"

        isSBytesNoSize = case output method of SBytes -> isNothing (cSizeFunctionName mapping); _ -> False

        hasErrorPtr = any (\(a) -> case a of ErrorPtr -> True; _ -> False) cArgs

        rawRetType = if isCrypto
                     then TyGeneric "Vec" [TyPath "u8"]
                     else if isGetSalt
                     then TyArray (TyPath "u8") "ffi::TOX_PASS_SALT_LENGTH as usize"
                     else if isSBytesNoSize then TyPath "*const u8"
                     else if output method == SVoid then TyUnit else toSafeRetType model res (Just (methodRole method)) (output method)

        errEnumName = fromMaybe "Tox_Err_Unknown" (methodErrorType method)
        screaming = T.toUpper $ T.pack $ Casing.toSnake $ Casing.fromAny $ T.unpack errEnumName
        okVariant = "ffi::" <> errEnumName <> "::" <> screaming <> "_OK"

        rawRetType' = case methodResultStrategy method of
                        IgnoreReturn -> TyUnit
                        _            -> rawRetType

        finalRetType = if hasErrorPtr
                       then Just (TyGeneric "std::result::Result" [rawRetType', TyPath ("ffi::" <> errEnumName)])
                       else if rawRetType == TyUnit then Nothing else Just rawRetType

        -- 5. Dispatch
        hasBufferPtr = any (\(a) -> case a of BufferPtr _ -> True; _ -> False) cArgs
        macroArgs = filterArgExprs cArgs argExprs

        bodyStmt = if isCrypto
            then generateEncryptionBody cFunc cArgs semParams ctxNames hasErrorPtr okVariant isDecrypt
            else if isGetSalt
            then generateFixedBytesBody method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr okVariant "TOX_PASS_SALT_LENGTH"
            else case output method of
                SFixedBytes sizeConst _ ->
                    generateFixedBytesBody method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr okVariant sizeConst
                _ -> case cSizeFunctionName mapping of
                    Just sizeFunc ->
                        if hasBufferPtr then
                             generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant
                        else
                             generateAccessBody cFunc macroArgs sizeFunc
                    Nothing ->
                         generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant

    in (finalArgs, finalRetType, RsBlock (stmts ++ [bodyStmt]))

generateEncryptionBody :: Text -> [CArgSource] -> [SParameter] -> [Text] -> Bool -> Text -> Bool -> RsStmt
generateEncryptionBody cFunc cArgs semParams ctxNames _hasErrorPtr okVariant isDecrypt =
    let
        (allArgExprs, _) = resolveArguments semParams ctxNames cArgs

        -- The input parameter is usually the one that matches the output length (plus/minus extra)
        inputParamIdx = if isDecrypt
                        then fromMaybe 0 (findIdx isSBytes semParams)
                        else fromMaybe 0 (findIdx isSBytes semParams)
          where findIdx p xs = findIndex p xs
                isSBytes p = paramType p == SBytes

        inputParam = if null semParams then error "generateEncryptionBody: empty semParams" else semParams !! inputParamIdx
        inputName = idToRustSnake (paramName inputParam)
        inputLen = EMethodCall (EVar inputName) "len" []

        extraLen = ECast (EVar "ffi::TOX_PASS_ENCRYPTION_EXTRA_LENGTH") (TyPath "usize")

        checkStmt = if isDecrypt
                    then [ StmtExpr (EIf (EBinOp "<" inputLen extraLen)
                            (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "ffi::Tox_Err_Decryption::TOX_ERR_DECRYPTION_INVALID_LENGTH.into()"])])
                            Nothing) ]
                    else []

        calcLen = if isDecrypt
                  then ECall (EVar "std::ops::Sub::sub") [inputLen, extraLen]
                  else ECall (EVar "std::ops::Add::add") [inputLen, extraLen]

        lenStmt = StmtLet "len" Nothing calcLen
        bufInit = EMacroCall "vec" ["0u8; len"]

        fixedArgs = zipWith (\(src) expr -> case src of
            BufferPtr _ -> EMethodCall (EVar "buf") "as_mut_ptr" []
            ErrorPtr    -> EVar "&mut err"
            _           -> expr) cArgs allArgExprs

        stmts = checkStmt
                ++ [ lenStmt
                   , StmtLet "mut buf" Nothing bufInit
                   ]
                ++ [ StmtLet "mut err" Nothing (EVar okVariant)
                   , StmtExpr (ECall (EVar ("ffi::" <> cFunc)) fixedArgs)
                   , StmtExpr (EIf (EVar ("err != " <> okVariant))
                          (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "err"])])
                          Nothing)
                   , StmtExprNoSemi (ECall (EVar "Ok") [EVar "buf"])
                   ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateFixedBytesBody :: SMethod -> Text -> [CArgSource] -> [SParameter] -> [Text] -> Bool -> Bool -> Text -> Text -> RsStmt
generateFixedBytesBody method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr okVariant sizeConst =
    let (allArgExprs, _) = resolveArguments semParams ctxNames cArgs
        macroArgs = filterArgExprs cArgs allArgExprs
        c' = if "TOX_" `T.isPrefixOf` sizeConst then T.drop 4 sizeConst else sizeConst

        wrapType' t expr = case t of
            SFixedBytes sc _ ->
                case lookup sc sizeToType of
                    Just ty -> ECall (EVar ty) [expr]
                    Nothing -> expr
            _ -> expr

    in if hasBufferPtr && hasErrorPtr then
        let macroCall = ECall (EVar "ffi_get_array!") ([EVar cFunc, EVar okVariant, EVar ("types::" <> c')] ++ macroArgs)
        in case lookup sizeConst sizeToType of
            Just ty -> StmtExprNoSemi (EMethodCall macroCall "map" [EVar ty])
            Nothing -> StmtExprNoSemi macroCall
    else if hasBufferPtr then
        let
            sizeExpr = constSizeExpr sizeConst
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
                                    (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "err"])])
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
            sizeExpr = constSizeExpr sizeConst

            stmts = [ StmtLet "ptr" Nothing callPtr
                    , StmtLet "slice" Nothing (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr])
                    , StmtExprNoSemi (wrapType' (output method) (EMethodCall (EMethodCall (EMethodCall (EVar "slice") "try_into" []) "unwrap" []) "clone" []))
                    ]
        in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateAccessBody :: Text -> [RsExpr] -> Text -> RsStmt
generateAccessBody cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        accessStmts = [ StmtLet "size" Nothing callSize
                , StmtLet "ptr" Nothing callPtr
                , StmtExprNoSemi (EMethodCall (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", ECast (EVar "size") (TyPath "usize")]) "to_vec" [])
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

generateStandardBody :: SemanticModel -> SResource -> SMethod -> CFunctionMapping -> Text -> [CArgSource] -> [RsExpr] -> Bool -> Bool -> RsType -> Text -> RsStmt
generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType okVariant =
    let
        macroArgs = filterArgExprs cArgs argExprs

        (rawElemTyName, mapResult) = case output method of
            SList (SResourceId resName) ->
                let idName = idToRustPascal resName
                    wrapper = if "Number" `T.isSuffixOf` idName || idName `elem` knownTypes
                              then idName
                              else idName <> "Number"
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
                                    wrapper = if "Number" `T.isSuffixOf` idName || idName `elem` knownTypes
                                              then idName
                                              else idName <> "Number"
                                in EMethodCall callExpr "map" [EVar wrapper]
                            t ->
                                if any (\r -> resourceName r == case t of SHandle h -> h; _ -> "") (S.resources model)
                                   || case t of SFixedBytes sc _ -> isJust (lookup sc sizeToType); _ -> False
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
                    else
                         let call = ECall (EVar ("ffi::" <> cFunc)) argExprs
                             wrapped = wrapType model res (Just (methodRole method)) (output method) (EUnsafe (RsBlock [StmtExprNoSemi call]))
                         in StmtReturn wrapped

constSizeExpr :: Text -> RsExpr
constSizeExpr c =
    let c' = if "TOX_" `T.isPrefixOf` c then T.drop 4 c else c
        name = "types::" <> c'
    in if T.any isUpper c'
       then ECast (EVar name) (TyPath "usize") -- Constant
       else ECast (ECall (EVar name) []) (TyPath "usize") -- Function call

-- | Resolves all C arguments to Rust expressions + statements
resolveArguments :: [SParameter] -> [Text] -> [CArgSource] -> ([RsExpr], [RsStmt])
resolveArguments semParams ctxNames cArgs =
    let (exprs, (stmts, _)) = mapAccumL (resolveOne semParams ctxNames) ([], -1) cArgs
    in (exprs, stmts)

-- | Accumulator: (Statements, Last Semantic Param Index)
resolveOne :: [SParameter] -> [Text] -> ([RsStmt], Int) -> CArgSource -> (([RsStmt], Int), RsExpr)
resolveOne semParams _ (stmts, _) (SemanticArg i) =
    let param = semParams !! i
        name = idToRustSnake (paramName param)
        isMutable = paramConstness param == MutableThis
        ptrMethod = if isMutable then "as_mut_ptr" else "as_ptr"

        expr = case paramType param of
            SString ->
                let ptr = EMethodCall (EVar name) "as_ptr" []
                in if isMutable
                   then ECast ptr (TyPath "*mut std::os::raw::c_char")
                   else ECast ptr (TyPath "*const std::os::raw::c_char")
            SBytes          -> EMethodCall (EVar name) ptrMethod []
            SList _         -> EMethodCall (EVar name) ptrMethod []
            SFixedList {}   -> EMethodCall (EVar name) ptrMethod []
            SFixedBytes sizeConst _ ->
                if isJust (lookup sizeConst sizeToType)
                then
                    if name == "file_id"
                    then EMethodCall (EVar name) "map_or" [EVar "std::ptr::null()", ELambda ["v"] (EMethodCall (EFieldAccess (EVar "v") "0") ptrMethod [])]
                    else EMethodCall (EFieldAccess (EVar name) "0") ptrMethod []
                else EMethodCall (EVar name) ptrMethod []
            SResourceId _   -> EFieldAccess (EVar name) "0"
            SHandle _       -> EFieldAccess (EVar name) "ptr"
            _               -> EVar name
    in ((stmts, i), expr)

resolveOne semParams _ (stmts, lastIdx) BufferSize =
    let expr = if lastIdx >= 0 && lastIdx < length semParams then
            let param = semParams !! lastIdx
                name = idToRustSnake (paramName param)
            in EMethodCall (EVar name) "len" []
        else ELit (LInt 0)
    in ((stmts, lastIdx), expr)

resolveOne _ ctxNames (stmts, lastIdx) (PathObject n _) =
    let name = if n < length ctxNames then ctxNames !! n else "tox"
    in ((stmts, lastIdx), EVar name)

resolveOne _ ctxNames (stmts, lastIdx) (PathId n) =
    let idx = n
        name = if idx < length ctxNames then ctxNames !! idx else "unknown_id"
    in ((stmts, lastIdx), EFieldAccess (EVar name) "0")

resolveOne _ _ (stmts, lastIdx) ErrorPtr =
    ((stmts, lastIdx), EVar "&mut err")

resolveOne _ _ (stmts, lastIdx) (BufferPtr _) =
    ((stmts, lastIdx), EVar "std::ptr::null_mut()") -- Placeholder

resolveOne _ ctxNames (stmts, lastIdx) (ThisObject _) =
    ((stmts, lastIdx), EVar (last ctxNames))

resolveOne _ _ (stmts, lastIdx) UserData =
    ((stmts, lastIdx), ECast (ECast (EVar "user_data") (TyPath "*mut T")) (TyPath "*mut std::ffi::c_void"))

resolveOne _ _ _ arg =
    error $ "Apigen.Language.Rust: Unhandled CArgSource: " ++ show arg


generateContextArgs :: SemanticModel -> ModuleKind -> SResource -> Constness -> SMethod -> [CArgSource] -> ([RsArg], [Text])
generateContextArgs model kind res cns method cArgs =
    let
        path = getHierarchy (resources model) res
        fullArgs = case path of
            [] -> []
            (rootName:rest) ->
                let
                    rootCName = case find (\(r) -> resourceName r == rootName) (resources model) of
                        Just r -> cName r
                        Nothing -> if rootName == "Tox" then "Tox" else "Tox_" <> rootName

                    ptrType = if cns == MutableThis then "*mut ffi::" <> rootCName else "*const ffi::" <> rootCName
                    rootArg = RsArg (idToRustSnake rootName) (TyPath ptrType)

                    idArgs = map (toContextArg model cns) rest
                in [rootArg] ++ idArgs

        -- selfArg covers one of the used args
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
            Just r  -> if isReferenceResource r then "self.0" else "self.ptr"
            Nothing -> "self.ptr"

        ctxNames = [ if isCoveredBySelf i then selfName else argName (fullArgs !! i)
                   | i <- [0 .. length fullArgs - 1] ]

        -- Map CArgSource to index in fullArgs
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
        name = idToRustSnake (paramName param)
        baseTy = toSafeRsType model (paramType param)
        ty = if paramConstness param == MutableThis
             then case baseTy of
                 TyPath p -> if "*const" `T.isPrefixOf` p
                             then TyPath ("*mut" <> T.drop 6 p)
                             else if "&[u8]" == p
                             then TyPath "&mut [u8]"
                             else if p `elem` ["&PublicKey", "&SecretKey", "&Address"]
                             then TyPath ("&mut " <> T.drop 1 p)
                             else baseTy
                 _ -> baseTy
             else baseTy

        safeTy = case ty of
            TyPath "&FileId" | name == "file_id" -> TyGeneric "Option" [ty]
            TySlice _                            -> TyRef Nothing ty False
            _                                    -> ty
    in Just (RsArg name safeTy)
toSafeArg _ _ _ = Nothing

toSafeRetType :: SemanticModel -> SResource -> Maybe SMethodRole -> SType -> RsType
toSafeRetType model res mRole t =
    case t of
        SHandle n ->
            let name = idToRustPascal n
                isLife = any (\r -> resourceName r == n && hasLifetime r) (S.resources model)
                resName = if isLife then name <> "<'a>" else name
            in if n == "void" then TyPath "*mut std::ffi::c_void"
               else if n `elem` ["uint8_t", "char", "unsigned char"] then TyPath "u8"
               else if n == resourceName res && mRole == Just Constructor
               then TyPath "Self"
               else if any (\r -> resourceName r == n) (S.resources model)
               then TyPath resName
               else
                    let cTypeName = if n == "AV" then "ToxAV" else if "Tox" `T.isPrefixOf` n then n else "Tox" <> n -- Fallback
                    in TyPath ("*const ffi::" <> cTypeName)
        SString -> if mRole == Just StaticRole then TyPath "&'static str" else TyPath "&str"
        SBytes  -> TyGeneric "Vec" [TyPath "u8"]
        SFixedBytes sizeConst _ ->
            case lookup sizeConst sizeToType of
                Just ty -> TyPath ty
                Nothing ->
                    if not (T.null sizeConst) && T.all (\c -> isAlphaNum c || c == '_') sizeConst
                    then
                        let c' = if "TOX_" `T.isPrefixOf` sizeConst then T.drop 4 sizeConst else sizeConst
                        in TyArray (TyPath "u8") ("types::" <> c' <> " as usize")
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
toSafeRsType _ (SFixedBytes sizeConst _) =
    case lookup sizeConst sizeToType of
        Just ty -> TyPath ("&" <> ty)
        Nothing -> TyPath "&[u8]"

toSafeRsType model (SFixedList t _ _) = TySlice (toSafeRsType model t)
toSafeRsType model (SList t)          = TySlice (toSafeRsType model t)
toSafeRsType _ SVoid = TyUnit
toSafeRsType model (SEnum n) =
    let cName = case find (\(e) -> S.enumSemanticName e == n) (S.enums model) of
            Just e  -> S.enumName e
            Nothing -> "Tox_" <> n -- Fallback guess
    in TyPath ("ffi::" <> cName)
toSafeRsType _ (SResourceId n) =
    let name = idToRustPascal n
    in if "Number" `T.isSuffixOf` name || name `elem` knownTypes
       then TyPath name
       else TyPath (name <> "Number")
toSafeRsType model (SHandle n) =
    if n == "void" then TyPath "*mut std::ffi::c_void"
    else
        case find (\r -> resourceName r == n) (resources model) of
            Just r  ->
                let name = idToRustPascal n
                    isLife = hasLifetime r
                    resName = if name == "ToxAV" then "ToxAV<'a, H>" else if isLife then name <> "<'a>" else name
                in TyRef Nothing (TyPath resName) False
            Nothing ->
                let cTypeName = if n == "AV" then "ToxAV" else if "Tox" `T.isPrefixOf` n then n else "Tox" <> n -- Fallback
                in TyPath ("*const ffi::" <> cTypeName)
toSafeRsType model (SCallback n) =
    let cName = case find (\(cb) -> S.cbName cb == n) (S.callbacks model) of
            Just cb -> S.cbCName cb
            Nothing -> "tox_" <> n -- Fallback
    in TyPath ("ffi::" <> cName)
toSafeRsType _ _ = TyPath "()"

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
                   then ECall (EVar (structName <> "::from_ptr")) [expr]
                   else if h == "AV" && mRole == Just Constructor
                   then EStructInit "Self" [("ptr", expr), ("handler", ECall (EVar "Box::new") [EVar "handler"]), ("_tox", EVar "std::marker::PhantomData")]
                   else if isReferenceResource r
                   then ECall (EVar structName) [ERef (EDeref expr) False]
                   else if hasLifetime r
                   then EStructInit structName [("ptr", expr), ("_marker", EVar "std::marker::PhantomData")]
                   else EStructInit structName [("ptr", expr)]
            Nothing -> expr
    SFixedBytes sizeConst _ ->
        case lookup sizeConst sizeToType of
            Just ty -> ECall (EVar ty) [expr]
            Nothing -> expr
    _ -> expr

idToRustSnake :: Text -> Text
idToRustSnake t = safeName (T.toLower (T.pack (Casing.toSnake (Casing.fromAny (T.unpack t)))))

safeName :: Text -> Text
safeName "type"     = "r#type"
safeName "mod"      = "r#mod"
safeName "crate"    = "r#crate"
safeName "self"     = "self" -- self is allowed as first arg
safeName "super"    = "r#super"
safeName "fn"       = "r#fn"
safeName "let"      = "r#let"
safeName "if"       = "r#if"
safeName "else"     = "r#else"
safeName "match"    = "r#match"
safeName "while"    = "r#while"
safeName "for"      = "r#for"
safeName "loop"     = "r#loop"
safeName "break"    = "r#break"
safeName "continue" = "r#continue"
safeName "return"   = "r#return"
safeName "in"       = "r#in"
safeName "ref"      = "r#ref"
safeName "mut"      = "r#mut"
safeName "unsafe"   = "r#unsafe"
safeName "where"    = "r#where"
safeName "pub"      = "r#pub"
safeName "use"      = "r#use"
safeName "trait"    = "r#trait"
safeName "impl"     = "r#impl"
safeName "struct"   = "r#struct"
safeName "enum"     = "r#enum"
safeName "const"    = "r#const"
safeName "static"   = "r#static"
safeName "extern"   = "r#extern"
safeName "as"       = "r#as"
safeName "move"     = "r#move"
safeName "async"    = "r#async"
safeName "await"    = "r#await"
safeName "dyn"      = "r#dyn"
safeName "abstract" = "r#abstract"
safeName "become"   = "r#become"
safeName "box"      = "r#box"
safeName "do"       = "r#do"
safeName "final"    = "r#final"
safeName "macro"    = "r#macro"
safeName "override" = "r#override"
safeName "priv"     = "r#priv"
safeName "typeof"   = "r#typeof"
safeName "unsized"  = "r#unsized"
safeName "virtual"  = "r#virtual"
safeName "yield"    = "r#yield"
safeName "try"      = "r#try"
safeName n          = n

idToRustPascal :: Text -> Text
idToRustPascal "Events" = "ToxEvents"
idToRustPascal "AV"     = "ToxAV"
idToRustPascal "ToxAV"  = "ToxAV"
idToRustPascal t        = T.pack $ Casing.toPascal $ Casing.fromSnake $ T.unpack $ T.toLower t

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> ([y], acc)
mapAccumL _ z [] = ([], z)
mapAccumL f z (x:xs) =
    let (z', y) = f z x
        (ys, z'') = mapAccumL f z' xs
    in (y:ys, z'')
