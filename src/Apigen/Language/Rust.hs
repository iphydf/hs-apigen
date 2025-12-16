{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apigen.Language.Rust (generate, byteArrayConstants, Options (..)) where

import           Apigen.Inference            (deriveHandleName, getHierarchy,
                                              inferCFunctionMapping)
import qualified Apigen.Inference            as I
import           Apigen.Language.Rust.AST
import           Apigen.Language.Rust.Naming (idToRustPascal, idToRustSnake,
                                              paramSnake, safeName,
                                              toxErrorVariant)
import           Apigen.Language.Rust.Pretty (prettyType, render)
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

-- | The byte-array newtype for each size constant, from the
-- @typedef uint8_t X[SIZE]@ typedefs.
sizeToType :: SemanticModel -> [(Text, Text)]
sizeToType model =
    [ (sizeConst, idToRustPascal semName)
    | (sizeConst, semName) <- S.arrayTypes model
    ]

byteArrayConstants :: SemanticModel -> [Text]
byteArrayConstants = map fst . S.arrayTypes

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

        groupedItems = groupByModuleName allItems

        modules = map (generateModule opts model) groupedItems

        -- `dispatch` isn't a resource module, but `core` re-exports its traits.
        moduleNames = nub (map (\(fp, _) -> T.pack (takeBaseName fp)) modules)
                   ++ ["dispatch"]
        modRs = generateAggregateModule "mod" moduleNames

        -- Not in mod.rs: the hand-written `src/types.rs` includes it with
        -- `#[path]`.
        typesRs = generateTypesModule model

        -- Included by the hand-written `tox/mod.rs` and `toxav/mod.rs`.
        safeModules = generateSafeModules model allItems

        -- Pulled into `core` by hand-written glue.
        dispatchModule = generateDispatchModule model

        -- Included by the hand-written `tox/mod.rs`.
        resourceModules = generateResourceWrappers model allItems

    in modules ++ [modRs, typesRs] ++ safeModules ++ [dispatchModule] ++ resourceModules

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

-- | C enums the public API needs from headers apigen doesn't read (e.g.
-- @tox_private.h@).
privateEnumExtras :: [(Text, [Text])]
privateEnumExtras =
    [ ( "Tox_Err_Iterate_Options_New"
      , [ "TOX_ERR_ITERATE_OPTIONS_NEW_OK"
        , "TOX_ERR_ITERATE_OPTIONS_NEW_MALLOC"
        ]
      )
    ]

-- | Public newtypes with no C @idType@ in the parsed headers.
extraNewtypes :: [Text]
extraNewtypes = ["MessageId"]

-- | The generated half of @crate::types@. The @macro_rules!@ definitions
-- stay hand-written in @src/types.rs@, which includes this file.
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
            [ RsConst Pub (stripToxPrefix (constantName c))
                      (TyPath "usize")
                      ("ffi::" <> constantName c <> " as usize")
            | c <- constants model
            , isSizeConstant (constantName c)
            ]

        newtypeNames = map (idToRustPascal . idName) (idTypes model) ++ extraNewtypes
        newtypeInvocations =
            [ RsMacroCall "impl_safe_newtype" [n] | n <- nub newtypeNames ]

        byteArrayInvocations =
            [ RsMacroCall "impl_byte_array_type" [ty, stripToxPrefix sizeConst]
            | (sizeConst, ty) <- sizeToType model
            ]

        modelEnumPairs = [ (S.enumName e, map S.enumMemberCName (S.enumMembers e)) | e <- enums model ]
        allEnumPairs = sortOn fst (modelEnumPairs ++ privateEnumExtras)
        enumInvocations = map (uncurry mkEnumInvocation) allEnumPairs

        toxError = RsRaw (renderToxError model)

    in ( "types.rs"
       , render (RsModule
           ( [header]
          ++ sizeConstants
          ++ newtypeInvocations
          ++ byteArrayInvocations
          ++ enumInvocations
          ++ [toxError]
           ))
       )

isSizeConstant :: Text -> Bool
isSizeConstant n = "_SIZE" `T.isSuffixOf` n || "_LENGTH" `T.isSuffixOf` n

stripToxPrefix :: Text -> Text
stripToxPrefix n = fromMaybe n (T.stripPrefix "TOX_" n)

-- | Error enums keep their C name, others use 'safeEnumName'.
mkEnumInvocation :: Text -> [Text] -> RsItem
mkEnumInvocation cName members =
    let safe = fromMaybe cName (safeEnumName cName)
        block = "{\n" <> T.concat ["    " <> m <> ",\n" | m <- members] <> "}"
    in RsMacroCall "impl_tox_enum" [safe, "ffi::" <> cName, block]

errorEnumCName :: SemanticModel -> Text -> Maybe Text
errorEnumCName model sem =
    fmap S.enumName (find (\e -> S.enumName e == sem || S.enumSemanticName e == sem) (enums model))

-- | @ToxError@: one variant per error enum a generated method can return,
-- plus the private extras.
renderToxError :: SemanticModel -> Text
renderToxError model =
    let
        methodErrEnums = nub $ mapMaybe (errorEnumCName model)
            (concatMap (mapMaybe methodErrorType . methods) (resources model))
        privateErrEnums = [ c | (c, _) <- privateEnumExtras, "_Err_" `T.isInfixOf` c ]
        errEnums = sortOn toxErrorVariant (nub (methodErrEnums ++ privateErrEnums))

        variants = [ "    " <> toxErrorVariant c <> "(" <> c <> ")," | c <- errEnums ]
                ++ [ "    AvGroupError,"
                   , "    InvalidString(std_ffi::NulError),"
                   ]
    in T.unlines
        ( [ "// --- Safe Results ---"
          , ""
          , "#[derive(Debug, Clone, PartialEq, Eq)]"
          , "pub enum ToxError {"
          ]
       ++ variants
       ++ [ "}"
          , ""
          , "impl error::Error for ToxError {}"
          , "impl fmt::Display for ToxError {"
          , "    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {"
          , "        write!(f, \"{:?}\", self)"
          , "    }"
          , "}"
          , ""
          , "pub type Result<T> = std::result::Result<T, ToxError>;"
          ] )
generateModule :: Options -> SemanticModel -> [(ModuleKind, SResource)] -> (FilePath, Text)
generateModule _opts model items =
    let
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
        imports = nub $ [RsUse Private "crate::ffi", RsUseAttr "allow(unused_imports)" "crate::types::*"] ++ extraImports ++ toxImport ++ [RsUse Private "crate::core::dispatch::ToxAVHandler" | needsAVHandler]

        genItem (kind, res) = generateResourceItems model kind res

        itemsRs = concatMap genItem items
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
    -- The toxcore log callback can run on another thread.
    { traitName = "ToxLogger: Send + Sync"
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
        , fnUnsafe = False
        , fnDoc = []
        }
    ]
    }

-- | The C log callback. `CStr::from_ptr` and the `user_data` deref are UB on
-- null, and a panic must not unwind into C.
generateToxLoggerProxy :: RsItem
generateToxLoggerProxy = RsRaw $ T.unlines
    [ "extern \"C\" fn tox_log_handler("
    , "    _tox: *mut ffi::Tox,"
    , "    level: ffi::Tox_Log_Level,"
    , "    file: *const std::os::raw::c_char,"
    , "    line: u32,"
    , "    func: *const std::os::raw::c_char,"
    , "    message: *const std::os::raw::c_char,"
    , "    user_data: *mut std::ffi::c_void,"
    , ") {"
    , "    if user_data.is_null() {"
    , "        return;"
    , "    }"
    , "    // SAFETY: `user_data` is the `Box<dyn ToxLogger>` installed by"
    , "    // `set_logger`; a panic in the user logger must not unwind into C."
    , "    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| unsafe {"
    , "        let logger = &mut *(user_data as *mut Box<dyn ToxLogger>);"
    , "        let file = if file.is_null() {"
    , "            \"\""
    , "        } else {"
    , "            std::ffi::CStr::from_ptr(file).to_str().unwrap_or(\"\")"
    , "        };"
    , "        let func = if func.is_null() {"
    , "            \"\""
    , "        } else {"
    , "            std::ffi::CStr::from_ptr(func).to_str().unwrap_or(\"\")"
    , "        };"
    , "        let message = if message.is_null() {"
    , "            \"\""
    , "        } else {"
    , "            std::ffi::CStr::from_ptr(message).to_str().unwrap_or(\"\")"
    , "        };"
    , "        logger.log(level.into(), file, line, func, message);"
    , "    }));"
    , "}"
    ]

generateOptionsSetLogger :: RsItem
generateOptionsSetLogger = RsItemFn RsFn
    { fnName = "set_logger"
    , fnVis = Pub
    , fnAbi = Nothing
    , fnGenerics = ["T: ToxLogger + 'static"]
    , fnArgs = [ RsSelfArg True
               , RsArg "logger" (TyPath "T")
               ]
    , fnUnsafe = False
    , fnDoc = []
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

generateResourceItems :: SemanticModel -> ModuleKind -> SResource -> [RsItem]
generateResourceItems model kind res =
    let
        structs = generateResourceStructs model kind res
        (implItems, staticItems) = generateResourceMethods model kind res
        traitItems = case kind of
            MainModule -> concatMap (generateTraitItems model res) (traits res)
            _          -> []
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

        -- A `_Nullable` accessor already returns `Option<T>`.
        accNullable = any (\m -> T.toLower (S.methodName m)
                                   == T.toLower (S.cPrefix res <> accessor)
                                 && methodReturnNullable m) (S.methods res)

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
                    ] ++ [("_marker", EVar "std::marker::PhantomData") | isLife && not (isReferenceResource res)]))
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

        structHasLife = case find (\r -> idToRustPascal (resourceName r) == structBaseName) (S.resources model) of
            Just r  -> hasLifetime r
            Nothing -> False

        isLife = structHasLife
        gens = if structBaseName == "ToxAV" then ["'a", "H: ToxAVHandler"]
               else if isLife then ["'a"] else []
        structTy = if structBaseName == "ToxAV" then TyGeneric structBaseName [TyPath "'a", TyPath "H"]
                   else if isLife then TyGeneric structBaseName [TyPath "'a"] else TyPath structBaseName

        -- Hand-written in core/av_dispatch.rs.
        notHandWritten m = S.methodName m `notElem` handWrittenFunctions
        isEventResource = let rn = resourceName res
                          in rn == "Event" || "Event_" `T.isPrefixOf` rn
        refineEventOutput m
            | isEventResource = m { output = refineEventField (S.methodName m) (output m) }
            | otherwise       = m
        -- The destructor is the `Drop` impl. A method too would double-free.
        resMethods = map refineEventOutput
            (filter (\m -> notHandWritten m && methodRole m /= Destructor) (S.methods res))

        (staticMethods, instanceMethods') = partition (\m -> methodRole m == Constructor || methodRole m == StaticRole) resMethods
        instanceMethods = if resourceName res == "Options"
                          then filter (\m -> S.methodName m `notElem` ["tox_options_set_log_callback", "tox_options_set_log_user_data", "tox_options_get_log_callback", "tox_options_get_log_user_data"]) instanceMethods'
                          else instanceMethods'

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
                    then trace ("Skipping " ++ show (resourceName res) ++ " kind=" ++ show kind ++ " isVar=" ++ show (isVariantResource model res)) []
                    else trace ("Generating " ++ show (resourceName res) ++ " kind=" ++ show kind) [RsItemImpl (RsImpl Nothing structTy gens (concatMap genMethodItems (ctors ++ instanceMethods ++ realStatic) ++ extraImplItems))]
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
                        -- ToxAV must be killed before its parent Tox, or a C
                        -- assertion fails.
                        , RsItemImpl (RsImpl (Just "Drop")
                            (TyGeneric "ToxAV" [TyPath "'a", TyPath "H"])
                            ["'a", "H: ToxAVHandler"]
                            [ RsItemFn (RsFn "drop" Private Nothing [] [RsSelfArg True] Nothing
                                (Just (RsBlock [StmtExprNoSemi (EUnsafe (RsBlock
                                    [StmtExprNoSemi (ECall (EVar "ffi::toxav_kill") [EFieldAccess (EVar "self") "ptr"])]))])) False []) ])
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

            in [RsItemImpl (RsImpl (Just "Drop") structTy gens [RsItemFn (RsFn "drop" Private Nothing [] [RsSelfArg True] Nothing (Just (RsBlock [body])) False [])])]
        Nothing -> []

-- | Safe types for well-known event accessors, which return raw scalars in C.
refineEventField :: Text -> SType -> SType
refineEventField cName t
    | "_get_friend_number"     `T.isSuffixOf` cName = SResourceId "Friend"
    | "_get_file_number"       `T.isSuffixOf` cName = SResourceId "File"
    | "_get_group_number"      `T.isSuffixOf` cName = SResourceId "Group"
    | "_get_conference_number" `T.isSuffixOf` cName = SResourceId "Conference"
    | "_get_peer_id"           `T.isSuffixOf` cName = SResourceId "Group_Peer"
    | "_get_peer_number"       `T.isSuffixOf` cName = SResourceId "Conference_Peer"
    | "peer_id"                `T.isSuffixOf` cName = SResourceId "Group_Peer"
    -- The only other `_get_message_id` is the friend read receipt's.
    | "_get_message_id" `T.isSuffixOf` cName && "_group_" `T.isInfixOf` cName
                                                    = SResourceId "Group_Message_Id"
    | "_get_message_id"        `T.isSuffixOf` cName = SResourceId "Friend_Message_Id"
    | "_get_length"            `T.isSuffixOf` cName = SSizeT
    | "_get_public_key"        `T.isSuffixOf` cName = SFixedBytes "TOX_PUBLIC_KEY_SIZE" False
    | otherwise                                     = t

-- | Hand-written in core/av_dispatch.rs: they take handlers.
handWrittenFunctions :: [Text]
handWrittenFunctions =
    [ "toxav_add_av_groupchat"
    , "toxav_join_av_groupchat"
    , "toxav_group_send_audio"
    , "toxav_groupchat_enable_av"
    -- These must register the Rust dispatch callbacks first.
    , "toxav_iterate"
    , "toxav_audio_iterate"
    , "toxav_video_iterate"
    -- Returns a non-owning `Tox*`. An owning `Tox` would `tox_kill` it again.
    , "toxav_get_tox"
    ]

knownTypes :: [Text]
knownTypes =
    [
 "FriendNumber"
    , "GroupNumber"
    , "FileNumber"
    , "ConferenceNumber"
    , "ConferencePeerNumber"
    , "ConferenceOfflinePeerNumber"
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

                -- `toxav_` doesn't share the common `tox_` prefix.
                stripped' = if "toxav_" `T.isPrefixOf` stripped
                            then T.drop 6 stripped
                            else if "toxav" `T.isPrefixOf` stripped
                            then T.drop 5 stripped
                            else stripped

                -- Event accessors are field getters:
                -- `tox_event_file_recv_get_data` is `data`.
                isEventRes = let rn = resourceName res
                             in rn == "Event" || "Event_" `T.isPrefixOf` rn
                strippedEvent = fromMaybe stripped' (T.stripPrefix "get_" stripped')

                -- Some events call their enum accessor `type`, the group
                -- message one `message_type`. Name it after its enum
                -- (`message_type`, `conference_type`).
                eventTypeName = case output method of
                    SEnum n ->
                        let cName = case find (\e -> S.enumSemanticName e == n) (S.enums model) of
                                Just e  -> S.enumName e
                                Nothing -> "Tox_" <> n
                        in T.intercalate "_"
                             (filter (/= "tox")
                               (map T.toLower (T.splitOn "_" cName)))
                    _ -> "type"
                stripped'' = if isEventRes && strippedEvent == "type"
                             then eventTypeName
                             else strippedEvent
            in idToRustSnake (if isEventRes then stripped'' else stripped')

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
                        -- Tox and ToxAV hold a raw pointer, so a shared borrow
                        -- is enough.
                        let constness = if isTox || structName == "ToxAV" then ConstThis else methodConstness method
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
        (argExprs, stmts) = resolveArguments model semParams ctxNames cArgs

        -- A `size_t` size function needs no `as usize` cast (event `_length`
        -- accessors return `uint32_t` and do).
        sizeFuncReturnsUsize = case cSizeFunctionName mapping of
            Just sf -> any (\(r, m) ->
                              cFunctionName (inferCFunctionMapping (S.resources model) r m) == sf
                              && output m == SSizeT)
                           [ (r, m) | r <- S.resources model, m <- S.methods r ]
            Nothing -> False

        -- 4. Return Type Logic
        isPassFunc = "tox_pass_" `T.isPrefixOf` cFunc
        isEncrypt = "encrypt" `T.isSuffixOf` cFunc
        isDecrypt = "decrypt" `T.isSuffixOf` cFunc
        isCrypto = isPassFunc && (isEncrypt || isDecrypt)
        isGetSalt = cFunc == "tox_get_salt"

        isSBytesNoSize = case output method of SBytes -> isNothing (cSizeFunctionName mapping); _ -> False

        -- Event structs borrow the C event, so their bytes can be a borrowed
        -- slice.
        isEventRes = let rn = resourceName res
                     in rn == "Event" || "Event_" `T.isPrefixOf` rn
        isEventBytes = isEventRes && output method == SBytes
                       && isJust (cSizeFunctionName mapping)

        hasErrorPtr = any (\(a) -> case a of ErrorPtr -> True; _ -> False) cArgs

        rawRetType = if isCrypto
                     then TyGeneric "Vec" [TyPath "u8"]
                     else if isGetSalt
                     then case lookup "TOX_PASS_SALT_LENGTH" (sizeToType model) of
                            Just ty -> TyPath ty
                            Nothing -> TyArray (TyPath "u8") "ffi::TOX_PASS_SALT_LENGTH as usize"
                     else if isSBytesNoSize then TyPath "*const u8"
                     else if isEventBytes then TyRef (Just "'a") (TySlice (TyPath "u8")) False
                     else if output method == SVoid then TyUnit else toSafeRetType model res (Just (methodRole method)) (output method)

        errEnumName = fromMaybe "Tox_Err_Unknown" (methodErrorType method)
        screaming = T.toUpper $ T.pack $ Casing.toSnake $ Casing.fromAny $ T.unpack errEnumName
        okVariant = "ffi::" <> errEnumName <> "::" <> screaming <> "_OK"

        rawRetType' = case methodResultStrategy method of
                        IgnoreReturn -> TyUnit
                        _            -> rawRetType

        -- 5. Dispatch
        hasBufferPtr = any (\(a) -> case a of BufferPtr _ -> True; _ -> False) cArgs

        -- An instance getter returning a `bool` presence flag, with no error
        -- enum, returns `Option` (static helpers like tox_hash don't).
        isOptionFixedBytes = case output method of
            SFixedBytes _ _ -> not hasErrorPtr && hasBufferPtr
                               && cReturnType mapping == SBool
                               && methodRole method /= StaticRole
                               && methodRole method /= Constructor
            _               -> False

        -- A `_Nullable` return with no error enum may be null.
        isNullableHandle = methodReturnNullable method && not hasErrorPtr
            && case output method of SHandle _ -> True; _ -> False
        finalRetType = if hasErrorPtr
                       then Just (TyGeneric "std::result::Result" [rawRetType', TyPath errEnumName])
                       else if isOptionFixedBytes || isNullableHandle
                       then Just (TyGeneric "Option" [rawRetType])
                       else if rawRetType == TyUnit then Nothing else Just rawRetType
        macroArgs = filterArgExprs cArgs argExprs

        bodyStmt = if isCrypto
            then generateEncryptionBody model cFunc cArgs semParams ctxNames hasErrorPtr okVariant isDecrypt
            else if isGetSalt
            then generateFixedBytesBody model method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr False okVariant "TOX_PASS_SALT_LENGTH"
            else case output method of
                SFixedBytes sizeConst _ ->
                    generateFixedBytesBody model method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr isOptionFixedBytes okVariant sizeConst
                -- A `const char *` accessor is a `&str`, even with a `_length`
                -- function.
                SString -> generateStringAccessBody cFunc macroArgs
                _ -> case cSizeFunctionName mapping of
                    Just sizeFunc ->
                        if hasBufferPtr then
                             generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant
                        else if isEventBytes then
                             generateEventBytesBody cFunc macroArgs sizeFunc
                        else
                             generateAccessBody sizeFuncReturnsUsize cFunc macroArgs sizeFunc
                    Nothing ->
                         generateStandardBody model res method mapping cFunc cArgs argExprs hasErrorPtr hasBufferPtr rawRetType' okVariant

    in (finalArgs, finalRetType, RsBlock (stmts ++ [bodyStmt]))

generateEncryptionBody :: SemanticModel -> Text -> [CArgSource] -> [SParameter] -> [Text] -> Bool -> Text -> Bool -> RsStmt
generateEncryptionBody model cFunc cArgs semParams ctxNames _hasErrorPtr okVariant isDecrypt =
    let
        (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs

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
                          (RsBlock [StmtReturn (ECall (EVar "Err") [EVar "err.into()"])])
                          Nothing)
                   , StmtExprNoSemi (ECall (EVar "Ok") [EVar "buf"])
                   ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateFixedBytesBody :: SemanticModel -> SMethod -> Text -> [CArgSource] -> [SParameter] -> [Text] -> Bool -> Bool -> Bool -> Text -> Text -> RsStmt
generateFixedBytesBody model method cFunc cArgs semParams ctxNames hasErrorPtr hasBufferPtr isOption okVariant sizeConst =
    let (allArgExprs, _) = resolveArguments model semParams ctxNames cArgs
        macroArgs = filterArgExprs cArgs allArgExprs
        c' = if "TOX_" `T.isPrefixOf` sizeConst then T.drop 4 sizeConst else sizeConst

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
            sizeExpr = constSizeExpr sizeConst
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
            sizeExpr = constSizeExpr sizeConst

            stmts = [ StmtLet "ptr" Nothing callPtr
                    , StmtLet "slice" Nothing (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr])
                    , StmtExprNoSemi (wrapType' (output method) (EMethodCall (EMethodCall (EVar "slice") "try_into" []) "unwrap" []))
                    ]
        in StmtExprNoSemi (EUnsafe (RsBlock stmts))

-- | A `const char *` with a `_length` function isn't nul-terminated.
generateStringAccessBody :: Text -> [RsExpr] -> RsStmt
generateStringAccessBody cFunc macroArgs =
    let
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs
        cstr = ECall (EVar "std::ffi::CStr::from_ptr") [EVar "ptr"]
        toStr = EMethodCall (EMethodCall cstr "to_str" []) "unwrap_or" [ELit (LString "")]
        stmts = [ StmtLet "ptr" Nothing (ECast callPtr (TyPath "*const std::os::raw::c_char"))
                , StmtExprNoSemi toStr
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock stmts))

generateAccessBody :: Bool -> Text -> [RsExpr] -> Text -> RsStmt
generateAccessBody sizeIsUsize cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        -- Clippy rejects a reflexive `as usize`.
        sizeExpr = if sizeIsUsize then EVar "size" else ECast (EVar "size") (TyPath "usize")

        accessStmts = [ StmtLet "size" Nothing callSize
                , StmtLet "ptr" Nothing callPtr
                , StmtExprNoSemi (EMethodCall (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr]) "to_vec" [])
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

-- | Like 'generateAccessBody', but borrowing the event's bytes.
generateEventBytesBody :: Text -> [RsExpr] -> Text -> RsStmt
generateEventBytesBody cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        -- An empty C buffer may be null, and `slice::from_raw_parts` is UB on
        -- null even for length 0.
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

constSizeExpr :: Text -> RsExpr
constSizeExpr c =
    let c' = if "TOX_" `T.isPrefixOf` c then T.drop 4 c else c
    -- The size constants are already `usize`.
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
            SString ->
                [ StmtLet cstrName Nothing
                    (EMethodCall (ECall (EVar "std::ffi::CString::new") [EVar name]) "unwrap" []) ]
            -- A plain `uint8_t x[CONST]` is read for `CONST` bytes, so a
            -- shorter slice would be an out-of-bounds read.
            SFixedBytes sizeConst _
                | isNothing (lookup sizeConst (sizeToType model))
                , not (T.null sizeConst)
                , T.all (\c -> isAlphaNum c || c == '_') sizeConst ->
                let c' = if "TOX_" `T.isPrefixOf` sizeConst
                         then T.drop 4 sizeConst else sizeConst
                in [ StmtExpr (EMacroCall "assert"
                        [ name <> ".len() >= " <> c' ]) ]
            _ -> []

        expr = case paramType param of
            SString -> EMethodCall (EVar cstrName) "as_ptr" []
            SBytes          -> EMethodCall (EVar name) ptrMethod []
            SList _         -> EMethodCall (EVar name) ptrMethod []
            SFixedList {}   -> EMethodCall (EVar name) ptrMethod []
            SFixedBytes sizeConst _ ->
                if isJust (lookup sizeConst (sizeToType model))
                then
                    if name == "file_id"
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
    let name = if n < length ctxNames then ctxNames !! n else "tox"
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
        -- With a safe wrapper struct, an explicit root argument is `&Root`.
        rootRes = case path of
            (rootName:_) -> find (\r -> resourceName r == rootName) (resources model)
            []           -> Nothing
        rootIsHandle = case rootRes of
            Just r  -> resourceType r == ResHandle
            Nothing -> False
        fullArgs = case path of
            [] -> []
            (rootName:rest) ->
                let
                    rootCName = case rootRes of
                        Just r -> cName r
                        Nothing -> if rootName == "Tox" then "Tox" else "Tox_" <> rootName

                    ptrType = if cns == MutableThis then "*mut ffi::" <> rootCName else "*const ffi::" <> rootCName
                    rootArg = if rootIsHandle
                              then RsArg (idToRustSnake rootName) (TyRef Nothing (TyPath (idToRustPascal rootName)) False)
                              else RsArg (idToRustSnake rootName) (TyPath ptrType)

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
            Just r  -> if isReferenceResource r then "self.0" else "self.ptr"
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
        -- A named array typedef is a fixed newtype. A plain `uint8_t x[CONST]`
        -- is a `&[u8]` whose length `resolveOne` checks.
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
            case lookup sizeConst (sizeToType model) of
                Just ty -> TyPath ty
                Nothing ->
                    if not (T.null sizeConst) && T.all (\c -> isAlphaNum c || c == '_') sizeConst
                    then
                        let c' = if "TOX_" `T.isPrefixOf` sizeConst then T.drop 4 sizeConst else sizeConst
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
    let cName = case find (\(e) -> S.enumSemanticName e == n) (S.enums model) of
            Just e  -> S.enumName e
            Nothing -> "Tox_" <> n -- Fallback guess
    -- In scope through `use crate::types::*;`.
    in case safeEnumName cName of
        Just safe -> TyPath safe
        Nothing   -> TyPath ("ffi::" <> cName)
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

-- | The `crate::types` name of a C enum. Error enums keep their C name.
safeEnumName :: Text -> Maybe Text
safeEnumName cName
    | "_Err_" `T.isInfixOf` cName = Nothing
    | cName == "Tox_Message_Type" = Just "MessageType"
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
                   else if h == "AV" && mRole == Just Constructor
                   then EStructInit "Self" [("ptr", expr), ("handler", ECall (EVar "Box::new") [EVar "handler"]), ("_tox", EVar "std::marker::PhantomData")]
                   else if isReferenceResource r
                   then ECall (EVar structName) [ERef (EDeref expr) False]
                   else if hasLifetime r
                   then EStructInit structName [("ptr", expr), ("_marker", EVar "std::marker::PhantomData")]
                   else EStructInit structName [("ptr", expr)]
            Nothing -> expr
    SFixedBytes sizeConst _ ->
        case lookup sizeConst (sizeToType model) of
            Just ty -> ECall (EVar ty) [expr]
            Nothing -> expr
    SEnum _ | isSafeEnum model t -> EMethodCall expr "into" []
    SResourceId n ->
        let name = idToRustPascal n
            wrapper = if "Number" `T.isSuffixOf` name || name `elem` knownTypes
                      then name
                      else name <> "Number"
        in ECall (EVar wrapper) [expr]
    SSizeT -> ECast expr (TyPath "usize")
    _ -> expr


-- ---------------------------------------------------------------------------
-- Safe wrappers on `tox::Tox`, `tox::ToxAV` and `tox::Options`: each
-- delegates to the `core` method 'generateMethod' builds, under the name
-- 'safeMethodName' gives it.
-- ---------------------------------------------------------------------------

-- | Hand-written in `tox/mod.rs` and `toxav/mod.rs`.
safeBespoke :: [Text]
safeBespoke =
    [ "tox_iterate"                    -- takes a &mut handler
    , "tox_events_init"                -- folded into the bespoke `events()`
    , "tox_events_iterate"             -- ditto
    , "toxav_audio_send_frame"         -- derives sample_count from pcm.len()
    , "tox_options_set_proxy_host"     -- safe API discards the core `bool`
    , "tox_options_set_savedata_data"  -- ditto
    , "tox_options_set_log_callback"   -- replaced by the typed `set_logger`
    , "tox_options_set_log_user_data"  -- ditto
    , "toxav_iterate"                  -- &mut self: runs &mut handler callbacks
    , "toxav_audio_iterate"            -- ditto
    , "toxav_video_iterate"            -- ditto
    ]

safeRenames :: [(Text, Text)]
safeRenames = [("friend_list_size", "friend_list_len")]

safeMethodName :: SResource -> Text -> Text
safeMethodName res core = fromMaybe mechanical (lookup core safeRenames)
  where
    mechanical
        | resourceName res == "Options" =
            if "set_" `T.isPrefixOf` core then core else "set_" <> core
        | Just r <- T.stripPrefix "self_get_" core = r
        | Just r <- T.stripPrefix "self_set_" core = "set_" <> r
        | Just r <- T.stripPrefix "get_" core      = r
        | otherwise                                = core

safeInScope :: SResource -> SMethod -> Bool
safeInScope res m =
    methodRole m `notElem` [Constructor, Destructor, StaticRole]
    && not (methodHasUserData m)
    && S.methodName m `notElem` safeBespoke
    && not (returnsHandle (output m))
    && (resourceName res /= "Options" || "_set_" `T.isInfixOf` S.methodName m)
  where
    returnsHandle (SHandle _)             = True
    returnsHandle (SList (SResourceId _)) = True
    returnsHandle _                       = False

-- | The safe wrapper for one method, delegating to the `core` one. @prefix@
-- is the path from `self` to the `core` object.
safeWrapper :: SemanticModel -> SResource -> Text -> SMethod -> Maybe RsItem
safeWrapper model res prefix method
    | not (safeInScope res method) = Nothing
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
                        [EVar ("ToxError::" <> toxErrorVariant (errName errTy))] )
                other -> (other, call)
        in RsItemFn RsFn
            { fnName     = safeMethodName res coreName
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
    errName _          = "Tox_Err_Unknown"

-- | Handles that get a safe-wrapper module: those with a constructor, not
-- in the `Event` family (`Tox`, `AV`, `Options`, `Pass_Key`).
safeHandleResources :: SemanticModel -> [SResource]
safeHandleResources model =
    [ r
    | r <- S.resources model
    , resourceType r == ResHandle
    , any ((== Constructor) . methodRole) (S.methods r)
    , not ("event" `T.isPrefixOf` idToRustSnake (resourceName r))
    ]

-- | The hand-written parts of a flat-`impl` safe handle. @hgField@ is the
-- path from `self` to the `core` object.
data HandleGlue = HandleGlue
    { hgFile     :: FilePath
    , hgImports  :: [Text]
    , hgImplType :: RsType
    , hgGenerics :: [Text]
    , hgField    :: Text
    }

-- | `Pass_Key` isn't here: 'generatePassKeyModule' generates all of it.
flatHandleGlue :: Text -> Maybe HandleGlue
flatHandleGlue "Tox" = Just HandleGlue
    { hgFile     = "safe_tox.rs"
    , hgImports  = ["use crate::tox::Tox;", "use crate::types::*;"]
    , hgImplType = TyPath "Tox"
    , hgGenerics = []
    , hgField    = "inner.core"
    }
flatHandleGlue "ToxAV" = Just HandleGlue
    { hgFile     = "safe_toxav.rs"
    , hgImports  = [ "use crate::toxav::ToxAV;"
                   , "use crate::toxav::ToxAVHandler;"
                   , "use crate::types::*;"
                   ]
    , hgImplType = TyGeneric "ToxAV" [TyPath "'a", TyPath "H"]
    , hgGenerics = ["'a", "H: ToxAVHandler"]
    , hgField    = "inner"
    }
flatHandleGlue "Options" = Just HandleGlue
    { hgFile     = "safe_options.rs"
    , hgImports  = ["use crate::tox::Options;", "use crate::types::*;"]
    , hgImplType = TyPath "Options"
    , hgGenerics = []
    , hgField    = "inner"
    }
flatHandleGlue _ = Nothing

-- | One safe-wrapper file per handle. For `Tox`, `ToxAV` and `Options` the
-- struct is hand-written and this emits its `impl`.
generateSafeModules :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateSafeModules model items =
    [ safeFileFor handle res
    | handle <- safeHandleResources model
    , let name = idToRustPascal (resourceName handle)
    -- The `Tox` wrapper covers the `tox` subsystem's slice.
    , Just res <- [mainModuleResource name]
    ]
  where
    mainModuleResource name =
        fmap snd (find isTarget items)
      where
        isTarget (MainModule, r) = idToRustPascal (resourceName r) == name
        isTarget _               = False

    safeFileFor handle res =
        case flatHandleGlue (idToRustPascal (resourceName handle)) of
            Just glue -> generateFlatHandle model res glue
            Nothing   -> generatePassKeyModule model res

generateFlatHandle :: SemanticModel -> SResource -> HandleGlue -> (FilePath, Text)
generateFlatHandle model res glue =
    mkSafeFile (hgFile glue)
        (hgImports glue)
        (RsItemImpl (RsImpl Nothing (hgImplType glue) (hgGenerics glue)
            (mapMaybe (safeWrapper model res (hgField glue)) (S.methods res))))

-- | `Pass_Key`'s safe wrapper: a newtype over the `core` handle, with its
-- static helpers as module functions. The zero-argument `*_length` helpers
-- are dropped, as the size constants say the same.
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

    -- Only used for the safe argument list and error type.
    coreFn m = case generateMethod model MainModule res m of
        (RsItemFn fn : _) -> Just fn
        _                 -> Nothing

    argText (RsArg n ty)      = n <> ": " <> prettyType ty
    argText (RsSelfArg False) = "&self"
    argText (RsSelfArg True)  = "&mut self"
    argText RsSelf            = "self"

    valArgs fn  = [ a | a@RsArg{} <- fnArgs fn ]
    argExprs fn = [ n | RsArg n _ <- valArgs fn ]

    -- The safe return type, and the `.map_err`/`.map` adapting the call.
    -- Constructors (@rewrapHandle@) rewrap `Self` in the newtype.
    adapt rewrapHandle fn =
        case fnRet fn of
            Just (TyGeneric "std::result::Result" [t, TyPath errTy]) ->
                let t'   = if rewrapHandle then TyPath typeName else t
                    mapH = if rewrapHandle then ".map(" <> typeName <> ")" else ""
                in ( Just (prettyType (TyGeneric "Result" [t']))
                   , mapH <> ".map_err(ToxError::" <> toxErrorVariant errTy <> ")" )
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

    -- A `pass_` prefix is dropped (`pass_encrypt` is `encrypt`). Stringizers
    -- come first, then the other helpers, each in model order.
    rawModuleFns = methodsOf (\m -> methodRole m == StaticRole && not (null (S.inputs m)))
    isStringizer m = "to_string" `T.isSuffixOf` S.methodName m
    (stringizers, helpers) = partition isStringizer rawModuleFns
    moduleFns = mapMaybe
        (renderFn [] ("core::" <> typeName <> "::") stripPassPrefix False)
        (stringizers ++ helpers)

    stripPassPrefix n = fromMaybe n (T.stripPrefix "pass_" n)

-- | Indent by four spaces, leaving blank lines empty.
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
-- Callback dispatch: the `ToxHandler` and `ToxAVHandler` traits, an
-- `extern "C"` trampoline per event, and the registration functions. It's
-- one `RsRaw` block, as the trampolines are macro invocations.
-- ---------------------------------------------------------------------------

enumCName :: SemanticModel -> Text -> Text
enumCName model sem =
    case find (\e -> S.enumSemanticName e == sem || S.enumName e == sem) (S.enums model) of
        Just e  -> S.enumName e
        Nothing -> "Tox_" <> sem

-- | The handler-method type of an event parameter.
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

idResourceType :: Text -> Text
idResourceType n =
    let name = idToRustPascal n
    in if "Number" `T.isSuffixOf` name || name `elem` knownTypes
       then name
       else name <> "Number"

scalarRsType :: SType -> Text
scalarRsType (SInt b)  = "i" <> T.pack (show b)
scalarRsType (SUInt b) = "u" <> T.pack (show b)
scalarRsType _         = "u8"

-- | The C parameters of an event parameter. The model drops the `size_t`
-- after each byte array, so it's added back here.
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

-- | The conversion from an event parameter's C arguments to the handler's,
-- and the `let`s it needs.
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
                let szConst = stripToxPrefix sz
                in ( [ "let mut " <> nm <> "_arr = [0u8; " <> szConst <> "];"
                     , "let " <> nm <> "_src = unsafe { slice::from_raw_parts(" <> nm
                         <> ", " <> szConst <> ") };"
                     , nm <> "_arr.copy_from_slice(" <> nm <> "_src);" ]
                   , "crate::types::" <> ty <> "(" <> nm <> "_arr)" )
            | otherwise ->
                -- A video plane, sized by a `/*! ... */` expression over other
                -- parameters.
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

data SizTok = SId Text | SNum Text | SOp Text | SLParen | SRParen | SComma
            | SDotAbs -- ^ Synthetic: renders as the Rust `.abs()` method call.
    deriving (Eq, Show)

sizTokens :: Text -> [SizTok]
sizTokens = go . T.unpack
  where
    go [] = []
    go (c:cs)
        | c == ' '  = go cs
        | c == '('  = SLParen : go cs
        | c == ')'  = SRParen : go cs
        | c == ','  = SComma : go cs
        | c `elem` ("*+-/" :: String) = SOp (T.singleton c) : go cs
        | isAlphaNum c || c == '_' =
            let (tok, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
            in (if all (`elem` ("0123456789" :: String)) tok
                then SNum (T.pack tok) else SId (T.pack tok)) : go rest
        | otherwise = go cs

-- | A C sizer expression in Rust, e.g. @max(width / 2, abs(ustride)) *
-- (height / 2)@. Operands are cast to `i64` so nothing overflows before the
-- final `as usize`.
sizerToRust :: Text -> Text
sizerToRust = T.concat . map render . absToMethod . sizTokens
  where
    render (SId "max") = "std::cmp::max"
    render (SId "min") = "std::cmp::min"
    -- `as` binds tighter than arithmetic, and clippy rejects redundant
    -- parentheses.
    render (SId n)     = n <> " as i64"
    -- Clippy rejects `as i64` on a literal (`unnecessary_cast`).
    render (SNum n)    = n <> "_i64"
    render (SOp o)     = " " <> o <> " "
    render SLParen     = "("
    render SRParen     = ")"
    render SComma      = ", "
    render SDotAbs     = ".abs()"

-- | C's @abs(X)@ becomes Rust's @(X).abs()@.
absToMethod :: [SizTok] -> [SizTok]
absToMethod [] = []
absToMethod (SId fn : SLParen : rest)
    | fn `elem` ["abs", "labs", "llabs"] =
        let (inside, after) = splitParen 0 [] rest
        in SLParen : absToMethod inside ++ [SRParen, SDotAbs] ++ absToMethod after
  where
    splitParen _ acc [] = (reverse acc, [])
    splitParen d acc (SRParen : xs)
        | d == 0    = (reverse acc, xs)
        | otherwise = splitParen (d-1) (SRParen : acc) xs
    splitParen d acc (SLParen : xs) = splitParen (d+1) (SLParen : acc) xs
    splitParen d acc (x : xs)       = splitParen d (x : acc) xs
absToMethod (t : ts) = t : absToMethod ts

-- | One `extern "C"` trampoline. It takes every C parameter, but passes the
-- handler only those its trait method has.
renderTrampoline :: SemanticModel -> Bool -> Text -> SEvent -> Text
renderTrampoline model isAv macroName ev =
    let fnName = "dispatch_" <> eventName ev
        rawArgs = concatMap (rawCArgs model) (eventParams ev)
        rawSig  = T.intercalate ", " [ n <> ": " <> ty | (n, ty) <- rawArgs ]
        -- Size-only parameters get `let`s, for the sizers that use them, but
        -- aren't passed on.
        conv p  = (handlerVisible ev p, convertArg model (fixAvFriendNumber isAv p))
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
    , SId ident <- sizTokens sz
    , ident `notElem` ["max", "min", "abs", "labs", "llabs"]
    ]

-- | Size parameters only used by a sibling's sizer aren't in the trait.
handlerVisible :: SEvent -> SParameter -> Bool
handlerVisible ev p = case paramType p of
    SSizeT -> paramSnake (paramName p) `notElem` sizerIdentifiers ev
    _      -> True

-- | toxav calls `friend_number` a bare `u32` (it predates
-- `Tox_Friend_Number`), but the handler takes a `FriendNumber`.
fixAvFriendNumber :: Bool -> SParameter -> SParameter
fixAvFriendNumber isAv p
    | isAv, paramName p == "friend_number", paramType p == SUInt 32 =
        p { paramType = SResourceId "Friend_Number" }
    | otherwise = p

renderTraitMethod :: SemanticModel -> Bool -> SEvent -> Text
renderTraitMethod model isAv ev =
    let -- `_type` needs no `r#` escape.
        plainSnake = T.toLower . T.pack . Casing.toSnake . Casing.fromAny . T.unpack
        params = [ "_" <> plainSnake (paramName p) <> ": "
                     <> handlerArgType model (fixAvFriendNumber isAv p)
                 | p <- eventParams ev, handlerVisible ev p ]
        allParams = "&mut self" : params
    in "    fn on_" <> eventName ev <> "(" <> T.intercalate ", " allParams <> ") {}"

-- | @core/generated/dispatch.rs@.
generateDispatchModule :: SemanticModel -> (FilePath, Text)
generateDispatchModule model =
    let toxEvents = maybe [] events (find ((== "Tox") . resourceName) (S.resources model))
        avEvents  = maybe [] events (find ((== "AV") . resourceName) (S.resources model))

        toxTrait = T.unlines
            ( "pub trait ToxHandler {"
            : map (renderTraitMethod model False) toxEvents
           ++ ["}"] )

        avTrait = T.unlines
            ( "pub trait ToxAVHandler {"
            : map (renderTraitMethod model True) avEvents
           ++ ["}"] )

        toxRegister = T.unlines
            ( [ "pub fn tox_iterate<H: ToxHandler>(tox: &Tox, handler: &mut H) {"
              , "    let tox_ptr = tox.ptr;"
              , "    unsafe {" ]
           ++ [ "        ffi::tox_callback_" <> eventName ev <> "(tox_ptr, Some(dispatch_"
                  <> eventName ev <> "::<H>));"
              | ev <- toxEvents ]
           ++ [ "        ffi::tox_iterate(tox_ptr, handler as *mut H as *mut c_void);"
              , "    }"
              , "}" ] )

        avRegister = T.unlines
            ( [ "pub(crate) unsafe fn register_av_callbacks<H: ToxAVHandler>("
              , "    av_ptr: *mut ffi::ToxAV,"
              , "    handler: &mut H,"
              , ") {"
              , "    unsafe {" ]
           ++ concat
              [ [ "        ffi::toxav_callback_" <> eventName ev <> "("
                , "            av_ptr,"
                , "            Some(dispatch_" <> eventName ev <> "::<H>),"
                , "            handler as *mut H as *mut c_void,"
                , "        );" ]
              | ev <- avEvents ]
           ++ [ "    }"
              , "}" ] )

        toxTrampolines = T.intercalate "\n"
            (map (renderTrampoline model False "define_dispatch") toxEvents)

        avTrampolines = T.intercalate "\n"
            (map (renderTrampoline model True "define_av_dispatch") avEvents)

        content = T.unlines
            [ "// Generated by apigen. Do not edit."
            , "//"
            , "// The Tox / ToxAV callback dispatch layer: the `ToxHandler` and"
            , "// `ToxAVHandler` traits, one `extern \"C\"` trampoline per event and the"
            , "// callback-registration entry points."
            , "#![allow(unused_imports)]"
            , "#![allow(clippy::too_many_arguments)]"
            , "use super::tox::Tox;"
            , "use crate::ffi;"
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
            , toxTrait
            , avTrait
            , toxRegister
            , avRegister
            , dispatchMacros
            , toxTrampolines
            , ""
            , avTrampolines
            ]
    in ("dispatch.rs", content)

-- | The trampoline macros. A panic in the handler must not unwind into C.
dispatchMacros :: Text
dispatchMacros = T.unlines
    [ "macro_rules! define_dispatch {"
    , "    ($func_name:ident, ($($arg_name:ident: $arg_type:ty),*), $body:expr) => {"
    , "        unsafe extern \"C\" fn $func_name<H: ToxHandler>("
    , "            _tox: *mut ffi::Tox,"
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
    , ""
    , "macro_rules! define_av_dispatch {"
    , "    ($func_name:ident, ($($arg_name:ident: $arg_type:ty),*), $body:expr) => {"
    , "        unsafe extern \"C\" fn $func_name<H: ToxAVHandler>("
    , "            _av: *mut ffi::ToxAV,"
    , "            $($arg_name: $arg_type),*,"
    , "            user_data: *mut c_void,"
    , "        ) {"
    , "            if !user_data.is_null() {"
    , "                let handler = unsafe { &mut *(user_data as *mut H) };"
    , "                let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {"
    , "                    $body(handler)"
    , "                }));"
    , "            }"
    , "        }"
    , "    }"
    , "}"
    ]

-- ---------------------------------------------------------------------------
-- Resource wrappers: `Friend`, `Group`, `File` and `Conference` borrow the
-- `Tox` and carry their ids, and their methods delegate to `core`. Some
-- `Conference` methods stay hand-written (see 'nonGeneratableNote').
-- ---------------------------------------------------------------------------

-- | A wrapper struct, and the model resources whose methods it has.
data ResWrapper = ResWrapper
    { rwName      :: Text       -- ^ Wrapper struct name (e.g. @Friend@).
    , rwIdType    :: Text       -- ^ Primary id newtype (e.g. @FriendNumber@).
    , rwExtraId   :: Maybe (Text, Text) -- ^ Extra id field @(name, type)@ (File only).
    , rwResources :: [Text]     -- ^ Model resource names contributing methods.
    }

-- | Ids a wrapper carries besides its own: a `File` is addressed by friend
-- and file number, but the model keys it on the file number alone.
resWrapperExtraId :: Text -> Maybe (Text, Text)
resWrapperExtraId "File" = Just ("friend", "FriendNumber")
resWrapperExtraId _      = Nothing

-- | One wrapper per id resource under `Tox`. Id sub-resources (the `*_Peer`
-- ones) fold their methods into it, plainer names first.
resWrappers :: SemanticModel -> [ResWrapper]
resWrappers model =
    [ ResWrapper
        { rwName      = idToRustPascal (resourceName r)
        , rwIdType    = idToRustPascal idTypeName
        , rwExtraId   = resWrapperExtraId (idToRustPascal (resourceName r))
        , rwResources = resourceName r : foldedSubResources r
        }
    | r <- S.resources model
    , Just idTypeName <- [idResourceIdType r]
    , parent r == Just rootResourceName
    ]
  where
    rootResourceName =
        maybe "Tox" resourceName (find isRoot (S.resources model))

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

-- | Wrappers that consume @self@: the resource is gone after the call.
resConsumingSelf :: [Text]
resConsumingSelf = ["tox_friend_delete", "tox_conference_delete", "tox_group_leave"]

-- | @(wrapperName, cFunctionName)@: methods the model puts on `Tox` (file
-- transfers, `tox_self_set_typing`) that the wrappers expose.
resAdoptedMethods :: [(Text, Text)]
resAdoptedMethods =
    [ ("Friend", "tox_self_set_typing")
    , ("File", "tox_file_send_chunk")
    , ("File", "tox_file_control")
    , ("File", "tox_file_seek")
    , ("File", "tox_file_get_file_id")
    ]

-- | Hand-written: `tox_group_invite_friend` takes a `&Friend`, and
-- `tox_group_leave` and `tox_group_set_password` an `Option<&[u8]>`.
resBespoke :: [Text]
resBespoke =
    [ "tox_group_invite_friend"
    , "tox_group_leave"
    , "tox_group_set_password"
    ]

-- | A wrapper method's name: strip @tox_@, a leading @self_@ and the
-- resource's own prefix, drop @get_@, give @bool@ getters an @is_@, and
-- escape keywords with a trailing underscore.
resWrapperMethodName :: ResWrapper -> SMethod -> Text -> Text
resWrapperMethodName rw method cfn = isPrefixed (paramSnake (deGet stripped))
  where
    bare = fromMaybe cfn (T.stripPrefix "tox_" cfn)
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

-- | A wrapper method: the `core` signature with the wrapper's ids taken from
-- `self`.
resWrapper :: SemanticModel -> ResWrapper -> SResource -> SMethod -> Maybe RsItem
resWrapper model rw res method
    | methodRole method `elem` [Constructor, Destructor, StaticRole] = Nothing
    | methodHasUserData method = Nothing
    | S.methodName method `elem` resBespoke = Nothing
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
            -- A kept parameter loses the wrapper's prefix
            -- (`conference_peer_number` on `Conference`), as its type already
            -- says it.
            wrapperPrefix = idToRustSnake (rwName rw) <> "_"
            stripPrefix (RsArg n ty)
                | Just bare <- T.stripPrefix wrapperPrefix n = RsArg bare ty
            stripPrefix a = a
            keptArgs   = [ stripPrefix a | Right a <- classified ]
            callArgs   = [ either EVar (\(RsArg n _) -> EVar n) (fmap stripPrefix c) | c <- classified ]
            recv = if cfn `elem` resConsumingSelf then RsSelf else RsSelfArg False
            target = EFieldAccess (EFieldAccess (EFieldAccess (EVar "self") "tox") "inner") "core"
            call = EMethodCall target (fnName core) callArgs
            (safeRet, body) = case fnRet core of
                Just (TyGeneric "std::result::Result" [t, errTy]) ->
                    ( Just (TyGeneric "Result" [t])
                    , EMethodCall call "map_err"
                        [EVar ("ToxError::" <> toxErrorVariant (errPath errTy))] )
                other -> (other, call)
        in RsItemFn RsFn
            { fnName     = resWrapperMethodName rw method cfn
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
    errPath _          = "Tox_Err_Unknown"

-- | Size functions for another method's buffer, which wrappers don't expose.
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
        toxRes = find ((== "Tox") . resourceName) (S.resources model)
        adopted =
            [ (tr, m)
            | (w, c) <- resAdoptedMethods, w == rwName rw
            , tr <- maybe [] (:[]) toxRes
            , m  <- S.methods tr, S.methodName m == c
            ]
    in mapMaybe (\(r, m) -> resWrapper model rw r m) (ownMethods ++ adopted)

-- | The wrappers' id getters.
resWrapperGetters :: ResWrapper -> [RsItem]
resWrapperGetters rw = case rwName rw of
    "File"       -> [ getter "number" "FileNumber" "number"
                    , getter "friend_number" "FriendNumber" "friend"
                    ]
    "Conference" -> [ getter "number" (rwIdType rw) "number" ]
    _            -> [ getter "get_number" (rwIdType rw) "number" ]
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

resWrapperHeader :: ResWrapper -> Text
resWrapperHeader rw =
    let n = rwName rw
        fields = case rwExtraId rw of
            Just (fn, ft) -> [ "    pub(crate) friend: " <> ft <> ","
                             , "    pub(crate) number: " <> rwIdType rw <> ","
                             ]
            Nothing       -> [ "    pub(crate) number: " <> rwIdType rw <> "," ]
        dbgFields = case rwExtraId rw of
            Just (fn, _) -> [ "            .field(\"friend\", &self.friend)"
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
          , "    pub(crate) tox: &'a Tox,"
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
nonGeneratableNote "Conference" = T.unlines
    [ "// The handler-based AV group-chat methods (`enable_av`, `av_enabled`,"
    , "// `send_audio`) and the `peer_list` helper cannot be derived from the"
    , "// model; they stay hand-written in `tox/conference.rs`."
    ]
nonGeneratableNote _ = ""

generateResourceWrapperFile :: SemanticModel -> ResWrapper -> (FilePath, Text)
generateResourceWrapperFile model rw =
    let n = rwName rw
        fileName = "safe_" <> T.toLower n <> ".rs"
        items = resWrapperGetters rw ++ resWrapperMethods model rw
        implBody = render (RsModule [RsItemImpl (RsImpl Nothing
                       (TyGeneric n [TyPath "'a"]) ["'a"] items)])
        content = "// Generated by apigen. Do not edit.\n"
               <> "use crate::tox::Tox;\n"
               <> "use crate::types::*;\n\n"
               <> nonGeneratableNote n
               <> resWrapperHeader rw
               <> implBody
    in (T.unpack fileName, content)

-- | The `Tox` resource accessors and constructors.
generateToxResourceAccessors :: SemanticModel -> (FilePath, Text)
generateToxResourceAccessors _model =
    ( "safe_tox_resources.rs"
    , "// Generated by apigen. Do not edit.\n"
   <> "//\n"
   <> "// The `Tox` resource accessors and constructors. `add_av_groupchat` /\n"
   <> "// `join_av_groupchat` are handler-based and stay hand-written in\n"
   <> "// `tox/mod.rs`; `group_count` is a Tox-level query kept there too.\n"
   <> "use crate::tox::Conference;\n"
   <> "use crate::tox::File;\n"
   <> "use crate::tox::Friend;\n"
   <> "use crate::tox::Group;\n"
   <> "use crate::tox::Tox;\n"
   <> "use crate::types::*;\n\n"
   <> "impl Tox {\n"
   <> T.intercalate "\n" toxAccessors
   <> "}\n"
    )
  where
    toxAccessors =
        [ T.unlines
            [ "    pub fn friend(&self, number: FriendNumber) -> Friend<'_> {"
            , "        Friend { tox: self, number }"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn friend_add(&self, address: &Address, message: &[u8]) -> Result<Friend<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .friend_add(address, message)"
            , "            .map_err(ToxError::FriendAdd)?;"
            , "        Ok(self.friend(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn friend_add_norequest(&self, public_key: &PublicKey) -> Result<Friend<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .friend_add_norequest(public_key)"
            , "            .map_err(ToxError::FriendAdd)?;"
            , "        Ok(self.friend(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn lookup_friend(&self, public_key: &PublicKey) -> Result<Friend<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .friend_by_public_key(public_key)"
            , "            .map_err(ToxError::FriendByPublicKey)?;"
            , "        Ok(self.friend(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn friend_list(&self) -> Vec<Friend<'_>> {"
            , "        self.inner"
            , "            .core"
            , "            .self_get_friend_list()"
            , "            .into_iter()"
            , "            .map(|n| self.friend(n))"
            , "            .collect()"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group(&self, number: GroupNumber) -> Group<'_> {"
            , "        Group { tox: self, number }"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group_new("
            , "        &self,"
            , "        privacy_state: ToxGroupPrivacyState,"
            , "        group_name: &[u8],"
            , "        name: &[u8],"
            , "    ) -> Result<Group<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .group_new(privacy_state, group_name, name)"
            , "            .map_err(ToxError::GroupNew)?;"
            , "        Ok(self.group(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group_join("
            , "        &self,"
            , "        chat_id: &GroupChatId,"
            , "        name: &[u8],"
            , "        password: Option<&[u8]>,"
            , "    ) -> Result<Group<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .group_join(chat_id, name, password.unwrap_or(&[]))"
            , "            .map_err(ToxError::GroupJoin)?;"
            , "        Ok(self.group(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group_invite_accept("
            , "        &self,"
            , "        friend: &Friend,"
            , "        invite_data: &[u8],"
            , "        name: &[u8],"
            , "        password: Option<&[u8]>,"
            , "    ) -> Result<Group<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .group_invite_accept("
            , "                friend.get_number(),"
            , "                invite_data,"
            , "                name,"
            , "                password.unwrap_or(&[]),"
            , "            )"
            , "            .map_err(ToxError::GroupInviteAccept)?;"
            , "        Ok(self.group(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group_by_id(&self, chat_id: &GroupChatId) -> Result<Group<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .group_by_id(chat_id)"
            , "            .map_err(ToxError::GroupById)?;"
            , "        Ok(self.group(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn group_list(&self) -> Vec<Group<'_>> {"
            , "        self.inner"
            , "            .core"
            , "            .group_get_group_list()"
            , "            .into_iter()"
            , "            .map(|n| self.group(n))"
            , "            .collect()"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn file(&self, friend: &Friend, number: FileNumber) -> File<'_> {"
            , "        File {"
            , "            tox: self,"
            , "            friend: friend.get_number(),"
            , "            number,"
            , "        }"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn file_send("
            , "        &self,"
            , "        friend: &Friend,"
            , "        kind: u32,"
            , "        file_size: u64,"
            , "        file_id: Option<&FileId>,"
            , "        filename: &[u8],"
            , "    ) -> Result<File<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .file_send(friend.get_number(), kind, file_size, file_id, filename)"
            , "            .map_err(ToxError::FileSend)?;"
            , "        Ok(self.file(friend, number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn conference(&self, number: ConferenceNumber) -> Conference<'_> {"
            , "        Conference { tox: self, number }"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn conference_new(&self) -> Result<Conference<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .conference_new()"
            , "            .map_err(ToxError::ConferenceNew)?;"
            , "        Ok(self.conference(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn conference_join(&self, friend: &Friend, cookie: &[u8]) -> Result<Conference<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .conference_join(friend.get_number(), cookie)"
            , "            .map_err(ToxError::ConferenceJoin)?;"
            , "        Ok(self.conference(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn conference_by_id(&self, id: &ConferenceId) -> Result<Conference<'_>> {"
            , "        let number = self"
            , "            .inner"
            , "            .core"
            , "            .conference_by_id(id)"
            , "            .map_err(ToxError::ConferenceById)?;"
            , "        Ok(self.conference(number))"
            , "    }"
            ]
        , T.unlines
            [ "    pub fn conference_chatlist(&self) -> Vec<Conference<'_>> {"
            , "        self.inner"
            , "            .core"
            , "            .conference_get_chatlist()"
            , "            .into_iter()"
            , "            .map(|n| self.conference(n))"
            , "            .collect()"
            , "    }"
            ]
        ]

-- | Every resource wrapper file, and the `Tox` accessor file.
generateResourceWrappers :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateResourceWrappers model _items =
    map (generateResourceWrapperFile model) (resWrappers model)
 ++ [generateToxResourceAccessors model]

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> ([y], acc)
mapAccumL _ z [] = ([], z)
mapAccumL f z (x:xs) =
    let (z', y) = f z x
        (ys, z'') = mapAccumL f z' xs
    in (y:ys, z'')
