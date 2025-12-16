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

-- | Maps a C size constant to the Rust newtype that wraps that fixed-size byte
-- array. Derived from the @typedef uint8_t X[SIZE]@ array typedefs apigen
-- parses: the model carries @(sizeConstant, semanticTypeName)@ pairs and the
-- newtype name is the semantic name PascalCased via 'idToRustPascal'.
sizeToType :: SemanticModel -> [(Text, Text)]
sizeToType model =
    [ (sizeConst, idToRustPascal semName)
    | (sizeConst, semName) <- S.arrayTypes model
    ]

-- | The set of size constants that have a corresponding byte-array newtype.
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

        -- Group by module name
        groupedItems = groupByModuleName allItems

        modules = map (generateModule opts model) groupedItems

        -- Generate mod.rs. `dispatch` is appended explicitly: it is not a
        -- resource module but the `core` tier must re-export its
        -- `ToxHandler`/`ToxAVHandler` traits and `tox_iterate`.
        moduleNames = nub (map (\(fp, _) -> T.pack (takeBaseName fp)) modules)
                   ++ ["dispatch"]
        modRs = generateAggregateModule "mod" moduleNames

        -- The declarative half of `crate::types`: size constants, the
        -- newtype/byte-array/enum macro invocations and the `ToxError`
        -- result type. Emitted standalone (not listed in mod.rs); the
        -- hand-written `src/types.rs` pulls it in via `#[path = ...]`.
        typesRs = generateTypesModule model

        -- Phase 2: the flat per-method safe wrappers on `tox::Tox`,
        -- `tox::ToxAV` and `tox::Options`. Each is a mechanical delegation
        -- over the `core` tier, mapping the `core` `Tox_Err_*` result to the
        -- unified `ToxError`. Emitted standalone; the hand-written
        -- `tox/mod.rs` / `toxav/mod.rs` pull them in via `#[path = ...]`.
        safeModules = generateSafeModules model allItems

        -- The Tox/ToxAV callback dispatch layer: the `ToxHandler` /
        -- `ToxAVHandler` traits, one `extern "C"` trampoline per event and
        -- the callback-registration entry points. Emitted standalone; pulled
        -- into the `core` module by hand-written glue.
        dispatchModule = generateDispatchModule model

        -- Phase 2: the resource wrapper objects (`Friend`, `Group`, `File`,
        -- `Conference`) and the `Tox` resource accessors/constructors. Each
        -- wrapper struct borrows the `Tox` and carries its id; its methods
        -- delegate to the flat `core` tier. Emitted standalone; the
        -- hand-written `tox/mod.rs` pulls them in via `#[path = ...]`.
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

-- | C enums that the public API depends on but that live in headers not fed
-- to apigen (e.g. @tox_private.h@). The generator still needs to emit their
-- safe wrappers so @crate::types@ stays complete. Each entry is
-- @(cName, [cMemberName])@.
privateEnumExtras :: [(Text, [Text])]
privateEnumExtras =
    [ ( "Tox_Err_Iterate_Options_New"
      , [ "TOX_ERR_ITERATE_OPTIONS_NEW_OK"
        , "TOX_ERR_ITERATE_OPTIONS_NEW_MALLOC"
        ]
      )
    ]

-- | Safe newtypes that have no corresponding C @idType@ in the parsed
-- headers but are still part of the public @crate::types@ surface.
extraNewtypes :: [Text]
extraNewtypes = ["MessageId"]

-- | Generates the declarative half of @crate::types@: the size constants, the
-- newtype/byte-array/enum macro invocations and the @ToxError@ result type.
-- The three @macro_rules!@ definitions and crypto helpers stay hand-written in
-- @src/types.rs@, which pulls this file in via @#[path = ...] mod generated@.
generateTypesModule :: SemanticModel -> (FilePath, Text)
generateTypesModule model =
    let
        -- `hex` is the private helper module from the hand-written
        -- `src/types.rs`; the byte-array macro expands to `hex::encode` calls,
        -- so it must be in scope at this (call-site) expansion point.
        header = RsRaw $ T.unlines
            [ "// Generated by apigen. Do not edit."
            , "use crate::ffi;"
            , "use crate::types::hex;"
            , "use serde::{Deserialize, Serialize};"
            , "use std::{error, ffi as std_ffi, fmt};"
            ]

        -- Size constants: every C #define whose name denotes a size or length.
        sizeConstants =
            [ RsConst Pub (stripToxPrefix (constantName c))
                      (TyPath "usize")
                      ("ffi::" <> constantName c <> " as usize")
            | c <- constants model
            , isSizeConstant (constantName c)
            ]

        -- impl_safe_newtype! for every u32 resource-id type.
        newtypeNames = map (idToRustPascal . idName) (idTypes model) ++ extraNewtypes
        newtypeInvocations =
            [ RsMacroCall "impl_safe_newtype" [n] | n <- nub newtypeNames ]

        -- impl_byte_array_type! for every fixed-size byte array newtype.
        byteArrayInvocations =
            [ RsMacroCall "impl_byte_array_type" [ty, stripToxPrefix sizeConst]
            | (sizeConst, ty) <- sizeToType model
            ]

        -- impl_tox_enum! for every C enum (parsed plus private extras).
        modelEnumPairs = [ (S.enumName e, map S.enumMemberCName (S.enumMembers e)) | e <- enums model ]
        allEnumPairs = sortOn fst (modelEnumPairs ++ privateEnumExtras)
        enumInvocations = map (uncurry mkEnumInvocation) allEnumPairs

        -- ToxError: one variant per error enum used by a generated method,
        -- plus the private-extra error enums and the two bespoke variants.
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

-- | True for C @#define@s that denote a buffer size or length, e.g.
-- @TOX_ADDRESS_SIZE@ or @TOX_MAX_NAME_LENGTH@.
isSizeConstant :: Text -> Bool
isSizeConstant n = "_SIZE" `T.isSuffixOf` n || "_LENGTH" `T.isSuffixOf` n

-- | Drops the leading @TOX_@ from a C identifier.
stripToxPrefix :: Text -> Text
stripToxPrefix n = fromMaybe n (T.stripPrefix "TOX_" n)

-- | Builds a single @impl_tox_enum!@ invocation for one C enum. Error enums
-- keep their C name; plain enums use the renamed safe name from 'safeEnumName'.
mkEnumInvocation :: Text -> [Text] -> RsItem
mkEnumInvocation cName members =
    let safe = fromMaybe cName (safeEnumName cName)
        block = "{\n" <> T.concat ["    " <> m <> ",\n" | m <- members] <> "}"
    in RsMacroCall "impl_tox_enum" [safe, "ffi::" <> cName, block]

-- | The C enum name of an error type identified by its semantic name in the
-- model, or 'Nothing' if no such enum exists.
errorEnumCName :: SemanticModel -> Text -> Maybe Text
errorEnumCName model sem =
    fmap S.enumName (find (\e -> S.enumName e == sem || S.enumSemanticName e == sem) (enums model))

-- | Renders the @ToxError@ enum and its @Error@/@Display@ impls plus the
-- @Result@ type alias. The variant set is every error enum reachable from a
-- generated method, plus the private-extra error enums.
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
        -- The plain `use crate::types;` is no longer needed: every safe
        -- enum/constant is brought in by the wildcard import below.
        imports = nub $ [RsUse Private "crate::ffi", RsUseAttr "allow(unused_imports)" "crate::types::*"] ++ extraImports ++ toxImport ++ [RsUse Private "crate::core::dispatch::ToxAVHandler" | needsAVHandler]

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
    -- `Send + Sync`: the boxed logger is handed to C and the toxcore log
    -- callback may invoke it from another thread.
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

-- | The C log-callback trampoline. Emitted verbatim: it must null-check every
-- incoming pointer (`CStr::from_ptr` / dereferencing `user_data` is UB on
-- null) and wrap the user logger in `catch_unwind` so a panic cannot unwind
-- across the C frame.
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

        -- A catch-all `Unknown` variant keeps `Event` forward-compatible: an
        -- event type the bindings were not generated for is surfaced rather
        -- than panicking — rs-toxcore-c may be built against a different
        -- c-toxcore than the library it links at runtime.
        variants' = map mkVariant (variantMembers var)
                 ++ [RsEnumVariant "Unknown" [] | isEvent]

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
                    Just e -> fromMaybe (memberName mem) $ lookup (memberName mem) (map (\m -> (S.enumMemberName m, S.enumMemberCName m)) (S.enumMembers e))
                    Nothing -> memberName mem

                enumTypeName = fromMaybe (idToRustPascal (S.variantTypeEnum var)) (fmap S.enumName enumMod)
                enumVariant = "ffi::" <> enumTypeName <> "::" <> cVariantName

                -- The `&*` raw-pointer deref must sit *inside* the unsafe block.
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

        -- `from_ptr` takes a raw `*const` event pointer and dereferences it,
        -- so it is an `unsafe fn`: the caller must guarantee the pointer is
        -- valid for `'a` (clippy's `not_unsafe_ptr_arg_deref` otherwise).
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

        -- A `_Nullable` element accessor already yields `Option<T>`, so `next`
        -- returns its result directly instead of re-wrapping it in `Some`.
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

        -- Determine if the target struct itself has a lifetime
        structHasLife = case find (\r -> idToRustPascal (resourceName r) == structBaseName) (S.resources model) of
            Just r  -> hasLifetime r
            Nothing -> False

        isLife = structHasLife
        gens = if structBaseName == "ToxAV" then ["'a", "H: ToxAVHandler"]
               else if isLife then ["'a"] else []
        structTy = if structBaseName == "ToxAV" then TyGeneric structBaseName [TyPath "'a", TyPath "H"]
                   else if isLife then TyGeneric structBaseName [TyPath "'a"] else TyPath structBaseName

        -- These functions need bespoke, handler-based safe wrappers that the
        -- generator cannot express; they are hand-written in core/av_dispatch.rs.
        notHandWritten m = S.methodName m `notElem` handWrittenFunctions
        -- Event accessors use raw C scalar types; refine well-known fields to
        -- the corresponding safe newtypes so the public API stays consistent.
        isEventResource = let rn = resourceName res
                          in rn == "Event" || "Event_" `T.isPrefixOf` rn
        refineEventOutput m
            | isEventResource = m { output = refineEventField (S.methodName m) (output m) }
            | otherwise       = m
        -- The destructor is rendered as a `Drop` impl (see generateDropImpl),
        -- never as a regular method: emitting both would double-free.
        resMethods = map refineEventOutput
            (filter (\m -> notHandWritten m && methodRole m /= Destructor) (S.methods res))

        (staticMethods, instanceMethods') = partition (\m -> methodRole m == Constructor || methodRole m == StaticRole) resMethods
        instanceMethods = if resourceName res == "Options"
                          then filter (\m -> S.methodName m `notElem` ["tox_options_set_log_callback", "tox_options_set_log_user_data", "tox_options_get_log_callback", "tox_options_get_log_user_data"]) instanceMethods'
                          else instanceMethods'

        (realStatic, ctors) = partition (\m -> methodRole m == StaticRole) staticMethods

        methodToItem = generateMethod model kind res

        -- The iterator's element accessor (e.g. `tox_events_get`) is unsound
        -- as a public method: it takes an unchecked index and feeds a
        -- possibly-null pointer to the `unsafe` `from_ptr`. Only the
        -- bounds-checked iterator may call it, so it is emitted private.
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
                        -- ToxAV must be killed before its parent Tox; a Drop
                        -- impl runs toxav_kill so the C assertion is not hit.
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

-- | C functions whose safe wrapper is hand-written (handler-based AV group
-- chat API) rather than generated. See core/av_dispatch.rs.
-- | Refines the output type of a well-known event accessor. The C events API
-- exposes raw scalars; the safe API uses newtypes for IDs and `usize` for the
-- standalone chunk length. Companion `_length` accessors are left untouched.
refineEventField :: Text -> SType -> SType
refineEventField cName t
    | "_get_friend_number"     `T.isSuffixOf` cName = SResourceId "Friend"
    | "_get_file_number"       `T.isSuffixOf` cName = SResourceId "File"
    | "_get_group_number"      `T.isSuffixOf` cName = SResourceId "Group"
    | "_get_conference_number" `T.isSuffixOf` cName = SResourceId "Conference"
    -- `peer_id` is the group-chat peer identifier; `peer_number` is the
    -- conference peer identifier. Each only appears on its own event family.
    | "_get_peer_id"           `T.isSuffixOf` cName = SResourceId "Group_Peer"
    | "_get_peer_number"       `T.isSuffixOf` cName = SResourceId "Conference_Peer"
    | "peer_id"                `T.isSuffixOf` cName = SResourceId "Group_Peer"
    -- `tox_event_group_message_get_message_id` -> GroupMessageId; the only
    -- other `_get_message_id` is the friend read-receipt id.
    | "_get_message_id" `T.isSuffixOf` cName && "_group_" `T.isInfixOf` cName
                                                    = SResourceId "Group_Message_Id"
    | "_get_message_id"        `T.isSuffixOf` cName = SResourceId "Friend_Message_Id"
    | "_get_length"            `T.isSuffixOf` cName = SSizeT
    -- `*const uint8_t` public-key accessors point at a fixed 32-byte buffer;
    -- expose them as the `PublicKey` newtype rather than a raw pointer.
    | "_get_public_key"        `T.isSuffixOf` cName = SFixedBytes "TOX_PUBLIC_KEY_SIZE" False
    | otherwise                                     = t

-- | C functions whose safe wrapper is hand-written (handler-based AV group
-- chat API) rather than generated. See core/av_dispatch.rs.
handWrittenFunctions :: [Text]
handWrittenFunctions =
    [ "toxav_add_av_groupchat"
    , "toxav_join_av_groupchat"
    , "toxav_group_send_audio"
    , "toxav_groupchat_enable_av"
    -- The iterate functions must register the Rust dispatch callbacks first.
    , "toxav_iterate"
    , "toxav_audio_iterate"
    , "toxav_video_iterate"
    -- toxav_get_tox returns a *non-owning* `Tox*`; a generated wrapper would
    -- box it in an owning `Tox` whose `Drop` calls `tox_kill` -> double-free.
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

                -- Special case for toxav_ which doesn't match the common prefix tox_
                stripped' = if "toxav_" `T.isPrefixOf` stripped
                            then T.drop 6 stripped
                            else if "toxav" `T.isPrefixOf` stripped
                            then T.drop 5 stripped
                            else stripped

                -- Event accessors are plain field getters; drop the `get_`
                -- prefix so e.g. tox_event_file_recv_get_data -> `data`.
                isEventRes = let rn = resourceName res
                             in rn == "Event" || "Event_" `T.isPrefixOf` rn
                strippedEvent = fromMaybe stripped' (T.stripPrefix "get_" stripped')

                -- The C events API names the enum-typed accessor `type` on
                -- some events (friend/conference message, conference invite)
                -- but `message_type` on others (group message). Normalise the
                -- bare `type` accessor to a name derived from its return enum
                -- so the generated API is consistent: Tox_Message_Type ->
                -- `message_type`, Tox_Conference_Type -> `conference_type`.
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
                        -- Tox and ToxAV wrappers hold a raw pointer, so FFI
                        -- calls only need a shared borrow; matches the safe layer.
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

        -- True when the companion `_length`/`_size` C function already
        -- returns `size_t` (-> `usize` in FFI). Then the slice-length value
        -- needs no `as usize` cast; event `_length` accessors return a
        -- narrower `uint32_t` and still need it.
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

        -- Event structs hold a `&'a` borrow of the underlying C event, so
        -- their byte accessors can hand out a zero-copy borrowed slice
        -- (`&'a [u8]`) rather than allocating a fresh `Vec<u8>`.
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

        -- An instance fixed-bytes getter with no error enum whose C function
        -- returns `bool` (presence flag) maps to Option<FixedBytes>. Static
        -- helpers (e.g. tox_hash) return the buffer unconditionally instead.
        isOptionFixedBytes = case output method of
            SFixedBytes _ _ -> not hasErrorPtr && hasBufferPtr
                               && cReturnType mapping == SBool
                               && methodRole method /= StaticRole
                               && methodRole method /= Constructor
            _               -> False

        -- The error type is the safe re-wrapped enum from `crate::types`
        -- (in scope via `use crate::types::*`), not the raw `ffi::` enum; the
        -- ffi_* macros convert with `.into()`.
        -- A `_Nullable` C return with no error enum: the pointer may be null,
        -- so the safe wrapper hands back `Option<T>` (see generateStandardBody).
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
                -- A `const char *` accessor returns a borrowed &str even when a
                -- companion `_length` function exists; decode it as a string.
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

-- | A `const char *` accessor with a companion `_length` function: the
-- string is not nul-terminated, so build a &str from the raw parts.
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

        -- Skip the `as usize` cast when the size function already returns
        -- `usize` (clippy rejects the reflexive cast).
        sizeExpr = if sizeIsUsize then EVar "size" else ECast (EVar "size") (TyPath "usize")

        accessStmts = [ StmtLet "size" Nothing callSize
                , StmtLet "ptr" Nothing callPtr
                , StmtExprNoSemi (EMethodCall (ECall (EVar "std::slice::from_raw_parts") [EVar "ptr", sizeExpr]) "to_vec" [])
                ]
    in StmtExprNoSemi (EUnsafe (RsBlock accessStmts))

-- | Like 'generateAccessBody', but for event byte accessors: the event
-- struct borrows the underlying C event for `'a`, so hand out a zero-copy
-- borrowed `&'a [u8]` slice instead of allocating a fresh `Vec<u8>`.
generateEventBytesBody :: Text -> [RsExpr] -> Text -> RsStmt
generateEventBytesBody cFunc macroArgs sizeFunc =
    let
        callSize = ECall (EVar ("ffi::" <> sizeFunc)) macroArgs
        callPtr = ECall (EVar ("ffi::" <> cFunc)) macroArgs

        -- A zero-length C buffer may be a null pointer, and
        -- `slice::from_raw_parts` is UB on null even when the length is 0.
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
                                -- ffi_call returns Result<RawEnum, _>; convert
                                -- the success payload to the safe `types::` enum.
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
                         -- `_Nullable` handle return, no error enum: null-check
                         -- the pointer and yield `Option<T>`.
                         let call = ECall (EVar ("ffi::" <> cFunc)) argExprs
                             wrapped = wrapType model res (Just (methodRole method))
                                                (output method) (EVar "raw")
                             -- `wrapType` may add its own `unsafe` block (e.g.
                             -- `from_ptr`); unwrap it — the whole body is
                             -- already inside one `unsafe` block here.
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
                             -- `wrapType` casts an `SSizeT` output to `usize`.
                             -- A C function that already returns `size_t`
                             -- yields `usize` in the FFI layer, so the cast
                             -- would be reflexive and clippy rejects it; skip
                             -- it and return the call result directly.
                             wrapped = if output method == SSizeT
                                          && cReturnType mapping == SSizeT
                                       then callBlock
                                       else wrapType model res (Just (methodRole method)) (output method) callBlock
                         in StmtReturn wrapped

constSizeExpr :: Text -> RsExpr
constSizeExpr c =
    let c' = if "TOX_" `T.isPrefixOf` c then T.drop 4 c else c
    -- The `*_SIZE`/`*_LENGTH` constants are already declared `usize`, so no
    -- cast is needed (clippy rejects the reflexive `as usize`). The names
    -- come from `crate::types` and are in scope via `use crate::types::*;`.
    in if T.any isUpper c'
       then EVar c' -- Constant (already usize)
       else ECast (ECall (EVar c') []) (TyPath "usize") -- Function call

-- | Resolves all C arguments to Rust expressions + statements
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

        -- A `&str` argument is always an input string; pass it to C as a
        -- nul-terminated string by building a CString first.
        cstrName = name <> "_cstr"
        extraStmts = case paramType param of
            SString ->
                [ StmtLet cstrName Nothing
                    (EMethodCall (ECall (EVar "std::ffi::CString::new") [EVar name]) "unwrap" []) ]
            -- A plain `uint8_t x[CONST]` buffer is a minimum-size slice: the C
            -- callee dereferences a `CONST`-byte prefix, so a shorter slice is
            -- an out-of-bounds read. Guard the length before the FFI call.
            -- (A named array typedef is a fixed newtype -- exact by
            -- construction -- and an expression-sized buffer has no constant
            -- to check; both skip the guard.)
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
            -- Plain enum args are the safe `types::` wrapper; convert to the
            -- raw `ffi::` enum the C function expects. Error enums are passed
            -- through unchanged (no safe wrapper), so `.into()` would be a
            -- reflexive no-op that clippy rejects -- omit it for those.
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
        -- Is the root a handle resource with a safe wrapper struct? If so an
        -- explicit root argument is the safe `&Root` wrapper, not a raw ptr.
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

        -- The root handle wrapper exposes its raw pointer via `.ptr`.
        ctxNames = [ if isCoveredBySelf i then selfName
                     else if i == 0 && rootIsHandle then argName (fullArgs !! i) <> ".ptr"
                     else argName (fullArgs !! i)
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
        name = paramSnake (paramName param)
        -- A fixed-size buffer parameter's C structure fully determines its safe
        -- type: a named array typedef (`Tox_Public_Key`) is a fixed newtype
        -- (handled via `sizeToType`/`arrayTypes`); a plain `uint8_t x[CONST]`
        -- is a minimum-size slice `&[u8]` (the callee reads a `CONST`-byte
        -- prefix, so the wrapper guards `x.len() >= CONST` before the FFI
        -- call -- see `resolveOne`).
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
    -- The safe enum names live in `crate::types`; every generated module
    -- already brings them in via `use crate::types::*;` (the wildcard is
    -- 'sometimes' all that's needed -- the explicit `use crate::types;` is
    -- emitted only where a `crate::core` body still refers to the module
    -- itself), so emit them unqualified.
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

-- | Maps a C enum name to the safe wrapper name exposed by `crate::types`.
-- Error enums (Tox_Err_*, Toxav_Err_*) keep their C name in `types.rs`, so
-- they are passed through as raw `ffi::` types. Plain enums are renamed.
safeEnumName :: Text -> Maybe Text
safeEnumName cName
    | "_Err_" `T.isInfixOf` cName = Nothing
    | cName == "Tox_Message_Type" = Just "MessageType"
    | otherwise                   = Just (idToRustPascal cName)

-- | True if the C enum is a plain (renamed) enum wrapped by `crate::types`.
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
                   -- `from_ptr` is an `unsafe fn`; the surrounding wrapper
                   -- owns the raw pointer's validity, so call it in `unsafe`.
                   -- The arg expression already carries its own `unsafe`
                   -- block for the ffi call -- unwrap it to avoid a nested
                   -- (clippy `unused_unsafe`) block.
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
    -- Used for event `length` accessors refined to usize from a narrower C int.
    SSizeT -> ECast expr (TyPath "usize")
    _ -> expr


-- ---------------------------------------------------------------------------
-- Phase 2: flat per-method safe wrappers on `tox::Tox` / `tox::ToxAV` /
-- `tox::Options`.
--
-- Each safe method is a mechanical delegation over the `core` tier. It is
-- derived from the very same `SemanticModel` method that produced the `core`
-- method: `generateMethod` is reused to obtain the exact argument list, types,
-- receiver and return type, so the safe signature can never drift from the
-- `core` one. Only two things are not derived from the model:
--
--   * the public *name* — the safe API renames `self_get_x` -> `x` and
--     prefixes `Options` setters with `set_` ('safeMethodName'); and
--   * a small exclusion list of methods whose safe body is non-mechanical
--     ('safeBespoke'), which stay hand-written.
-- ---------------------------------------------------------------------------

-- | C function names whose safe wrapper has a non-mechanical body; these stay
-- hand-written in `tox/mod.rs` / `toxav/mod.rs`.
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

-- | The few safe names that are not a mechanical transform of the core name.
safeRenames :: [(Text, Text)]
safeRenames = [("friend_list_size", "friend_list_len")]

-- | Mechanical core-name -> safe-name transform.
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

-- | Whether a method gets a generated flat safe wrapper.
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

-- | The delegating safe wrapper for one method. The signature is taken from
-- the `core` method `generateMethod` produces; only the name is transformed
-- and a `Tox_Err_*` result is remapped to the unified `ToxError`. @prefix@ is
-- the field path from `self` to the wrapped `core` object.
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

-- | The top-level handle resources that receive a generated safe-wrapper
-- module (C1). Inferred from the model: a `ResHandle` resource that has a
-- constructor and is not part of the `Event` variant family (the `Event_*`
-- resources are also `ResHandle` but route to the hand-written `events`
-- module). This yields `Tox`, `AV`, `Options` and `Pass_Key`; the handle
-- resources `Events` / `System` / `Iterate_Options` are excluded — they have
-- no constructor and no public safe surface.
safeHandleResources :: SemanticModel -> [SResource]
safeHandleResources model =
    [ r
    | r <- S.resources model
    , resourceType r == ResHandle
    , any ((== Constructor) . methodRole) (S.methods r)
    , not ("event" `T.isPrefixOf` idToRustSnake (resourceName r))
    ]

-- | Rust-domain glue for one flat-`impl` safe handle (`Tox` / `ToxAV` /
-- `Options`). The wrapper struct, its `Inner` indirection (`Tox`) and its
-- `<'a, H>` generics (`ToxAV`) are genuine hand-written ownership/safety
-- decisions, so this stays an explicit per-resource override keyed by handle
-- name; only the *set* of handles is inferred. @hgField@ is the field path
-- from `self` to the wrapped `core` object.
data HandleGlue = HandleGlue
    { hgFile     :: FilePath
    , hgImports  :: [Text]
    , hgImplType :: RsType
    , hgGenerics :: [Text]
    , hgField    :: Text
    }

-- | The glue table for the flat-`impl` safe handles. `Pass_Key` is absent: it
-- has no hand-written struct and is generated whole by 'generatePassKeyModule'.
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

-- | Emits the Phase-2 safe-wrapper files, one per top-level handle resource
-- (C1). For the flat-`impl` handles (`Tox` / `ToxAV` / `Options`) the wrapper
-- struct is hand-written and this emits a single delegating `impl` block over
-- it; every in-scope method gets a wrapper, mapping the `core` `Tox_Err_*`
-- result to the unified `ToxError`. `Pass_Key` owns its `core` handle and is
-- generated whole (struct + methods + module functions) by
-- 'generatePassKeyModule'. Each file carries its own `use` imports; the
-- hand-written `tox/mod.rs` / `toxav/mod.rs` pull them in via `#[path = ...]`.
generateSafeModules :: SemanticModel -> [(ModuleKind, SResource)] -> [(FilePath, Text)]
generateSafeModules model items =
    [ safeFileFor handle res
    | handle <- safeHandleResources model
    , let name = idToRustPascal (resourceName handle)
    -- `Tox` is split into per-subsystem `MainModule` slices; the safe wrapper
    -- covers the `tox`-subsystem slice (the un-prefixed methods). For the
    -- unsplit handles the `MainModule` item is the resource itself.
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

-- | The safe-wrapper file for one flat-`impl` handle.
generateFlatHandle :: SemanticModel -> SResource -> HandleGlue -> (FilePath, Text)
generateFlatHandle model res glue =
    mkSafeFile (hgFile glue)
        (hgImports glue)
        (RsItemImpl (RsImpl Nothing (hgImplType glue) (hgGenerics glue)
            (mapMaybe (safeWrapper model res (hgField glue)) (S.methods res))))

-- | Emits the safe-wrapper file for an owned-handle resource that has no
-- hand-written wrapper struct — `Pass_Key`. Unlike the flat-`impl` handles
-- (`Tox` / `ToxAV` / `Options`), the safe wrapper here is a generated newtype
-- over the `core` handle: it carries the `derive`/`derive_with_salt`
-- constructors and the `encrypt`/`decrypt` instance methods, plus the
-- module-level free functions for the resource's static helpers
-- (`pass_encrypt` -> `encrypt`, `pass_decrypt` -> `decrypt`, `get_salt`,
-- `is_data_encrypted`). The zero-argument `*_length` static helpers are
-- dropped — they duplicate the size constants in `crate::types`.
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
       -- A blank line between every fn in the impl block, matching the
       -- baseline. Each `*Fns` entry already carries a trailing newline; we
       -- splice them with a blank line in between, then indent each non-empty
       -- line four spaces.
       ++ indentNonEmpty (T.intercalate "\n" (ctorFns ++ instanceFns))
       ++ [ "}" ]
       -- The module-level free fns sit *below* the impl block, separated by
       -- a blank line from each other (and from the impl above).
       ++ ("" : T.lines (T.intercalate "\n" moduleFns)) ) )
  where
    -- The safe wrapper newtype and the wrapped `core` type share a name.
    typeName = idToRustPascal (resourceName res)

    -- The `core` `RsFn` for one model method, used purely to recover the exact
    -- safe argument list and error type.
    coreFn m = case generateMethod model MainModule res m of
        (RsItemFn fn : _) -> Just fn
        _                 -> Nothing

    argText (RsArg n ty)      = n <> ": " <> prettyType ty
    argText (RsSelfArg False) = "&self"
    argText (RsSelfArg True)  = "&mut self"
    argText RsSelf            = "self"

    valArgs fn  = [ a | a@RsArg{} <- fnArgs fn ]
    argExprs fn = [ n | RsArg n _ <- valArgs fn ]

    -- Maps a `core` return type to the safe wrapper return type and the
    -- `.map_err`/`.map` suffix that adapts the delegated call. @rewrapHandle@
    -- is set for constructors, whose `Self` result is rewrapped in the safe
    -- newtype.
    adapt rewrapHandle fn =
        case fnRet fn of
            Just (TyGeneric "std::result::Result" [t, TyPath errTy]) ->
                let t'   = if rewrapHandle then TyPath typeName else t
                    mapH = if rewrapHandle then ".map(" <> typeName <> ")" else ""
                in ( Just (prettyType (TyGeneric "Result" [t']))
                   , mapH <> ".map_err(ToxError::" <> toxErrorVariant errTy <> ")" )
            other -> (fmap prettyType other, "")

    -- One delegating wrapper function: @recv@ is the receiver args (empty for
    -- a static fn, `&self` for an instance method), @receiver@ the call
    -- target (`core::PassKey::` or `self.0.`), @rename@ the name transform
    -- and @rewrapHandle@ whether to rewrap a `Self` result.
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

    -- Constructors: `core::PassKey::derive(args).map(PassKey).map_err(..)`.
    ctorFns = mapMaybe
        (renderFn [] ("core::" <> typeName <> "::") id True)
        (methodsOf ((== Constructor) . methodRole))

    -- Instance methods: `self.0.encrypt(args).map_err(..)`.
    instanceFns = mapMaybe
        (renderFn ["&self"] "self.0." id False)
        (methodsOf ((== ActionRole) . methodRole))

    -- Module-level free functions for the static helpers that take arguments
    -- (the zero-arg `*_length` helpers are dropped). A `pass_`-prefixed helper
    -- drops that word (`pass_encrypt` -> `encrypt`); others keep their `core`
    -- name (`get_salt`, `is_data_encrypted`).
    --
    -- Q9: Stringizers (`err_*_to_string`) are grouped above the operational
    -- helpers (`encrypt`/`decrypt`/`get_salt`/`is_data_encrypted`); within
    -- each group methods stay in model order. The C interleaves them; the
    -- generated module presents them grouped for readability.
    rawModuleFns = methodsOf (\m -> methodRole m == StaticRole && not (null (S.inputs m)))
    isStringizer m = "to_string" `T.isSuffixOf` S.methodName m
    (stringizers, helpers) = partition isStringizer rawModuleFns
    moduleFns = mapMaybe
        (renderFn [] ("core::" <> typeName <> "::") stripPassPrefix False)
        (stringizers ++ helpers)

    stripPassPrefix n = fromMaybe n (T.stripPrefix "pass_" n)

-- | Splits text into lines and prefixes each non-empty line with four
-- spaces. Blank lines stay truly blank, so an emitted impl-block body keeps
-- diff-friendly empty separators between fns.
indentNonEmpty :: Text -> [Text]
indentNonEmpty = map indentLine . T.lines
  where
    indentLine "" = ""
    indentLine l  = "    " <> l

-- | Wraps a generated `impl` (or other top-level item) in a safe-wrapper file:
-- the `Generated by apigen` marker and the `use` imports.
mkSafeFile :: FilePath -> [Text] -> RsItem -> (FilePath, Text)
mkSafeFile name imports item =
    ( name
    , T.unlines ("// Generated by apigen. Do not edit." : imports)
      <> "\n"
      <> render (RsModule [item])
    )

-- ---------------------------------------------------------------------------
-- Callback dispatch layer.
--
-- For the `Tox` and `AV` resources, apigen emits the safe handler-trait layer
-- that bridges the raw C event callbacks to Rust:
--
--   * the `ToxHandler` / `ToxAVHandler` traits — one `on_<event>` default
--     method per event, taking safe Rust types;
--   * one `extern "C"` trampoline per event — it converts the raw C callback
--     arguments to safe types (newtype-wrapping ids, slicing pointer+length
--     pairs, `.into()`-ing enums) and forwards to `handler.on_<event>(...)`,
--     all inside `catch_unwind` so a handler panic cannot unwind into C;
--   * the callback-registration entry points (`tox_iterate`,
--     `register_av_callbacks` and the AV `iterate` family).
--
-- The whole layer is emitted as one `RsRaw` block: the trampolines follow a
-- fixed macro template (`define_dispatch!` / `define_av_dispatch!`) that the
-- typed `RsItem` AST cannot express compactly.
-- ---------------------------------------------------------------------------

-- | The C enum name backing a semantic `SEnum` reference.
enumCName :: SemanticModel -> Text -> Text
enumCName model sem =
    case find (\e -> S.enumSemanticName e == sem || S.enumName e == sem) (S.enums model) of
        Just e  -> S.enumName e
        Nothing -> "Tox_" <> sem

-- | The safe Rust handler-method type for one event parameter. Mirrors the
-- conversions in the hand-written `core/dispatch.rs` oracle.
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

-- | The safe newtype name for a `SResourceId` semantic id.
idResourceType :: Text -> Text
idResourceType n =
    let name = idToRustPascal n
    in if "Number" `T.isSuffixOf` name || name `elem` knownTypes
       then name
       else name <> "Number"

-- | Rust scalar type for a numeric `SType` (used for `SFixedList` elements).
scalarRsType :: SType -> Text
scalarRsType (SInt b)  = "i" <> T.pack (show b)
scalarRsType (SUInt b) = "u" <> T.pack (show b)
scalarRsType _         = "u8"

-- | The raw C-ABI parameters of one event parameter, as `(name, rustCType)`
-- pairs for the `extern "C"` trampoline signature. A byte array expands to a
-- `(ptr, len)` pair; the model dropped the `size_t` length the C signature
-- carries after each byte array, so it is reconstructed here.
rawCArgs :: SemanticModel -> SParameter -> [(Text, Text)]
rawCArgs model p =
    -- The trampoline parameter list uses the same `type_`-style keyword
    -- escape as the public safe wrappers (Q12: never emit `r#type` in
    -- generated parameter positions).
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

-- | The Rust expression converting one event parameter's raw C argument(s)
-- into the safe handler-method argument, together with any `let` bindings it
-- needs (emitted before the handler call).
--
-- @sliceFn@ is the name of the pointer+length slicing helper; @scalarSlice@
-- is the raw `slice::from_raw_parts` form used for sizer-expression arrays.
convertArg :: SemanticModel -> SParameter -> ([Text], Text)
convertArg model p =
    -- Must agree with the names 'rawCArgs' emits, so the `type_`-style
    -- escape (Q12) is shared here.
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
                -- A video plane: sized by a `/*! ... */` expression over the
                -- other (already-named) C parameters.
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

-- | A token of a C sizer expression.
data SizTok = SId Text | SNum Text | SOp Text | SLParen | SRParen | SComma
            | SDotAbs -- ^ Synthetic: renders as the Rust `.abs()` method call.
    deriving (Eq, Show)

-- | Tokenises a C sizer expression (as captured by `getSizerName`).
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

-- | Rewrites a C sizer expression (e.g. @max(width / 2, abs(ustride)) *
-- (height / 2)@) into a Rust expression. Every operand is a numeric C
-- callback parameter; identifiers are cast to `i64` so `abs()` and the
-- products cannot overflow before the final `as usize`. The C function call
-- `abs(X)` becomes the Rust method call `(X).abs()`.
sizerToRust :: Text -> Text
sizerToRust = T.concat . map render . absToMethod . sizTokens
  where
    render (SId "max") = "std::cmp::max"
    render (SId "min") = "std::cmp::min"
    -- `as` binds tighter than the arithmetic operators, so the cast needs no
    -- parentheses (clippy rejects redundant ones). `abs()` is the lone
    -- exception and `absToMethod` parenthesises its operand explicitly.
    render (SId n)     = n <> " as i64"
    -- A literal carries its type as a suffix: an `as i64` cast on a literal
    -- is redundant (clippy `unnecessary_cast`).
    render (SNum n)    = n <> "_i64"
    render (SOp o)     = " " <> o <> " "
    render SLParen     = "("
    render SRParen     = ")"
    render SComma      = ", "
    render SDotAbs     = ".abs()"

-- | Rewrites the C-call form `abs ( ... )` into a marker that renders as the
-- Rust method call `( ... ).abs()`: drop the `abs` identifier and append a
-- `.abs()` marker after the matching close paren.
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

-- | One `extern "C"` trampoline for an event, rendered verbatim. The raw C
-- signature keeps every C-ABI parameter (size parameters included); only the
-- handler call drops the size-only parameters hidden from the trait.
renderTrampoline :: SemanticModel -> Bool -> Text -> SEvent -> Text
renderTrampoline model isAv macroName ev =
    let fnName = "dispatch_" <> eventName ev
        -- The raw C ABI: friend_number stays a bare `u32`, every parameter
        -- present.
        rawArgs = concatMap (rawCArgs model) (eventParams ev)
        rawSig  = T.intercalate ", " [ n <> ": " <> ty | (n, ty) <- rawArgs ]
        -- The conversion: re-tag the AV friend_number, then convert each
        -- parameter. Size-only parameters still get their `let` bindings (the
        -- sizer that consumes them needs them) but are not passed on.
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

-- | The set of identifiers consumed by a sibling array's sizer expression
-- within one event. A length/count parameter that only appears in such a
-- sizer (e.g. @sample_count@ feeding @pcm[sample_count * channels]@) carries
-- no information the safe `&[..]` slice does not already, so it is dropped
-- from the handler-trait signature — matching the hand-written oracle.
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

-- | Whether an event parameter is visible in the safe handler trait. A
-- size-typed parameter consumed only by a sibling array sizer is hidden: the
-- slice it sizes already carries that length.
handlerVisible :: SEvent -> SParameter -> Bool
handlerVisible ev p = case paramType p of
    SSizeT -> paramSnake (paramName p) `notElem` sizerIdentifiers ev
    _      -> True

-- | An AV event's `friend_number` parameter is modelled as a bare `u32` (the
-- toxav C API predates the `Tox_Friend_Number` typedef), but the safe handler
-- still takes a `FriendNumber`. This re-tags it so the dispatch layer matches
-- the hand-written oracle.
fixAvFriendNumber :: Bool -> SParameter -> SParameter
fixAvFriendNumber isAv p
    | isAv, paramName p == "friend_number", paramType p == SUInt 32 =
        p { paramType = SResourceId "Friend_Number" }
    | otherwise = p

-- | One default trait method for an event.
renderTraitMethod :: SemanticModel -> Bool -> SEvent -> Text
renderTraitMethod model isAv ev =
    let -- An underscored param name is unused and need not be `r#`-escaped;
        -- `_type` is a plain identifier even though `type` is a keyword.
        plainSnake = T.toLower . T.pack . Casing.toSnake . Casing.fromAny . T.unpack
        params = [ "_" <> plainSnake (paramName p) <> ": "
                     <> handlerArgType model (fixAvFriendNumber isAv p)
                 | p <- eventParams ev, handlerVisible ev p ]
        allParams = "&mut self" : params
    in "    fn on_" <> eventName ev <> "(" <> T.intercalate ", " allParams <> ") {}"

-- | The complete Tox/ToxAV dispatch module emitted to
-- @core/generated/dispatch.rs@.
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

        -- `tox_iterate`: install every Tox event callback, then iterate.
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

-- | The two trampoline macros. Each builds an `unsafe extern "C"` callback
-- that null-checks `user_data`, recovers the `&mut H` handler and runs the
-- conversion body inside `catch_unwind` so a panic cannot unwind into C.
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
-- Phase 2: resource wrapper objects.
--
-- Each non-root id-resource (`Friend`, `Group`, `File`, `Conference`) gets a
-- generated wrapper struct that borrows the owning `Tox` and carries the
-- resource id(s). Its methods delegate to the flat `core` tier, injecting the
-- wrapper's id where the `core` signature expects it and remapping the
-- `core` `Tox_Err_*` result to the unified `ToxError`. The `Tox` resource
-- accessors/constructors (`tox.friend(n)`, `tox.friend_add(..)`, ...) are
-- generated alongside.
--
-- The wrapper struct boilerplate (`Debug`, `From` impls) and the bodies are
-- emitted as `RsRaw`: they follow a fixed template the typed `RsItem` AST
-- cannot express compactly, and the oracle is a strict signature contract.
--
-- A handful of oracle methods cannot be derived from the model — the AV
-- group-chat methods on `Conference` and the `Conference::peer_list` helper —
-- and stay hand-written in `tox/conference.rs`; see `nonGeneratableNote`.
-- ---------------------------------------------------------------------------

-- | A wrapped resource: the wrapper struct and the model resources whose
-- methods it exposes (the resource itself plus any sub-resources folded in).
data ResWrapper = ResWrapper
    { rwName      :: Text       -- ^ Wrapper struct name (e.g. @Friend@).
    , rwIdType    :: Text       -- ^ Primary id newtype (e.g. @FriendNumber@).
    , rwExtraId   :: Maybe (Text, Text) -- ^ Extra id field @(name, type)@ (File only).
    , rwResources :: [Text]     -- ^ Model resource names contributing methods.
    }

-- | Extra (non-primary) id fields a wrapper carries beyond its own id. This
-- is a Rust-domain ownership decision the model does not express: a `File` is
-- addressed by a @(friend, file)@ id pair, but the model keys it on the file
-- number alone. Keyed by wrapper name; absent means the wrapper carries only
-- its own id.
resWrapperExtraId :: Text -> Maybe (Text, Text)
resWrapperExtraId "File" = Just ("friend", "FriendNumber")
resWrapperExtraId _      = Nothing

-- | The wrapped resources, inferred from the resource hierarchy (C6). A
-- wrapper is generated for every id-keyed (`ResId` with an id type) resource
-- whose parent is the root handle (`Tox`): `Friend`, `Group`, `File`,
-- `Conference`. Each id-keyed *sub*-resource (a `ResId` descendant that
-- carries methods — the `*_Peer` resources) folds its methods into its
-- wrapping ancestor; folded sub-resources are ordered by name-component count
-- then name, so `Conference_Peer` precedes `Conference_Offline_Peer`. The
-- primary id newtype is derived from the resource's id type; the extra id
-- field (`File`'s `friend`) is the one Rust-domain override, see
-- 'resWrapperExtraId'.
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

    -- The semantic id-type name of a `ResId` resource, if it has one.
    idResourceIdType res = case resourceType res of
        ResId (SResourceId n) -> Just n
        _                     -> Nothing

    -- Every id-keyed descendant resource that carries methods, folded into
    -- its wrapping ancestor and ordered so a plainer name (`Conference_Peer`)
    -- precedes a more-qualified one (`Conference_Offline_Peer`).
    foldedSubResources wrapper =
        map resourceName $
        sortOn (\r -> (length (T.splitOn "_" (resourceName r)), resourceName r))
            [ r
            | r <- S.resources model
            , parent r == Just (resourceName wrapper)
            , isJust (idResourceIdType r)
            , not (null (S.methods r))
            ]

-- | C functions whose wrapper consumes @self@ (the resource is gone after the
-- call), rather than taking @&self@.
resConsumingSelf :: [Text]
resConsumingSelf = ["tox_friend_delete", "tox_conference_delete", "tox_group_leave"]

-- | Methods folded onto a wrapper from outside its own resource: the `core`
-- function lives on `Tox` but the oracle exposes it as a resource method.
-- @(wrapperName, cFunctionName)@. The model attributes the per-file transfer
-- functions to `Tox` (they predate the `Tox_File` resource split), and
-- `tox_self_set_typing` to `Tox`'s `self` family, but the oracle exposes them
-- on `File` / `Friend`.
resAdoptedMethods :: [(Text, Text)]
resAdoptedMethods =
    [ ("Friend", "tox_self_set_typing")
    , ("File", "tox_file_send_chunk")
    , ("File", "tox_file_control")
    , ("File", "tox_file_seek")
    , ("File", "tox_file_get_file_id")
    ]

-- | Resource-method C functions whose wrapper is hand-written. Each has an
-- oracle signature richer than the model: `tox_group_invite_friend` takes a
-- `&Friend` (not a bare `FriendNumber`), and `tox_group_leave` /
-- `tox_group_set_password` take an `Option<&[u8]>` the model cannot express
-- (the C buffer is mandatory; the oracle defaults a `None` to an empty slice).
resBespoke :: [Text]
resBespoke =
    [ "tox_group_invite_friend"
    , "tox_group_leave"
    , "tox_group_set_password"
    ]

-- | Mechanical core-fn -> oracle wrapper-method name. Every step is a general
-- rule, with no per-function table:
--
--   * strip the @tox_@ prefix, then a leading @self_@ — the C @tox_self_*@
--     family marks the implicit receiver, which is noise once the function is
--     a resource method (`tox_self_set_typing` on `Friend` -> `set_typing`).
--     The `self_` is only stripped in this primary-prefix position; a
--     sub-resource `self_` (`tox_group_self_get_name`) is kept, exactly like
--     `tox_group_peer_get_name` -> `peer_name`. Then the wrapper's *primary*
--     resource prefix word is stripped.
--   * drop a redundant `get_` segment, whether leading
--     (`conference_get_title` -> `title`) or after another segment
--     (`group_self_get_name` -> `self_name`). A `set_` segment is always kept.
--   * a `bool`-returning getter (a `get_*` whose model return is `SBool`) gets
--     the conventional Rust `is_` prefix (`tox_friend_get_typing` ->
--     `is_typing`), unless it already reads as a predicate (`is_`/`has_`).
--   * keyword collisions use the trailing-underscore escape, the same fix as
--     for parameter names (`tox_conference_get_type` -> `type_`).
resWrapperMethodName :: ResWrapper -> SMethod -> Text -> Text
resWrapperMethodName rw method cfn = isPrefixed (paramSnake (deGet stripped))
  where
    bare = fromMaybe cfn (T.stripPrefix "tox_" cfn)
    -- A leading `self_` (the C implicit-receiver marker, in primary-prefix
    -- position) is dropped; a sub-resource `self_` after a resource word is
    -- kept (handled below by stripping only the primary prefix).
    deSelf = fromMaybe bare (T.stripPrefix "self_" bare)
    -- Only the primary resource prefix (e.g. `group_`, `conference_`) is
    -- stripped; `tox_group_peer_*` keeps its `peer_` word.
    primaryPrefix = idToRustSnake (rwName rw) <> "_"
    stripped = fromMaybe deSelf (T.stripPrefix primaryPrefix deSelf)
    -- Whether the original C function is a getter (`get_*` segment present).
    isGetter = "get_" `T.isPrefixOf` stripped || "_get_" `T.isInfixOf` stripped
    -- Drop a `get_` segment anywhere: leading, or after a `_`. `_set_` is
    -- never touched.
    deGet n
        | Just r <- T.stripPrefix "get_" n = r
        | "_get_" `T.isInfixOf` n          = T.replace "_get_" "_" n
        | otherwise                        = n
    -- A bool-returning getter conventionally reads `is_*` in Rust.
    isPrefixed n
        | isGetter
        , output method == SBool
        , not ("is_" `T.isPrefixOf` n)
        , not ("has_" `T.isPrefixOf` n)
        = "is_" <> n
        | otherwise = n

-- | The wrapper method for one model method. Reuses 'generateMethod' to obtain
-- the exact `core` signature, then rewrites it into a delegating wrapper:
-- every id argument matching one of the wrapper's id fields is replaced by the
-- corresponding `self.<field>` access; remaining value arguments are kept.
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
            -- Split each value arg into either an id substitution (consumed
            -- from `self`) or a kept wrapper parameter.
            classify a@(RsArg _ ty) = case ty of
                TyPath p | Just selfExpr <- lookup p idFields -> Left selfExpr
                _                                            -> Right a
            classify a = Right a
            classified = map classify valArgs
            -- Q7: A kept parameter whose C name still carries the wrapper's
            -- primary prefix (e.g. `conference_peer_number` on a `Conference`
            -- wrapper, or `conference_offline_peer_number`) is renamed by
            -- dropping that prefix — the type already encodes the resource.
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

-- | Every C function used purely as another method's buffer-size companion
-- (`tox_friend_get_name_size` backs `tox_friend_get_name`). The resource
-- wrappers expose only the value getter, not the size helper.
sizeCompanionFunctions :: SemanticModel -> [Text]
sizeCompanionFunctions model = nub
    [ sf
    | r <- S.resources model
    , m <- S.methods r
    , CustomMapping cm <- [methodMapping m]
    , Just sf <- [cSizeFunctionName cm]
    ]

-- | All wrapper-method `RsItemFn`s for one wrapped resource, in model order.
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

-- | The id-getter(s) every wrapper exposes. @Friend@/@Group@ expose
-- @get_number@; @File@ exposes @number@/@friend_number@; @Conference@ exposes
-- @number@ — matching the oracle exactly. Emitted as `RsItemFn`s so they
-- share the single delegating `impl` block with the generated methods.
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

-- | The struct definition, `Debug` and `From` impls for one wrapper.
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
        -- The `From::from` parameter is named after the first lowercase
        -- letter of the wrapper struct (`f` for Friend, `g` for Group,
        -- `c` for Conference), matching the hand-written baseline.
        ctxLetter = T.toLower (T.take 1 n)
        fromImpls = case rwExtraId rw of
            -- File carries two ids and has no single canonical `From` impl.
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
          -- `Tox` is brought into scope by an explicit `use crate::tox::Tox;`
          -- at the top of the file. The `use` is module-local to the
          -- `#[path]`-included file, so resolving `Tox` to `crate::tox::Tox`
          -- works regardless of where the wrapper is pulled in.
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

-- | A note, emitted as a comment in `safe_conference.rs`, recording the oracle
-- methods that are not generatable and so stay hand-written.
nonGeneratableNote :: Text -> Text
nonGeneratableNote "Conference" = T.unlines
    [ "// The handler-based AV group-chat methods (`enable_av`, `av_enabled`,"
    , "// `send_audio`) and the `peer_list` helper cannot be derived from the"
    , "// model; they stay hand-written in `tox/conference.rs`."
    ]
nonGeneratableNote _ = ""

-- | Emits the resource wrapper file for one wrapped resource: the struct
-- header, then a single `impl` block that opens with the id-getter(s) and
-- closes with the delegating methods.
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

-- | The `Tox` resource accessors and constructors. Each is hand-templated:
-- the constructors delegate to a `core` constructor and wrap the returned id
-- in the wrapper struct; the plain accessors just build the struct.
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

-- | All Phase-2 resource wrapper files: one per wrapped resource plus the
-- `Tox` accessor file.
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
