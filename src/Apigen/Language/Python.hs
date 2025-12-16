{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cython binding generator.
--
module Apigen.Language.Python (generate) where

import qualified Apigen.Inference              as I
import qualified Apigen.Knowledge              as K
import           Apigen.Language.Python.AST
import qualified Apigen.Language.Python.Pretty as Pretty
import           Apigen.Model                  (headerOf, isEnumNamer,
                                                membersOf)
import           Apigen.Namespace              (rootNamespace)
import           Apigen.Semantic
import           Apigen.SizeExpr               (SizeExpr (..), parseSizeExpr)
import qualified Apigen.Types                  as T
import           Data.Char                     (isDigit, isUpper)
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

data Pkg = Pkg
    { pkgBase   :: FilePath           -- ^ Path stem, e.g. @"toxcore/tox"@.
    , pkgHeader :: Text               -- ^ C header for the @cdef extern@ block.
    , pkgName   :: Text               -- ^ Package id ("toxcore"/"toxav"/...).
    , pkgFiles  :: [Text]             -- ^ The headers it binds.
    }

-- | tox_events.h isn't bound.
packages :: [Pkg]
packages =
    [ Pkg "toxcore/tox"                   "tox/tox.h"             "toxcore"        ["tox.h", "tox_options.h"]
    , Pkg "toxav/toxav"                   "tox/toxav.h"           "toxav"          ["toxav.h"]
    , Pkg "toxencryptsave/toxencryptsave" "tox/toxencryptsave.h"  "toxencryptsave" ["toxencryptsave.h"]
    ]

-- | Whether a package has another library's handle.
holdsLibrary :: SemanticModel -> [SResource] -> Bool
holdsLibrary model = any ((== LibraryHandle) . K.resourceKind model)

-- | The Python module of the root's package: @pytox.toxcore.tox@.
rootModule :: SemanticModel -> Text
rootModule model = case [ p | r <- resources model, isRoot r, p <- packages, headerOf (resourceFile r) `elem` pkgFiles p ] of
    p : _ -> "pytox." <> Text.replace "/" "." (Text.pack (pkgBase p))
    []    -> error "rootModule: no package has the root"

filePkg :: Text -> Maybe Text
filePkg file = pkgName <$> List.find ((headerOf file `elem`) . pkgFiles) packages

resourcePkg :: SResource -> Maybe Text
resourcePkg = filePkg . resourceFile

generate :: SemanticModel -> [(FilePath, Text)]
generate model = concatMap (genPkg model) packages

genPkg :: SemanticModel -> Pkg -> [(FilePath, Text)]
genPkg model pkg =
    [ (pkgBase pkg ++ ".pxd", Pretty.render (genPxd model pkg pkgRes))
    , (pkgBase pkg ++ ".pyx", Pretty.render (genPyx model pkg pkgRes))
    ]
  where
    pkgRes = filter ((== Just (pkgName pkg)) . resourcePkg) (resources model)

cythonHeader :: Text
cythonHeader = "cython: language_level=3, linetrace=True"

--------------------------------------------------------------------------------
-- Model lookups
--------------------------------------------------------------------------------

tshow :: Show a => a -> Text
tshow = Text.pack . show

enumCName :: SemanticModel -> Text -> Text
enumCName model n =
    maybe n enumName (List.find ((== n) . enumSemanticName) (enums model))

resCName :: SemanticModel -> Text -> Text
resCName model n =
    maybe n cName (List.find ((== n) . resourceName) (resources model))

idCNameOf :: SemanticModel -> Text -> Text
idCNameOf model n =
    maybe n idCName (List.find ((== n) . idName) (idTypes model))

findRes :: SemanticModel -> Text -> Maybe SResource
findRes model n = List.find ((== n) . resourceName) (resources model)

resolvedEnum :: SemanticModel -> Text -> Maybe Text
resolvedEnum model n = enumName <$> List.find ((== n) . enumSemanticName) (enums model)

resolvedId :: SemanticModel -> Text -> Maybe Text
resolvedId model n = idCName <$> List.find ((== n) . idName) (idTypes model)

callbackCName :: SemanticModel -> Text -> Text
callbackCName model n =
    maybe n cbCName
        (List.find (\c -> cbName c == n || cbCName c == n) (callbacks model))

-- | Fixed-size byte buffers can come through as an 'SEnum'/'SResourceId'
-- naming no enum or id type.
norm :: SemanticModel -> SType -> SType
norm model = \case
    SEnum n       | resolvedEnum model n == Nothing -> SFixedBytes n False
    SResourceId n | resolvedId model n == Nothing   -> SFixedBytes n False
    t                                               -> t

okMember :: SemanticModel -> Text -> Text
okMember model errC =
    case List.find ((== errC) . enumName) (enums model) of
        Just e | (m : _) <- enumMembers e -> enumMemberCName m
        _                                 -> errC <> "_OK"

-- | Getter, setter and size function can each have a different error enum.
accessorErr :: SResource -> Maybe Text -> Maybe Text
accessorErr res mfn = do
    fn <- mfn
    m  <- List.find ((== fn) . methodName) (methods res)
    methodErrorType m

isHandle :: SResource -> Bool
isHandle r = case resourceType r of
    ResHandle -> True
    _         -> False

isResId :: SResource -> Bool
isResId = not . isHandle

cmOf :: SMethod -> CFunctionMapping
cmOf meth = case methodMapping meth of
    CustomMapping c -> c
    StandardMapping -> CFunctionMapping (methodName meth) [] Nothing Nothing
                                        (methodErrorType meth) (inputs meth) (output meth) []

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

cType :: SemanticModel -> SType -> Text
cType model t0 = case norm model t0 of
    SVoid            -> "void"
    SBool            -> "bool"
    SInt n           -> "int" <> tshow n <> "_t"
    SUInt n          -> "uint" <> tshow n <> "_t"
    SSizeT           -> "size_t"
    SString          -> "char*"
    SBytes           -> "uint8_t*"
    SFixedBytes{}    -> "uint8_t*"
    SFixedList t _ _ -> cType model t <> "*"
    SEnum n          -> enumCName model n
    SHandle "void"   -> "void*"
    SHandle n        -> resCName model n <> "*"
    SCallback n      -> callbackCName model n <> "*"
    SResourceId n    -> idCNameOf model n
    SList t          -> cType model t <> "*"

pyType :: SemanticModel -> SType -> Text
pyType model t0 = case norm model t0 of
    SVoid            -> "None"
    SBool            -> "bool"
    SInt _           -> "int"
    SUInt _          -> "int"
    SSizeT           -> "int"
    SString          -> "str"
    SBytes           -> "bytes"
    SFixedBytes{}    -> "bytes"
    SFixedList t _ _ | isNumericElem t -> "array"
                     | otherwise       -> "list[" <> pyType model t <> "]"
    SEnum n          -> enumCName model n
    SHandle "void"   -> "object"
    SHandle n        -> resCName model n <> "_Ptr"
    SCallback _      -> "object"
    SResourceId n    -> idCNameOf model n
    SList t          -> "list[" <> pyType model t <> "]"

-- | Unlike a parameter, an 'SString' return is @const char *@.
cRetText :: SemanticModel -> SType -> Text
cRetText model t = case norm model t of
    SString -> "const char*"
    _       -> cType model t

-- | Puts back the @size_t@ lengths the model drops. A sized array, such as
-- a video plane, has its size expression instead.
expandedCbParams :: SemanticModel -> [SParameter] -> [Param]
expandedCbParams model = concatMap expand
  where
    expand p =
        cParam model (paramConstness p) (paramType p) (paramName p)
        : [ Param (paramName p <> "_length") "size_t" Nothing | norm model (paramType p) == SBytes ]

isPtrType :: SType -> Bool
isPtrType = \case
    SString -> True; SBytes -> True; SFixedBytes{} -> True
    SHandle{} -> True; SList{} -> True; SFixedList{} -> True
    SCallback{} -> True; _ -> False

cParam :: SemanticModel -> T.Constness -> SType -> Text -> Param
cParam model cns ty name = Param name tyText Nothing
  where
    base = cType model ty
    nty  = norm model ty
    -- Byte-array parameters are always inputs, hence always const.
    constByte = case nty of
        SBytes -> True; SString -> True; SFixedBytes{} -> True; _ -> False
    tyText | constByte                            = "const " <> base
           | cns == T.ConstThis && isPtrType nty  = "const " <> base
           | otherwise                            = base

--------------------------------------------------------------------------------
-- Hierarchy helpers
--------------------------------------------------------------------------------

rootHandle :: SemanticModel -> [SResource] -> SResource -> SResource
rootHandle model allRes res = fromMaybe res (findRes model (I.findRoot allRes res))

pathIdResources :: SemanticModel -> [SResource] -> SResource -> [SResource]
pathIdResources model allRes res =
    [ r | nm <- I.getHierarchy allRes res, Just r <- [findRes model nm], isResId r ]

idParamName :: SResource -> Text
idParamName r = Text.toLower (resourceName r) <> "_number"

--------------------------------------------------------------------------------
-- .pxd generation
--------------------------------------------------------------------------------

genPxd :: SemanticModel -> Pkg -> [SResource] -> Module
genPxd model pkg pkgRes = Module cythonHeader $
    [ CImport "libcpp" ["bool"]
    , CImport "libc.stdint"
        ["uint8_t", "uint16_t", "uint32_t", "uint64_t", "int16_t", "int32_t", "int64_t"]
    , CImport "libc.stdlib" ["malloc", "free"]
    , Import "typing" ["Optional"]
    ]
    -- Another library's package uses the root's handle, its wrapper class
    -- and the id types.
    ++ [ CImport (rootModule model) (cName root : cName root <> "_Ptr" : map idCName (idTypes model))
       | holdsLibrary model pkgRes, root <- filter isRoot allRes ]
    ++ [ Extern (pkgHeader pkg) externDecls ]
    ++ map (pxdClass model allRes) (filter isHandle pkgRes)
  where
    allRes = resources model
    pkgCbs = [ cb | cb <- callbacks model, filePkg (cbFile cb) == Just (pkgName pkg) ]
    externDecls =
        [ ExEnum (enumName e) (map enumMemberCName (enumMembers e))
        | e <- enumsFor model pkgRes pkgCbs ]
        ++ [ ExOpaque (cName r) | r <- pkgRes, isHandle r ]
        ++ [ ExAlias "uint32_t" (idCName it)
           | any isRoot pkgRes, it <- idTypes model ]
        ++ [ ExCallback (cbCName cb) (expandedCbParams model (cbParams cb))
           | cb <- pkgCbs ]
        ++ map ExFunc (nubByName (concatMap (resExternFuncs model allRes) pkgRes))

-- | Methods its package binds: not the old group calls on @Tox@,
-- @tox_events_init@ or @*_to_string@.
ownMethods :: SResource -> [SMethod]
ownMethods r = filter ok (methods r)
  where
    ok m = filePkg (methodFile m) == resourcePkg r
           && not (isEnumNamer m)

nubByName :: [Func] -> [Func]
nubByName = List.nubBy (\a b -> fnName a == fnName b)

resExternFuncs :: SemanticModel -> [SResource] -> SResource -> [Func]
resExternFuncs model allRes r =
    -- Getter/setter functions are declared via 'propExterns' instead.
    map (methodExtern model allRes r)
        [ m | m <- ownMethods r, methodRole m `notElem` [GetterRole, SetterRole] ]
    ++ concatMap (propExterns model allRes r) (properties r)
    ++ map (eventExtern model r) (events r)

enumsFor :: SemanticModel -> [SResource] -> [SCallbackTypeModel] -> [SEnumModel]
enumsFor model pkgRes pkgCbs = filter ((`elem` refs) . enumName) (enums model)
  where
    refs = List.nub $ concatMap resRefs pkgRes ++ concatMap cbRefs pkgCbs
    cbRefs cb = concatMap (typeEnums . paramType) (cbParams cb)
    resRefs r =
        mapMaybe methodErrorType (ownMethods r)
        ++ mapMaybe propErrorType (properties r)
        ++ concatMap (typeEnums . output) (ownMethods r)
        ++ concatMap (concatMap (typeEnums . paramType) . inputs) (ownMethods r)
        ++ concatMap (typeEnums . propType) (properties r)
        ++ concatMap (concatMap (typeEnums . paramType) . eventParams) (events r)
    typeEnums = \case
        SEnum n -> [enumCName model n]
        _       -> []

methodExtern :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
methodExtern model allRes res meth =
    (externFunc (cFunctionName cm) (cRetText model (cReturnType cm))
                (reconArgs model allRes res cm bufElemC))
        -- tox_iterate runs Python callbacks, so it must propagate exceptions.
        { fnExcept = any isUserData (argMapping cm) }
  where
    cm = cmOf meth
    isUserData UserData = True
    isUserData _        = False
    bufElemC = case norm model (output meth) of
        SList el          -> cType model el
        SFixedList el _ _ -> cType model el
        _                 -> "uint8_t"

propExterns :: SemanticModel -> [SResource] -> SResource -> SProperty -> [Func]
propExterns model allRes res prop =
    [ externFunc fn (propRet model prop rErr) (propParams model allRes res prop PRead rErr)
    | Just fn <- [propRead prop] ]
    ++ [ externFunc fn "size_t" (propParams model allRes res prop PSize rErr)
       | Just fn <- [propSize prop] ]
    ++ [ externFunc fn (if wErr /= Nothing then "bool" else "void")
                       (propParams model allRes res prop PWrite wErr)
       | Just fn <- [propWrite prop] ]
  where
    rErr = accessorErr res (propRead prop)
    wErr = accessorErr res (propWrite prop)

eventExtern :: SemanticModel -> SResource -> SEvent -> Func
eventExtern model res ev = externFunc
    (cPrefix res <> "callback_" <> eventName ev) "void"
    ( [ cParam model T.MutableThis (SHandle (resourceName res)) "self"
      , Param "callback" (cCallback ev <> "*") Nothing
      ]
      ++ [ Param "user_data" "void*" Nothing | eventHasUserData ev ] )

externFunc :: Text -> Text -> [Param] -> Func
externFunc name ret params = Func
    { fnKind = KindCdef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = CStyle, fnExcept = False, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

-- | @bufElemC@ is the C element type of the output buffer, if any.
reconArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> Text -> [Param]
reconArgs model allRes res cm bufElemC =
    snd (List.mapAccumL step Nothing (argMapping cm))
  where
    semP  = cSemParams cm
    errTy = fromMaybe "void" (cErrorType cm)
    step prev = \case
        ThisObject cns ->
            (prev, cParam model cns (SHandle (resourceName res)) "self")
        PathObject n cns ->
            let tgt = I.resolvePathTarget allRes res n
            in (prev, cParam model cns (SHandle tgt) (Text.toLower tgt))
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in (prev, Param (Text.toLower tgt <> "_number")
                            (idCNameOf model (tgt <> "_Number")) Nothing)
        SemanticArg n | n < length semP ->
            let p = semP !! n
            in (Just (paramName p), cParam model (paramConstness p) (paramType p) (paramName p))
        SemanticArg _ ->
            (prev, Param "arg" "void*" Nothing)
        BufferSize ->
            (prev, Param (maybe "length" (<> "_len") prev) "size_t" Nothing)
        ErrorPtr ->
            (prev, Param "error" (errTy <> "*") Nothing)
        BufferPtr _ ->
            (prev, Param "data" (bufElemC <> "*") Nothing)
        UserData ->
            (prev, Param "user_data" "void*" Nothing)
        Constant v ->
            (prev, Param "value" "int" (Just (tshow v)))

data PropKind = PRead | PSize | PWrite

propParams :: SemanticModel -> [SResource] -> SResource -> SProperty -> PropKind -> Maybe Text -> [Param]
propParams model allRes res prop kind mErr =
    selfP : idParams ++ valueParams ++ errParams
  where
    root  = rootHandle model allRes res
    cns   = case kind of PWrite -> T.MutableThis; _ -> T.ConstThis
    selfP = cParam model cns (SHandle (resourceName root)) "self"
    idParams =
        [ Param (idParamName r) (idCNameOf model (resourceName r <> "_Number")) Nothing
        | r <- pathIdResources model allRes res ]
    errParams =
        [ Param "error" (errC <> "*") Nothing | Just errC <- [mErr] ]
    bufElem = case norm model (propType prop) of
        SList el          -> cType model el
        SFixedList el _ _ -> cType model el
        _                 -> "uint8_t"
    valueParams = case kind of
        PSize  -> []
        PRead
            | isBufferType (propType prop) -> [Param (propName prop) (bufElem <> "*") Nothing]
            | otherwise                    -> []
        PWrite
            | isBufferType (propType prop) ->
                [ Param (propName prop) ("const " <> bufElem <> "*") Nothing
                , Param "length" "size_t" Nothing ]
            | otherwise ->
                [ cParam model T.ConstThis (propType prop) (propName prop) ]

propRet :: SemanticModel -> SProperty -> Maybe Text -> Text
propRet model prop mErr
    | isBufferType (norm model (propType prop)) =
        if mErr /= Nothing then "bool" else "void"
    | otherwise = cRetText model (propType prop)

-- | Types the size-then-fill idiom returns. Not 'SString': a @char *@
-- getter returns its pointer.
isBufferType :: SType -> Bool
isBufferType = \case
    SBytes -> True; SFixedBytes{} -> True
    SList{} -> True; SFixedList{} -> True; _ -> False

isNumericElem :: SType -> Bool
isNumericElem = \case
    SInt n  -> n > 8
    SUInt n -> n > 8
    _       -> False

numericArray :: SemanticModel -> SType -> Maybe (SType, Text)
numericArray model t = case norm model t of
    SFixedList el sz _ | isNumericElem el -> Just (el, sz)
    _                                     -> Nothing

arrayTypecode :: SType -> Text
arrayTypecode = \case
    SInt 8  -> "b"; SUInt 8  -> "B"
    SInt 16 -> "h"; SUInt 16 -> "H"
    SInt 32 -> "i"; SUInt 32 -> "I"
    SInt 64 -> "q"; SUInt 64 -> "Q"
    _       -> "B"

pxdClass :: SemanticModel -> [SResource] -> SResource -> Item
pxdClass model allRes r = ClassDef Class
    { clsCdef   = True
    , clsName   = cName r <> "_Ptr"
    , clsBases  = []
    , clsFields = [(cName r <> "*", "_ptr")]
    , clsItems  =
        Decl (sigFunc "_get" [self] (cName r <> "*") True)
        : [ Decl (ctorSig model allRes r m) | m <- ctorsOf r ]
    }
  where
    self = Param "self" "" Nothing

sigFunc :: Text -> [Param] -> Text -> Bool -> Func
sigFunc name params ret excpt = Func
    { fnKind = KindCdef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = CStyle, fnExcept = excpt, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

ctorSig :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
ctorSig model allRes r m =
    sigFunc ("_" <> shortName r m)
            (Param "self" "" Nothing : ctorParams model allRes r (cmOf m))
            (cName r <> "*")
            False

-- | With @optional@ (for @__init__@), handles default to @None@, which @.pxd@
-- declarations can't.
ctorParamsWith :: Bool -> SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
ctorParamsWith optional model allRes res cm = mapMaybe pick (argMapping cm)
  where
    semP = cSemParams cm
    pick = \case
        -- None gives the C API's NULL error.
        PathObject n _ ->
            let tgt = I.resolvePathTarget allRes res n
                ty  = resCName model tgt <> "_Ptr"
            in Just (if optional
                     then Param (Text.toLower tgt) ("Optional[" <> ty <> "]") (Just "None")
                     else Param (Text.toLower tgt) ty Nothing)
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in Just (Param (Text.toLower tgt <> "_number")
                           (idCNameOf model (tgt <> "_Number")) Nothing)
        SemanticArg n | n < length semP ->
            let p = semP !! n
            in Just (case norm model (paramType p) of
                -- The C API accepts NULL (e.g. tox_new's options).
                SHandle _ | optional ->
                    Param (paramName p)
                          ("Optional[" <> pyType model (paramType p) <> "]")
                          (Just "None")
                _ -> pyParam model p)
        _ -> Nothing

ctorParams :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
ctorParams = ctorParamsWith False

initParams :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
initParams = ctorParamsWith True

ctorsOf :: SResource -> [SMethod]
ctorsOf r =
    [ m | m <- methods r
        , methodRole m == Constructor
        , cPrefix r `Text.isPrefixOf` methodName m  -- exclude cross-package ctors
        , output m /= SVoid                         -- exclude copy-style "ctors"
    ]

--------------------------------------------------------------------------------
-- .pyx generation
--------------------------------------------------------------------------------

genPyx :: SemanticModel -> Pkg -> [SResource] -> Module
genPyx model _pkg pkgRes = Module cythonHeader $
    [ Import "array" ["array"]
    , Import "pytox" ["common"]
    , Import "types" ["TracebackType"]
    , Import "typing" ["Optional"]
    , Import "typing" ["TypeVar"]
    ]
    ++ [ CImport (rootModule model) [cName root <> "_Ptr"] | holdsLibrary model pkgRes, root <- filter isRoot (resources model) ]
    ++ [ RawItem "T = TypeVar(\"T\")"
       , ClassDef (Class False "ApiException" ["common.ApiException"] [] [])
       ]
    ++ concatMap (pyxClass model allRes) handles
    ++ moduleConsts (rootNamespace model) handles
    ++ concatMap (map FuncDef . moduleFns model allRes) pkgRes
  where
    allRes  = resources model
    handles = filter isHandle pkgRes

pyxClass :: SemanticModel -> [SResource] -> SResource -> [Item]
pyxClass model allRes h =
    [ CdefBlock (trampolines ++ [installHandlers model h evs]) | not (null evs) ]
    ++ [ ClassDef Class
        { clsCdef   = True
        , clsName   = cName h <> "_Ptr"
        , clsBases  = []
        , clsFields = []
        , clsItems  = map FuncDef classFuncs
        } ]
  where
    mems        = membersOf allRes h
    evs         = concatMap events mems
    trampolines = concatMap (eventTrampolines model h) evs
    classFuncs  =
        [ getMethod h, delMethod, enterMethod ]
        ++ [ exitMethod model d | d <- take 1 dtors ]
        ++ [ ctorMethod model allRes h c | c <- ctorsOf h ]
        ++ initMethods model allRes h
        ++ concatMap (resMethods model allRes h) mems
        ++ [ handleStub model ev | ev <- evs ]
    dtors = [ m | m <- methods h, methodRole m == Destructor ]

resMethods :: SemanticModel -> [SResource] -> SResource -> SResource -> [Func]
resMethods model allRes h res =
    concatMap memberFunc
        [ m | m <- ownMethods res
            , methodRole m `elem` roles
            , not (touchesOpaque model m)
            , methodName m `notElem` accessorFns ]  -- exposed via a property
    ++ concatMap (propertyFuncs model allRes h res) (properties res)
  where
    -- A handle's constructor/destructor are __init__/__exit__, a
    -- sub-resource's ordinary methods (friend_add, ...).
    roles | resourceName res == resourceName h = [ActionRole, IterateRole]
          | otherwise = [Constructor, Destructor, ActionRole, IterateRole]
    accessorFns = mapMaybe id
        (concatMap (\p -> [propRead p, propWrite p, propSize p]) (properties res))

    memberFunc m
        | isNullaryGetter m =
            [ (defFunc (flatName h res m) [Param "self" "" Nothing]
                       (pyType model (output m)))
                { fnDecorators = ["property"]
                , fnBody = callBody model allRes res m } ]
        | otherwise = [ actionMethod model allRes h res m ]
    -- E.g. iteration_interval. A sub-resource getter still takes an id.
    isNullaryGetter m =
        methodRole m `elem` [ActionRole, IterateRole]
        && null (inputs m)
        && not (any isPathId (argMapping (cmOf m)))
        && norm model (output m) /= SVoid
        && methodErrorType m == Nothing
    isPathId PathId{} = True
    isPathId _        = False

touchesOpaque :: SemanticModel -> SMethod -> Bool
touchesOpaque model m = any bad (output m : map paramType (inputs m))
  where
    bad t = case norm model t of
        SCallback{}    -> True
        SHandle "void" -> True
        _              -> False

--------------------------------------------------------------------------------
-- Fixed class members
--------------------------------------------------------------------------------

getMethod :: SResource -> Func
getMethod r = (sigFunc "_get" [Param "self" "" Nothing] (cName r <> "*") True)
    { fnBody =
        [ SIf (EBinOp "is" (EAttr (EName "self") "_ptr") (EName "NULL"))
              [ SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") []) ]
        , SReturn (Just (EAttr (EName "self") "_ptr"))
        ]
    }

delMethod :: Func
delMethod = (defFunc "__del__" [Param "self" "" Nothing] "None")
    { fnBody = [ SExpr (ECall (EAttr (EName "self") "__exit__")
                              [EName "None", EName "None", EName "None"]) ] }

enterMethod :: Func
enterMethod = (defFunc "__enter__" [Param "self" "T" Nothing] "T")
    { fnBody = [ SReturn (Just (EName "self")) ] }

exitMethod :: SemanticModel -> SMethod -> Func
exitMethod _ dtor = (defFunc "__exit__" params "None")
    { fnBody =
        [ SExpr (ECall (EName (cFunctionName (cmOf dtor))) [EAttr (EName "self") "_ptr"])
        , SAssign (EAttr (EName "self") "_ptr") (EName "NULL")
        ]
    }
  where
    params =
        [ Param "self" "" Nothing
        , Param "exc_type" "type[BaseException] | None" Nothing
        , Param "exc_value" "BaseException | None" Nothing
        , Param "exc_traceback" "TracebackType | None" Nothing
        ]

ctorMethod :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
ctorMethod model allRes r m =
    (sigFunc ("_" <> shortName r m)
             (Param "self" "" Nothing : ctorParams model allRes r cm)
             (cName r <> "*")
             False)
    { fnBody = checkLenStmts model m ++ errLeadIn ++
        [ SCdef (cName r <> "*") "ptr" (Just (ECall (EName (cFunctionName cm)) callArgs)) ]
        ++ errCheck ++ [ SReturn (Just (EName "ptr")) ]
    }
  where
    cm = cmOf m
    callArgs = genCallArgs model allRes r cm "ptr" "error" True
    (errLeadIn, errCheck) = case methodErrorType m of
        Just errC ->
            ( [ SCdef errC "error" (Just (EName (okMember model errC))) ]
            , [ SIf (EName "error") [ SRaise (raiseExc errC "error") ] ] )
        Nothing -> ([], [])

-- | Several (prefix-nested) constructors become one @__init__@ with optional
-- tail parameters.
initMethods :: SemanticModel -> [SResource] -> SResource -> [Func]
initMethods model allRes r =
    case List.sortOn (length . inputs) (ctorsOf r) of
        []          -> []
        [c]         -> [ initMethod model allRes r c ]
        (c0 : rest)-> [ overloadedInit model allRes r c0 (last rest) ]

initMethod :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
initMethod model allRes r m = (defFunc "__init__" params "None")
    { fnDoc  = Just ("Create new " <> cName r <> " object.")
    , fnBody =
        SAssign (EAttr (EName "self") "_ptr")
                (ECall (EAttr (EName "self") ("_" <> shortName r m))
                       (map (EName . pmName) cps))
        : installHandlersStmts r
    }
  where
    cps = initParams model allRes r (cmOf m)
    params = Param "self" "" Nothing : cps

overloadedInit :: SemanticModel -> [SResource] -> SResource -> SMethod -> SMethod -> Func
overloadedInit model allRes r short long = (defFunc "__init__" params "None")
    { fnDoc  = Just ("Create new " <> cName r <> " object.")
    , fnBody =
        [ SIf (EBinOp "is not" (EName disc) (EName "None"))
              [ assignTo (shortName r long) longArgs ]
        , SIf (EBinOp "is" (EName disc) (EName "None"))
              [ assignTo (shortName r short) shortArgs ]
        ] ++ installHandlersStmts r
    }
  where
    minN     = length (inputs short)
    longP    = initParams model allRes r (cmOf long)
    params   = Param "self" "" Nothing : zipWith optionalise [0 ..] longP
    optionalise i p
        | i >= minN = optionalParam p
        | otherwise = p
    disc      = pmName (longP !! minN)
    longArgs  = map (EName . pmName) longP
    shortArgs = map (EName . pmName) (take minN longP)
    assignTo nm args =
        SAssign (EAttr (EName "self") "_ptr")
                (ECall (EAttr (EName "self") ("_" <> nm)) args)

optionalParam :: Param -> Param
optionalParam (Param n t _) =
    Param n (if "Optional[" `Text.isPrefixOf` t then t else "Optional[" <> t <> "]")
            (Just "None")

installHandlersStmts :: SResource -> [Stmt]
installHandlersStmts r =
    [ SExpr (ECall (EName "install_handlers")
               [EName "self", ECall (EAttr (EName "self") "_get") []])
    | not (null (events r)) ]

--------------------------------------------------------------------------------
-- Action methods
--------------------------------------------------------------------------------

actionMethod :: SemanticModel -> [SResource] -> SResource -> SResource -> SMethod -> Func
actionMethod model allRes h res m = (defFunc (flatName h res m) params ret)
    { fnBody = callBody model allRes res m }
  where
    cm     = cmOf m
    params = Param "self" "" Nothing : callerParams model allRes res cm
    ret    = pyType model (output m)

moduleFns :: SemanticModel -> [SResource] -> SResource -> [Func]
moduleFns model allRes r =
    [ (defFunc (modName m) (callerParams model allRes r (cmOf m)) (pyType model (output m)))
        { fnBody = callBody model allRes r m }
    | m <- ownMethods r
    , methodRole m == StaticRole
    , not (null (inputs m))
    , not (touchesOpaque model m)
    ]
  where
    modName m = fromMaybe (methodName m) (Text.stripPrefix (rootNamespace model <> "_") (methodName m))

callerParams :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
callerParams model allRes res cm = mapMaybe pick (argMapping cm)
  where
    semP = cSemParams cm
    pick = \case
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in Just (Param (Text.toLower tgt <> "_number")
                           (idCNameOf model (tgt <> "_Number")) Nothing)
        SemanticArg n | n < length semP -> Just (pyParam model (semP !! n))
        _ -> Nothing

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

badPropType :: SType -> Bool
badPropType = \case
    SCallback{} -> True
    SHandle{}   -> True
    _           -> False

propertyFuncs :: SemanticModel -> [SResource] -> SResource -> SResource -> SProperty -> [Func]
propertyFuncs model allRes h res prop
    | propRead prop == Nothing    = []
    | badPropType (propType prop) = []
    | otherwise                   = getter ++ setter
  where
    ty       = norm model (propType prop)
    isBuf    = isBufferType ty
    rErr     = accessorErr res (propRead prop)
    wErr     = accessorErr res (propWrite prop)
    -- A buffer getter fills a caller buffer, or returns the data pointer
    -- (tox_options_get_savedata_data).
    getterNullable = maybe False methodReturnNullable
        (propRead prop >>= \fn -> List.find ((== fn) . methodName) (methods res))
    getterReturnsPtr = case propRead prop of
        Just fn -> case List.find ((== fn) . methodName) (methods res) of
            Just m  -> isBufferType (norm model (cReturnType (cmOf m)))
            Nothing -> False
        Nothing -> False
    idParams = [ Param (idParamName r) (idCNameOf model (resourceName r <> "_Number")) Nothing
               | r <- pathIdResources model allRes res ]
    idArgs   = map (EName . pmName) idParams
    asProp   = isHandle res && null idParams
    selfP    = Param "self" "" Nothing
    selfGet  = ECall (EAttr (EName "self") "_get") []

    errArg  e = [ EAddr (EName "err") | e /= Nothing ]
    errDecl e = [ SCdef ec "err" (Just (EName (okMember model ec))) | Just ec <- [e] ]
    errChk  e = [ SIf (EName "err") [ SRaise (raiseExc ec "err") ] | Just ec <- [e] ]

    getter
        | asProp =
            [ (defFunc (propPyName prop) [selfP] (pyType model ty))
                { fnDecorators = ["property"], fnBody = readBody } ]
        | otherwise =
            [ (defFunc (flatPrefix h res <> "get_" <> propName prop)
                       (selfP : idParams) (pyType model ty))
                { fnBody = readBody } ]

    setter = case propWrite prop of
        Nothing          -> []
        Just fn | asProp ->
            [ (defFunc (propPyName prop) [selfP, valueP] "None")
                { fnDecorators = [propPyName prop <> ".setter"]
                , fnBody = writeBody fn } ]
        Just fn ->
            [ (defFunc (flatPrefix h res <> "set_" <> propName prop)
                       (selfP : idParams ++ [valueP]) "None")
                { fnBody = writeBody fn } ]
    valueP = Param (propName prop) (pyType model ty) Nothing

    readBody = case (propRead prop, isBuf) of
        (Nothing, _)     -> [ SPass ]
        (Just fn, False) | ty == SString, getterNullable ->
            [ SCdef (cRetText model ty) "res" (Just (ECall (EName fn) (selfGet : idArgs)))
            , SReturn (Just (ECond (ERaw "res is not NULL") (wrapResult model ty (EName "res")) (EStr "")))
            ]
        (Just fn, False) -> case rErr of
            Nothing ->
                [ SReturn (Just (wrapResult model ty
                    (ECall (EName fn) (selfGet : idArgs)))) ]
            Just _ ->
                errDecl rErr
                ++ [ SCdef (cType model ty) "res"
                       (Just (ECall (EName fn) (selfGet : idArgs ++ errArg rErr))) ]
                ++ errChk rErr
                ++ [ SReturn (Just (wrapResult model ty (EName "res"))) ]
        (Just fn, True) | getterReturnsPtr ->
            [ SReturn (Just (ESlice (ECall (EName fn) (selfGet : idArgs))
                                    Nothing (Just sizeExpr))) ]
        (Just fn, True) ->
            errDecl rErr
            ++ [ SCdef "size_t" "size" (Just sizeExpr) ]
            ++ errChk rErr
            ++ [ SCdef (bufElemC <> "*") "buf"
                   (Just (ECast (bufElemC <> "*") (ECall (EName "malloc")
                       [EBinOp "*" (EName "size") (ECall (EName "sizeof") [EName bufElemC])])))
               , STryFinally
                   ( SExpr (ECall (EName fn) (selfGet : idArgs ++ [EName "buf"] ++ errArg rErr))
                     : errChk rErr
                     ++ [ SReturn (Just bufResult) ] )
                   [ SExpr (ECall (EName "free") [EName "buf"]) ] ]
      where
        (bufElemC, bufIsList) = case ty of
            SList el          -> (cType model el, True)
            SFixedList el _ _ -> (cType model el, True)
            _                 -> ("uint8_t", False)
        bufResult | bufIsList = ERaw "[buf[i] for i in range(size)]"
                  | otherwise = ESlice (EName "buf") Nothing (Just (EName "size"))
        -- Only a variable-length buffer needs its size function.
        sizeExpr = case ty of
            SFixedBytes s _ -> ERaw (translateSizer s)
            _ -> case propSize prop of
                Just sz -> ECall (EName sz) (selfGet : idArgs ++ errArg rErr)
                Nothing -> EInt 0

    writeBody fn
        -- Make C copy the strings and bytes first (Python frees its buffer after
        -- this).
        | borrows, Just owned <- ownedSetter =
            [ SExpr (ECall (EName owned) [selfGet, EName "True"])
            , SIf (ENot call) [SRaise (ECall (EName "MemoryError") [])]
            ]
        | otherwise = errDecl wErr ++ [SExpr call] ++ errChk wErr
      where
        call = ECall (EName fn) (selfGet : idArgs ++ valueArgs ++ errArg wErr)
        borrows = ty `elem` [SString, SBytes] && setterFails fn
        setterFails f = maybe False ((== SBool) . output) (List.find ((== f) . methodName) (methods res))
        ownedSetter = List.find ((== "experimental_owned_data") . propName) (properties res) >>= propWrite
        valueArgs
            | isBuf     = [ EName (propName prop)
                          , ECall (EName "len") [EName (propName prop)] ]
            | otherwise = [ coerceArg model ty (EName (propName prop)) ]

propPyName :: SProperty -> Text
propPyName prop = fromMaybe nm (Text.stripPrefix "self_" nm)
  where nm = propName prop

wrapResult :: SemanticModel -> SType -> Expr -> Expr
wrapResult model ty e = case norm model ty of
    SEnum n -> ECall (EName (enumCName model n)) [e]
    SString -> ECall (EAttr e "decode") [EStr "utf-8"]
    _       -> e

coerceArg :: SemanticModel -> SType -> Expr -> Expr
coerceArg model ty e = case norm model ty of
    SString        -> ECall (EAttr e "encode") [EStr "utf-8"]
    SHandle "void" -> e
    -- Not @_get()@: calling a @cdef@ method across modules trips UBSan's
    -- @-fsanitize=function@ (SIGILL). A freed handle's NULL is a C error.
    SHandle _      -> ECond e (EAttr e "_ptr") (EName "NULL")
    _              -> e

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

-- | @handle_*@ matches the C callback, size parameters and all.
-- @py_handle_*@ takes the high-level ones.
eventTrampolines :: SemanticModel -> SResource -> SEvent -> [Func]
eventTrampolines model h ev =
    [ (sigFunc ("py_handle_" <> eventName ev)
               (Param "self" (cName h <> "_Ptr") Nothing : pyParams) "void" True)
        { fnParamStyle = PyStyle
        , fnBody = [ SExpr (ECall (EAttr (EName "self") ("handle_" <> eventName ev))
                                  [ EName (paramName p) | p <- eventParams ev ]) ] }
    , (sigFunc ("handle_" <> eventName ev) cParams "void" True)
        { fnBody = [ SExpr (ECall (EName ("py_handle_" <> eventName ev))
                       (ECast (cName h <> "_Ptr") (EName "user_data")
                        : map eventArg (eventParams ev))) ] }
    ]
  where
    pyParams = map (pyParam model) (eventParams ev)
    cParams = case List.find ((== cCallback ev) . cbCName) (callbacks model) of
        Just cb -> expandedCbParams model (cbParams cb)
        Nothing ->
            cParam model T.MutableThis (SHandle (resourceName h)) (Text.toLower (resourceName h))
            : expandedCbParams model (eventParams ev)
            ++ [ Param "user_data" "void*" Nothing ]
    eventArg p = case norm model (paramType p) of
        SBytes          -> sliceLen p
        SFixedBytes sz _ -> ESlice (EName (paramName p)) Nothing
                                   (Just (ERaw (translateSizer sz)))
        t | Just (el, sz) <- numericArray model t -> ERaw
              ( "array(\"" <> arrayTypecode el <> "\", ["
                <> paramName p <> "[i] for i in range(" <> translateSizer sz <> ")])" )
        _ -> EName (paramName p)
    sliceLen p = ESlice (EName (paramName p)) Nothing
                        (Just (EName (paramName p <> "_length")))

installHandlers :: SemanticModel -> SResource -> [SEvent] -> Func
installHandlers _model h evs =
    (sigFunc "install_handlers"
             [ Param "self" (cName h <> "_Ptr") Nothing, Param "ptr" (cName h <> "*") Nothing ]
             "void" False)
    { fnBody =
        [ SExpr (ECall (EName (cPrefix h <> "callback_" <> eventName ev))
                       ( [ EName "ptr", EName ("handle_" <> eventName ev) ]
                         ++ [ ECast "void*" (EName "self") | eventHasUserData ev ] ))
        | ev <- evs ]
    }

handleStub :: SemanticModel -> SEvent -> Func
handleStub model ev =
    (defFunc ("handle_" <> eventName ev)
             (Param "self" "" Nothing : map (pyParam model) (eventParams ev))
             "None")
    { fnBody = [ SPass ] }

--------------------------------------------------------------------------------
-- Module-level constants
--------------------------------------------------------------------------------

-- | The root library's version, if it has @lib_version_major@ and so on,
-- and the sizes and limits its functions give.
moduleConsts :: Text -> [SResource] -> [Item]
moduleConsts root handles =
    [ RawItem ("VERSION: str = \"%d.%d.%d\" % (" <> Text.intercalate ", " [ v <> "()" | v <- versionParts ] <> ")")
    | hasVersion ]
    ++ [ Assign (Text.toUpper (stripRoot (methodName m))) "int"
                (ECall (EName (methodName m)) [])
       | m <- consts, not (isVersionPart m) ]
  where
    consts =
        [ m | h <- handles, m <- methods h
        , methodRole m == StaticRole, null (inputs m), isUIntOut (output m) ]
    versionParts = [ root <> "_version_" <> p | p <- ["major", "minor", "patch"] ]
    hasVersion = any ((== root <> "_version_major") . methodName) consts
    isVersionPart m = methodName m `elem` versionParts
    stripRoot n = fromMaybe n (Text.stripPrefix (root <> "_") n)
    isUIntOut = \case SUInt _ -> True; _ -> False

--------------------------------------------------------------------------------
-- Method body shapes
--------------------------------------------------------------------------------

callBody :: SemanticModel -> [SResource] -> SResource -> SMethod -> [Stmt]
callBody model allRes res m = checkLenStmts model m ++ viewDecls ++ body
  where
    body | hasBuffer = bufferShape
         | otherwise = case methodErrorType m of
             Just errC -> scalarErrShape errC
             Nothing   -> noErrShape
    viewDecls =
        [ SCdef ("const " <> cType model el <> "[:]") (paramName p <> "_arr")
                (Just (EName (paramName p)))
        | p <- inputs m, Just (el, _) <- [numericArray model (paramType p)] ]
    cm        = cmOf m
    cfunc     = cFunctionName cm
    hasBuffer = any isBufPtr (argMapping cm)
    isBufPtr (BufferPtr _) = True
    isBufPtr _             = False

    sizeInit = case norm model (output m) of
        SFixedBytes s _ -> ERaw (translateSizer s)
        _ -> case cSizeFunctionName cm of
            Just sz -> ECall (EName sz) sizeArgs
            Nothing -> EInt 0
    sizeArgs =
        [ e | (src, e) <- zip (argMapping cm) (genCallArgs model allRes res cm "buf" "err" False)
            , structuralArg src ]

    args v = genCallArgs model allRes res cm "buf" v False

    (bufElemC, bufIsList) = case norm model (output m) of
        SList el          -> (cType model el, True)
        SFixedList el _ _ -> (cType model el, True)
        _                 -> ("uint8_t", False)
    bufResult | bufIsList = ERaw "[buf[i] for i in range(size)]"
              | otherwise = ESlice (EName "buf") Nothing (Just (EName "size"))

    bufferShape =
        [ SCdef errC "err" (Just (EName (okMember model errC))) | Just errC <- [methodErrorType m] ]
        ++ [ SCdef "size_t" "size" (Just sizeInit)
           , SCdef (bufElemC <> "*") "buf"
               (Just (ECast (bufElemC <> "*") (ECall (EName "malloc")
                   [EBinOp "*" (EName "size") (ECall (EName "sizeof") [EName bufElemC])])))
           , STryFinally
               ( SExpr (ECall (EName cfunc) (args "err"))
                 : [ SIf (EName "err") [ SRaise (raiseExc errC "err") ]
                   | Just errC <- [methodErrorType m] ]
                 ++ [ SReturn (Just bufResult) ] )
               [ SExpr (ECall (EName "free") [EName "buf"]) ]
           ]

    scalarErrShape errC = case norm model (output m) of
        SVoid ->
            [ SCdef errC "err" (Just (EName (okMember model errC)))
            , SExpr (ECall (EName cfunc) (args "err"))
            , SIf (EName "err") [ SRaise (raiseExc errC "err") ]
            ]
        _ ->
            [ SCdef errC "err" (Just (EName (okMember model errC)))
            , SCdef (cType model (output m)) "res" (Just (ECall (EName cfunc) (args "err")))
            , SIf (EName "err") [ SRaise (raiseExc errC "err") ]
            , SReturn (Just (wrapResult model (output m) (EName "res")))
            ]

    noErrShape =
        let call = ECall (EName cfunc) (args "err")
        in case norm model (output m) of
            SVoid -> [ SExpr call ]
            _     -> [ SReturn (Just (wrapResult model (output m) call)) ]

-- | In a constructor the parent handle ('PathObject') comes from a
-- parameter, not @self@.
genCallArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> Text -> Text -> Bool -> [Expr]
genCallArgs model allRes res cm bufVar errVar isCtor =
    snd (List.mapAccumL step Nothing (argMapping cm))
  where
    semP = cSemParams cm
    step prev = \case
        ThisObject _   -> (prev, ECall (EAttr (EName "self") "_get") [])
        PathObject n _
            -- Not @_get()@: see 'coerceArg'.
            | isCtor    -> let nm = Text.toLower (I.resolvePathTarget allRes res n)
                           in (prev, ECond (EName nm)
                                           (EAttr (EName nm) "_ptr")
                                           (EName "NULL"))
            | otherwise -> (prev, ECall (EAttr (EName "self") "_get") [])
        PathId n       -> let tgt = I.resolvePathTarget allRes res n
                          in (prev, EName (Text.toLower tgt <> "_number"))
        SemanticArg n
            | n < length semP ->
                let p = semP !! n
                    e = case numericArray model (paramType p) of
                            Just _  -> EAddr (EIndex (EName (paramName p <> "_arr")) (EInt 0))
                            Nothing
                                | optionalBytes model p ->
                                    ECond (ERaw (paramName p <> " is not None"))
                                          (ECast "const uint8_t*" (EName (paramName p)))
                                          (EName "NULL")
                                | otherwise -> coerceArg model (paramType p) (EName (paramName p))
                in (Just (paramName p), e)
            | otherwise -> (prev, EName "None")
        BufferSize     -> (prev, ECall (EName "len") [EName (fromMaybe "data" prev)])
        ErrorPtr       -> (prev, EAddr (EName errVar))
        BufferPtr _    -> (prev, EName bufVar)
        UserData       -> (prev, ECast "void*" (EName "self"))
        Constant v     -> (prev, EInt (fromIntegral v))

raiseExc :: Text -> Text -> Expr
raiseExc errC var = ECall (EName "ApiException") [ECall (EName errC) [EName var]]

checkLenStmts :: SemanticModel -> SMethod -> [Stmt]
checkLenStmts model m =
    [ if optionalBytes model p then SIf (ERaw (paramName p <> " is not None")) [check] else check
    | p <- inputs m
    , sz <- case norm model (paramType p) of
        SFixedBytes sz _  -> [sz]
        SFixedList _ sz _ -> [sz]
        _                 -> []
    , let check = SExpr (ECall (EAttr (EName "common") "check_len")
                                [ EStr (paramName p), EName (paramName p), ERaw (translateSizer sz) ])
    ]

structuralArg :: CArgSource -> Bool
structuralArg ThisObject{} = True
structuralArg PathObject{} = True
structuralArg PathId{}     = True
structuralArg ErrorPtr     = True
structuralArg _            = False

--------------------------------------------------------------------------------
-- Naming & misc helpers
--------------------------------------------------------------------------------

defFunc :: Text -> [Param] -> Text -> Func
defFunc name params ret = Func
    { fnKind = KindDef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = PyStyle, fnExcept = False, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

pyParam :: SemanticModel -> SParameter -> Param
pyParam model p
    | optionalBytes model p = Param (paramName p) "Optional[bytes]" Nothing
    | otherwise = Param (paramName p) (pyType model (paramType p)) Nothing

-- | A byte array parameter that may be NULL: Python's @None@.
optionalBytes :: SemanticModel -> SParameter -> Bool
optionalBytes model p = paramNullable p && case norm model (paramType p) of
    SBytes        -> True
    SFixedBytes{} -> True
    _             -> False

shortName :: SResource -> SMethod -> Text
shortName r m = fromMaybe (methodName m) (Text.stripPrefix (cPrefix r) (methodName m))

flatName :: SResource -> SResource -> SMethod -> Text
flatName h _res m =
    -- No @self_@ on a top-level @tox_self_*@: set_typing, not self_set_typing.
    let n = fromMaybe (methodName m) (Text.stripPrefix (cPrefix h) (methodName m))
    in fromMaybe n (Text.stripPrefix "self_" n)

flatPrefix :: SResource -> SResource -> Text
flatPrefix h res =
    fromMaybe "" (Text.stripPrefix (cPrefix h) (cPrefix res))

translateSizer :: Text -> Text
translateSizer t = maybe (error ("translateSizer: can't parse " <> Text.unpack t)) expr (parseSizeExpr t)
  where
    expr e = case e of
        SizeNum n -> Text.pack (show n)
        SizeVar v
            | isUpperConst v           -> Text.toLower v <> "()"
            | "_len" `Text.isSuffixOf` v -> "len(" <> Text.dropEnd 4 v <> ")"
            | otherwise                -> v
        -- C checks the size, but a negative one would raise before C sees it.
        SizeOp '-' a b -> "max(0, " <> operand a <> " - " <> operand b <> ")"
        -- C's @/@ is Python's @//@ here.
        SizeOp '/' a b -> operand a <> " // " <> operand b
        SizeOp op a b  -> operand a <> " " <> Text.singleton op <> " " <> operand b
        SizeCall f as  -> f <> "(" <> Text.intercalate ", " (map expr as) <> ")"
    operand x = case x of
        SizeOp '-' _ _ -> expr x
        SizeOp{}       -> "(" <> expr x <> ")"
        _              -> expr x
    isUpperConst t' =
        not (Text.null t')
        && Text.any isUpper t'
        && Text.all (\c -> isUpper c || isDigit c || c == '_') t'
