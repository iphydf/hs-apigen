{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cython binding generator.
--
-- Produces @.pxd@ (C declarations) and @.pyx@ (implementation) files for the
-- @py_toxcore_c@ packages (@toxcore@, @toxav@, @toxencryptsave@) from the
-- semantic model.
module Apigen.Language.Python (generate) where

import qualified Apigen.Inference              as I
import           Apigen.Language.Python.AST
import qualified Apigen.Language.Python.Pretty as Pretty
import           Apigen.Semantic
import qualified Apigen.Types                  as T
import           Data.Char                     (isDigit, isUpper)
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

-- | A target Cython package.
data Pkg = Pkg
    { pkgBase   :: FilePath           -- ^ Path stem, e.g. @"toxcore/tox"@.
    , pkgHeader :: Text               -- ^ C header for the @cdef extern@ block.
    , pkgName   :: Text               -- ^ Package id ("toxcore"/"toxav"/...).
    }

packages :: [Pkg]
packages =
    [ Pkg "toxcore/tox"                   "tox/tox.h"             "toxcore"
    , Pkg "toxav/toxav"                   "tox/toxav.h"           "toxav"
    , Pkg "toxencryptsave/toxencryptsave" "tox/toxencryptsave.h"  "toxencryptsave"
    ]

-- | Which package a resource belongs to (or 'Nothing' to skip it).
resourcePkg :: SResource -> Maybe Text
resourcePkg r
    | "Event" `Text.isPrefixOf` resourceName r          = Nothing
    | resourceName r `elem` ["Events", "System", "Iterate_Options"] = Nothing
    | cName r == "ToxAV"                                = Just "toxav"
    | cName r == "Tox_Pass_Key"                         = Just "toxencryptsave"
    | otherwise                                         = Just "toxcore"

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

-- | Callback semantic name -> C typedef name.
callbackCName :: SemanticModel -> Text -> Text
callbackCName model n =
    maybe n cbCName
        (List.find (\c -> cbName c == n || cbCName c == n) (callbacks model))

-- | Normalise a type. Mistyped fixed-size byte buffers come through as
-- 'SEnum'/'SResourceId' with names that resolve to no enum/id-type; treat
-- those as fixed byte arrays.
norm :: SemanticModel -> SType -> SType
norm model = \case
    SEnum n       | resolvedEnum model n == Nothing -> SFixedBytes n False
    SResourceId n | resolvedId model n == Nothing   -> SFixedBytes n False
    t                                               -> t

-- | First (OK) member of an error enum, by its C name.
okMember :: SemanticModel -> Text -> Text
okMember model errC =
    case List.find ((== errC) . enumName) (enums model) of
        Just e | (m : _) <- enumMembers e -> fst m
        _                                 -> errC <> "_OK"

-- | The error enum of a property accessor, looked up by its C function name.
-- A property's getter, setter and size function can each carry a different
-- error enum, which 'SProperty' does not distinguish.
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
                                        (methodErrorType meth) (inputs meth) (output meth)

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

-- | A C return type. Unlike a parameter, an 'SString' return is @const char *@
-- (it points into static or library-owned storage).
cRetText :: SemanticModel -> SType -> Text
cRetText model t = case norm model t of
    SString -> "const char*"
    _       -> cType model t

-- | The C parameter list of a callback, with the @size_t@ length parameters
-- that the model drops after byte-array parameters restored.
--
-- A solitary byte pointer is a length-paired array (@const uint8_t *x,
-- size_t length@); a run of consecutive byte pointers is multi-plane data
-- (the @y@/@u@/@v@ video planes) with no per-plane length.
expandedCbParams :: SemanticModel -> [SParameter] -> [Param]
expandedCbParams model ps =
    concat (zipWith3 expand (Nothing : nbs) ps (drop 1 nbs ++ [Nothing]))
  where
    nbs = map Just ps
    expand mprev p mnext =
        cParam model (paramConstness p) (paramType p) (paramName p)
        : [ Param (paramName p <> "_length") "size_t" Nothing
          | cbParamHasLength (norm model (paramType p))
          , not (byteNeighbour mprev), not (byteNeighbour mnext) ]
    byteNeighbour = maybe False (cbParamHasLength . norm model . paramType)

-- | Whether a callback parameter is a (potentially) length-paired byte array.
cbParamHasLength :: SType -> Bool
cbParamHasLength SBytes  = True
cbParamHasLength SString = True
cbParamHasLength _       = False

-- | An event we can generate a trampoline for. Multi-plane byte data (video
-- frames) has no inferable per-plane length, so such events are skipped.
eventGeneratable :: SemanticModel -> SEvent -> Bool
eventGeneratable model ev =
    case List.find ((== cCallback ev) . cbCName) (callbacks model) of
        Nothing -> True
        Just cb -> not (byteRun (map (norm model . paramType) (cbParams cb)))
  where
    byteRun ts = or (zipWith (\a b -> cbParamHasLength a && cbParamHasLength b)
                             ts (drop 1 ts))

isPtrType :: SType -> Bool
isPtrType = \case
    SString -> True; SBytes -> True; SFixedBytes{} -> True
    SHandle{} -> True; SList{} -> True; SFixedList{} -> True
    SCallback{} -> True; _ -> False

-- | A C parameter (type carries @const@ where applicable).
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

-- | The handle resource a resource ultimately belongs to.
rootHandle :: SemanticModel -> [SResource] -> SResource -> SResource
rootHandle model allRes res = fromMaybe res (findRes model (I.findRoot allRes res))

-- | The ResId resources along a resource's hierarchy path (root-first).
pathIdResources :: SemanticModel -> [SResource] -> SResource -> [SResource]
pathIdResources model allRes res =
    [ r | nm <- I.getHierarchy allRes res, Just r <- [findRes model nm], isResId r ]

-- | The id parameter name for a ResId resource (e.g. @"friend_number"@).
idParamName :: SResource -> Text
idParamName r = Text.toLower (resourceName r) <> "_number"

-- | Resources whose methods/properties/events flatten into a handle's class:
-- the handle itself plus every ResId resource rooted at it.
membersOf :: SemanticModel -> [SResource] -> SResource -> [SResource]
membersOf model allRes h =
    h : [ r | r <- allRes, isResId r, I.findRoot allRes r == resourceName h ]

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
    -- toxav references the core Tox handle, its wrapper class and id types.
    ++ [ CImport "pytox.toxcore.tox"
            ("Tox" : "Tox_Ptr" : map idCName (idTypes model))
       | pkgName pkg == "toxav" ]
    ++ [ Extern (pkgHeader pkg) externDecls ]
    ++ map (pxdClass model allRes) (filter isHandle pkgRes)
  where
    allRes = resources model
    pkgCbs = [ cb | cb <- callbacks model, callbackInPkg pkg (cbCName cb) ]
    externDecls =
        [ ExEnum (enumName e) (map fst (enumMembers e))
        | e <- enumsFor model pkgRes pkgCbs ]
        ++ [ ExOpaque (cName r) | r <- pkgRes, isHandle r ]
        ++ [ ExAlias "uint32_t" (idCName it)
           | pkgName pkg == "toxcore", it <- idTypes model ]
        ++ [ ExCallback (cbCName cb) (expandedCbParams model (cbParams cb))
           | cb <- pkgCbs ]
        ++ map ExFunc (nubByName (concatMap (resExternFuncs model allRes) pkgRes))

-- | A resource's own methods, excluding cross-package leaks (e.g.
-- @toxav_get_tox@ attributed to @Tox@): a method must agree with its resource
-- on whether it is a @toxav_@ symbol.
ownMethods :: SResource -> [SMethod]
ownMethods r = filter ok (methods r)
  where
    rIsAv  = resourcePkg r == Just "toxav"
    ok m   = (("toxav_" `Text.isPrefixOf` methodName m) == rIsAv)
             && not (excluded m)
    -- The experimental events/dispatch API is not bound; the per-enum
    -- *_to_string helpers are not surfaced either.
    excluded m =
        any (`Text.isPrefixOf` methodName m) ["tox_events_", "tox_dispatch_"]
        || "_to_string" `Text.isSuffixOf` methodName m

nubByName :: [Func] -> [Func]
nubByName = List.nubBy (\a b -> fnName a == fnName b)

-- | Whether a callback typedef belongs to a package.
callbackInPkg :: Pkg -> Text -> Bool
callbackInPkg pkg name =
    pkgName pkg == (if "toxav" `Text.isPrefixOf` name then "toxav" else "toxcore")

-- | All @cdef@ function declarations contributed by one resource.
resExternFuncs :: SemanticModel -> [SResource] -> SResource -> [Func]
resExternFuncs model allRes r =
    -- Getter/setter functions are declared via 'propExterns' instead.
    map (methodExtern model allRes r)
        [ m | m <- ownMethods r, methodRole m `notElem` [GetterRole, SetterRole] ]
    ++ concatMap (propExterns model allRes r) (properties r)
    ++ map (eventExtern model r) (filter (eventGeneratable model) (events r))

-- | Enums referenced (by C name) by the given resources or callbacks.
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

-- | A @cdef <ret> name(args)@ extern declaration for a method.
methodExtern :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
methodExtern model allRes res meth =
    (externFunc (cFunctionName cm) (cRetText model (cReturnType cm))
                (reconArgs model allRes res cm bufElemC))
        -- A function that takes the user_data context (tox_iterate) runs
        -- Python callbacks, so it must be declared to propagate exceptions.
        { fnExcept = any isUserData (argMapping cm) }
  where
    cm = cmOf meth
    isUserData UserData = True
    isUserData _        = False
    bufElemC = case norm model (output meth) of
        SList el          -> cType model el
        SFixedList el _ _ -> cType model el
        _                 -> "uint8_t"

-- | Extern declarations for a property's accessor functions.
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

-- | The @tox_callback_<event>@ registrar declaration.
eventExtern :: SemanticModel -> SResource -> SEvent -> Func
eventExtern model res ev = externFunc
    (cPrefix res <> "callback_" <> eventName ev) "void"
    ( [ cParam model T.MutableThis (SHandle (resourceName res)) "self"
      , Param "callback" (cCallback ev <> "*") Nothing
      ]
      -- Some registrars (toxav) also take the user_data context pointer.
      ++ [ Param "user_data" "void*" Nothing | eventHasUserData ev ] )

externFunc :: Text -> Text -> [Param] -> Func
externFunc name ret params = Func
    { fnKind = KindCdef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = CStyle, fnExcept = False, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

-- | Reconstruct the C parameter list of a function from its argument mapping.
-- @bufElemC@ is the C element type of the output buffer, if any.
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

-- | Property accessor kinds.
data PropKind = PRead | PSize | PWrite

-- | Reconstruct the C parameter list of a property accessor by convention.
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
    -- The C element type of a buffer property (uint8_t, or a wider list element).
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

-- | Types returned by the size-then-fill buffer idiom. A @char *@ ('SString')
-- getter returns its pointer directly, so it is /not/ a buffer type here.
isBufferType :: SType -> Bool
isBufferType = \case
    SBytes -> True; SFixedBytes{} -> True
    SList{} -> True; SFixedList{} -> True; _ -> False

-- | A wide numeric element (16-bit or larger) — marshalled via @array.array@
-- rather than as @bytes@.
isNumericElem :: SType -> Bool
isNumericElem = \case
    SInt n  -> n > 8
    SUInt n -> n > 8
    _       -> False

-- | A numeric (non-byte) array: its element type and size expression.
numericArray :: SemanticModel -> SType -> Maybe (SType, Text)
numericArray model t = case norm model t of
    SFixedList el sz _ | isNumericElem el -> Just (el, sz)
    _                                     -> Nothing

-- | Element typecode for Python's @array.array@.
arrayTypecode :: SType -> Text
arrayTypecode = \case
    SInt 8  -> "b"; SUInt 8  -> "B"
    SInt 16 -> "h"; SUInt 16 -> "H"
    SInt 32 -> "i"; SUInt 32 -> "I"
    SInt 64 -> "q"; SUInt 64 -> "Q"
    _       -> "B"

-- | The @cdef class X_Ptr:@ declaration block for a @.pxd@.
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

-- | The parameters a constructor exposes: parent handles ('PathObject'), ids
-- and semantic inputs. Unlike a flattened sub-resource method, a handle
-- constructor receives its parent as an argument rather than via @self@.
-- | Constructor parameters. With @optional@ (the @__init__@ form) a handle
-- argument becomes @Optional[...] = None@; the bare @cdef@ helpers, whose
-- @.pxd@ declarations may not carry defaults, use plain typed parameters.
ctorParamsWith :: Bool -> SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
ctorParamsWith optional model allRes res cm = mapMaybe pick (argMapping cm)
  where
    semP = cSemParams cm
    pick = \case
        -- The parent handle of a handle constructor (e.g. toxav_new's Tox) is
        -- optional on __init__: passing None yields the C API's NULL error.
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
                -- A handle argument to a constructor (e.g. tox_new's options)
                -- is optional: the C API accepts NULL.
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

-- | The constructor used for @__init__@: prefer one whose C name matches the
-- resource's own prefix (e.g. @tox_new@ over @toxav_get_tox@).
primaryCtor :: SResource -> Maybe SMethod
primaryCtor r = case filter own cs ++ cs of
    (m : _) -> Just m
    []      -> Nothing
  where
    cs = ctorsOf r
    own m = cPrefix r `Text.isPrefixOf` methodName m

--------------------------------------------------------------------------------
-- .pyx generation
--------------------------------------------------------------------------------

genPyx :: SemanticModel -> Pkg -> [SResource] -> Module
genPyx model pkg pkgRes = Module cythonHeader $
    [ Import "array" ["array"]
    , Import "pytox" ["common"]
    , Import "types" ["TracebackType"]
    , Import "typing" ["Optional"]
    , Import "typing" ["TypeVar"]
    ]
    -- toxav constructors take a core Tox_Ptr.
    ++ [ CImport "pytox.toxcore.tox" ["Tox_Ptr"] | pkgName pkg == "toxav" ]
    ++ [ RawItem "T = TypeVar(\"T\")"
       , ClassDef (Class False "ApiException" ["common.ApiException"] [] [])
       ]
    ++ concatMap (pyxClass model allRes) handles
    ++ moduleConsts model handles
    ++ concatMap (map FuncDef . moduleFns model allRes) pkgRes
  where
    allRes  = resources model
    handles = filter isHandle pkgRes

-- | A @cdef class X_Ptr:@ implementation block, plus its trampoline @cdef:@
-- block (returned as separate top-level items).
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
    mems        = membersOf model allRes h
    evs         = filter (eventGeneratable model) (concatMap events mems)
    trampolines = concatMap (eventTrampolines model h) evs
    classFuncs  =
        [ getMethod h, delMethod, enterMethod ]
        ++ [ exitMethod model d | d <- take 1 dtors ]
        ++ [ ctorMethod model allRes h c | c <- ctorsOf h ]
        ++ initMethods model allRes h
        ++ concatMap (resMethods model allRes h) mems
        ++ [ handleStub model ev | ev <- evs ]
    dtors = [ m | m <- methods h, methodRole m == Destructor ]

-- | Methods/properties contributed by one (possibly sub-) resource.
resMethods :: SemanticModel -> [SResource] -> SResource -> SResource -> [Func]
resMethods model allRes h res =
    concatMap memberFunc
        [ m | m <- ownMethods res
            , methodRole m `elem` roles
            , not (touchesOpaque model m)
            , methodName m `notElem` accessorFns ]  -- exposed via a property
    ++ concatMap (propertyFuncs model allRes h res) (properties res)
  where
    -- For the handle itself, the constructor/destructor are __init__/__exit__;
    -- for a flattened sub-resource they become ordinary methods (friend_add,
    -- friend_delete, conference_new, ...).
    roles | resourceName res == resourceName h = [ActionRole]
          | otherwise = [Constructor, Destructor, ActionRole]
    accessorFns = mapMaybe id
        (concatMap (\p -> [propRead p, propWrite p, propSize p]) (properties res))

    memberFunc m
        | isNullaryGetter m =
            [ (defFunc (flatName h res m) [Param "self" "" Nothing]
                       (pyType model (output m)))
                { fnDecorators = ["property"]
                , fnBody = callBody model allRes res m } ]
        | otherwise = [ actionMethod model allRes h res m ]
    -- A no-argument, value-returning action (e.g. iteration_interval) reads
    -- naturally as a read-only property. A flattened sub-resource getter,
    -- though it has no semantic inputs, still takes an id (PathId) — exclude it.
    isNullaryGetter m =
        methodRole m == ActionRole
        && null (inputs m)
        && not (any isPathId (argMapping (cmOf m)))
        && norm model (output m) /= SVoid
        && methodErrorType m == Nothing
    isPathId PathId{} = True
    isPathId _        = False

-- | A method we cannot surface yet: it traffics in raw callbacks or @void *@.
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

-- | The @__init__@ for a handle class. With several (prefix-nested)
-- constructors it is overloaded — one signature with optional tail
-- parameters that dispatches on which were supplied.
initMethods :: SemanticModel -> [SResource] -> SResource -> [Func]
initMethods model allRes r =
    case List.sortOn (length . inputs) (ctorsOf r) of
        []         -> []
        [c]        -> [ initMethod model allRes r c ]
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

-- | An overloaded @__init__@ over a short and a longer constructor whose
-- parameters extend the short one's; dispatches on the first extra argument.
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

-- | Make a parameter optional with a @None@ default.
optionalParam :: Param -> Param
optionalParam (Param n t _) =
    Param n (if "Optional[" `Text.isPrefixOf` t then t else "Optional[" <> t <> "]")
            (Just "None")

-- | The @install_handlers@ call emitted at the end of a constructor with events.
installHandlersStmts :: SResource -> [Stmt]
installHandlersStmts r =
    [ SExpr (ECall (EName "install_handlers")
               [EName "self", ECall (EAttr (EName "self") "_get") []])
    | not (null (events r)) ]

--------------------------------------------------------------------------------
-- Action methods
--------------------------------------------------------------------------------

-- | An action / getter / setter method exposed as a @def@ on the handle class.
actionMethod :: SemanticModel -> [SResource] -> SResource -> SResource -> SMethod -> Func
actionMethod model allRes h res m = (defFunc (flatName h res m) params ret)
    { fnBody = callBody model allRes res m }
  where
    cm     = cmOf m
    params = Param "self" "" Nothing : callerParams model allRes res cm
    ret    = pyType model (output m)

-- | Module-level @def@s for static methods that take arguments.
moduleFns :: SemanticModel -> [SResource] -> SResource -> [Func]
moduleFns model allRes r =
    [ (defFunc (modName m) (callerParams model allRes r (cmOf m)) (pyType model (output m)))
        { fnBody = callBody model allRes r m }
    | m <- ownMethods r
    , methodRole m == StaticRole
    , not (null (inputs m))
    , not ("_to_string" `Text.isSuffixOf` methodName m)
    , not (touchesOpaque model m)
    ]
  where
    modName m = fromMaybe (methodName m) (Text.stripPrefix "tox_" (methodName m))

-- | The caller-facing (Python) parameters of a call: its path ids and inputs.
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

-- | Property types we cannot surface idiomatically yet (callbacks, raw handles).
badPropType :: SType -> Bool
badPropType = \case
    SCallback{} -> True
    SHandle{}   -> True
    _           -> False

-- | A property contributes a @\@property@ (on a handle resource, no path ids)
-- or a getter\/setter method pair (on a sub-resource).
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
    -- A buffer getter either fills a caller buffer (returning void/bool) or
    -- returns the data pointer directly (e.g. tox_options_get_savedata_data,
    -- which returns const uint8_t *).
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

    -- Error scaffolding for a given accessor's error enum.
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
        -- A fixed-size buffer knows its own length; only a variable-length
        -- one needs to call its size function.
        sizeExpr = case ty of
            SFixedBytes s _ -> ERaw (translateSizer s)
            _ -> case propSize prop of
                Just sz -> ECall (EName sz) (selfGet : idArgs ++ errArg rErr)
                Nothing -> EInt 0

    writeBody fn =
        errDecl wErr
        ++ [ SExpr (ECall (EName fn)
               (selfGet : idArgs ++ valueArgs ++ errArg wErr)) ]
        ++ errChk wErr
      where
        -- A byte/list buffer is passed as (data, length); a scalar as itself.
        valueArgs
            | isBuf     = [ EName (propName prop)
                          , ECall (EName "len") [EName (propName prop)] ]
            | otherwise = [ coerceArg model ty (EName (propName prop)) ]

propPyName :: SProperty -> Text
propPyName prop = fromMaybe nm (Text.stripPrefix "self_" nm)
  where nm = propName prop

-- | Wrap a C result for the high-level API (enum constructor, string decode).
wrapResult :: SemanticModel -> SType -> Expr -> Expr
wrapResult model ty e = case norm model ty of
    SEnum n -> ECall (EName (enumCName model n)) [e]
    SString -> ECall (EAttr e "decode") [EStr "utf-8"]
    _       -> e

-- | Coerce a high-level argument to what the C function expects.
coerceArg :: SemanticModel -> SType -> Expr -> Expr
coerceArg model ty e = case norm model ty of
    SString        -> ECall (EAttr e "encode") [EStr "utf-8"]
    SHandle "void" -> e
    -- A handle argument may be None; pass NULL through to C in that case.
    -- Read the raw @_ptr@ field rather than calling @_get()@: @_get()@ is a
    -- @cdef@ method, and a cross-module call to one is an indirect call that
    -- trips UBSan's @-fsanitize=function@ check (the per-module type hashes
    -- disagree) and traps with SIGILL. The @if e@ guard already covers None;
    -- a freed handle yields NULL, which the C function reports as an error.
    SHandle _      -> ECond e (EAttr e "_ptr") (EName "NULL")
    _              -> e

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

-- | The two trampoline @cdef@ functions for one event. The @handle_*@ function
-- matches the real C callback signature (size params and all); @py_handle_*@
-- exposes the high-level parameters.
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
    -- The C-ABI signature, from the callback table (size params restored).
    cParams = case List.find ((== cCallback ev) . cbCName) (callbacks model) of
        Just cb -> expandedCbParams model (cbParams cb)
        Nothing ->
            cParam model T.MutableThis (SHandle (resourceName h)) (Text.toLower (resourceName h))
            : expandedCbParams model (eventParams ev)
            ++ [ Param "user_data" "void*" Nothing ]
    -- How each high-level argument is recovered from the C parameters.
    eventArg p = case norm model (paramType p) of
        SBytes          -> sliceLen p
        SString         -> sliceLen p
        -- A fixed buffer (e.g. a video plane) is sliced by its size expression.
        SFixedBytes sz _ -> ESlice (EName (paramName p)) Nothing
                                   (Just (ERaw (translateSizer sz)))
        t | Just (el, sz) <- numericArray model t -> ERaw
              ( "array(\"" <> arrayTypecode el <> "\", ["
                <> paramName p <> "[i] for i in range(" <> translateSizer sz <> ")])" )
        _ -> EName (paramName p)
    sliceLen p = ESlice (EName (paramName p)) Nothing
                        (Just (EName (paramName p <> "_length")))

-- | The @install_handlers@ helper.
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

-- | The overridable @handle_<event>@ stub method.
handleStub :: SemanticModel -> SEvent -> Func
handleStub model ev =
    (defFunc ("handle_" <> eventName ev)
             (Param "self" "" Nothing : map (pyParam model) (eventParams ev))
             "None")
    { fnBody = [ SPass ] }

--------------------------------------------------------------------------------
-- Module-level constants
--------------------------------------------------------------------------------

moduleConsts :: SemanticModel -> [SResource] -> [Item]
moduleConsts model handles =
    [ RawItem ("VERSION: str = \"%d.%d.%d\" % (tox_version_major(), "
               <> "tox_version_minor(), tox_version_patch())")
    | hasVersion ]
    ++ [ Assign (Text.toUpper (stripTox (methodName m))) "int"
                (ECall (EName (methodName m)) [])
       | m <- consts, not (isVersionPart m) ]
  where
    consts =
        [ m | h <- handles, m <- methods h
        , methodRole m == StaticRole, null (inputs m), isUIntOut (output m) ]
    hasVersion = any ((== "tox_version_major") . methodName) consts
    isVersionPart m = methodName m `elem`
        ["tox_version_major", "tox_version_minor", "tox_version_patch"]
    stripTox n = fromMaybe n (Text.stripPrefix "tox_" n)
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
    -- Typed-memoryview locals for numeric-array inputs (e.g. audio pcm).
    viewDecls =
        [ SCdef ("const " <> cType model el <> "[:]") (paramName p <> "_arr")
                (Just (EName (paramName p)))
        | p <- inputs m, Just (el, _) <- [numericArray model (paramType p)] ]
    cm        = cmOf m
    cfunc     = cFunctionName cm
    hasBuffer = any isBufPtr (argMapping cm)
    isBufPtr (BufferPtr _) = True
    isBufPtr _             = False

    -- Buffer size: a fixed expression, or a call to the size function with the
    -- same structural arguments (this/parent/id/error) as the main call.
    sizeInit = case norm model (output m) of
        SFixedBytes s _ -> ERaw (translateSizer s)
        _ -> case cSizeFunctionName cm of
            Just sz -> ECall (EName sz) sizeArgs
            Nothing -> EInt 0
    sizeArgs =
        [ e | (src, e) <- zip (argMapping cm) (genCallArgs model allRes res cm "buf" "err" False)
            , structuralArg src ]

    args v = genCallArgs model allRes res cm "buf" v False

    -- A buffer is a byte array (returned as a slice) or a list of a wider
    -- element type (returned as a Python list).
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

-- | Python expressions for the C arguments of a call. In a constructor the
-- parent handle ('PathObject') comes from a parameter, not @self@.
genCallArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> Text -> Text -> Bool -> [Expr]
genCallArgs model allRes res cm bufVar errVar isCtor =
    snd (List.mapAccumL step Nothing (argMapping cm))
  where
    semP = cSemParams cm
    step prev = \case
        ThisObject _   -> (prev, ECall (EAttr (EName "self") "_get") [])
        PathObject n _
            -- Read the raw @_ptr@ field, not @_get()@: the parent handle is
            -- often from another extension module, and a cross-module call to
            -- the @cdef@ @_get()@ is an indirect call that trips UBSan's
            -- @-fsanitize=function@ check and traps with SIGILL. The @if nm@
            -- guard covers None; a freed handle yields NULL (a clean C error).
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
                            -- &<name>_arr[0] of the typed-memoryview local.
                            Just _  -> EAddr (EIndex (EName (paramName p <> "_arr")) (EInt 0))
                            Nothing -> coerceArg model (paramType p) (EName (paramName p))
                in (Just (paramName p), e)
            | otherwise -> (prev, EName "None")
        BufferSize     -> (prev, ECall (EName "len") [EName (fromMaybe "data" prev)])
        ErrorPtr       -> (prev, EAddr (EName errVar))
        BufferPtr _    -> (prev, EName bufVar)
        UserData       -> (prev, ECast "void*" (EName "self"))
        Constant v     -> (prev, EInt (fromIntegral v))

raiseExc :: Text -> Text -> Expr
raiseExc errC var = ECall (EName "ApiException") [ECall (EName errC) [EName var]]

-- | @common.check_len@ guards for a method's fixed-size byte arguments.
checkLenStmts :: SemanticModel -> SMethod -> [Stmt]
checkLenStmts model m =
    [ SExpr (ECall (EAttr (EName "common") "check_len")
               [ EStr (paramName p), EName (paramName p)
               , ERaw (translateSizer sz) ])
    | p <- inputs m, SFixedBytes sz _ <- [norm model (paramType p)] ]

-- | Argument-mapping entries a size function shares with the main call.
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
pyParam model p = Param (paramName p) (pyType model (paramType p)) Nothing

-- | A method's exposed name within its own resource's class.
shortName :: SResource -> SMethod -> Text
shortName r m = fromMaybe (methodName m) (Text.stripPrefix (cPrefix r) (methodName m))

-- | A method's exposed name when flattened onto handle @h@.
flatName :: SResource -> SResource -> SMethod -> Text
flatName h _res m =
    -- A top-level @tox_self_*@ operates on the handle itself, so the @self_@
    -- is redundant on the method name (set_typing, not self_set_typing).
    let n = fromMaybe (methodName m) (Text.stripPrefix (cPrefix h) (methodName m))
    in fromMaybe n (Text.stripPrefix "self_" n)

-- | Prefix for a sub-resource's flattened property accessors.
flatPrefix :: SResource -> SResource -> Text
flatPrefix h res =
    fromMaybe "" (Text.stripPrefix (cPrefix h) (cPrefix res))

-- | Translate a C buffer-size expression into a Python expression.
translateSizer :: Text -> Text
-- Buffer sizes are integer arithmetic; C's @/@ is integer division, so map it
-- to Python's @//@.
translateSizer = Text.replace "/" "//" . Text.unwords . map tok . Text.words
  where
    tok t
        | t `elem` ["+", "-", "*", "/", "(", ")"] = t
        | isUpperConst t                          = Text.toLower t <> "()"
        | "_len" `Text.isSuffixOf` t              = "len(" <> Text.dropEnd 4 t <> ")"
        | otherwise                               = t
    isUpperConst t =
        not (Text.null t)
        && Text.any isUpper t
        && Text.all (\c -> isUpper c || isDigit c || c == '_') t
