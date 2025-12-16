{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cython binding generator.
--
module Apigen.Language.Python (generate) where

import qualified Apigen.Inference              as I
import qualified Apigen.Knowledge              as K
import           Apigen.Language.Cpp.Names     (enumTypeName)
import           Apigen.Language.Python.AST
import qualified Apigen.Language.Python.Pretty as Pretty
import           Apigen.Model                  (findEnum, headerOf, isEnumNamer,
                                                membersOf)
import           Apigen.Namespace              (namespaces, rootNamespace)
import           Apigen.Semantic
import           Apigen.SizeExpr               (SizeExpr (..), sizeExprVars)
import qualified Apigen.Types                  as T
import           Control.Applicative           ((<|>))
import           Data.Char                     (isAlphaNum, isDigit, isUpper)
import           Data.Function                 (on)
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, isJust, isNothing,
                                                listToMaybe, mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           System.FilePath               (takeBaseName, takeDirectory,
                                                takeFileName, (</>))

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

data Pkg = Pkg
    { pkgBase   :: FilePath           -- ^ Path stem: @toxcore/tox@.
    , pkgHeader :: Text               -- ^ C header for the @cdef extern@ block.
    , pkgName   :: Text               -- ^ Package id: @toxcore@.
    , pkgFiles  :: [Text]             -- ^ The headers it binds.
    }

-- | A package per library: its handle's header, and the others in its
-- directory but the events'.
packages :: SemanticModel -> [Pkg]
packages model = List.nubBy ((==) `on` pkgName)
    [ Pkg
        { pkgBase   = Text.unpack dir </> takeBaseName (Text.unpack (resourceFile r))
        , pkgHeader = includeDir (targets model) <> "/" <> headerOf (resourceFile r)
        , pkgName   = dir
        , pkgFiles  = List.nub
            [ headerOf f
            | x <- resources model, let f = resourceFile x, dirOf f == dir, f `notElem` eventHeaders ]
        }
    | kind <- [RootHandle, LibraryHandle, StandaloneHandle]
    , r <- K.handlesOfKind model kind
    , let dir = dirOf (resourceFile r)
    ]
  where
    dirOf = Text.pack . takeFileName . takeDirectory . Text.unpack
    eventHeaders = [ resourceFile x | x <- resources model, K.isView model x || K.isViewList model x ]

-- | Whether a package has another library's handle.
holdsLibrary :: SemanticModel -> [SResource] -> Bool
holdsLibrary model = any ((== LibraryHandle) . K.resourceKind model)

-- | The Python module of the root's package: @pytox.toxcore.tox@.
rootModule :: SemanticModel -> Text
rootModule model = case [ p | r <- resources model, isRoot r, p <- packages model, headerOf (resourceFile r) `elem` pkgFiles p ] of
    p : _ -> pythonPackage (targets model) <> "." <> Text.replace "/" "." (Text.pack (pkgBase p))
    []    -> error "rootModule: no package has the root"

filePkg :: SemanticModel -> Text -> Maybe Text
filePkg model file = pkgName <$> List.find ((headerOf file `elem`) . pkgFiles) (packages model)

resourcePkg :: SemanticModel -> SResource -> Maybe Text
resourcePkg model = filePkg model . resourceFile

generate :: SemanticModel -> [(FilePath, Text)]
generate model = concatMap (genPkg model) (packages model)

genPkg :: SemanticModel -> Pkg -> [(FilePath, Text)]
genPkg model pkg =
    [ (pkgBase pkg ++ ".pxd", Pretty.render (genPxd model pkg pkgRes))
    , (pkgBase pkg ++ ".pyx", Pretty.render pyx)
    , (pkgBase pkg ++ ".pyi", genStub model pkg pkgRes pyx)
    ]
    ++ [ (takeDirectory (pkgBase pkg) </> "events.py", genEvents model evs) | not (null evs) ]
  where
    pyx = withoutUnusedOptional (genPyx model pkg pkgRes)
    evs = pkgEvents model pkgRes
    pkgRes = filter ((== Just (pkgName pkg)) . resourcePkg model) (resources model)

-- | Only a module with an @Optional@ parameter imports it.
withoutUnusedOptional :: Module -> Module
withoutUnusedOptional (Module h items)
    | "Optional[" `Text.isInfixOf` Pretty.render (Module h rest) = Module h items
    | otherwise = Module h rest
  where
    rest = filter (not . isOptionalImport) items
    isOptionalImport = \case
        Import "typing" ["Optional"] -> True
        _                            -> False

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

-- | A handle's Cython struct name: its C name, unless its Python class has
-- that name: @CTox@ for @Tox@.
resCName :: SemanticModel -> Text -> Text
resCName model n = maybe n structName (List.find ((== n) . resourceName) (resources model))

structName :: SResource -> Text
structName r
    | pyClass r == cName r = "C" <> cName r
    | otherwise            = cName r

-- | A handle's Python class: @Tox@, @Options@, @ToxAv@, @PassKey@.
pyClass :: SResource -> Text
pyClass = pascalName . resourceName

-- | An id type's Cython name, C's: @Tox_Friend_Number@.
idCNameOf :: SemanticModel -> Text -> Text
idCNameOf model n = maybe n idCName (List.find ((== n) . idName) (idTypes model))

-- | The C name of a resource's id type: @Tox_Friend_Number@ for @Friend@,
-- @Tox_Group_Peer_Id@ for @Group_Peer@.
resourceIdCName :: SemanticModel -> Text -> Text
resourceIdCName model r = maybe (r <> "_Number") idCName (K.resourceIdType model r)

-- | An id type's Python name, a NewType of int: @FriendNumber@.
idPyName :: SemanticModel -> Text -> Text
idPyName model n = maybe n (pascalName . idName) (List.find ((== n) . idName) (idTypes model))

pascalName :: Text -> Text
pascalName = Text.concat . map capitalise . Text.splitOn "_"
  where
    capitalise w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

findRes :: SemanticModel -> Text -> Maybe SResource
findRes model n = List.find ((== n) . resourceName) (resources model)

callbackCName :: SemanticModel -> Text -> Text
callbackCName model n =
    maybe n cbCName
        (List.find (\c -> cbName c == n || cbCName c == n) (callbacks model))

-- | What a method gives back: nothing for a @bool@ that only says whether its
-- error is set, as an error raises.
result :: SMethod -> SType
result m
    | methodResultStrategy m == IgnoreReturn = SVoid
    | otherwise                              = output m

-- | A buffer of @size@ elements, raising if there's no memory for it. At
-- least one, as @malloc(0)@ may give @NULL@.
allocBuffer :: Text -> [Stmt]
allocBuffer elemC =
    [ SCdef (elemC <> "*") "buf"
        (Just (ECast (elemC <> "*") (ECall (EName "malloc")
            [EBinOp "*" (ECall (EName "max") [EName "size", EInt 1]) (ECall (EName "sizeof") [EName elemC])])))
    , SIf (EBinOp "is" (EName "buf") (EName "NULL")) [SRaise (ECall (EName "MemoryError") [])]
    ]

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
cType model ty = case ty of
    SScalar _ b      -> cType model b
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
pyType model ty = case ty of
    SScalar _ b      -> pyType model b
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
    SEnum n          -> pyEnumName model (enumCName model n)
    SHandle "void"   -> "object"
    SHandle n        -> maybe n pyClass (findRes model n)
    SCallback _      -> "object"
    SResourceId n    -> idCNameOf model n
    SList t          -> "list[" <> pyType model t <> "]"

-- | Unlike a parameter, a returned string or bytes is @const@.
cRetText :: SemanticModel -> SType -> Text
cRetText model t = case t of
    SString -> "const char*"
    SBytes  -> "const uint8_t*"
    _       -> cType model t

-- | Puts back the @size_t@ lengths the model drops. A sized array, such as
-- a video plane, has its size expression instead.
expandedCbParams :: SemanticModel -> [SParameter] -> [Param]
expandedCbParams model = concatMap expand
  where
    expand p =
        cParam model (paramConstness p) (paramType p) (paramName p)
        : [ Param (paramName p <> "_length") "size_t" Nothing | (paramType p) == SBytes ]

isPtrType :: SType -> Bool
isPtrType = \case
    SString -> True; SBytes -> True; SFixedBytes{} -> True
    SHandle{} -> True; SList{} -> True; SFixedList{} -> True
    SCallback{} -> True; _ -> False

cParam :: SemanticModel -> T.Constness -> SType -> Text -> Param
cParam model cns ty name = Param name tyText Nothing
  where
    base = cType model ty
    -- Byte-array parameters are always inputs, hence always const.
    constByte = case ty of
        SBytes -> True; SString -> True; SFixedBytes{} -> True; _ -> False
    tyText | constByte                            = "const " <> base
           | cns == T.ConstThis && isPtrType ty  = "const " <> base
           | otherwise                            = base

--------------------------------------------------------------------------------
-- Hierarchy helpers
--------------------------------------------------------------------------------

rootHandle :: SemanticModel -> [SResource] -> SResource -> SResource
rootHandle model allRes res = fromMaybe res (findRes model (I.findRoot allRes res))

pathIdResources :: SemanticModel -> [SResource] -> SResource -> [SResource]
pathIdResources model allRes res =
    [ r | nm <- I.getHierarchy allRes res, Just r <- [findRes model nm], isResId r ]

-- | The parameter for a resource's id, named after its type: @friend_number@,
-- @group_peer_id@.
idParamName :: SemanticModel -> SResource -> Text
idParamName model = idParamName' model . resourceName

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
    ++ [ CImport "cpython.pythread" ["PyThread_type_lock"] | any (isShared allRes) pkgRes ]
    -- Another library's package uses the root's handle, its wrapper class
    -- and the id types.
    ++ [ CImport (rootModule model) (structName root : pyClass root : map idCName (idTypes model))
       | holdsLibrary model pkgRes, root <- filter isRoot allRes ]
    ++ [ Extern (pkgHeader pkg) externDecls ]
    ++ map (pxdClass model allRes) (filter isHandle pkgRes)
  where
    allRes = resources model
    pkgCbs = [ cb | cb <- callbacks model, filePkg model (cbFile cb) == Just (pkgName pkg) ]
    externDecls =
        [ ExEnum (enumName e) (map enumMemberCName (enumMembers e))
        | e <- enumsFor model pkgRes pkgCbs ]
        ++ [ ExOpaque (structName r) (cName r) | r <- pkgRes, isHandle r ]
        ++ [ ExAlias "uint32_t" (idCName it) (idCName it)
           | any isRoot pkgRes, it <- idTypes model ]
        ++ [ ExCallback (cbCName cb) (expandedCbParams model (cbParams cb))
           | cb <- pkgCbs ]
        ++ map ExFunc (nubByName (concatMap (resExternFuncs model allRes) pkgRes))

-- | C functions neither raise nor need the GIL. A callback can't raise
-- either: the trampolines keep what Python raises for the loop to raise.
externFunc :: Text -> Text -> [Param] -> Func
externFunc name ret params = Func
    { fnKind = KindCdef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = CStyle, fnExcept = NoExceptNogil, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

-- | Methods its package binds: not the old group calls on @Tox@,
-- @tox_events_init@ or @*_to_string@.
ownMethods :: SemanticModel -> SResource -> [SMethod]
ownMethods model r = filter ok (methods r)
  where
    ok m = filePkg model (methodFile m) == resourcePkg model r
           && not (isEnumNamer m)

nubByName :: [Func] -> [Func]
nubByName = List.nubBy (\a b -> fnName a == fnName b)

resExternFuncs :: SemanticModel -> [SResource] -> SResource -> [Func]
resExternFuncs model allRes r =
    -- Getter/setter functions are declared via 'propExterns' instead.
    map (methodExtern model allRes r)
        [ m | m <- ownMethods model r, methodRole m `notElem` [GetterRole, SetterRole] ]
    ++ concatMap (propExterns model allRes r) (properties r)
    ++ map (eventExtern model r) (events r)

-- | The enums the package's functions use, and the others its headers
-- declare (the call states a callback gives as a number, file kinds), but
-- not errors of functions it doesn't bind.
enumsFor :: SemanticModel -> [SResource] -> [SCallbackTypeModel] -> [SEnumModel]
enumsFor model pkgRes pkgCbs = filter used (enums model)
  where
    used e = enumName e `elem` refs
        || (enumKind e /= ErrorEnum && filePkg model (enumFile e) == (resourcePkg model =<< listToMaybe pkgRes))
    refs = List.nub $ concatMap resRefs pkgRes ++ concatMap cbRefs pkgCbs
    cbRefs cb = concatMap (typeEnums . paramType) (cbParams cb)
    resRefs r =
        mapMaybe methodErrorType (ownMethods model r)
        ++ mapMaybe propErrorType (properties r)
        ++ concatMap (typeEnums . output) (ownMethods model r)
        ++ concatMap (concatMap (typeEnums . paramType) . inputs) (ownMethods model r)
        ++ concatMap (typeEnums . propType) (properties r)
        ++ concatMap (concatMap (typeEnums . paramType) . eventParams) (events r)
    typeEnums = \case
        SEnum n -> [enumCName model n]
        _       -> []

methodExtern :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
methodExtern model allRes res meth =
    externFunc (cFunctionName cm) (cRetText model (cReturnType cm))
               (reconArgs model allRes res cm bufElemC)
  where
    cm = cmOf meth
    bufElemC = case output meth of
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
    (eventRegistrar ev) "void"
    ( [ cParam model T.MutableThis (SHandle (resourceName res)) "self"
      , Param "callback" (cCallback ev <> "*") Nothing
      ]
      ++ [ Param "user_data" "void*" Nothing | eventHasUserData ev ] )

-- | @bufElemC@ is the C element type of the output buffer, if any. The
-- buffer has its C name (@tox_hash@'s @hash@).
reconArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> Text -> [Param]
reconArgs model allRes res cm bufElemC =
    snd (List.mapAccumL step Nothing (argMapping cm))
  where
    semP  = cSemParams cm
    bufName = fromMaybe "data" (lookup True (zip (map isBufPtr (argMapping cm)) (cArgNames cm)))
    isBufPtr BufferPtr{} = True
    isBufPtr _           = False
    errTy = fromMaybe "void" (cErrorType cm)
    step prev = \case
        ThisObject cns ->
            (prev, cParam model cns (SHandle (resourceName res)) "self")
        PathObject n cns ->
            let tgt = I.resolvePathTarget allRes res n
            in (prev, cParam model cns (SHandle tgt) (Text.toLower tgt))
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in (prev, Param (idParamName' model tgt)
                            (resourceIdCName model tgt) Nothing)
        SemanticArg n | n < length semP ->
            let p = semP !! n
            in (Just (paramName p), cParam model (paramConstness p) (paramType p) (paramName p))
        SemanticArg _ ->
            (prev, Param "arg" "void*" Nothing)
        BufferSize t ->
            (prev, Param (maybe "length" (<> "_len") prev) (cType model t) Nothing)
        ErrorPtr ->
            (prev, Param "error" (errTy <> "*") Nothing)
        BufferPtr _ ->
            (prev, Param bufName (bufElemC <> "*") Nothing)
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
        [ Param (idParamName model r) (resourceIdCName model (resourceName r)) Nothing
        | r <- pathIdResources model allRes res ]
    errParams =
        [ Param "error" (errC <> "*") Nothing | Just errC <- [mErr] ]
    bufElem = case propType prop of
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
    | isBufferType (propType prop) =
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

numericArray :: SType -> Maybe (SType, SizeExpr)
numericArray t = case t of
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
    { clsDecorators = []
    , clsCdef   = True
    , clsName   = pyClass r
    , clsBases  = []
    , clsFields =
        [(structName r <> "*", "_ptr")]
        ++ [ (pyClass p, parentField p) | Just p <- [owner] ]
        ++ [ ("object", childField r c) | c <- childrenOf model allRes r ]
        ++ concat [ [("PyThread_type_lock", "_lock"), ("object", "_events")] | isShared allRes r ]
    , clsItems  =
        Decl (sigFunc "_get" [self] (structName r <> "*") True)
        : [ Decl closeSig | hasDtor r ]
        ++ [ Decl (ctorSig model allRes r m) | m <- ctorsOf r ]
        ++ concat [ [ Decl acquireSig, Decl releaseSig ] | isShared allRes r ]
    }
  where
    self = Param "self" "" Nothing
    owner = parentOf model allRes r

-- | Whether a handle or its members have events.
hasEvents :: [SResource] -> SResource -> Bool
hasEvents allRes h = any (not . null . events) (membersOf allRes h)

-- | Whether other threads may use a handle while its loop runs (C's
-- callbacks, an AV thread): it has a lock, and its calls let go of the GIL.
isShared :: [SResource] -> SResource -> Bool
isShared = hasEvents

-- | The handle another handle is made on (@ToxAv@'s @Tox@), which must
-- outlive it.
parentOf :: SemanticModel -> [SResource] -> SResource -> Maybe SResource
parentOf model allRes r = listToMaybe
    [ p | c <- ctorsOf r, PathObject n _ <- argMapping (cmOf c)
        , Just p <- [findRes model (I.resolvePathTarget allRes r n)] ]

childrenOf :: SemanticModel -> [SResource] -> SResource -> [SResource]
childrenOf model allRes r =
    [ c | c <- K.handlesOfKind model LibraryHandle, fmap resourceName (parentOf model allRes c) == Just (resourceName r) ]

-- | A child's reference to its parent: @_tox@.
parentField :: SResource -> Text
parentField p = "_" <> Text.toLower (resourceName p)

-- | A parent's reference to a child: @_av@ for @Tox_Av@ on @Tox@. The two
-- hold each other, so the cyclic GC frees a child only with its parent,
-- whose lock nobody holds then; one freed in a callback of a live parent
-- would wait for the lock its own thread holds.
childField :: SResource -> SResource -> Text
childField p c = "_" <> Text.toLower (fromMaybe (resourceName c) (Text.stripPrefix (resourceName p <> "_") (resourceName c)))

sigFunc :: Text -> [Param] -> Text -> Bool -> Func
sigFunc name params ret excpt = Func
    { fnKind = KindCdef, fnName = name, fnRet = ret, fnParams = params
    , fnParamStyle = CStyle, fnExcept = if excpt then ExcStar else ExcDefault, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

ctorSig :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
ctorSig model allRes r m =
    sigFunc ("_" <> shortName r m)
            (Param "self" "" Nothing : ctorParams model allRes r (cmOf m))
            (structName r <> "*")
            False

-- | With @optional@ (for @__init__@), handles default to @None@, which @.pxd@
-- declarations can't.
ctorParamsWith :: Bool -> SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
ctorParamsWith optional model allRes res cm = mapMaybe pick (argMapping cm)
  where
    semP = cSemParams cm
    pick = \case
        -- The handle it's made on: without one, C only gives its NULL error.
        PathObject n _ ->
            let tgt = I.resolvePathTarget allRes res n
            in Just (Param (Text.toLower tgt) (maybe tgt pyClass (findRes model tgt)) Nothing)
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in Just (Param (idParamName' model tgt)
                           (resourceIdCName model tgt) Nothing)
        SemanticArg n | n < length semP ->
            let p = semP !! n
            in Just (case paramType p of
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
genPyx model pkg pkgRes = Module cythonHeader $
    [ RawItem "cimport cython" | hasChild ]
    ++ [ Import "array" ["array"] | usesArrays ]
    ++ [ CImport "cpython" ["array as c_array"] | not (null copiedArrays) ]
    ++ [ CImport "cpython.bytes" ["PyBytes_FromStringAndSize", "PyBytes_AS_STRING"] | copiesPlanes ]
    ++ [ Import "enum" bases | not (null bases) ]
    ++ [ CImport "libc.string" ["memcpy"] | copiesPlanes || not (null copiedArrays) ]
    ++ [ Import (pythonPackage (targets model)) ["common"] ]
    ++ [ Import (pythonPackage (targets model) <> "." <> pkgName pkg) ["events"] | anyEvents ]
    ++ [ CImport "cpython.pythread" ["PyThread_allocate_lock", "PyThread_free_lock", "PyThread_acquire_lock", "PyThread_release_lock", "WAIT_LOCK"] | anyEvents ]
    ++
    [ Import "types" ["TracebackType"]
    , Import "typing" ["Optional"]
    , Import "typing" ["TypeVar"]
    ]
    ++ [ Import "typing" ["NewType"] | any isRoot pkgRes ]
    ++ [ CImport (rootModule model) [structName root, pyClass root] | holdsLibrary model pkgRes, root <- filter isRoot (resources model) ]
    ++ [ Assign "T" "" (ECall (EName "TypeVar") [EStr "T"]) ]
    -- Ids are ints to C, and each its own type to a type checker.
    ++ [ Assign (pascalName (idName it)) "" (ECall (EName "NewType") [EStr (pascalName (idName it)), EName "int"])
       | any isRoot pkgRes, it <- idTypes model ]
    ++ [ RawItem t | copiesPlanes, t <- planeHelpers ]
    ++ [ RawItem t | el <- copiedArrays, t <- arrayCopier el ]
    ++ [ RawItem t | el <- takenBuffers, t <- bufferTaker el ]
    ++ [ ClassDef (Class [] False "ApiException" ["common.ApiException"] [] []) ]
    ++ concatMap (\e -> [Blank, pyEnum model e]) pkgEnums
    ++ concatMap (pyxClass model allRes) handles
    ++ moduleConsts (rootNamespace model) handles
    ++ concatMap (map FuncDef . moduleFns model allRes) pkgRes
  where
    allRes  = resources model
    handles = filter isHandle pkgRes
    pkgCbs  = [ cb | cb <- callbacks model, filePkg model (cbFile cb) == Just (pkgName pkg) ]
    pkgEnums = enumsFor model pkgRes pkgCbs
    bases = List.nub [ if enumKind e == FlagsEnum then "IntFlag" else "IntEnum" | e <- pkgEnums ]
    hasChild = any (isJust . parentOf model allRes) handles
    anyEvents = any (isShared allRes) handles
    pkgEvents' = pkgEvents model pkgRes
    takenBuffers = List.nub
        [ el | h <- handles, isShared allRes h, r <- membersOf allRes h, m <- methods r
        , any isBufferPtr (argMapping (cmOf m)), Just el <- [bufferElem (output m)] ]
    pkgMethods = concatMap (ownMethods model) pkgRes
    eventTypes = [ paramType p | ev <- pkgEvents', p <- eventParams ev ]
    usesArrays = any (isJust . numericArray) (eventTypes ++ [ paramType p | m <- pkgMethods, p <- inputs m ])
    copiedArrays = List.nub [ el | t <- eventTypes, Just (el, _) <- [numericArray t] ]
    copiesPlanes = any (isJust . planeOf) eventTypes


-- | A received picture's plane as bytes, rows packed: its width is its
-- stride. A plane stored bottom up (a negative stride) is copied top down.
planeHelpers :: [Text]
planeHelpers =
    [ ""
    , "cdef bytes _copy_plane(const uint8_t* ptr, size_t width, size_t height, int32_t stride):"
    , "    if ptr is NULL or width == 0 or height == 0:"
    , "        return b\"\""
    , "    if stride == <Py_ssize_t> width:"
    , "        return PyBytes_FromStringAndSize(<const char*> ptr, width * height)"
    , "    cdef bytes out = PyBytes_FromStringAndSize(NULL, width * height)"
    , "    cdef char* dst = PyBytes_AS_STRING(out)"
    , "    cdef size_t row"
    , "    for row in range(height):"
    , "        memcpy(dst + row * width, ptr + (<Py_ssize_t> row) * stride, width)"
    , "    return out"
    ]

-- | What an event holds: its parameters, but for a plane's stride (planes
-- are packed) and a count its array's length gives.
eventFields :: SEvent -> [SParameter]
eventFields ev =
    [ p | p <- eventParams ev
        , paramName p `notElem` [ stride | q <- eventParams ev, Just (_, _, stride) <- [planeOf (paramType q)] ]
        , paramName p `notElem` map derivedName (derivedParams (eventParams ev)) ]

-- | A received array of numbers as an @array@, copied in one go.
arrayCopier :: SType -> [Text]
arrayCopier el =
    [ ""
    , "cdef c_array.array " <> arrayCopierName el <> "(const " <> c <> "* data, size_t count):"
    , "    cdef c_array.array out = c_array.clone(array(\"" <> arrayTypecode el <> "\"), count, zero=False)"
    , "    if count != 0:"
    , "        memcpy(out.data.as_voidptr, data, count * sizeof(" <> c <> "))"
    , "    return out"
    ]
  where
    c = elemCType el

arrayCopierName :: SType -> Text
arrayCopierName el = "_copy_" <> elemCType el

elemCType :: SType -> Text
elemCType = \case
    SInt n  -> "int" <> tshow n <> "_t"
    SUInt n -> "uint" <> tshow n <> "_t"
    _       -> "uint8_t"

-- | A plane of a received picture: bytes sized @max(width, abs(stride)) *
-- height@, as its width, height and stride.
planeOf :: SType -> Maybe (SizeExpr, SizeExpr, Text)
planeOf = \case
    SFixedBytes (SizeOp '*' (SizeCall "max" [w, SizeCall "abs" [SizeVar stride]]) h) _ -> Just (w, h, stride)
    _ -> Nothing

pyxClass :: SemanticModel -> [SResource] -> SResource -> [Item]
pyxClass model allRes h =
    [ CdefBlock (trampolines ++ [installHandlers model h evs]) | not (null evs) ]
    ++ [ ClassDef Class
        -- Cyclic GC mustn't clear the parent before the handle is freed.
        { clsDecorators = [ "cython.no_gc_clear" | isJust (parentOf model allRes h) ]
        , clsCdef   = True
        , clsName   = pyClass h
        , clsBases  = []
        , clsFields = []
        , clsItems  = map FuncDef classFuncs
        } ]
  where
    mems        = membersOf allRes h
    evs         = concatMap events mems
    trampolines = concatMap (eventTrampolines model allRes h) evs
    classFuncs  =
        [ getMethod h ]
        ++ concat [ [cinitMethod, acquireMethod, releaseMethod] | isShared allRes h ]
        ++ concat [ [closeMethod model allRes h d, deallocMethod allRes h] | d <- take 1 (dtorsOf h) ]
        ++ [ enterMethod ]
        ++ [ exitMethod | hasDtor h ]
        ++ [ ctorMethod model allRes h c | c <- ctorsOf h ]
        ++ initMethods model allRes h
        ++ concatMap (resMethods model allRes h) mems

dtorsOf :: SResource -> [SMethod]
dtorsOf h = [ m | m <- methods h, methodRole m == Destructor ]

hasDtor :: SResource -> Bool
hasDtor = not . null . dtorsOf

resMethods :: SemanticModel -> [SResource] -> SResource -> SResource -> [Func]
resMethods model allRes h res =
    concatMap member
        [ m | m <- ownMethods model res
            , methodRole m `elem` roles
            , not (touchesOpaque m)
            , methodName m `notElem` accessorFns    -- exposed via a property
            , methodName m `notElem` sizeSetters
            , not (partLoop m) ]
    -- Owned data and C's locking are always on (see 'K.alwaysOnOptions' and
    -- 'K.lockingOptions').
    ++ concatMap (if isShared allRes h then lockedPropertyFuncs model allRes h res else propertyFuncs model allRes h res)
        (filter (`notElem` (K.alwaysOnOptions model ++ K.lockingOptions model)) (properties res))
  where
    member m
        | isShared allRes h = [ lockedMember model allRes h res (isNullaryGetter m) m ]
        | otherwise         = memberFunc m
    -- Setting the bytes sets their size: tox_options_set_savedata_length.
    sizeSetters = [ Text.replace "_get_" "_set_" sz | p <- properties res, Just sz <- [propSize p] ]
    -- A handle's constructor/destructor are __init__/__exit__, a
    -- sub-resource's ordinary methods (friend_add, ...).
    roles | resourceName res == resourceName h = [ActionRole, IterateRole]
          | otherwise = [Constructor, Destructor, ActionRole, IterateRole]
    accessorFns = mapMaybe id
        (concatMap (\p -> [propRead p, propWrite p, propSize p]) (properties res))
    -- One loop gives a handle's events: a part of it (toxav_audio_iterate)
    -- couldn't tell its own from those of the rest, made on other threads.
    partLoop m = K.isLoopFunction model (methodName m) && methodName m `notElem` wholeLoop
    wholeLoop = concat
        [ [n, Text.replace "_iterate" "_iteration_interval" n]
        | m <- methods res, methodRole m == IterateRole, let n = methodName m ]

    memberFunc m
        | isNullaryGetter m =
            [ (defFunc (flatName h res m) [Param "self" "" Nothing]
                       (pyType model (result m)))
                { fnDecorators = ["property"]
                , fnBody = callBody model allRes res m } ]
        | otherwise = [ actionMethod model allRes h res m ]
    -- E.g. iteration_interval. A sub-resource getter still takes an id.
    isNullaryGetter m =
        methodRole m `elem` [ActionRole, IterateRole]
        && null (inputs m)
        && not (any isPathId (argMapping (cmOf m)))
        && (output m) /= SVoid
        && methodErrorType m == Nothing
    isPathId PathId{} = True
    isPathId _        = False

touchesOpaque :: SMethod -> Bool
touchesOpaque m = any bad (output m : map paramType (inputs m))
  where
    bad t = case t of
        SCallback{}    -> True
        SHandle "void" -> True
        _              -> False

--------------------------------------------------------------------------------
-- Fixed class members
--------------------------------------------------------------------------------

getMethod :: SResource -> Func
getMethod r = (sigFunc "_get" [Param "self" "" Nothing] (structName r <> "*") True)
    { fnBody =
        [ SIf (EBinOp "is" (EAttr (EName "self") "_ptr") (EName "NULL"))
              [ SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") []) ]
        , SReturn (Just (EAttr (EName "self") "_ptr"))
        ]
    }

selfAttr :: Text -> Expr
selfAttr = EAttr (EName "self")

-- | The lock and the events, before anything else runs.
cinitMethod :: Func
cinitMethod = (defFunc "__cinit__" [Param "self" "" Nothing] "")
    { fnBody =
        [ SAssign (selfAttr "_lock") (ECall (EName "PyThread_allocate_lock") [])
        , SIf (EBinOp "is" (selfAttr "_lock") (EName "NULL")) [SRaise (ECall (EName "MemoryError") [])]
        , SAssign (selfAttr "_events") (EList [])
        ]
    }

-- | Takes the handle's lock, without the GIL; a closed handle raises.
acquireSig, releaseSig, acquireMethod, releaseMethod :: Func
acquireSig = (sigFunc "_acquire" [Param "self" "" Nothing] "int" False) { fnExcept = ExceptNogil "-1" }
releaseSig = (sigFunc "_release" [Param "self" "" Nothing] "void" False) { fnExcept = NoExceptNogil }
acquireMethod = acquireSig
    { fnBody =
        [ lockAcquire (selfAttr "_lock")
        , SIf (EBinOp "is" (selfAttr "_ptr") (EName "NULL"))
              [ lockRelease (selfAttr "_lock")
              , SGil [SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") [])] ]
        , SReturn (Just (EInt 0)) ]
    }
releaseMethod = releaseSig { fnBody = [ lockRelease (selfAttr "_lock") ] }

-- | Frees the handle, once: @__exit__@ and @__dealloc__@ both close it.
closeSig :: Func
closeSig = sigFunc "_close" [Param "self" "" Nothing] "void" True

closeMethod :: SemanticModel -> [SResource] -> SResource -> SMethod -> Func
closeMethod model allRes h dtor = closeSig { fnBody = closeStmts model allRes h dtor }

-- | A child leaves its parent first, which it holds until C has freed it. A
-- parent closes its children before C frees it, as C asserts.
closeStmts :: SemanticModel -> [SResource] -> SResource -> SMethod -> [Stmt]
closeStmts model allRes h dtor
    | isShared allRes h = lockedClose model allRes h dtor
    | otherwise = case parentOf model allRes h of
    Just p ->
        [ SCdef (structName h <> "*") "ptr" (Just (selfAttr "_ptr"))
        , SCdef (pyClass p) "parent" (Just (selfAttr (parentField p)))
        , SAssign (selfAttr "_ptr") (EName "NULL")
        , SAssign (selfAttr (parentField p)) (EName "None")
        , SIf (EBinOp "is not" (EName "parent") (EName "None"))
              [SAssign (EAttr (EName "parent") (childField p h)) (EName "None")]
        , SIf (EBinOp "is not" (EName "ptr") (EName "NULL")) [kill] ]
    Nothing ->
        [ SCdef (structName h <> "*") "ptr" (Just (selfAttr "_ptr"))
        , SAssign (selfAttr "_ptr") (EName "NULL") ]
        ++ closeChildren model allRes h
        ++ [ SIf (EBinOp "is not" (EName "ptr") (EName "NULL")) [kill] ]
  where
    kill = SExpr (ECall (EName (cFunctionName (cmOf dtor))) [EName "ptr"])

closeChildren :: SemanticModel -> [SResource] -> SResource -> [Stmt]
closeChildren model allRes h =
    [ SIf (EBinOp "is not" (selfAttr ref) (EName "None"))
          [SExpr (ECall (EAttr (selfAttr ref) "__exit__") [EName "None", EName "None", EName "None"])]
    | c <- childrenOf model allRes h, let ref = childField h c ]

-- | A shared handle is taken out under its lock, so no call is in C then and
-- none starts (nor a child on it). C frees it under the parent's lock, as it
-- changes that too (no other thread runs the parent's loop).
lockedClose :: SemanticModel -> [SResource] -> SResource -> SMethod -> [Stmt]
lockedClose model allRes h dtor =
    [ SIf (EBinOp "is" (selfAttr "_lock") (EName "NULL")) [SReturn Nothing]
    , SCdef (structName h <> "*") "ptr" (Just (EName "NULL")) ]
    ++ concat
        [ [ SCdef (pyClass p) "parent" (Just (selfAttr (parentField p)))
          , SCdef "PyThread_type_lock" "parent_lock" (Just (EName "NULL"))
          , SAssign (selfAttr (parentField p)) (EName "None")
          , SIf (EBinOp "is not" (EName "parent") (EName "None"))
                [ SAssign (EAttr (EName "parent") (childField p h)) (EName "None")
                , SAssign (EName "parent_lock") (EAttr (EName "parent") "_lock") ] ]
        | Just p <- [owner] ]
    ++ (if null children
        then [ SNogil (underParent (takeOut ++ [kill])) ]
        else [ SNogil takeOut ] ++ children ++ [ SNogil (underParent [kill]) ])
  where
    owner = parentOf model allRes h
    children = closeChildren model allRes h
    takeOut =
        [ lockAcquire (selfAttr "_lock")
        , SAssign (EName "ptr") (selfAttr "_ptr")
        , SAssign (selfAttr "_ptr") (EName "NULL")
        , lockRelease (selfAttr "_lock") ]
    kill = SIf (EBinOp "is not" (EName "ptr") (EName "NULL"))
               [SExpr (ECall (EName (cFunctionName (cmOf dtor))) [EName "ptr"])]
    underParent body
        | isJust owner =
            [ SIf (EBinOp "is not" (EName "parent_lock") (EName "NULL")) [lockAcquire (EName "parent_lock")] ]
            ++ body
            ++ [ SIf (EBinOp "is not" (EName "parent_lock") (EName "NULL")) [lockRelease (EName "parent_lock")] ]
        | otherwise = body

lockAcquire, lockRelease :: Expr -> Stmt
lockAcquire l = SExpr (ECall (EName "PyThread_acquire_lock") [l, EName "WAIT_LOCK"])
lockRelease l = SExpr (ECall (EName "PyThread_release_lock") [l])

deallocMethod :: [SResource] -> SResource -> Func
deallocMethod allRes h = (defFunc "__dealloc__" [Param "self" "" Nothing] "None")
    { fnBody = SExpr (ECall (selfAttr "_close") [])
        : [ s' | isShared allRes h
               , s' <- [ SIf (EBinOp "is not" (selfAttr "_lock") (EName "NULL"))
                             [ SExpr (ECall (EName "PyThread_free_lock") [selfAttr "_lock"])
                             , SAssign (selfAttr "_lock") (EName "NULL") ] ] ] }

enterMethod :: Func
enterMethod = (defFunc "__enter__" [Param "self" "T" Nothing] "T")
    { fnBody = [ SReturn (Just (EName "self")) ] }

exitMethod :: Func
exitMethod = (defFunc "__exit__" params "None")
    { fnBody = [ SExpr (ECall (selfAttr "_close") []) ] }
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
             (structName r <> "*")
             False)
    { fnBody = case lockedParent of
        Just (p, n) -> lockedCreate p n
        Nothing ->
            checkLenStmts model m ++ defaultOptions ++ argChecksOr model maybeNone (inputs m)
            ++ parentStmts ++ lockingStmts ++ cValueDecls cm ++ errLeadIn ++ create
            ++ errCheck ++ [ SReturn (Just (EName "ptr")) ]
    }
  where
    cm = cmOf m
    call = ECall (EName (cFunctionName cm)) callArgs
    -- Made on a shared handle: C changes that one too, so under its lock.
    lockedParent = listToMaybe
        [ (p, Text.toLower (I.resolvePathTarget allRes r t))
        | PathObject t _ <- argMapping cm
        , Just p <- [findRes model (I.resolvePathTarget allRes r t)]
        , isShared allRes p ]
    lockedCreate p n =
        errLeadIn
        ++ [ SCdef (structName p <> "*") (n <> "_ptr") (Just (EName "NULL"))
           , SCdef (structName r <> "*") "ptr" (Just (EName "NULL"))
           , SNogil
               [ lockAcquire (EAttr (EName n) "_lock")
               , SAssign (EName (n <> "_ptr")) (EAttr (EName n) "_ptr")
               , SIf (EBinOp "is not" (EName (n <> "_ptr")) (EName "NULL"))
                     [ SAssign (EName "ptr") call
                     -- Known to the parent before its lock goes, or a close
                     -- in between would free the parent under it.
                     , SIf (EBinOp "is not" (EName "ptr") (EName "NULL"))
                           [ SAssign (selfAttr "_ptr") (EName "ptr")
                           , SGil ([ SAssign (selfAttr (parentField p)) (EName n)
                                   , SAssign (EAttr (EName n) (childField p r)) (EName "self") ]
                                   ++ installHandlersStmts allRes r) ] ]
               , lockRelease (EAttr (EName n) "_lock") ]
           , SIf (EBinOp "is" (EName (n <> "_ptr")) (EName "NULL"))
                 [SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") [])] ]
        ++ errCheck ++ [ SReturn (Just (EName "ptr")) ]
    create
        | withoutGil cm = [ SCdef (structName r <> "*") "ptr" Nothing, SNogil [SAssign (EName "ptr") call] ]
        | otherwise     = [ SCdef (structName r <> "*") "ptr" (Just call) ]
    -- The handle it's made on: a closed one is ours to raise.
    parentStmts = concat
        [ [ SIf (EBinOp "is" (EAttr (EName n) "_ptr") (EName "NULL"))
                [SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") [])]
          , SCdef (structName p <> "*") (n <> "_ptr") (Just (EAttr (EName n) "_ptr")) ]
        | PathObject t _ <- argMapping cm
        , Just p <- [findRes model (I.resolvePathTarget allRes r t)]
        , let n = Text.toLower (I.resolvePathTarget allRes r t) ]
    -- C's locking is always on (see 'K.lockingOptions'): the constructor
    -- taking the options turns it on in them, making them if it got none.
    lockingSetters = [ w | o <- K.lockingOptions model, Just w <- [propWrite o] ]
    defaultOptions =
        [ SIf (EBinOp "is" (EName n) (EName "None"))
              [SAssign (EName n) (ECall (EName (pyClass settings)) [])]
        | (n, settings) <- lockedOptions ]
    lockingStmts =
        [ SExpr (ECall (EName w) [EAttr (EName n) "_ptr", EName "True"])
        | (n, _) <- lockedOptions, w <- lockingSetters ]
    -- Other handles C takes as NULL.
    maybeNone =
        [ paramName p | p <- inputs m, SHandle h <- [paramType p], h /= "void"
        , paramName p `notElem` map fst lockedOptions ]
    lockedOptions =
        [ (paramName p, settings)
        | not (null lockingSetters)
        , p <- inputs m
        , SHandle h <- [paramType p]
        , settings <- K.handlesOfKind model SettingsHandle
        , resourceName settings == h ]
    callArgs =
        [ case src of
            SemanticArg i | i < length semP, n <- paramName (semP !! i), n `elem` maybeNone ->
                ECond (EBinOp "is not" (EName n) (EName "None")) (EAttr (EName n) "_ptr") (EName "NULL")
            _ -> e
        | (src, e) <- zip (argMapping cm) (genCallArgs model allRes r cm "ptr" "error" True) ]
    semP = cSemParams cm
    (errLeadIn, errCheck) = case methodErrorType m of
        Just errC ->
            ( [ SCdef errC "error" (Just (EName (okMember model errC))) ]
            , [ SIf (EName "error") [ SRaise (raiseExc model errC "error") ] ] )
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
    { fnBody =
        initGuard r
        ++ (if madeUnderParentLock model allRes r
            -- The constructor sets it all up under the parent's lock.
            then [ SExpr create ]
            else [ SAssign (EAttr (EName "self") "_ptr") create ]
                 ++ keepParentStmts model allRes r
                 ++ installHandlersStmts allRes r)
    }
  where
    cps = initParams model allRes r (cmOf m)
    params = Param "self" "" Nothing : cps
    create = ECall (EAttr (EName "self") ("_" <> shortName r m)) (map (EName . pmName) cps)

-- | Made on a shared handle: its constructor runs under that one's lock.
madeUnderParentLock :: SemanticModel -> [SResource] -> SResource -> Bool
madeUnderParentLock model allRes r = maybe False (isShared allRes) (parentOf model allRes r)

overloadedInit :: SemanticModel -> [SResource] -> SResource -> SMethod -> SMethod -> Func
overloadedInit model allRes r short long = (defFunc "__init__" params "None")
    { fnBody =
        initGuard r
        ++ [ SIfElse (EBinOp "is not" (EName disc) (EName "None"))
              [ assignTo (shortName r long) longArgs ]
              [ assignTo (shortName r short) shortArgs ] ]
        ++ keepParentStmts model allRes r
        ++ installHandlersStmts allRes r
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

-- | An instance is made once.
initGuard :: SResource -> [Stmt]
initGuard r =
    [ SIf (EBinOp "is not" (selfAttr "_ptr") (EName "NULL"))
          [SRaise (ECall (EName "RuntimeError") [EStr (pyClass r <> " can't be initialised twice")])] ]

-- | A child and its parent hold each other (see 'childField'): the parent
-- closes it first. A shared parent learns of it under its lock, in the
-- constructor.
keepParentStmts :: SemanticModel -> [SResource] -> SResource -> [Stmt]
keepParentStmts model allRes r =
    concat
        [ [ SAssign (selfAttr (parentField p)) (EName n)
          , SAssign (EAttr (EName n) (childField p r)) (EName "self") ]
        | Just p <- [parentOf model allRes r]
        , not (isShared allRes p)
        , let n = Text.toLower (resourceName p) ]

installHandlersStmts :: [SResource] -> SResource -> [Stmt]
installHandlersStmts allRes r =
    [ SExpr (ECall (EName "install_handlers") [EName "self", selfAttr "_ptr"])
    | hasEvents allRes r ]

--------------------------------------------------------------------------------
-- Action methods
--------------------------------------------------------------------------------

actionMethod :: SemanticModel -> [SResource] -> SResource -> SResource -> SMethod -> Func
actionMethod model allRes h res m = (defFunc (flatName h res m) params ret)
    { fnBody = loop (callBody model allRes res m) }
  where
    loop body
        | not (K.isLoopFunction model (methodName m)) || output m /= SVoid || not (hasEvents allRes h) = body
        | otherwise = body ++ [raisePending]
    raisePending = SExpr (ECall (selfAttr "_raise_pending") [])
    cm     = cmOf m
    params = Param "self" "" Nothing : callerParams model allRes res cm
    ret    = pyType model (result m)

--------------------------------------------------------------------------------
-- Calls on shared handles
--------------------------------------------------------------------------------

-- | A method (or a getter as a property) of a shared handle. A loop gives
-- the events its callbacks made.
lockedMember :: SemanticModel -> [SResource] -> SResource -> SResource -> Bool -> SMethod -> Func
lockedMember model allRes h res asProperty m
    | asProperty = (defFunc (flatName h res m) [Param "self" "" Nothing] (pyType model (result m)))
        { fnDecorators = ["property"], fnBody = lockedBody model allRes h res Nothing m }
    | otherwise = (defFunc (flatName h res m) (Param "self" "" Nothing : callerParams model allRes res (cmOf m)) ret)
        { fnBody = lockedBody model allRes h res Nothing m }
  where
    ret | isLoop model m = "list[events.Event]"
        | otherwise      = pyType model (result m)

isLoop :: SemanticModel -> SMethod -> Bool
isLoop model m = K.isLoopFunction model (methodName m) && output m == SVoid

lockedPropertyFuncs :: SemanticModel -> [SResource] -> SResource -> SResource -> SProperty -> [Func]
lockedPropertyFuncs model allRes h res prop
    | badPropType (propType prop) = []
    | otherwise = getter ++ setter
  where
    method fn = List.find ((== fn) . methodName) (methods res)
    selfP = Param "self" "" Nothing
    idParams = [ Param (idParamName model r) (resourceIdCName model (resourceName r)) Nothing
               | r <- pathIdResources model allRes res ]
    asProp = isHandle res && null idParams
    ty = pyType model (propType prop)
    getter = case propRead prop >>= method of
        Nothing -> []
        Just r
            | asProp -> [ (defFunc (propPyName prop) [selfP] ty)
                            { fnDecorators = ["property"], fnBody = body r } ]
            | otherwise -> [ (defFunc (flatPrefix h res <> "get_" <> propName prop) (selfP : idParams) ty)
                               { fnBody = body r } ]
    setter = case propWrite prop >>= method of
        Just w | valueP : _ <- map (pyParam model) (inputs w) ->
            if asProp
                then [ (defFunc (propPyName prop) [selfP, valueP] "None")
                         { fnDecorators = [propPyName prop <> ".setter"], fnBody = body w } ]
                else [ (defFunc (flatPrefix h res <> "set_" <> propName prop) (selfP : idParams ++ [valueP]) "None")
                         { fnBody = body w } ]
        _ -> []
    body = lockedBody model allRes h res (propSize prop)

-- | A call on a shared handle: the C values are made with the GIL, then C
-- runs without it, under the handle's lock (a closed handle raises), and the
-- result is made with the GIL again. A buffer is sized, made and filled
-- under the one lock, so nothing changes its size in between.
lockedBody :: SemanticModel -> [SResource] -> SResource -> SResource -> Maybe Text -> SMethod -> [Stmt]
lockedBody model allRes _h res sizeFn m =
    sizingDecls model m ++ checkLenStmts model m ++ argChecks model (inputs m)
    ++ cArgDecls model allRes res cm
    ++ outDecls
    ++ [ SNogil ( SExpr (ECall (selfAttr "_acquire") []) : cBody ++ [ SExpr (ECall (selfAttr "_release") []) ] ) ]
    ++ post
  where
    cm = cmOf m
    call = ECall (EName (cFunctionName cm)) args
    args = cCallArgs model allRes res cm
    errC = methodErrorType m
    errDecl = [ SCdef e "err" (Just (EName (okMember model e))) | Just e <- [errC] ]
    hasBuffer = any isBufferPtr (argMapping cm)
    -- A getter whose @false@ has no error to say why (no such conference).
    missing = hasBuffer && cReturnType cm == SBool && errC == Nothing && not (null pathIds)
    elemC = maybe "uint8_t" elemCType (bufferElem (output m))
    outDecls
        | hasBuffer =
            errDecl
            ++ [ SCdef "size_t" "size" (Just (EInt 0))
               , SCdef (elemC <> "*") "buf" (Just (EName "NULL")) ]
            ++ [ SCdef "bool" "found" (Just (EName "False")) | missing ]
        | result m /= SVoid = errDecl ++ [ SCdef (cType model (output m)) "res" (Just (ECast (cType model (output m)) (EInt 0))) ]
        | otherwise = errDecl
    cBody
        | hasBuffer =
            SAssign (EName "size") sizeExpr
            : unlessErr
                [ SAssign (EName "buf") (ECast (elemC <> "*") (ECall (EName "malloc")
                      [EBinOp "*" (ECond (EBinOp ">" (EName "size") (EInt 0)) (EName "size") (EInt 1))
                                  (ECall (EName "sizeof") [EName elemC])]))
                , SIf (EBinOp "is not" (EName "buf") (EName "NULL"))
                      [ if missing then SAssign (EName "found") call else SExpr call ] ]
        | result m /= SVoid = [ SAssign (EName "res") call ]
        | otherwise = [ SExpr call ]
    -- The size function's error leaves the buffer unmade.
    unlessErr body
        | isJust errC && sizerFails = [ SIf (ENot (EName "err")) body ]
        | otherwise                 = body
    (sizeExpr, sizerFails) = case output m of
        SFixedBytes sz _  -> (cSizer sz, False)
        SFixedList _ sz _ -> (cSizer sz, False)
        _ -> case cSizeFunctionName cm <|> sizeFn of
            Just f  -> (ECall (EName f) [ e | (src, e) <- zip (argMapping cm) args, structuralArg src ], True)
            Nothing -> (EInt 0, False)
    pathIds = [ EName (idParamName' model (I.resolvePathTarget allRes res n)) | PathId n <- argMapping cm ]
    freeing e = [ SExpr (ECall (EName "free") [EName "buf"]), SRaise e ]
    post
        | hasBuffer =
            [ SIf (EName "err") (freeing (raiseExc model e "err")) | Just e <- [errC] ]
            ++ [ SIf (ENot (EName "found")) (freeing (ECall (EName "KeyError") (case pathIds of
                    [i] -> [i]
                    is  -> [ETuple is | not (null is)]))) | missing ]
            ++ [ SReturn (Just (ECall (EName (bufferTakerName elemC)) [EName "buf", EName "size"])) ]
        | isLoop model m =
            [ SCdef "object" "result" (Just (selfAttr "_events"))
            , SAssign (selfAttr "_events") (EList [])
            , SReturn (Just (EName "result")) ]
        | result m /= SVoid =
            [ SIf (EName "err") [SRaise (raiseExc model e "err")] | Just e <- [errC] ]
            ++ [ SReturn (Just (wrapResult model (output m) (EName "res"))) ]
        | otherwise = [ SIf (EName "err") [SRaise (raiseExc model e "err")] | Just e <- [errC] ]

isBufferPtr :: CArgSource -> Bool
isBufferPtr = \case
    BufferPtr{} -> True
    _           -> False

-- | What a buffer C fills holds: bytes, or numbers (ids are @uint32_t@).
bufferElem :: SType -> Maybe SType
bufferElem = \case
    SBytes            -> Just (SUInt 8)
    SFixedBytes{}     -> Just (SUInt 8)
    SList el          -> Just (number el)
    SFixedList el _ _ -> Just (number el)
    _                 -> Nothing
  where
    number = \case
        SResourceId _ -> SUInt 32
        SScalar _ b   -> b
        t             -> t

bufferTakerName :: Text -> Text
bufferTakerName c = "_take_" <> c

-- | A buffer C filled, as Python's bytes or list, freed.
bufferTaker :: SType -> [Text]
bufferTaker el =
    [ ""
    , "cdef " <> ty <> " " <> bufferTakerName c <> "(" <> c <> "* buf, size_t size):"
    , "    if buf is NULL:"
    , "        raise MemoryError()"
    , "    try:"
    , "        return " <> (if el == SUInt 8 then "buf[:size]" else "[buf[i] for i in range(size)]")
    , "    finally:"
    , "        free(buf)"
    ]
  where
    c = elemCType el
    ty = if el == SUInt 8 then "bytes" else "list"

-- | An id parameter's Python name: @friend_number@.
idParamName' :: SemanticModel -> Text -> Text
idParamName' model r = Text.toLower (maybe (r <> "_Number") idName (K.resourceIdType model r))

-- | The C values of a call's arguments, made with the GIL: pointers to the
-- bytes and numbers, ids, strings and handles as C takes them.
cArgDecls :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Stmt]
cArgDecls model _allRes _res cm =
    take 1 [ SCdef "void*" "me" (Just (ECast "void*" (EName "self"))) | UserData <- argMapping cm ]
    ++ concatMap decl (cSemParams cm)
  where
    sized = map paramName (sizingParams (cSemParams cm))
    decl p = case paramType p of
        t | Just (el, _) <- numericArray t ->
            [ SCdef ("const " <> cType model el <> "[::1]") (n <> "_arr") (Just (EName n))
            , SCdef ("const " <> cType model el <> "*") (n <> "_ptr")
                (Just (ECond (EBinOp ">" (EIndex (EAttr (EName (n <> "_arr")) "shape") (EInt 0)) (EInt 0))
                             (EAddr (EIndex (EName (n <> "_arr")) (EInt 0))) (EName "NULL"))) ]
        t | isBytes t ->
            SCdef "const uint8_t*" (n <> "_ptr")
                (Just (if optionalBytes p
                       then ECond (isNotNone p) (ECast "const uint8_t*" (EName n)) (EName "NULL")
                       else EName n))
            : [ SCdef "size_t" (n <> "_len")
                    (Just (if optionalBytes p
                           then ECond (isNotNone p) (ECall (EName "len") [EName n]) (EInt 0)
                           else ECall (EName "len") [EName n]))
              | t == SBytes ]
        SString -> [ SCdef "const char*" (n <> "_cstr") (Just (EName (n <> "_bytes"))) ]
        SEnum _ -> []
        SResourceId _ -> []
        SHandle h' ->
            [ SCdef (resCName model h' <> "*") (n <> "_ptr") (Just (EAttr (EName n) "_ptr")) ]
        t | n `elem` sized -> []
          | otherwise      -> [ SCdef (cType model t) (n <> "_c") (Just (EName n)) ]
      where
        n = paramName p

-- | The arguments of a call made of 'cArgDecls' values.
cCallArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Expr]
cCallArgs model allRes res cm = snd (List.mapAccumL step Nothing (argMapping cm))
  where
    semP = cSemParams cm
    step prev = \case
        ThisObject _ -> (prev, selfAttr "_ptr")
        PathObject _ _ -> (prev, selfAttr "_ptr")
        PathId n -> (prev, EName (idParamName' model (I.resolvePathTarget allRes res n)))
        SemanticArg i
            | i < length semP -> let p = semP !! i in (Just (paramName p), cArg p)
            | otherwise       -> (prev, EName "NULL")
        BufferSize _ -> (prev, EName (fromMaybe "data" prev <> "_len"))
        ErrorPtr -> (prev, EAddr (EName "err"))
        BufferPtr _ -> (prev, EName "buf")
        UserData -> (prev, EName "me")
        Constant v -> (prev, EInt (fromIntegral v))
    cArg p = case paramType p of
        t | isJust (numericArray t) || isBytes t -> EName (n <> "_ptr")
        SString -> EName (n <> "_cstr")
        SEnum e -> ECast (enumCName model e) (EName (n <> "_val"))
        SHandle _ -> EName (n <> "_ptr")
        SResourceId _ -> EName n
        _ -> EName (n <> "_c")
      where
        n = paramName p

-- | A size made of constants, as C works it out.
cSizer :: SizeExpr -> Expr
cSizer = \case
    SizeNum n -> EInt n
    SizeVar v
        | isUpperConst v -> ECall (EName (Text.toLower v)) []
        | otherwise      -> EName (v <> "_c")
    SizeOp op a b -> EBinOp (Text.singleton op) (cSizer a) (cSizer b)
    SizeCall f as -> ECall (EName f) (map cSizer as)

moduleFns :: SemanticModel -> [SResource] -> SResource -> [Func]
moduleFns model allRes r =
    [ (defFunc (modName m) (callerParams model allRes r (cmOf m)) (pyType model (result m)))
        { fnBody = callBody model allRes r m }
    | m <- ownMethods model r
    , methodRole m == StaticRole
    , not (null (inputs m))
    , not (touchesOpaque m)
    ]
  where
    modName m = fromMaybe (methodName m) (Text.stripPrefix (rootNamespace model <> "_") (methodName m))

callerParams :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> [Param]
callerParams model allRes res cm = mapMaybe pick (argMapping cm)
  where
    semP = cSemParams cm
    derived = map derivedName (derivedParams semP)
    pick = \case
        PathId n ->
            let tgt = I.resolvePathTarget allRes res n
            in Just (Param (idParamName' model tgt)
                           (resourceIdCName model tgt) Nothing)
        SemanticArg n | n < length semP, paramName (semP !! n) `notElem` derived -> Just (pyParam model (semP !! n))
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
    ty       = (propType prop)
    isBuf    = isBufferType ty
    rErr     = accessorErr res (propRead prop)
    wErr     = accessorErr res (propWrite prop)
    -- A buffer getter fills a caller buffer, or returns the data pointer
    -- (tox_options_get_savedata_data).
    getterNullable = maybe False methodReturnNullable
        (propRead prop >>= \fn -> List.find ((== fn) . methodName) (methods res))
    getterReturnsPtr = case propRead prop of
        Just fn -> case List.find ((== fn) . methodName) (methods res) of
            Just m  -> isBufferType (cReturnType (cmOf m))
            Nothing -> False
        Nothing -> False
    idParams = [ Param (idParamName model r) (resourceIdCName model (resourceName r)) Nothing
               | r <- pathIdResources model allRes res ]
    idArgs   = map (EName . pmName) idParams
    asProp   = isHandle res && null idParams
    selfP    = Param "self" "" Nothing
    selfGet  = ECall (EAttr (EName "self") "_get") []

    errArg  e = [ EAddr (EName "err") | e /= Nothing ]
    errDecl e = [ SCdef ec "err" (Just (EName (okMember model ec))) | Just ec <- [e] ]
    errChk  e = [ SIf (EName "err") [ SRaise (raiseExc model ec "err") ] | Just ec <- [e] ]

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
            , SReturn (Just (ECond (EBinOp "is not" (EName "res") (EName "NULL")) (wrapResult model ty (EName "res")) (EStr "")))
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
            [ SCdef "const uint8_t*" "data" (Just (ECall (EName fn) (selfGet : idArgs)))
            , SCdef "size_t" "length" (Just sizeExpr)
            , SIf (EBinOp "or" (EBinOp "is" (EName "data") (EName "NULL")) (EBinOp "==" (EName "length") (EInt 0)))
                  [SReturn (Just (EBytes ""))]
            , SReturn (Just (ESlice (EName "data") Nothing (Just (EName "length")))) ]
        (Just _, True) ->
            errDecl rErr
            ++ [ SCdef "size_t" "size" (Just sizeExpr) ]
            -- Only a size function can have set the error.
            ++ (if sizeChecks then errChk rErr else [])
            ++ allocBuffer bufElemC
            ++ [ STryFinally
                   ( fill
                     ++ errChk rErr
                     ++ [ SReturn (Just bufResult) ] )
                   [ SExpr (ECall (EName "free") [EName "buf"]) ] ]
      where
        (bufElemC, bufIsList) = case ty of
            SList el          -> (cType model el, True)
            SFixedList el _ _ -> (cType model el, True)
            _                 -> ("uint8_t", False)
        reader = propRead prop >>= \f -> List.find ((== f) . methodName) (methods res)
        fillCall = ECall (EName (fromMaybe "" (propRead prop))) (selfGet : idArgs ++ [EName "buf"] ++ errArg rErr)
        -- A @false@ with no error to say why: there's no such item.
        fill = case reader of
            Just r | cReturnType (cmOf r) == SBool, rErr == Nothing ->
                [ SIf (ENot fillCall) [SRaise (ECall (EName "KeyError") (case idArgs of
                    [i] -> [i]
                    is  -> [ETuple is | not (null is)]))] ]
            _ -> [ SExpr fillCall ]
        bufResult | bufIsList = EListComp (EIndex (EName "buf") (EName "i")) "i" (ECall (EName "range") [EName "size"])
                  | otherwise = ESlice (EName "buf") Nothing (Just (EName "size"))
        sizeChecks = case (ty, propSize prop) of
            (SFixedBytes{}, _) -> False
            (_, Just _)        -> True
            _                  -> False
        -- Only a variable-length buffer needs its size function.
        sizeExpr = case ty of
            SFixedBytes s _ -> sizerExpr s
            _ -> case propSize prop of
                Just sz -> ECall (EName sz) (selfGet : idArgs ++ errArg rErr)
                Nothing -> EInt 0

    writeBody fn
        -- Make C copy the strings and bytes first (Python frees its buffer after
        -- this).
        | borrows, Just owned <- ownedSetter =
            prologue
            ++ [ SExpr (ECall (EName owned) [selfGet, EName "True"])
               , SIf (ENot call) [SRaise (ECall (EName "MemoryError") [])]
               ]
        | otherwise = prologue ++ errDecl wErr ++ [SExpr call] ++ errChk wErr
      where
        valueParam = SParameter { paramName = propName prop, paramType = ty, paramConstness = T.ConstThis, paramConstants = [], paramNullable = False }
        prologue = argChecks model [valueParam]
        call = ECall (EName fn) (selfGet : idArgs ++ valueArgs ++ errArg wErr)
        borrows = ty `elem` [SString, SBytes] && setterFails fn
        setterFails f = maybe False ((== SBool) . output) (List.find ((== f) . methodName) (methods res))
        ownedSetter = List.find (`elem` K.alwaysOnOptions model) (properties res) >>= propWrite
        -- C copies no bytes from a NULL: an empty buffer's malloc(0) may fail.
        valueArgs
            | isBuf, borrows, isJust ownedSetter = [ ECond (EBinOp "!=" (ECall (EName "len") [EName (propName prop)]) (EInt 0))
                                       (ECast "const uint8_t*" (EName (propName prop))) (EName "NULL")
                               , ECall (EName "len") [EName (propName prop)] ]
            | isBuf     = [ EName (propName prop), ECall (EName "len") [EName (propName prop)] ]
            | otherwise = [ argExpr model valueParam ]

propPyName :: SProperty -> Text
propPyName prop = fromMaybe nm (Text.stripPrefix "self_" nm)
  where nm = propName prop

wrapResult :: SemanticModel -> SType -> Expr -> Expr
wrapResult model ty e = case ty of
    SEnum n -> ECall (EName (pyEnumName model (enumCName model n))) [e]
    SString -> ECall (EAttr e "decode") [EStr "utf-8"]
    _       -> e

-- | The checks a call makes of its arguments first: a handle isn't closed;
-- an enum is one of its values; a string has no NUL in it.
argChecks :: SemanticModel -> [SParameter] -> [Stmt]
argChecks model = argChecksOr model []

-- | With the handles that may be None (a constructor's @Optional@ ones);
-- Cython lets no other be.
argChecksOr :: SemanticModel -> [Text] -> [SParameter] -> [Stmt]
argChecksOr model maybeNone ps =
    [ SIf (if n `elem` maybeNone then EBinOp "and" (EBinOp "is not" (EName n) (EName "None")) closed else closed)
          [SRaise (ECall (EAttr (EName "common") "UseAfterFreeException") [])]
    | p <- ps, SHandle h <- [paramType p], h /= "void"
    , let n = paramName p
          closed = EBinOp "is" (EAttr (EName n) "_ptr") (EName "NULL") ]
    ++ [ SCdef "int" (paramName p <> "_val") (Just (ECall (EName (pyEnumName model (enumCName model n))) [EName (paramName p)]))
    | p <- ps, SEnum n <- [paramType p] ]
    ++ [ SCdef "bytes" (paramName p <> "_bytes")
               (Just (ECall (EAttr (EName "common") "encode_cstr") [EStr (paramName p), EName (paramName p)]))
       | p <- ps, paramType p == SString ]

-- | An argument as C takes it, after 'argChecks'.
argExpr :: SemanticModel -> SParameter -> Expr
argExpr model p = case paramType p of
    SEnum n -> ECast (enumCName model n) (EName (paramName p <> "_val"))
    SString -> EName (paramName p <> "_bytes")
    t       -> coerceArg model t (EName (paramName p))

coerceArg :: SemanticModel -> SType -> Expr -> Expr
coerceArg model ty e = case ty of
    SEnum n        -> ECast (enumCName model n) e
    SString        -> ECall (EAttr e "encode") [EStr "utf-8"]
    SHandle "void" -> e
    -- Not @_get()@: calling a @cdef@ method across modules trips UBSan's
    -- @-fsanitize=function@ (SIGILL). 'argChecks' raised for a freed one.
    SHandle _      -> EAttr e "_ptr"
    _              -> e

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

-- | @handle_*@ matches the C callback, size parameters and all, and adds
-- the event with the Python values. Nobody's code runs in C's callback: the
-- loop gives the events once C has returned. It takes the GIL (C may call
-- from a thread of its own); what it can't raise into C (no memory) goes to
-- @sys.unraisablehook@.
eventTrampolines :: SemanticModel -> [SResource] -> SResource -> SEvent -> [Func]
eventTrampolines model _allRes h ev =
    [ (sigFunc ("handle_" <> eventName ev) cParams "void" False)
        { fnExcept = NoExceptWithGil
        , fnBody =
            -- The event is made before the list is looked up: the loop may
            -- take the list while Python runs.
            [ SAssign (EName "event") (ECall (EAttr (EName "events") (eventClassName ev)) (map eventArg (eventFields ev)))
            , SExpr (ECall (EAttr (EAttr (ECast (pyClass h) (EName userData)) "_events") "append") [EName "event"])
            ]
        }
    ]
  where
    cParams = case List.find ((== cCallback ev) . cbCName) (callbacks model) of
        Just cb -> expandedCbParams model (cbParams cb)
        Nothing ->
            cParam model T.MutableThis (SHandle (resourceName h)) (Text.toLower (resourceName h))
            : expandedCbParams model (eventParams ev)
            ++ [ Param "user_data" "void*" Nothing ]
    userData = maybe "user_data" pmName (listToMaybe (reverse cParams))
    orEmpty p e = ECond (EBinOp "is not" (EName (paramName p)) (EName "NULL")) e (EBytes "")
    eventArg p = case paramType p of
        SBytes -> orEmpty p (ESlice (EName (paramName p)) Nothing (Just (EName (paramName p <> "_length"))))
        t | Just (w, ht, stride) <- planeOf t ->
            ECall (EName "_copy_plane") [EName (paramName p), sizerExpr w, sizerExpr ht, EName stride]
        SFixedBytes sz _ -> orEmpty p (ESlice (EName (paramName p)) Nothing (Just (sizerExpr sz)))
        t | Just (el, sz) <- numericArray t ->
            ECall (EName (arrayCopierName el)) [EName (paramName p), sizerExpr sz]
        SEnum n -> ECall (EName (pyEnumName model (enumCName model n))) [EName (paramName p)]
        _ | Just e <- flagsParam model ev p ->
            ECall (EName (pyEnumName model (enumName e))) [EName (paramName p)]
        _ -> EName (paramName p)

installHandlers :: SemanticModel -> SResource -> [SEvent] -> Func
installHandlers _model h evs =
    (sigFunc "install_handlers"
             [ Param "self" (pyClass h) Nothing, Param "ptr" (structName h <> "*") Nothing ]
             "void" False)
    { fnExcept = NoExcept
    , fnBody =
        [ SExpr (ECall (EName (eventRegistrar ev))
                       ( [ EName "ptr", EName ("handle_" <> eventName ev) ]
                         ++ [ ECast "void*" (EName "self") | eventHasUserData ev ] ))
        | ev <- evs ]
    }

-- | An event's class: @FriendMessageEvent@.
eventClassName :: SEvent -> Text
eventClassName ev = pascalName (eventName ev) <> "Event"

-- | The events of a package's handles.
pkgEvents :: SemanticModel -> [SResource] -> [SEvent]
pkgEvents model pkgRes =
    concatMap events (concatMap (membersOf (resources model)) (filter isHandle pkgRes))

-- | @events.py@: each event as a frozen dataclass (so @match@ takes it
-- apart, checked), and the union of them a loop gives. Plain Python, which
-- mypy reads as it is.
genEvents :: SemanticModel -> [SEvent] -> Text
genEvents model evs = Text.unlines $
    [ "from __future__ import annotations"
    , ""
    , "from dataclasses import dataclass"
    , "from typing import TYPE_CHECKING"
    , "from typing import Union"
    , ""
    , "if TYPE_CHECKING:" ]
    ++ [ "    from " <> m <> " import " <> n | (m, n) <- imports ]
    ++ concat
        [ [ "", "", "@dataclass(frozen=True, slots=True)"
          , "class " <> eventClassName ev <> ":" ]
          ++ [ "    " <> paramName p <> ": " <> fieldType ev p | p <- eventFields ev ]
        | ev <- evs ]
    ++ [ "", "", "Event = Union[" ]
    ++ [ "    " <> eventClassName ev <> "," | ev <- evs ]
    ++ [ "]" ]
  where
    fieldType ev p = case paramType p of
        t | isJust (numericArray t)         -> "array[int]"
        SResourceId n                       -> idPyName model n
        _ | Just e <- flagsParam model ev p -> pyEnumName model (enumName e)
        t                                   -> pyType model t
    imports = List.sort $ List.nub $ concat
        [ case paramType p of
            SResourceId n -> [(rootModule model, idPyName model n)]
            SEnum n -> [ (moduleOf (enumFile e), pyEnumName model (enumName e)) | Just e <- [findEnum model n] ]
            t | isJust (numericArray t) -> [("array", "array")]
            _ | Just e <- flagsParam model ev p -> [(moduleOf (enumFile e), pyEnumName model (enumName e))]
            _ -> []
        | ev <- evs, p <- eventFields ev ]
    moduleOf f = case List.find ((headerOf f `elem`) . pkgFiles) (packages model) of
        Just pkg -> pythonPackage (targets model) <> "." <> Text.replace "/" "." (Text.pack (pkgBase pkg))
        Nothing  -> rootModule model

-- | An integer a callback gives that holds an enum's flags (a call's state).
-- An enum of values stays an int: clients use values of their own.
flagsParam :: SemanticModel -> SEvent -> SParameter -> Maybe SEnumModel
flagsParam model ev p = do
    e <- K.enumParam model (cCallback ev, paramName p) >>= findEnum model
    if enumKind e == FlagsEnum then Just e else Nothing

--------------------------------------------------------------------------------
-- Stubs
--------------------------------------------------------------------------------

-- | The @.pyi@ a type checker reads for the extension: the Python side of
-- the @.pyx@ we make (its @def@s, properties and constants), with each
-- id its own type and the package's enums.
genStub :: SemanticModel -> Pkg -> [SResource] -> Module -> Text
genStub model pkg pkgRes (Module _ items) = Text.unlines $
    List.sort (List.nub (fixedImports ++ rootImports)) ++ body
  where
    allRes = resources model
    pkgCbs = [ cb | cb <- callbacks model, filePkg model (cbFile cb) == Just (pkgName pkg) ]
    pkgEnums = enumsFor model pkgRes pkgCbs
    body =
        concat [ stubEnum e | e <- pkgEnums ]
        ++ concatMap stubItem items
    stubEnum e
        | enumKind e == ErrorEnum =
            [ "", "class " <> name <> "(ApiException):", "    class Code(enum.IntEnum):" ]
            ++ members "        "
            ++ [ "    code: " <> name <> ".Code" ]
        | otherwise =
            [ "", "class " <> name <> "(enum." <> (if enumKind e == FlagsEnum then "IntFlag" else "IntEnum") <> "):" ]
            ++ members "    "
      where
        name = pyEnumName model (enumName e)
        members ind =
            [ ind <> enumMemberName m <> " = " <> tshow (enumMemberValue m)
            | m <- enumMembers e, not (enumKind e == ErrorEnum && enumMemberValue m == 0) ]
    stubItem = \case
        ClassDef c
            | not (clsCdef c) -> [ "", "class " <> clsName c <> "(" <> Text.intercalate ", " (clsBases c) <> "): ..." ]
            | otherwise -> [ "", "class " <> clsName c <> ":" ]
                ++ concat [ map ("    " <>) (stubFunc f) | FuncDef f <- clsItems c, isStubbed f ]
        FuncDef f | isStubbed f -> "" : stubFunc f
        Assign n ann e
            | ECall (EName "NewType") _ <- e -> [ n <> " = NewType(\"" <> n <> "\", int)" ]
            | not (Text.null ann) -> [ n <> ": " <> ann ]
        _ -> []
    isStubbed f = fnKind f == KindDef && fnName f `notElem` ["__cinit__", "__dealloc__"]
    stubFunc f =
        map ("@" <>) (fnDecorators f)
        ++ [ "def " <> fnName f <> "(" <> Text.intercalate ", " (map stubParam (fnParams f)) <> ") -> " <> ret f <> ": ..." ]
    ret f
        | fnName f == "__enter__" = "Self"
        | Text.null (fnRet f)     = "None"
        | otherwise               = stubType model (fnRet f)
    stubParam (Param n t d)
        | n == "self" = n
        | otherwise   = n <> (if Text.null t then "" else ": " <> stubType model t) <> maybe "" (const " = ...") d
    fixedImports =
        [ "import enum" | not (null pkgEnums) ]
        ++ [ "from array import array" | uses "array[" ]
        ++ [ "from types import TracebackType" | uses "TracebackType" ]
        ++ [ "from typing import NewType" | uses "NewType(" ]
        ++ [ "from typing import Optional" | uses "Optional[" ]
        ++ [ "from typing import Self" | uses "Self" ]
        ++ [ "from " <> pythonPackage (targets model) <> " import common" ]
        ++ [ "from " <> pythonPackage (targets model) <> "." <> pkgName pkg <> " import events" | uses "events." ]
    -- Another library's package names the root's class and ids.
    rootImports =
        [ "from " <> rootModule model <> " import " <> n
        | not (any isRoot pkgRes)
        , n <- pyClass' : map (pascalName . idName) (idTypes model)
        , usesWord n ]
    pyClass' = maybe "" pyClass (List.find isRoot allRes)
    rendered = Text.unlines (concatMap stubItem items)
    uses t = t `Text.isInfixOf` rendered
    usesWord n = n `elem` stubWords rendered

-- | The words of a text, for which names a stub uses.
stubWords :: Text -> [Text]
stubWords = Text.split (\c -> not (isAlphaNum c || c == '_'))

-- | A type as a type checker sees it: ids are their NewTypes, an array of
-- numbers holds ints.
stubType :: SemanticModel -> Text -> Text
stubType model t = Text.concat (map word (Text.groupBy sameKind t))
  where
    sameKind a b = isIdent a == isIdent b
    isIdent c = isAlphaNum c || c == '_'
    word w
        | Just it <- List.find ((== w) . idCName) (idTypes model) = pascalName (idName it)
        | w == "array" = "array[int]"
        | otherwise = w

--------------------------------------------------------------------------------
-- Module-level constants
--------------------------------------------------------------------------------

-- | The root library's version, if it has @lib_version_major@ and so on,
-- and the sizes and limits its functions give.
moduleConsts :: Text -> [SResource] -> [Item]
moduleConsts root handles =
    [ Assign "VERSION" "str" (EBinOp "%" (EStr "%d.%d.%d") (ETuple [ ECall (EName v) [] | v <- versionParts ]))
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
callBody model allRes res m = sizingDecls model m ++ checkLenStmts model m ++ argChecks model (inputs m) ++ viewDecls ++ cValueDecls cm ++ body
  where
    body | hasBuffer = bufferShape
         | otherwise = case methodErrorType m of
             Just errC -> scalarErrShape errC
             Nothing   -> noErrShape
    -- C takes a pointer to the first number: the buffer is contiguous.
    viewDecls =
        [ SCdef ("const " <> cType model el <> "[::1]") (paramName p <> "_arr")
                (Just (EName (paramName p)))
        | p <- inputs m, Just (el, _) <- [numericArray (paramType p)] ]
    cm        = cmOf m
    cfunc     = cFunctionName cm
    hasBuffer = any isBufPtr (argMapping cm)
    isBufPtr (BufferPtr _) = True
    isBufPtr _             = False

    sizeInit = case output m of
        SFixedBytes s _ -> sizerExpr s
        _ -> case cSizeFunctionName cm of
            Just sz -> ECall (EName sz) sizeArgs
            Nothing -> EInt 0
    sizeArgs =
        [ e | (src, e) <- zip (argMapping cm) (genCallArgs model allRes res cm "buf" "err" False)
            , structuralArg src ]

    args v = genCallArgs model allRes res cm "buf" v False

    (bufElemC, bufIsList) = case output m of
        SList el          -> (cType model el, True)
        SFixedList el _ _ -> (cType model el, True)
        _                 -> ("uint8_t", False)
    bufResult | bufIsList = EListComp (EIndex (EName "buf") (EName "i")) "i" (ECall (EName "range") [EName "size"])
              | otherwise = ESlice (EName "buf") Nothing (Just (EName "size"))

    -- A getter whose @false@ has no error to say why (no such conference).
    -- Without one, @false@ is C's NULL check, which bytes pass (tox_hash).
    missing = cReturnType cm == SBool && methodErrorType m == Nothing && not (null pathIds)
    pathIds = [ EName (idParamName' model (I.resolvePathTarget allRes res n)) | PathId n <- argMapping cm ]
    keyError = SRaise (ECall (EName "KeyError") (case pathIds of
        [i] -> [i]
        is  -> [ETuple is | not (null is)]))

    bufferShape =
        [ SCdef errC "err" (Just (EName (okMember model errC))) | Just errC <- [methodErrorType m] ]
        ++ [ SCdef "size_t" "size" (Just sizeInit) ]
        ++ [ SCdef "bool" "found" Nothing | missing ]
        ++ allocBuffer bufElemC
        ++ [ STryFinally
               ( fill
                 ++ [ SIf (EName "err") [ SRaise (raiseExc model errC "err") ]
                    | Just errC <- [methodErrorType m] ]
                 ++ [ SReturn (Just bufResult) ] )
               [ SExpr (ECall (EName "free") [EName "buf"]) ]
           ]
    fill
        | missing   = cCallStmts cm [SAssign (EName "found") (ECall (EName cfunc) (args "err"))]
                      ++ [SIf (ENot (EName "found")) [keyError]]
        | otherwise = cCallStmts cm [SExpr (ECall (EName cfunc) (args "err"))]

    scalarErrShape errC = case result m of
        SVoid ->
            SCdef errC "err" (Just (EName (okMember model errC)))
            : cCallStmts cm [SExpr (ECall (EName cfunc) (args "err"))]
            ++ [ SIf (EName "err") [ SRaise (raiseExc model errC "err") ] ]
        _ ->
            [ SCdef errC "err" (Just (EName (okMember model errC))) ]
            ++ resultOf (ECall (EName cfunc) (args "err"))
            ++ [ SIf (EName "err") [ SRaise (raiseExc model errC "err") ]
               , SReturn (Just (wrapResult model (output m) (EName "res"))) ]

    noErrShape =
        let call = ECall (EName cfunc) (args "err")
        in case result m of
            SVoid -> cCallStmts cm [SExpr call]
            _ | withoutGil cm -> resultOf call ++ [SReturn (Just (wrapResult model (output m) (EName "res")))]
              | otherwise     -> [ SReturn (Just (wrapResult model (output m) call)) ]

    resultOf call
        | withoutGil cm = [ SCdef (cType model (output m)) "res" Nothing, SNogil [SAssign (EName "res") call] ]
        | otherwise     = [ SCdef (cType model (output m)) "res" (Just call) ]

-- | In a constructor the parent handle ('PathObject') comes from a
-- parameter, not @self@.
genCallArgs :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> Text -> Text -> Bool -> [Expr]
genCallArgs model allRes res cm bufVar errVar isCtor =
    snd (List.mapAccumL step Nothing (argMapping cm))
  where
    cvals = withoutGil cm
    sized = map paramName (sizingParams semP)
    semP = cSemParams cm
    step prev = \case
        ThisObject _   -> (prev, ECall (EAttr (EName "self") "_get") [])
        PathObject n _
            -- Not @_get()@: see 'coerceArg'.
            | isCtor    -> (prev, EName (Text.toLower (I.resolvePathTarget allRes res n) <> "_ptr"))
            | otherwise -> (prev, ECall (EAttr (EName "self") "_get") [])
        PathId n       -> let tgt = I.resolvePathTarget allRes res n
                          in (prev, EName (idParamName' model tgt))
        SemanticArg n
            | n < length semP ->
                let p = semP !! n
                    e = case numericArray (paramType p) of
                            Just _  -> ECond (EBinOp ">" (EIndex (EAttr (EName (paramName p <> "_arr")) "shape") (EInt 0)) (EInt 0))
                                             (EAddr (EIndex (EName (paramName p <> "_arr")) (EInt 0))) (EName "NULL")
                            Nothing
                                | cvals, isBytes (paramType p) -> EName (paramName p <> "_ptr")
                                | optionalBytes p ->
                                    ECond (isNotNone p)
                                          (ECast "const uint8_t*" (EName (paramName p)))
                                          (EName "NULL")
                                | paramName p `elem` sized -> EName (paramName p <> "_c")
                                | otherwise -> argExpr model p
                in (Just (paramName p), e)
            | otherwise -> (prev, EName "None")
        BufferSize _
            | cvals        -> (prev, EName (fromMaybe "data" prev <> "_len"))
            | otherwise    -> (prev, ECall (EName "len") [EName (fromMaybe "data" prev)])
        ErrorPtr       -> (prev, EAddr (EName errVar))
        BufferPtr _    -> (prev, EName bufVar)
        UserData       -> (prev, ECast "void*" (EName "self"))
        Constant v     -> (prev, EInt (fromIntegral v))

-- | Whether a call can let go of the GIL: it takes no handle (so it touches
-- nothing another thread uses), only bytes, which it works on (hashing,
-- encrypting, deriving a key).
withoutGil :: CFunctionMapping -> Bool
withoutGil cm = not (null (cSemParams cm)) && not (any usesHandle (argMapping cm))
    && all (isBytes . paramType) (cSemParams cm)
  where
    usesHandle = \case
        ThisObject{} -> True
        PathObject{} -> True
        UserData     -> True
        _            -> False

isBytes :: SType -> Bool
isBytes = \case
    SBytes        -> True
    SFixedBytes{} -> True
    _             -> False

-- | The C values a call without the GIL takes: the bytes' pointers and
-- lengths.
cValueDecls :: CFunctionMapping -> [Stmt]
cValueDecls cm =
    concat
        [ SCdef "const uint8_t*" (paramName p <> "_ptr")
              (Just (if optionalBytes p
                     then ECond (isNotNone p) (ECast "const uint8_t*" (EName (paramName p))) (EName "NULL")
                     else EName (paramName p)))
          : [ SCdef "size_t" (paramName p <> "_len") (Just (ECall (EName "len") [EName (paramName p)]))
            | paramType p == SBytes ]
        | withoutGil cm, p <- cSemParams cm, isBytes (paramType p) ]

-- | A C call, without the GIL where it can.
cCallStmts :: CFunctionMapping -> [Stmt] -> [Stmt]
cCallStmts cm body
    | withoutGil cm = [SNogil body]
    | otherwise     = body

-- | @FriendAddError(FriendAddError.Code(err))@: each error is its own
-- exception, so a type checker knows which codes a caught one has.
raiseExc :: SemanticModel -> Text -> Text -> Expr
raiseExc model errC var = ECall (EName exc) [ECall (EName (exc <> ".Code")) [EName var]]
  where exc = pyEnumName model errC

-- | A C enum's Python name, as in the other bindings: @Connection@,
-- @FriendAddError@, @AvCallControl@.
pyEnumName :: SemanticModel -> Text -> Text
pyEnumName model = enumTypeName (namespaces model)

-- | A C enum as a Python one: @Connection.TCP@. A set of flags combines.
-- An error enum is an exception with its codes as @Code@
-- (@FriendAddError.Code.ALREADY_SENT@), leaving out OK, which is never
-- raised.
-- The functional form, as a class body can't name a member @NULL@ in
-- Cython, and with its module, which Cython can't find for it.
pyEnum :: SemanticModel -> SEnumModel -> Item
pyEnum model e
    | enumKind e == ErrorEnum = RawItem $
        "class " <> name <> "(ApiException):\n    Code = "
        <> functional "    " "Code" (", qualname=\"" <> name <> ".Code\"")
    | otherwise = RawItem $ name <> " = " <> functional "" name ""
  where
    name = pyEnumName model (enumName e)
    base = if enumKind e == FlagsEnum then "IntFlag" else "IntEnum"
    functional ind n extra =
        base <> "(\"" <> n <> "\", [\n"
        <> Text.concat [ ind <> "    (\"" <> enumMemberName m <> "\", " <> enumMemberCName m <> "),\n"
                       | m <- enumMembers e, not (enumKind e == ErrorEnum && enumMemberValue m == 0) ]
        <> ind <> "], module=__name__" <> extra <> ")"

isNotNone :: SParameter -> Expr
isNotNone p = EBinOp "is not" (EName (paramName p)) (EName "None")

-- | The numbers other arguments' sizes are made of, as C takes them: one
-- out of range raises before a size is worked out from it.
sizingDecls :: SemanticModel -> SMethod -> [Stmt]
sizingDecls model m =
    [ SCdef (cType model (paramType p)) (paramName p <> "_c") (Just (EName (paramName p)))
    | p <- sizingParams (inputs m), paramName p `notElem` map derivedName derived ]
    ++ [ SCdef (cType model (paramType d)) (paramName d <> "_c")
               (Just (ECall (EAttr (EName "common") "count_of") [EStr (paramName a), EName (paramName a), EName (paramName per)]))
       | (d, a, per) <- derived ]
  where
    derived = derivedParams (inputs m)

sizingParams :: [SParameter] -> [SParameter]
sizingParams ps =
    [ p | p <- ps, isNumber (paramType p), paramName p `elem` sizeVars ]
  where
    sizeVars = concat [ sizeExprVars sz | q <- ps, Just sz <- [sizeOfType (paramType q)] ]

-- | A count (a @size_t@) only one array's size says, as the left of
-- @count * per@ (@sample_count@ in @pcm[sample_count * channels]@): Python
-- works it out from the array, which then holds whole groups of @per@. A
-- picture's @width * height@ stays: those are sizes, not counts.
derivedParams :: [SParameter] -> [(SParameter, SParameter, SParameter)]
derivedParams ps =
    [ (d, a, per)
    | a <- ps, Just (SizeOp '*' (SizeVar dn) (SizeVar pn)) <- [sizeOfType (paramType a)]
    , d <- ps, paramName d == dn, isSizeT (paramType d)
    , per <- ps, paramName per == pn, isNumber (paramType per)
    , length [ () | b <- ps, Just sz <- [sizeOfType (paramType b)], dn `elem` sizeExprVars sz ] == 1 ]

derivedName :: (SParameter, SParameter, SParameter) -> Text
derivedName (d, _, _) = paramName d

sizeOfType :: SType -> Maybe SizeExpr
sizeOfType = \case
    SFixedBytes sz _  -> Just sz
    SFixedList _ sz _ -> Just sz
    _                 -> Nothing

isSizeT :: SType -> Bool
isSizeT = \case
    SScalar _ b -> isSizeT b
    SSizeT      -> True
    _           -> False

isNumber :: SType -> Bool
isNumber = \case
    SScalar _ b -> isNumber b
    SInt _      -> True
    SUInt _     -> True
    SSizeT      -> True
    _           -> False

-- | A key's size is exact; a size from other arguments (a picture's
-- plane) or one C gives as a minimum (encrypted data) is the least.
checkLenStmts :: SemanticModel -> SMethod -> [Stmt]
checkLenStmts model m =
    [ if optionalBytes p then SIf (isNotNone p) [check] else check
    | p <- inputs m
    , sz <- case paramType p of
        SFixedBytes sz _  -> [sz]
        SFixedList _ sz _ -> [sz]
        _                 -> []
    -- 'common.count_of' checks an array a count is worked out from.
    , not (any (`elem` map derivedName (derivedParams (inputs m))) (sizeExprVars sz))
    , let exact = all isUpperConst (sizeExprVars sz) && isNothing (K.minimumSize model (methodName m, paramName p))
          checker = if exact then "check_exact_len" else "check_len"
          check = SExpr (ECall (EAttr (EName "common") checker)
                                [ EStr (paramName p), EName (paramName p), sizerExpr sz ])
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
    , fnParamStyle = PyStyle, fnExcept = ExcDefault, fnDoc = Nothing
    , fnDecorators = [], fnBody = []
    }

pyParam :: SemanticModel -> SParameter -> Param
pyParam model p
    | optionalBytes p = Param (paramName p) "Optional[bytes]" Nothing
    | otherwise = Param (paramName p) (pyType model (paramType p)) Nothing

-- | A byte array parameter that may be NULL: Python's @None@.
optionalBytes :: SParameter -> Bool
optionalBytes p = paramNullable p && case paramType p of
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

sizerExpr :: SizeExpr -> Expr
sizerExpr = expr
  where
    expr e = case e of
        SizeNum n -> EInt n
        SizeVar v
            | isUpperConst v             -> ECall (EName (Text.toLower v)) []
            | "_len" `Text.isSuffixOf` v -> ECall (EName "len") [EName (Text.dropEnd 4 v)]
            | otherwise                  -> EName v
        -- C checks the size, but a negative one would raise before C sees it.
        -- Signed, or a 32-bit @len(x) - uint32_t@ wraps round.
        SizeOp '-' a b -> ECall (EName "max") [EInt 0, EBinOp "-" (expr a) (ECast "Py_ssize_t" (expr b))]
        -- C's @/@ is Python's @//@ here.
        SizeOp '/' a b -> EBinOp "//" (expr a) (expr b)
        SizeOp op a b  -> EBinOp (Text.singleton op) (expr a) (expr b)
        SizeCall f as  -> ECall (EName f) (map expr as)
-- | A size constant, like @TOX_PUBLIC_KEY_SIZE@.
isUpperConst :: Text -> Bool
isUpperConst t' =
    not (Text.null t')
    && Text.any isUpper t'
    && Text.all (\c -> isUpper c || isDigit c || c == '_') t'
