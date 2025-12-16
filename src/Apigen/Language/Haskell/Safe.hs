{-# LANGUAGE OverloadedStrings #-}
-- | The safe API: a Haskell function for each C function a client calls,
-- taking Haskell types and returning C's errors as 'Either'.
module Apigen.Language.Haskell.Safe
    ( SafeFunction (..)
    , safeFunctions
    , safeDecls
    , safeModuleOf
    , safeType
    , convertOut
    , contextArg
    , sizeExprWith
    ) where

import qualified Apigen.Knowledge              as K
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Model
import           Apigen.Language.Haskell.Names (typeName, valueName, varName)
import           Apigen.Model                  (declaredElsewhere, headerOf,
                                                isEnumNamer, returnsBorrowed)
import           Apigen.Semantic
import           Apigen.SizeExpr
import           Data.List                     (find, sortOn)
import           Data.Maybe                    (fromMaybe, isJust, mapMaybe)
import           Data.Ord                      (Down (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

data SafeFunction = SafeFunction
    { sfName   :: Text
    , sfCName  :: Text
    , sfModule :: Text
    , sfParams :: [(Text, Text)]   -- ^ C parameter names and the Haskell ones
    , sfArgs   :: [(Pat, Type)]
    , sfResult :: Type
    , sfBody   :: Expr
    } deriving (Show, Eq)

-- | Why a C function has no safe function of its own.
rawOnlyReason :: SemanticModel -> SResource -> SMethod -> Maybe Text
rawOnlyReason model res m
    | isEnumNamer m = Just "'Show' names the values"
    | name `elem` sizeCompanions model = Just "the function it sizes asks"
    | methodRole m == StaticRole && null (argMapping mapping) = Just "a constant in Network.Tox.C.Constants"
    | K.isSettings model res = Just ("the " <> resourceName res <> " record covers it")
    | K.isView model res || K.isViewList model res = Just "Network.Tox.C.Events decodes events"
    | methodRole m == Constructor
    , K.resourceKind model res `elem` [RootHandle, LibraryHandle] = Just "Network.Tox.C.Lifecycle makes it"
    | methodRole m == Destructor = Just "Network.Tox.C.Lifecycle or a finalizer frees it"
    -- tox_iterate runs the callbacks it gets user data for, not toxav_iterate.
    | methodRole m == IterateRole && methodHasUserData m = Just "events come from toxEventsIterate"
    | declaredElsewhere res m =
        Just "tox_events_init sets up Network.Tox.C.Events, and the old group calls take a callback for the conference's life"
    | returnsBorrowed res m = Just "it returns a handle it doesn't own"
    | otherwise = Nothing
  where
    mapping = mappingOf model res m
    name = cFunctionName mapping

-- | The safe module of a resource: @Network.Tox.C.Friend@.
safeModuleOf :: SemanticModel -> SResource -> Text
safeModuleOf model res = "Network.Tox.C." <> case resourceType res of
    _ | isRoot res -> handleName res
    ResId _ -> case hierarchyOf model res of
        (_ : top : _) -> typeName top
        _             -> typeName (resourceName res)
    ResHandle -> Text.drop (Text.length "FFI.Tox.") (rawModuleName model (resourceFile res))

-- | Resource whose module a function goes in: the one in its header with
-- the longest C prefix it has, else its own.
placement :: SemanticModel -> SResource -> Text -> SResource
placement model res name =
    case sortOn (Down . Text.length . cPrefix) candidates of
        (r : _) -> r
        []      -> res
  where
    candidates =
        [ r | r <- resources model
            , resourceFile r == resourceFile res
            , cPrefix r `Text.isPrefixOf` name ]

hierarchyOf :: SemanticModel -> SResource -> [Text]
hierarchyOf model = go
  where
    go r = case parent r >>= \p -> find ((== p) . resourceName) (resources model) of
        Just p  -> go p ++ [resourceName r]
        Nothing -> [resourceName r]

safeFunctions :: SemanticModel -> [SafeFunction]
safeFunctions model =
    [ f | (r, m) <- allMethods model
        , rawOnlyReason model r m == Nothing
        , Just f <- [plan model r m] ]

safeDecls :: (Text -> Maybe Text) -> SafeFunction -> [Decl]
safeDecls doc f =
    [ Signature (doc (sfCName f)) (sfName f) ty
    , Binding (sfName f) (map fst (sfArgs f)) (sfBody f)
    ]
  where
    io = TApp (TCon "IO") [sfResult f]
    ty = case sfArgs f of
        []   -> io
        args -> TFun (map snd args) io

-- | How a C argument is passed: the Haskell parameter it comes from, a
-- continuation it needs, and the expression the raw function gets.
data Passed = Passed
    { pParam :: Maybe (Text, Pat, Type)  -- ^ C name, binding, type
    , pWith  :: Maybe (Expr, [Pat])
    , pExpr  :: Maybe Expr                -- ^ 'Nothing' for buffers and errors
    , pLen   :: Maybe Expr                -- ^ The length, for a 'BufferSize' after it
    }

plan :: SemanticModel -> SResource -> SMethod -> Maybe SafeFunction
plan model res m = do
    passed <- passArgs model res mapping params args
    result <- resultOf model m mapping passed
    let (sfArgs', names) = unzip [ ((pat, ty), (c, binder pat)) | Passed{pParam = Just (c, pat, ty)} <- passed ]
        wrappers = mapMaybe pWith passed
        body = case wrappers of
            [] -> snd result
            ws -> With ws (snd result)
    return SafeFunction
        { sfName   = valueName name
        , sfCName  = name
        , sfModule = safeModuleOf model (placement model res name)
        , sfParams = names
        , sfArgs   = sfArgs'
        , sfResult = fst result
        , sfBody   = body
        }
  where
    mapping = mappingOf model res m
    name = cFunctionName mapping
    params = cSemParams mapping
    args = argMapping mapping
    binder (PVar v)     = v
    binder (PCon _ [p]) = binder p
    binder _            = ""

passArgs :: SemanticModel -> SResource -> CFunctionMapping -> [SParameter] -> [CArgSource] -> Maybe [Passed]
passArgs model res mapping params args = go Nothing (zip args cNames)
  where
    -- An inferred mapping has no C names.
    cNames = map Just (cArgNames mapping) ++ repeat Nothing

    go _ [] = Just []
    go lastLen ((src, cn) : rest) = case src of
        BufferSize -> do
            len <- lastLen
            (Passed Nothing Nothing (Just len) Nothing :) <$> go lastLen rest
        _ -> do
            p <- passOne src cn
            (p :) <$> go (maybe lastLen Just (pLen p)) rest

    passOne src cn = case src of
        ThisObject _   -> contextArg model (resourceName res)
        PathObject n _ -> contextArg model (pathResource n)
        PathId n       -> idArg model (pathResource n) (fromMaybe (pathIdName (pathResource n)) cn)
        SemanticArg i  -> semanticArg model params (params !! i)
        ErrorPtr       -> Just (Passed Nothing Nothing Nothing Nothing)
        BufferPtr _    -> Just (Passed Nothing Nothing Nothing Nothing)
        BufferSize     -> Nothing
        UserData       -> Nothing
        Constant _     -> Nothing

    pathResource n = case drop n (hierarchyOf model res) of
        (r : _) -> r
        []      -> resourceName res

    -- What C usually calls a resource's number: @peer_number@ for a
    -- conference's peers, without the conference.
    pathIdName name =
        let r = find ((== name) . resourceName) (resources model)
            short = case r >>= parent of
                Just p  -> maybe name id (Text.stripPrefix (p <> "_") name)
                Nothing -> name
        in Text.toLower short <> "_number"

-- | A handle the function takes: @Tox tox@.
contextArg :: SemanticModel -> Text -> Maybe Passed
contextArg model name = do
    r <- find ((== name) . resourceName) (resources model)
    let tyName = handleName r
        -- Its name's last word: @tox@, @av@, @key@.
        var = varName (last (Text.splitOn "_" name))
    if name `notElem` handleResources model then Nothing else Just $
        if name `elem` foreignPtrHandles model
            then Passed (Just (name, PCon tyName [PVar var], TCon tyName))
                        (Just (App (Var "withForeignPtr") [Var var], [PVar (var <> "Ptr")]))
                        (Just (Var (var <> "Ptr"))) Nothing
            else Passed (Just (name, handlePat r tyName var, TCon tyName))
                        Nothing (Just (Var var)) Nothing
  where
    -- A handle keeping a handler also holds its callbacks.
    handlePat r tyName var
        | K.keepsHandler model r = PCon tyName [PVar var, PWild]
        | otherwise              = PCon tyName [PVar var]

idArg :: SemanticModel -> Text -> Text -> Maybe Passed
idArg model idRes cParam = do
    newtype' <- idNewtype model idRes
    let var = varName cParam
    return $ Passed (Just (cParam, PCon newtype' [PVar var], TCon newtype'))
                    Nothing (Just (Var var)) Nothing

semanticArg :: SemanticModel -> [SParameter] -> SParameter -> Maybe Passed
semanticArg model params p = case paramType p of
    SBool -> plain (TCon "Bool") (App (Var "fromBool") [Var var])
    SInt n -> plain (TCon ("Int" <> showT n)) (Var var)
    SUInt n -> plain (TCon ("Word" <> showT n)) (Var var)
    SSizeT -> plain (TCon "Int") (App (Var "fromIntegral") [Var var])
    SEnum e -> do
        enum <- findEnum model e
        plain (TCon (typeName (enumSemanticName enum))) (App (Var "toCEnum") [Var var])
    SResourceId i -> idArg model i (paramName p)
    SString -> withPtr (TCon "String") (App (Var "withCString") [Var var]) [PVar ptr]
    SBytes -> Just Passed
        { pParam = Just (paramName p, PVar var, TCon "ByteString")
        , pWith  = Just (App (Var "withBytesLen") [Var var], [PVar ptr, PVar len])
        , pExpr  = Just (Var ptr)
        , pLen   = Just (Var len)
        }
    SFixedBytes size _ -> case arrayType model size of
        Just ty
            | paramNullable p ->
                withPtr (TApp (TCon "Maybe") [TCon ty]) (App (Var "withMaybeFixedBytes") [Var var]) [PVar ptr]
            | otherwise -> withPtr (TCon ty) (App (Var "withFixedBytes") [Var var]) [PVar ptr]
        Nothing -> do
            sizeExpr <- sizeExprFor model params size
            withPtr (TCon "ByteString")
                (App (Var "withSizedBytes") [Str (paramName p), Var var, sizeExpr]) [PVar ptr]
    SFixedList (SInt 16) size _ -> do
        sizeExpr <- sizeExprFor model params size
        withPtr (TApp (TCon "Vector") [TCon "Int16"])
            (App (Var "withSizedVector") [Str (paramName p), Var var, sizeExpr]) [PVar ptr]
    SHandle h | h `elem` handleResources model -> contextArg model h
    _ -> Nothing
  where
    var = varName (paramName p)
    -- Derived names need no prime: @dataPtr@.
    ptr = valueName (paramName p) <> "Ptr"
    len = valueName (paramName p) <> "Len"
    plain ty e = Just (Passed (Just (paramName p, PVar var, ty)) Nothing (Just e) Nothing)
    withPtr ty w pats = Just Passed
        { pParam = Just (paramName p, PVar var, ty)
        , pWith  = Just (w, pats)
        , pExpr  = Just (Var ptr)
        , pLen   = Nothing
        }

sizeExprFor :: SemanticModel -> [SParameter] -> Text -> Maybe Expr
sizeExprFor = sizeExprWith param
  where
    -- Sizes are 'Int's already (other integers convert).
    param p
        | paramType p == SSizeT = Var (varName (paramName p))
        | otherwise = App (Var "fromIntegral") [Var (varName (paramName p))]

-- | A size expression in Haskell, referring to a parameter as @param@ does.
sizeExprWith :: (SParameter -> Expr) -> SemanticModel -> [SParameter] -> Text -> Maybe Expr
sizeExprWith param model params text = do
    e <- parseSizeExpr text
    toHaskell e
  where
    toHaskell e = case e of
        SizeNum n -> Just (Lit n)
        SizeVar v
            | Just base <- Text.stripSuffix "_len" v
            , any ((== base) . paramName) params ->
                Just (App (Var "BS.length") [Var (varName base)])
            | isJust (defineValue model v) -> Just (Var (valueName v))
            | Just p <- find ((== v) . paramName) params -> Just (param p)
            | otherwise -> Nothing
        SizeOp op l r -> do
            l' <- toHaskell l
            r' <- toHaskell r
            Just (Op l' (if op == '/' then "`div`" else Text.singleton op) r')
        SizeCall f fargs -> App (Var f) <$> mapM toHaskell fargs

resultOf :: SemanticModel -> SMethod -> CFunctionMapping -> [Passed] -> Maybe (Type, Expr)
resultOf model m mapping passed
    | Just size <- bufferSize = bufferResult size
    | otherwise = valueResult
  where
    args = argMapping mapping
    hasErr = ErrorPtr `elem` args
    rawCall = App (Var ("Raw." <> cFunctionName mapping))
    callArgs = mapMaybe pExpr passed
    errType = TCon . typeName . enumSemanticName <$> (methodErrorType m >>= findEnum model)
    withErr ty = case errType of
        Just e | hasErr -> TApp (TCon "Either") [e, ty]
        _               -> ty

    bufferSize = case [ s | BufferPtr s <- args ] of
        (s : _) -> Just s
        []      -> Nothing

    -- Arguments up to the buffer, which the helper passes with the error.
    bufferLast = case dropWhile (not . isBuffer) args of
        (_ : rest) -> all (== ErrorPtr) rest
        []         -> False
    isBuffer BufferPtr{} = True
    isBuffer _           = False

    -- @Raw.f a b@, or a lambda when the buffer comes before other arguments.
    bufferCall
        | bufferLast = rawCall beforeBuffer
        | otherwise  = Lam (PVar "buffer" : [PVar "err" | hasErr])
            (rawCall (beforeBuffer ++ [Var "buffer"] ++ afterBuffer ++ [Var "err" | hasErr]))
    (before, after) = break (isBuffer . fst) (zip args passed)
    beforeBuffer = mapMaybe (pExpr . snd) before
    afterBuffer = mapMaybe (pExpr . snd) (drop 1 after)

    bufferResult size = case output m of
        SFixedBytes sizeConst _ -> case arrayType model sizeConst of
            Just ty
                | hasErr -> Just (withErr (TCon ty), App (Var "getFixedBytesErr") [bufferCall])
                | cReturnType mapping == SBool && methodRole m /= StaticRole ->
                    Just (TApp (TCon "Maybe") [TCon ty], App (Var "getFixedBytesMaybe") [bufferCall])
                | otherwise -> Just (TCon ty, App (Var "getFixedBytes") [bufferCall])
            Nothing -> do
                sizeExpr <- sizeExprFor model (cSemParams mapping) (maybe sizeConst id size)
                if hasErr
                    then Just (withErr (TCon "ByteString"), App (Var "getBytesErr") [sizeExpr, bufferCall])
                    else Nothing
        SBytes -> do
            sizeCall <- sizeFunctionCall
            Just $ if hasErr
                then (withErr (TCon "ByteString"), App (Var "getSizedErr") [sizeCall, bufferCall])
                else (TCon "ByteString", App (Var "getSized") [sizeCall, bufferCall])
        SList elemTy | not hasErr -> do
            sizeCall <- sizeFunctionCall
            elemSafe <- safeType model elemTy
            let listed = App (Var "getList") [sizeCall, bufferCall]
            Just $ case convertOut model elemTy of
                Just f  -> (TApp (TCon "[]") [elemSafe], Op (App (Var "map") [f]) "<$>" listed)
                Nothing -> (TApp (TCon "[]") [elemSafe], listed)
        _ -> Nothing

    sizeFunctionCall = do
        sizeName <- cSizeFunctionName mapping
        (sr, sm) <- find (\(r, x) -> cFunctionName (mappingOf model r x) == sizeName) (allMethods model)
        let sizeArgs = argMapping (mappingOf model sr sm)
            exprs = [ e | (a, p) <- zip args passed, a `elem` sizeArgs, a /= ErrorPtr, Just e <- [pExpr p] ]
        return (App (Var ("Raw." <> sizeName)) exprs)

    valueResult = case methodResultStrategy m of
        _ | SHandle h <- output m, h `elem` foreignPtrHandles model, hasErr -> do
            r <- find ((== h) . resourceName) (resources model)
            free <- find ((== Destructor) . methodRole) (methods r)
            let freeName = cFunctionName (mappingOf model r free)
                adopt = Op (App (Var "fmap") [Var (handleName r)]) "."
                    (App (Var "newForeignPtr") [Var ("Raw.p_" <> freeName)])
            Just ( withErr (TCon (handleName r))
                 , Op (App (Var "callErrFun") [rawCall callArgs]) ">>="
                      (App (Var "traverse") [adopt]) )
        IgnoreReturn
            | hasErr    -> Just (withErr TUnit, App (Var "callErrFun_") [rawCall callArgs])
            | otherwise -> Just (TUnit, App (Var "void") [rawCall callArgs])
        _ -> do
            ty <- safeType model (output m)
            let call = rawCall callArgs
            Just $ case (hasErr, convertOut model (output m)) of
                (True, Just f)   -> (withErr ty, Op (App (Var "fmap") [f]) "<$>" (App (Var "callErrFun") [call]))
                (True, Nothing)  -> (withErr ty, App (Var "callErrFun") [call])
                (False, Just f)  -> (ty, Op f "<$>" call)
                (False, Nothing) -> (ty, call)

safeType :: SemanticModel -> SType -> Maybe Type
safeType model t = case t of
    SVoid           -> Just TUnit
    SBool           -> Just (TCon "Bool")
    SInt n          -> Just (TCon ("Int" <> showT n))
    SUInt n         -> Just (TCon ("Word" <> showT n))
    SSizeT          -> Just (TCon "Int")
    SEnum e         -> TCon . typeName . enumSemanticName <$> findEnum model e
    SResourceId i   -> TCon <$> idNewtype model i
    SBytes          -> Just (TCon "ByteString")
    SString         -> Just (TCon "String")
    SFixedBytes s _ -> Just (TCon (maybe "ByteString" id (arrayType model s)))
    SList e         -> (\x -> TApp (TCon "[]") [x]) <$> safeType model e
    _               -> Nothing

convertOut :: SemanticModel -> SType -> Maybe Expr
convertOut model t = case t of
    SBool         -> Just (Var "toBool")
    SEnum _       -> Just (Var "fromCEnum")
    SSizeT        -> Just (Var "fromIntegral")
    SResourceId i -> Var <$> idNewtype model i
    _             -> Nothing

showT :: Show a => a -> Text
showT = Text.pack . show
