{-# LANGUAGE OverloadedStrings #-}
-- | The raw FFI: every C function under its C name, with C types.
module Apigen.Language.Haskell.Raw
    ( rawModules
    , rawFunctions
    , rawType
    , registrationName
    , registrationTakesUserData
    ) where

import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Imports (baseScope, importsFor,
                                                  scopeOf)
import           Apigen.Language.Haskell.Model
import           Apigen.Semantic
import           Data.List                       (nub)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           System.FilePath                 (takeFileName)

-- | @FFI.Tox.Types@ and a module per header with functions.
rawModules :: SemanticModel -> [Module]
rawModules model = typesModule model : map (headerModule model) headers
  where
    headers = nub (map fst (rawFunctions model))

rawFunctions :: SemanticModel -> [(Text, Decl)]
rawFunctions model =
    [ (rawModuleFor model r name, Foreign name name (functionType model r m))
    | (r, m) <- allMethods model
    , let name = cFunctionName (mappingOf model r m) ]
    ++ [ (rawModuleFor model r name, Foreign name name (registrationType model r e))
       | r <- resources model, e <- events r
       , let name = registrationName r e ]
    ++ [ (rawModuleFor model r name, Foreign ("&" <> name) ("p_" <> name) (finalizerType r))
       | (r, m) <- allMethods model, methodRole m == Destructor
       , resourceName r `elem` foreignPtrHandles model
       , let name = cFunctionName (mappingOf model r m) ]

typesModule :: SemanticModel -> Module
typesModule model = Module
    { modName    = "FFI.Tox.Types"
    , modDoc     = Just "The C types the raw bindings share: opaque handles and callbacks."
    , modPragmas = []
    , modExports = Nothing
    , modImports = importsFor baseScope [] decls
    , modDecls   = decls
    }
  where
    decls = concatMap handle [ r | r <- resources model, resourceType r == ResHandle ]
        ++ concatMap callback (callbacks model)
    handle r =
        [ DataDecl Nothing (handleName r <> "Struct") [] []
        , TypeAlias Nothing (handleName r <> "Ptr") (TApp (TCon "Ptr") [TCon (handleName r <> "Struct")])
        ]
    callback cb =
        let name = callbackType (cbName cb)
        in [ TypeAlias Nothing name
                (TFun (map (rawType model . paramType) (cbParams cb)) (io TUnit))
           , Foreign "wrapper" ("wrap" <> name)
                (TFun [TCon name] (io (TApp (TCon "FunPtr") [TCon name])))
           ]

headerModule :: SemanticModel -> Text -> Module
headerModule model name = Module
    { modName    = name
    , modDoc     = Just ("Raw bindings to @" <> header <> "@. \"Network.Tox.C\" is the safe API.")
    , modPragmas = []
    , modExports = Nothing
    , modImports = importsFor scope [] decls
    , modDecls   = decls
    }
  where
    header = case [ resourceFile r | r <- resources model, rawModuleName model (resourceFile r) == name ] of
        (f : _) -> Text.pack (takeFileName (Text.unpack f))
        []      -> name
    decls = [ d | (m, d) <- rawFunctions model, m == name ]
    scope = baseScope <> scopeOf "FFI.Tox.Types" typesNames
    typesNames = concat
        [ [handleName r <> "Struct", handleName r <> "Ptr"]
        | r <- resources model, resourceType r == ResHandle ]
        ++ map (callbackType . cbName) (callbacks model)

functionType :: SemanticModel -> SResource -> SMethod -> Type
functionType model res m = case args of
    [] | pureConstant -> ret
    _                 -> TFun args (io ret)
  where
    mapping = mappingOf model res m
    params = cSemParams mapping
    args = map (argType model res mapping params) (argMapping mapping)
    ret = rawType model (cReturnType mapping)
    pureConstant = methodRole m == StaticRole && isScalar (cReturnType mapping)
    isScalar t = case t of
        SUInt _ -> True
        SInt _  -> True
        _       -> False

argType :: SemanticModel -> SResource -> CFunctionMapping -> [SParameter] -> CArgSource -> Type
argType model res mapping params src = case src of
    ThisObject _   -> TCon (handleName res <> "Ptr")
    PathObject n _ -> TCon (handlePtr model (pathResource n))
    PathId n       -> rawType model (idUnderlying model (pathResource n))
    SemanticArg i  -> rawType model (paramType (params !! i))
    ErrorPtr       -> TCon "CErr"
    BufferPtr _    -> case cReturnTypeOf of
        SList t -> TApp (TCon "Ptr") [rawElem model t]
        _       -> bytePtr
    BufferSize     -> TCon "CSize"
    UserData       -> voidPtr
    Constant _     -> TCon "CInt"
  where
    pathResource n = case drop n (hierarchy model res) of
        (r : _) -> r
        []      -> resourceName res
    cReturnTypeOf = resultType model res mapping

-- | What a buffer argument holds: a method's output, or its C return.
resultType :: SemanticModel -> SResource -> CFunctionMapping -> SType
resultType model res mapping =
    case [ output m | m <- methods res, cFunctionName (mappingOf model res m) == cFunctionName mapping ] of
        (t : _) -> t
        []      -> cReturnType mapping

hierarchy :: SemanticModel -> SResource -> [Text]
hierarchy model res = go res
  where
    go r = case parent r >>= findResource of
        Just p  -> go p ++ [resourceName r]
        Nothing -> [resourceName r]
    findResource n = case filter ((== n) . resourceName) (resources model) of
        (r : _) -> Just r
        []      -> Nothing

-- | A callback registration function: @tox_callback_friend_name@.
registrationName :: SResource -> SEvent -> Text
registrationName r e = cPrefix r <> "callback_" <> eventName e

-- | Whether the resource's callbacks get their user data at registration,
-- not when the resource iterates.
registrationTakesUserData :: SResource -> Bool
registrationTakesUserData r =
    not (any (\m -> methodRole m == IterateRole && methodHasUserData m) (methods r))

registrationType :: SemanticModel -> SResource -> SEvent -> Type
registrationType model r e =
    TFun ([TCon (handleName r <> "Ptr"), TApp (TCon "FunPtr") [TCon (callbackTypeOf model (cCallback e))]]
          ++ [voidPtr | registrationTakesUserData r])
         (io TUnit)

finalizerType :: SResource -> Type
finalizerType r =
    TApp (TCon "FunPtr") [TFun [TCon (handleName r <> "Ptr")] (io TUnit)]

rawType :: SemanticModel -> SType -> Type
rawType model t = case t of
    SVoid            -> TUnit
    SBool            -> TCon "CBool"
    SInt n           -> TCon ("Int" <> Text.pack (show n))
    SUInt n          -> TCon ("Word" <> Text.pack (show n))
    SSizeT           -> TCon "CSize"
    SEnum _          -> TCon "CInt"
    SHandle "void"   -> voidPtr
    SHandle h        -> TCon (handlePtr model h)
    SCallback cb     -> TApp (TCon "FunPtr") [TCon (callbackType cb)]
    SResourceId i    -> rawType model (idUnderlying model i)
    SString          -> TCon "CString"
    SBytes           -> bytePtr
    SFixedBytes _ _  -> bytePtr
    SList e          -> TApp (TCon "Ptr") [rawElem model e]
    SFixedList e _ _ -> TApp (TCon "Ptr") [rawElem model e]

rawElem :: SemanticModel -> SType -> Type
rawElem model t = case t of
    SResourceId i -> rawType model (idUnderlying model i)
    _             -> rawType model t

bytePtr, voidPtr :: Type
bytePtr = TApp (TCon "Ptr") [TCon "Word8"]
voidPtr = TApp (TCon "Ptr") [TUnit]

io :: Type -> Type
io t = TApp (TCon "IO") [t]

