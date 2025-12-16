{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
module Apigen.Language.Haskell (generate, generateRaw, generateSafe, Options (..), hasCFunction) where

import           Apigen.Inference               (deriveHandleName, getHierarchy,
                                                 inferCFunctionMapping)
import qualified Apigen.Inference               as I
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Pretty (render)
import           Apigen.Semantic                hiding (location)
import           Apigen.Types                   (Constness (..))
import           Data.Char                      (toUpper)
import           Data.List                      (find, groupBy, lookup, nub,
                                                 nubBy, partition, sortOn,
                                                 zipWith, zipWith3)
import           Data.Maybe                     (catMaybes, fromMaybe, isJust,
                                                 listToMaybe, mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Text.Casing                    as Casing

data Options = Options
    {
      importTypes :: Maybe Text
    , typesOnly   :: Bool
    }

generateSafe :: Options -> SemanticModel -> [(FilePath, Text)]
generateSafe opts model =
    let
        -- Reuse existing generate logic but adapt it to use FFI.Tox.Raw
        -- For now, let's just alias generate to see if we can get the structure right
        -- We will need to modify generate to optionally use Raw imports instead of foreign imports

        -- Actually, we should refactor generate to be configurable.
        -- But for this task, I'll duplicate/adapt the generateModule logic to generateSafeModule

        modules = generateSafeModules opts model

        moduleNames = map (\(fp, _) -> Text.replace ".hs" "" $ Text.replace "/" "." $ Text.pack fp) modules

        toxModule = generateAggregateModule "FFI.Tox.Tox" ("FFI.Tox.Types" : moduleNames)
        optionsModule = generateAggregateModule "FFI.Tox.ToxOptions" ["FFI.Tox.Options"]
        logLevelModule = generateAggregateModule "FFI.Tox.ToxLogLevel" ["FFI.Tox.Types"]

        types = ("FFI/Tox/Types.hs", render $ generateTypes opts model)
        raw = generateRaw opts model

    in if typesOnly opts then types : raw else types : modules ++ [toxModule, optionsModule, logLevelModule] ++ raw

generateSafeModules :: Options -> SemanticModel -> [(FilePath, Text)]
generateSafeModules opts model@SemanticModel{..} =
    let grouped = groupByPrefix resources
    in map (generateSafeModule opts model) grouped

generateSafeModule :: Options -> SemanticModel -> [SResource] -> (FilePath, Text)
generateSafeModule _opts model resources =
    let modName = case resources of
            (r:_) -> getModuleName r
            []    -> error "generateSafeModule: empty resources"

        -- Use modified generator that calls Raw functions
        functions = concatMap (generateSafeResourceFunctions model (resourcesFromModel model)) resources

        getDeclKey :: HsDecl -> Maybe (Text, Text)
        getDeclKey (HsDataDecl n _ _)        = Just ("Data", n)
        getDeclKey (HsNewtypeDecl n _ _)     = Just ("Newtype", n)
        getDeclKey (HsDataEnumDecl n _ _)    = Just ("Enum", n)
        getDeclKey (HsTypeAlias n _)         = Just ("Type", n)
        getDeclKey (HsForeignImport _ n _ _) = Just ("Foreign", n)
        getDeclKey (HsFunSig n _)            = Just ("Sig", n)
        getDeclKey (HsFunBind n _ _)         = Just ("Bind", n)
        getDeclKey _                         = Nothing

        dedupDecl a b = case (getDeclKey a, getDeclKey b) of
            (Just ka, Just kb) -> ka == kb
            (Nothing, Nothing) -> a == b
            _                  -> False

        uniqueFunctions = nubBy dedupDecl functions

        -- imports = [ HsImport "FFI.Tox.Raw" True (Just "Raw") Nothing ] -- Import Raw qualified

        -- Same infrastructure as generateModule but with Safe body generation
    in (Text.unpack (Text.replace "." "/" modName) <> ".hs", render $ HsModule
        {
          hsModName = modName
        , hsModExports = []
        , hsModImports =
            [ HsImport "Data.MessagePack" False Nothing (Just ["MessagePack"])
            , HsImport "Data.Int" False Nothing (Just ["Int32"])
            , HsImport "Data.Word" False Nothing (Just ["Word8", "Word16", "Word32", "Word64"])
            , HsImport "Data.ByteString" False Nothing (Just ["ByteString"])
            , HsImport "Data.ByteString" True (Just "BS") Nothing
            , HsImport "Foreign.C.Enum" False Nothing (Just ["CEnum(..)", "CErr", "callErrFun", "toCEnum", "fromCEnum"])
            , HsImport "Foreign.C.String" False Nothing (Just ["CString", "withCString"])
            , HsImport "Foreign.C.Types" False Nothing (Just ["CInt(..)", "CSize(..)", "CBool(..)"])
            , HsImport "Foreign.Ptr" False Nothing (Just ["FunPtr", "Ptr", "nullPtr", "castPtr", "castFunPtrToPtr", "castPtrToFunPtr"])
            , HsImport "Foreign.ForeignPtr" False Nothing (Just ["ForeignPtr", "withForeignPtr", "newForeignPtr"])
            , HsImport "Foreign.Storable" False Nothing (Just ["Storable"])
            , HsImport "Foreign.Marshal.Alloc" False Nothing (Just ["alloca"])
            , HsImport "Foreign.Marshal.Array" False Nothing (Just ["allocaArray", "peekArray"])
            , HsImport "Foreign.Marshal.Utils" False Nothing (Just ["fromBool", "toBool"])
            , HsImport "Control.Monad" False Nothing (Just ["(>=>)"])
            , HsImport "GHC.Generics" False Nothing (Just ["Generic"])
            , HsImport "FFI.Tox.Types" False Nothing Nothing
            , HsImport "FFI.Tox.Raw" True (Just "Raw") Nothing
            ] ++ (if modName == "FFI.Tox.Core" then [] else [HsImport "FFI.Tox.Core" False Nothing Nothing])
        , hsModDecls = uniqueFunctions
        , hsModOptions = ["-Wno-unused-imports", "-Wno-unused-matches", "-Wno-unused-local-binds"]
        , hsModPragmas = ["DeriveGeneric", "GeneralizedNewtypeDeriving"]
        })

generateSafeResourceFunctions :: SemanticModel -> [SResource] -> SResource -> [HsDecl]
generateSafeResourceFunctions model allRes res =
    concatMap (generateSafeMethod model allRes res) (methods res)
    ++ concatMap (generateSafeProperty model allRes res) (properties res)

generateSafeMethod :: SemanticModel -> [SResource] -> SResource -> SMethod -> [HsDecl]
generateSafeMethod model allRes res method =
    let mapping = case methodMapping method of
            StandardMapping -> inferCFunctionMapping allRes res method
            CustomMapping m -> m

        -- Use Raw function name
        cFunc = "Raw." <> idToHaskellCamel (cFunctionName mapping)

        wrapperName = idToHaskellCamel (methodName method)
        wrapperSig = generateWrapperSignature model allRes res (inputs method) (output method) (methodErrorType method) (methodRole method)
        wrapperBody = generateSafeWrapperBody model allRes res (inputs method) (output method) mapping cFunc (methodRole method)

        sigDecl = HsFunSig wrapperName wrapperSig
        bindDecl = HsFunBind wrapperName (collectBindArgs model allRes res (inputs method) (methodRole method)) wrapperBody

    in [sigDecl, bindDecl]

generateSafeProperty :: SemanticModel -> [SResource] -> SResource -> SProperty -> [HsDecl]
generateSafeProperty model allRes res prop =
    let
        findMethod mName = find (\m -> methodName m == mName) (methods res)

        getter = propRead prop >>= findMethod
        setter = propWrite prop >>= findMethod
        sizeGetter = propSize prop >>= findMethod

        ty = propType prop

        genWrapper m role _ retType errType =
             let mapping = case methodMapping m of
                     StandardMapping  -> inferCFunctionMapping allRes res m
                     CustomMapping cm -> cm

                 cFunc = "Raw." <> idToHaskellCamel (cFunctionName mapping)

                 wrapperName = idToHaskellCamel (cFunctionName mapping)
                 wrapperSig = generateWrapperSignature model allRes res (inputs m) retType errType role
                 wrapperBody = generateSafeWrapperBody model allRes res (inputs m) retType mapping cFunc role

                 sigDecl = HsFunSig wrapperName wrapperSig
                 bindDecl = HsFunBind wrapperName (collectBindArgs model allRes res (inputs m) role) wrapperBody
             in [sigDecl, bindDecl]

        genFunc _ Nothing = []
        genFunc typeHint (Just m) =
             let role = GetterRole
             in genWrapper m role typeHint typeHint (methodErrorType m)

        genSetter (Just m) =
             let role = SetterRole
                 retType = case methodMapping m of
                     StandardMapping  -> I.inferCFunctionMapping allRes res m
                     CustomMapping cm -> cm
                 cRet = cReturnType retType
             in genWrapper m role cRet cRet (methodErrorType m)
        genSetter _ = []

    in genFunc ty getter ++ genSetter setter ++ genFunc SSizeT sizeGetter

-- Similar to generateWrapperBody but uses Safe types and Raw calls
generateSafeWrapperBody :: SemanticModel -> [SResource] -> SResource -> [SParameter] -> SType -> CFunctionMapping -> Text -> SMethodRole -> HsExpr
generateSafeWrapperBody model allRes res semParams retType mapping rawFuncName role =
    let
        args = argMapping mapping

        paramNames = zipWith (\p i -> (paramName p, safeName (idToHaskellCamel (paramName p)), i)) semParams ([0..] :: [Int])
        pName i = case find (\(n, _, _) -> n == paramName (semParams !! i)) paramNames of
             Just (_, s, _) -> s
             Nothing        -> "undefined"

        mkWrapper (name, SBytes, i) body =
             EApp (EApp (EVar "BS.useAsCStringLen") (EVar name))
                  (ELam [PTuple [PVar ("ptr" <> Text.pack (show i)), PVar ("len" <> Text.pack (show i))]] body)
        mkWrapper (name, SString, i) body =
             EApp (EApp (EVar "BS.useAsCString") (EVar name))
                  (ELam [PVar ("ptr" <> Text.pack (show i))] body)
        mkWrapper (name, SFixedBytes _ _, i) body =
             EApp (EApp (EVar "BS.useAsCString") (EVar name))
                  (ELam [PVar ("ptr" <> Text.pack (show i))] body)
        mkWrapper _ body = body

        wrappers = map (\(_, s, i) -> (s, paramType (semParams !! i), i)) paramNames
        wrappedCall = foldr mkWrapper innerBody wrappers

        -- Buffer handling
        isBufferPtr (BufferPtr _) = True
        isBufferPtr _             = False
        hasBuffer = any isBufferPtr args

        bufferVar = "outPtr"

        -- Context args logic
        hierarchy = getHierarchy allRes res
        isIdRes = case resourceType res of { ResId _ -> True; _ -> False }

        contextArgNames = if isIdRes
             then
                 let relevantHierarchy = if role == Constructor || role == StaticRole
                                         then reverse (drop 1 (reverse (drop 1 hierarchy)))
                                         else drop 1 hierarchy
                     numIds = length relevantHierarchy
                 in "this" : map (\i -> "id" <> Text.pack (show i)) ([1..numIds] :: [Int])
             else
                 if role /= Constructor && role /= StaticRole
                 then ["this"]
                 else []

        contextArgExprs = map EVar contextArgNames

        -- Size call args
        sizeCallArgs = contextArgExprs ++ (if hasErrorPtr then [EVar "nullPtr"] else [])

        (innerBody, _) = if hasBuffer
            then
               let
                   (sizeStmt, sizeVar) = case (cSizeFunctionName mapping, retType) of
                       (Just sizeFn, _) ->
                           let haskellName = idToHaskellCamel sizeFn

                               checkProp propToCheck resToCheck =
                                   let getMethod mName = find (\m -> methodName m == mName) (methods resToCheck)

                                       methods' = catMaybes [ propRead propToCheck >>= getMethod
                                                            , propWrite propToCheck >>= getMethod
                                                            , propSize propToCheck >>= getMethod
                                                            ]

                                       extract m = case methodMapping m of
                                           CustomMapping cm -> idToHaskellCamel (cFunctionName cm) == haskellName
                                           _ -> False
                                   in any extract methods'

                               isMethodName = any (\resToCheck ->
                                       any (\m -> idToHaskellCamel (methodName m) == haskellName) (methods resToCheck) ||
                                       any (\p -> checkProp p resToCheck) (properties resToCheck)
                                   ) allRes

                               targetName = if isMethodName then haskellName <> "_Const" else haskellName

                               isInConstants = any (\c -> constantName c == sizeFn) (constants model)

                               takesArgs = checkSizeFnArgs allRes sizeFn

                           in if sizeFn == "TOX_ADDRESS_SIZE"
                              then (SLet [HsFunBind "size" [] (EVar "Raw.toxAddressSize")], "size")
                              else if takesArgs
                              then (SGenerator (PVar "size") (foldl EApp (EVar ("Raw." <> haskellName)) sizeCallArgs), "size")
                              else if isInConstants
                                   then (SLet [HsFunBind "size" [] (EVar ("FFI.Tox.Types." <> targetName))], "size")
                                   else (SLet [HsFunBind "size" [] (EVar ("Raw." <> haskellName))], "size")
                       (Nothing, SFixedBytes sizeName _) ->
                           let haskellName = idToHaskellCamel sizeName
                               isMethodName = any (\resToCheck ->
                                       any (\m -> idToHaskellCamel (methodName m) == haskellName) (methods resToCheck) ||
                                       any (\p -> checkProp p resToCheck) (properties resToCheck)
                                   ) allRes

                               checkProp propToCheck resToCheck =
                                   let getMethod mName = find (\m -> methodName m == mName) (methods resToCheck)

                                       methods' = catMaybes [ propRead propToCheck >>= getMethod
                                                            , propWrite propToCheck >>= getMethod
                                                            , propSize propToCheck >>= getMethod
                                                            ]

                                       extract m = case methodMapping m of
                                           CustomMapping cm -> idToHaskellCamel (cFunctionName cm) == haskellName
                                           _ -> False
                                   in any extract methods'

                               targetName = if isMethodName then haskellName <> "_Const" else haskellName
                           in if sizeName == "TOX_ADDRESS_SIZE"
                              then (SLet [HsFunBind "size" [] (EVar "Raw.toxAddressSize")], "size")
                              else (SLet [HsFunBind "size" [] (EVar ("FFI.Tox.Types." <> targetName))], "size")
                       _ -> (SLet [HsFunSig "size" (TyCon "CSize"), HsFunBind "size" [] (ELit (LInt 0))], "size")

                   allocApp = EApp (EVar "allocaArray") (EApp (EVar "fromIntegral") (EVar sizeVar))
                   allocLam = ELam [PVar bufferVar] bodyWithErr

                   callArgs = resolveArgs args (-1) True

                   packCode = case retType of
                       SList (SEnum _) ->
                           [ SGenerator (PVar "raw") (EApp (EApp (EVar "peekArray") (EApp (EVar "fromIntegral") (EVar sizeVar))) (EVar bufferVar))
                           , SLet [HsFunBind "val" [] (EVar "raw")]
                           ]
                       SList _ ->
                           [ SGenerator (PVar "val") (EApp (EApp (EVar "peekArray") (EApp (EVar "fromIntegral") (EVar sizeVar))) (EVar bufferVar)) ]
                       _ ->
                           [ SGenerator (PVar "val") (EApp (EVar "BS.packCStringLen") (ETuple [EVar bufferVar, EApp (EVar "fromIntegral") (EVar sizeVar)])) ]

                   resReturn = if hasErrorPtr
                               then EApp (EVar "return") (EApp (EVar "Right") (EVar "val"))
                               else EApp (EVar "return") (EVar "val")

                   bodyWithErr = EDo $
                       if hasErrorPtr
                       then [ SGenerator (PVar "res") (callExpr callArgs)
                            , SExpr $ ECase (EVar "res")
                                [ HsAlt (PCon "Left" [PVar "err"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "err")))
                                , HsAlt (PCon "Right" [PWildCard]) (EDo (packCode ++ [SExpr resReturn]))
                                ]
                            ]
                       else [ SGenerator (PVar "_") (callExpr callArgs) ] ++ packCode ++
                            [ SExpr resReturn ]

               in (EDo [sizeStmt, SExpr (EApp allocApp allocLam)], callArgs)
            else
               let
                   callArgs = resolveArgs args (-1) False
                   baseCall = callExpr callArgs

                   convert expr =
                       if False -- isPure is always False for Raw
                       then expr
                       else if hasErrorPtr
                       then case retType of
                           SEnum _ -> expr
                           SBool   -> expr
                           SInt _  -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SUInt _ -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SSizeT  -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SString -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           SFixedBytes _ _ -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           SBytes -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           SVoid -> expr
                           SHandle _ -> expr
                           SResourceId _ -> expr
                           _ -> expr
                       else case retType of
                           SEnum _ -> expr
                           SBool   -> expr
                           SInt _  -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SUInt _ -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SSizeT  -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SString -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           SFixedBytes _ _ -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           SBytes -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           SVoid -> expr
                           SHandle _ -> expr
                           SResourceId _ -> expr
                           _ -> expr

               in (convert baseCall, callArgs)

        resolveArgs [] _ _ = []
        resolveArgs (arg:rest) lastIdx useBuffer =
             case arg of
                 SemanticArg i ->
                     let val = mapArg arg
                         nextIdx = if isBytes (semParams !! i) then i else lastIdx
                     in val : resolveArgs rest nextIdx useBuffer
                 BufferSize ->
                     let val = EParen (EApp (EVar "fromIntegral") (EVar ("len" <> Text.pack (show lastIdx))))
                     in val : resolveArgs rest lastIdx useBuffer
                 BufferPtr _ -> (if useBuffer then EVar bufferVar else EVar "undefined") : resolveArgs rest lastIdx useBuffer
                 _ -> mapArg arg : resolveArgs rest lastIdx useBuffer

        isBytes p = case paramType p of
            SBytes          -> True
            SFixedBytes _ _ -> True
            SString         -> True
            _               -> False

        mapArg (ThisObject _) = EVar "this"
        mapArg (PathObject _ _) = EVar "this"
        mapArg (PathId n) = EVar ("id" <> Text.pack (show n))
        mapArg (SemanticArg i) =
             let name = pName i
                 ty = paramType (semParams !! i)
             in case ty of
                 SBytes -> EParen (EApp (EVar "castPtr") (EVar ("ptr" <> Text.pack (show i))))
                 SString -> EVar ("ptr" <> Text.pack (show i))
                 SFixedBytes _ _ -> EParen (EApp (EVar "castPtr") (EVar ("ptr" <> Text.pack (show i))))
                 SEnum _ -> EVar name -- Raw takes Enum
                 SBool   -> EVar name -- Raw takes Bool
                 SInt _ -> EParen (EApp (EVar "fromIntegral") (EVar name))
                 SUInt _ -> EParen (EApp (EVar "fromIntegral") (EVar name))
                 _ -> EVar name
        mapArg ErrorPtr = EParen (EApp (EVar "castPtr") (EVar "errPtr"))
        mapArg _ = EVar "undefined"

        hasErrorPtr = any isErrorPtr args
        isErrorPtr ErrorPtr = True
        isErrorPtr _        = False

        callExpr argsList =
            let f = EVar rawFuncName
                apply = foldl EApp f (map (\a -> if a == EVar "undefined" then EVar "errPtr" else a) argsList)
            in if hasErrorPtr
               then EApp (EVar "callErrFun") (ELam [PVar "errPtr"] apply)
               else foldl EApp f (map (\a -> if a == EVar "undefined" then EVar "nullPtr" else a) argsList)

    in wrappedCall

generateRaw :: Options -> SemanticModel -> [(FilePath, Text)]
generateRaw _opts model =
    let
        -- Generate typedefs for all enums to ensure c2hs can find them by name
        -- even if they are only defined as tags (e.g. enum Tag) in the header.
        -- We put this in a wrapper header because c2hs ignores C code in the .chs preamble
        -- for symbol lookup purposes (it only copies it to output), but it DOES read #includes.
        genTypedef SEnumModel{..} = "typedef enum " <> enumName <> " " <> enumName <> ";\n"
        wrapperContent = "#include <tox/tox.h>\n" <> Text.concat (map genTypedef (enums model))

        context = "{# context lib=\"toxcore\" #}\n\n"

        genEnum SEnumModel{..} =
             let hsName = idToHaskell enumSemanticName
                 cName = enumName
             in "{# enum " <> cName <> " as " <> hsName <> " { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}\n"
             <> "instance Storable " <> hsName <> " where\n"
             <> "  sizeOf _ = {# sizeof " <> cName <> " #}\n"
             <> "  alignment _ = {# alignof " <> cName <> " #}\n"
             <> "  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))\n"
             <> "  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))\n"

        enums_ = map genEnum (enums model)

        genDerivings SEnumModel{..} =
             let hsName = idToHaskell enumSemanticName
             in "deriving instance Bounded " <> hsName <> "\n"
             <> "deriving instance Read " <> hsName <> "\n"

        derivings_ = Text.concat (map genDerivings (enums model))

        resources_ = resources model

        genResource r =
            let name = idToHaskell (resourceName r)

                structDecl = case resourceType r of
                    ResHandle ->
                        "data " <> name <> "Struct\n" <>
                        "type " <> name <> "Ptr = Ptr " <> name <> "Struct\n"
                    _ -> ""

                methods_ = map (genMethod r) (methods r)
            in structDecl <> "\n" <> Text.concat methods_

        -- Helper functions for marshalling
        helpers = "enumToCInt :: Enum a => a -> CInt\n"
               <> "enumToCInt = fromIntegral . fromEnum\n\n"
               <> "cIntToEnum :: Enum a => CInt -> a\n"
               <> "cIntToEnum = toEnum . fromIntegral\n\n"
               <> "ptrToPtr :: Ptr a -> Ptr b\n"
               <> "ptrToPtr = castPtr\n\n"

        genMethod r m =
            let mapping = case methodMapping m of
                    StandardMapping  -> inferCFunctionMapping resources_ r m
                    CustomMapping cm -> cm
                cFunc = cFunctionName mapping
                cArgs = argMapping mapping
                semParams = cSemParams mapping

                (argTypes, argMarshals) = unzip $ map (toChsType (enums model) resources_ r semParams (cErrorType mapping)) (zip cArgs [0..])
                retType = toChsRetType (enums model) resources_ (cReturnType mapping)

                -- Input marshaller construction
                argsStr = Text.intercalate ", " (zipWith (\t m' -> if Text.null t then "" else m' <> " `" <> t <> "'") argTypes argMarshals)

                -- Output marshaller check
                retMarshaller = getRetMarshaller (cReturnType mapping)

            in "{# fun " <> cFunc <> " as ^ { " <> argsStr <> " } -> `" <> retType <> "' " <> retMarshaller <> " #}\n"

        chsContent = "{-# LANGUAGE DeriveGeneric #-}\n{-# LANGUAGE StandaloneDeriving #-}\n#include <FFI/Tox/tox_wrapper.h>\n\nmodule FFI.Tox.Raw where\n\nimport Foreign.Ptr\nimport Foreign.C.Types\nimport Foreign.C.String\nimport Foreign.Storable\nimport Foreign.Marshal.Utils (toBool, fromBool)\nimport GHC.Generics\nimport Data.Word\nimport Data.Int\n\ntype VoidPtr = Ptr ()\n\n"
                  <> helpers
                  <> context
                  <> Text.concat enums_
                  <> Text.concat (map genResource resources_)
                  <> derivings_

    in [ ("FFI/Tox/Raw.chs", chsContent)
       , ("FFI/Tox/tox_wrapper.h", wrapperContent)
       ]

getRetMarshaller :: SType -> Text
getRetMarshaller (SHandle _)     = "ptrToPtr"
getRetMarshaller (SList _)       = "ptrToPtr" -- Lists are pointers
getRetMarshaller SBytes          = "ptrToPtr"
getRetMarshaller SString         = "ptrToPtr"
getRetMarshaller SSizeT          = "fromIntegral"
getRetMarshaller (SEnum _)       = "cIntToEnum"
getRetMarshaller SBool           = "toBool"
getRetMarshaller (SResourceId _) = "fromIntegral"
getRetMarshaller _               = ""

resolveEnumName :: [SEnumModel] -> Text -> Text
resolveEnumName enumModels cName =
    case find (\e -> enumName e == cName) enumModels of
        Just e  -> idToHaskell (enumSemanticName e)
        Nothing -> idToHaskell cName

toChsType :: [SEnumModel] -> [SResource] -> SResource -> [SParameter] -> Maybe Text -> (CArgSource, Int) -> (Text, Text)
toChsType enumModels allRes res semParams mErr (src, _) = case src of
    ThisObject _ -> (idToHaskell (resourceName res) <> "Ptr", "ptrToPtr")
    SemanticArg i ->
        let ty = paramType (semParams !! i)
            hsTy = toChsParamType enumModels allRes ty
            marshal = case ty of
                SEnum _          -> "enumToCInt"
                SBool            -> "fromBool"
                SSizeT           -> "fromIntegral"
                SInt _           -> "fromIntegral"
                SUInt _          -> "fromIntegral"
                SResourceId _    -> "fromIntegral"
                SHandle _        -> "ptrToPtr"
                SCallback _      -> "castPtrToFunPtr"
                SString          -> "ptrToPtr"
                SBytes           -> "ptrToPtr"
                SFixedBytes _ _  -> "ptrToPtr"
                SList _          -> "ptrToPtr"
                SFixedList _ _ _ -> "ptrToPtr"
                _                -> "id"
        in (hsTy, marshal)
    ErrorPtr -> case mErr of
        Just e  -> ("Ptr " <> resolveEnumName enumModels e, "ptrToPtr")
        Nothing -> ("Ptr CInt", "ptrToPtr")
    BufferSize -> ("CSize", "fromIntegral")
    BufferPtr _ -> ("Ptr CUChar", "ptrToPtr")
    PathObject n _ ->
        let tyName = I.resolvePathTarget allRes res n
        in (idToHaskell tyName <> "Ptr", "ptrToPtr")
    PathId _ -> ("Word32", "fromIntegral")
    UserData -> ("Ptr ()", "ptrToPtr")
    Constant _ -> ("CInt", "id")

toChsParamType :: [SEnumModel] -> [SResource] -> SType -> Text
toChsParamType _ _ (SHandle h)          = idToHaskell h <> "Ptr"
toChsParamType _ _ (SInt 8)             = "CSChar"
toChsParamType _ _ (SInt 16)            = "CShort"
toChsParamType _ _ (SInt 32)            = "CInt"
toChsParamType _ _ (SInt 64)            = "CLLong"
toChsParamType _ _ (SUInt 8)            = "CUChar"
toChsParamType _ _ (SUInt 16)           = "CUShort"
toChsParamType _ _ (SUInt 32)           = "CUInt"
toChsParamType _ _ (SUInt 64)           = "CULLong"
toChsParamType _ _ SSizeT               = "CSize"
toChsParamType ems _ (SEnum n)          = resolveEnumName ems n
toChsParamType _ _ SBool                = "Bool" -- Use Bool for inputs too (will marshal to CInt)
toChsParamType _ _ SString              = "CString"
toChsParamType _ _ SBytes               = "Ptr CUChar"
toChsParamType _ _ (SFixedBytes _ _)    = "Ptr CUChar"
toChsParamType ems _ (SList t)          = "Ptr " <> toChsParamType ems [] t
toChsParamType ems _ (SFixedList t _ _) = "Ptr " <> toChsParamType ems [] t
toChsParamType _ _ (SResourceId _)      = "Word32"
toChsParamType _ _ _                    = "Ptr ()"

toChsRetType :: [SEnumModel] -> [SResource] -> SType -> Text
toChsRetType _ _ (SHandle h)          = idToHaskell h <> "Ptr"
toChsRetType _ _ (SResourceId _)      = "Word32"
toChsRetType _ _ SVoid                = "()"
toChsRetType _ _ (SUInt 8)            = "CUChar"
toChsRetType _ _ (SUInt 16)           = "CUShort"
toChsRetType _ _ (SUInt 32)           = "CUInt"
toChsRetType _ _ (SUInt 64)           = "CULLong"
toChsRetType _ _ (SInt 8)             = "CSChar"
toChsRetType _ _ (SInt 16)            = "CShort"
toChsRetType _ _ (SInt 32)            = "CInt"
toChsRetType _ _ (SInt 64)            = "CLLong"
toChsRetType _ _ SSizeT               = "CSize"
toChsRetType _ _ SBool                = "Bool"
toChsRetType ems _ (SEnum n)          = resolveEnumName ems n
toChsRetType _ _ SString              = "CString"
toChsRetType _ _ SBytes               = "Ptr CUChar"
toChsRetType _ _ (SFixedBytes _ _)    = "Ptr CUChar"
toChsRetType ems _ (SList t)          = "Ptr " <> toChsParamType ems [] t
toChsRetType ems _ (SFixedList t _ _) = "Ptr " <> toChsParamType ems [] t
toChsRetType _ _ _                    = "()"

generate :: Options -> SemanticModel -> [(FilePath, Text)]
generate opts model =
    let types = ("FFI/Tox/Types.hs", render $ generateTypes opts model)
        modules = generateModules opts model
        raw = generateRaw opts model

        moduleNames = map (\(fp, _) -> Text.replace ".hs" "" $ Text.replace "/" "." $ Text.pack fp) modules

        toxModule = generateAggregateModule "FFI.Tox.Tox" ("FFI.Tox.Types" : moduleNames)
        optionsModule = generateAggregateModule "FFI.Tox.ToxOptions" ["FFI.Tox.Options"]
        logLevelModule = generateAggregateModule "FFI.Tox.ToxLogLevel" ["FFI.Tox.Types"]

    in if typesOnly opts then types : raw else types : modules ++ [toxModule, optionsModule, logLevelModule] ++ raw

generateAggregateModule :: Text -> [Text] -> (FilePath, Text)
generateAggregateModule name imports =
    (Text.unpack (Text.replace "." "/" name) <> ".hs", render $ HsModule
        {
          hsModName = name
        , hsModExports = map ("module " <>) imports
        , hsModImports = map (\m -> HsImport m False Nothing Nothing) imports
        , hsModDecls = []
        , hsModOptions = ["-Wno-unused-imports", "-Wno-dodgy-exports"]
        , hsModPragmas = []
        })

generateTypes :: Options -> SemanticModel -> HsModule
generateTypes _opts model =
    let errorTypes = collectErrorTypes model
        -- Enums and Structs are now in Raw
        idTypes_ = concatMap generateIdType (idTypes model)
        usedNames = collectUsedNames (resources model)
        consts_ = concatMap (generateConstant usedNames) (constants model)
        callbacks_ = concatMap (generateCallback (resources model)) (callbacks model)

        enumInstances = concatMap (generateEnumInstances errorTypes) (enums model)

        -- Construct export list for Raw types (Enums, Structs, Ptrs)
        rawExports = map (\e -> idToHaskell (enumSemanticName e) <> "(..)") (enums model)
                  ++ concatMap getResExports (resources model)

        getResExports r =
            let name = idToHaskell (resourceName r)
            in case resourceType r of
                ResHandle -> [name <> "Struct", name <> "Ptr"]
                _         -> []

        -- Local exports (Id types, constants, callbacks)
        localExports = map getDeclName (idTypes_ ++ consts_ ++ callbacks_)

        getDeclName (HsNewtypeDecl n _ _)     = n <> "(..)"
        getDeclName (HsFunSig n _)            = n
        getDeclName (HsTypeAlias n _)         = n
        getDeclName (HsForeignImport _ n _ _) = n -- Wrapper callback
        getDeclName _                         = ""

    in HsModule
        {
          hsModName = "FFI.Tox.Types"
        , hsModExports = rawExports ++ filter (not . Text.null) localExports
        , hsModImports =
            [ HsImport "Data.MessagePack" False Nothing (Just ["MessagePack"])
            , HsImport "Data.Int" False Nothing (Just ["Int32"])
            , HsImport "Data.Word" False Nothing (Just ["Word8", "Word16", "Word32", "Word64"])
            , HsImport "Foreign.C.Enum" False Nothing (Just ["CEnum(..)", "CErr"])
            , HsImport "Foreign.C.String" False Nothing (Just ["CString"])
            , HsImport "Foreign.C.Types" False Nothing (Just ["CInt(..)", "CSize(..)", "CBool(..)"])
            , HsImport "Foreign.Ptr" False Nothing (Just ["FunPtr", "Ptr"])
            , HsImport "Foreign.Storable" False Nothing (Just ["Storable"])
            , HsImport "GHC.Generics" False Nothing (Just ["Generic"])
            , HsImport "Test.QuickCheck.Arbitrary" False Nothing (Just ["Arbitrary(..)", "arbitraryBoundedEnum"])
            , HsImport "FFI.Tox.Raw" False Nothing Nothing
            ]
        , hsModDecls = enumInstances ++ idTypes_ ++ consts_ ++ callbacks_
        , hsModOptions = ["-Wno-unused-imports", "-Wno-orphans", "-Wno-unused-top-binds"]
        , hsModPragmas = ["DeriveGeneric", "GeneralizedNewtypeDeriving"]
        }

collectErrorTypes :: SemanticModel -> [Text]
collectErrorTypes model =
    let fromMethods = mapMaybe methodErrorType (concatMap methods (resources model))
        fromProps = mapMaybe propErrorType (concatMap properties (resources model))
        fromMappings = mapMaybe cErrorType (collectMappings model)
    in nub (fromMethods ++ fromProps ++ fromMappings)

collectMappings :: SemanticModel -> [CFunctionMapping]
collectMappings model =
    concatMap getResMappings (resources model)
  where
    getResMappings res =
         map (getMapping res) (methods res) ++
         concatMap (getPropMappings res) (properties res)

    getMapping res m = case methodMapping m of
        StandardMapping  -> inferCFunctionMapping (resources model) res m
        CustomMapping cm -> cm

    getPropMappings res p =
        let getMethod name = find (\m -> methodName m == name) (methods res)

            mappings = catMaybes [ propRead p >>= getMethod
                                 , propWrite p >>= getMethod
                                 , propSize p >>= getMethod
                                 ]
        in map (getMapping res) mappings

collectUsedNames :: [SResource] -> [Text]
collectUsedNames resources =
    let
        getMethods r = map (\m -> idToHaskellCamel (cFunctionName (getMapping r m))) (methods r)
        getMapping r m = case methodMapping m of
            StandardMapping  -> inferCFunctionMapping resources r m
            CustomMapping cm -> cm

        getProps r = concatMap (getPropNames r) (properties r)
        getPropNames r p =
            let getMethod name = find (\m -> methodName m == name) (methods r)

                methods' = catMaybes [ propRead p >>= getMethod
                                     , propWrite p >>= getMethod
                                     , propSize p >>= getMethod
                                     ]

                extract m = case methodMapping m of
                    CustomMapping cm -> [idToHaskellCamel (cFunctionName cm)]
                    _                -> []
            in concatMap extract methods'

    in concatMap (\r -> getMethods r ++ getProps r) resources

generateEnumInstances :: [Text] -> SEnumModel -> [HsDecl]
generateEnumInstances _errorTypes SEnumModel{..} =
    let hsName = idToHaskell enumSemanticName
    in [ HsInstance "MessagePack" (TyCon hsName) []
       , HsInstance "Arbitrary" (TyCon hsName) [HsRawDecl "arbitrary = arbitraryBoundedEnum"]
       ]

generateIdType :: SIdTypeModel -> [HsDecl]
generateIdType SIdTypeModel{..} =
    let hsName = idToHaskell idName
    in [ HsNewtypeDecl
           {
             newtypeName = hsName
           , newtypeCon = HsConDecl hsName [TyCon "Word32"]
           , newtypeDeriving = ["Eq", "Ord", "Show", "Read", "Generic", "Storable", "Num", "Enum"]
           }
       , HsInstance "MessagePack" (TyCon hsName) []
       , HsInstance "Arbitrary" (TyCon hsName) [HsRawDecl $ "arbitrary = " <> hsName <> " <$> arbitrary"]
       ]

generateConstant :: [Text] -> SConstantModel -> [HsDecl]
generateConstant usedNames SConstantModel{..} =
    let baseName = idToHaskellCamel constantName
        hsName = if baseName `elem` usedNames then baseName <> "_Const" else baseName
    in [ HsFunSig hsName (TyCon "Word32")
       , HsFunBind hsName [] (ELit (LInt constantValue))
       ]

generateCallback :: [SResource] -> SCallbackTypeModel -> [HsDecl]
generateCallback resources SCallbackTypeModel{..} =
    let hsName = idToHaskell cbName
        params = map (toHsType resources . paramType) cbParams
        sig = foldr TyFun (TyApp (TyCon "IO") TyUnit) params
    in [ HsTypeAlias { aliasName = hsName, aliasType = sig }
       , HsForeignImport
           {
             foreignCName = "wrapper"
           , foreignHsName = "wrap" <> hsName
           , foreignType = TyFun (TyCon hsName) (TyApp (TyCon "IO") (TyApp (TyCon "FunPtr") (TyCon hsName)))
           , foreignPure = False
           }
       ]

generateModules :: Options -> SemanticModel -> [(FilePath, Text)]
generateModules opts model@SemanticModel{..} =
    let grouped = groupByPrefix resources
    in map (generateModule opts model) grouped

groupByPrefix :: [SResource] -> [[SResource]]
groupByPrefix = map (:[]) . sortOn resourceName


generateModule :: Options -> SemanticModel -> [SResource] -> (FilePath, Text)
generateModule _opts model resources =
    let modName = case resources of
            (r:_) -> getModuleName r
            []    -> error "generateModule: empty resources"
        functions = concatMap (generateResourceFunctions model (resourcesFromModel model)) resources

        getDeclKey :: HsDecl -> Maybe (Text, Text)
        getDeclKey (HsDataDecl n _ _)        = Just ("Data", n)
        getDeclKey (HsNewtypeDecl n _ _)     = Just ("Newtype", n)
        getDeclKey (HsDataEnumDecl n _ _)    = Just ("Enum", n)
        getDeclKey (HsTypeAlias n _)         = Just ("Type", n)
        getDeclKey (HsForeignImport _ n _ _) = Just ("Foreign", n)
        getDeclKey (HsFunSig n _)            = Just ("Sig", n)
        getDeclKey (HsFunBind n _ _)         = Just ("Bind", n)
        getDeclKey _                         = Nothing

        dedupDecl a b = case (getDeclKey a, getDeclKey b) of
            (Just ka, Just kb) -> ka == kb
            (Nothing, Nothing) -> a == b
            _                  -> False

        uniqueFunctions = nubBy dedupDecl functions

        imports = nub $ concatMap (getImports model) uniqueFunctions
    in (Text.unpack (Text.replace "." "/" modName) <> ".hs", render $ HsModule
        {
          hsModName = modName
        , hsModExports = []
        , hsModImports =
            [ HsImport "Data.MessagePack" False Nothing (Just ["MessagePack"])
            , HsImport "Data.Int" False Nothing (Just ["Int32"])
            , HsImport "Data.Word" False Nothing (Just ["Word8", "Word16", "Word32", "Word64"])
            , HsImport "Data.ByteString" False Nothing (Just ["ByteString"])
            , HsImport "Data.ByteString" True (Just "BS") Nothing
            , HsImport "Foreign.C.Enum" False Nothing (Just ["CEnum(..)", "CErr", "callErrFun", "toCEnum", "fromCEnum"])
            , HsImport "Foreign.C.String" False Nothing (Just ["CString", "withCString"])
            , HsImport "Foreign.C.Types" False Nothing (Just ["CInt(..)", "CSize(..)", "CBool(..)"])
            , HsImport "Foreign.Ptr" False Nothing (Just ["FunPtr", "Ptr", "nullPtr", "castFunPtrToPtr", "castPtrToFunPtr"])
            , HsImport "Foreign.Storable" False Nothing (Just ["Storable"])
            , HsImport "Foreign.Marshal.Alloc" False Nothing (Just ["alloca"])
            , HsImport "Foreign.Marshal.Array" False Nothing (Just ["allocaArray", "peekArray"])
            , HsImport "Foreign.Marshal.Utils" False Nothing (Just ["fromBool", "toBool"])
            , HsImport "Control.Monad" False Nothing (Just ["(>=>)"])
            , HsImport "GHC.Generics" False Nothing (Just ["Generic"])
            , HsImport "FFI.Tox.Types" False Nothing Nothing
            ] ++ (if modName == "FFI.Tox.Core" then [] else [HsImport "FFI.Tox.Core" False Nothing Nothing])
              ++ map (\m -> HsImport m False Nothing Nothing) imports
        , hsModDecls = uniqueFunctions
        , hsModOptions = ["-Wno-unused-imports", "-Wno-unused-matches", "-Wno-unused-local-binds"]
        , hsModPragmas = ["DeriveGeneric", "GeneralizedNewtypeDeriving"]
        })

resourcesFromModel :: SemanticModel -> [SResource]
resourcesFromModel = resources

getModuleName :: SResource -> Text
getModuleName res =
    let prefix = Text.takeWhile (/= '_') (cName res)
        baseName = idToHaskell (resourceName res)
        modName = if baseName == "Tox" then "Core" else baseName
    in "FFI." <> idToHaskell prefix <> "." <> modName

generateResourceFunctions :: SemanticModel -> [SResource] -> SResource -> [HsDecl]
generateResourceFunctions model allRes res =
    concatMap (generateMethod model allRes res) (methods res)
    ++ concatMap (generateProperty model allRes res) (properties res)

generateMethod :: SemanticModel -> [SResource] -> SResource -> SMethod -> [HsDecl]
generateMethod model allRes res method =
    let mapping = case methodMapping method of
            StandardMapping -> inferCFunctionMapping allRes res method
            CustomMapping m -> m
        cFunc = cFunctionName mapping
        cImportName = cFunc

        isPure = null (argMapping mapping)

        ffiSig = generateFFISignature model allRes res mapping (output method) isPure
        ffiDecl = HsForeignImport
            {
              foreignCName = cFunc
            , foreignHsName = cImportName
            , foreignType = ffiSig
            , foreignPure = isPure
            }

        wrapperName = idToHaskellCamel (methodName method)
        wrapperSig = generateWrapperSignature model allRes res (inputs method) (output method) (methodErrorType method) (methodRole method)
        wrapperBody = generateWrapperBody model allRes res (inputs method) (output method) mapping cImportName (methodRole method)

        sigDecl = HsFunSig wrapperName wrapperSig
        bindDecl = HsFunBind wrapperName (collectBindArgs model allRes res (inputs method) (methodRole method)) wrapperBody

    in [ffiDecl, sigDecl, bindDecl]

collectBindArgs :: SemanticModel -> [SResource] -> SResource -> [SParameter] -> SMethodRole -> [HsPat]
collectBindArgs _ allRes res semParams role =
    let
        paramNames = map (PVar . safeName . idToHaskellCamel . paramName) semParams

        hierarchy = getHierarchy allRes res
        isIdRes = case resourceType res of { ResId _ -> True; _ -> False }

        contextArgs =
               if isIdRes
               then
                   let relevantHierarchy = if role == Constructor || role == StaticRole
                                           then reverse (drop 1 (reverse (drop 1 hierarchy)))
                                           else drop 1 hierarchy
                       numIds = length relevantHierarchy
                   in PVar "this" : map (\i -> PVar ("id" <> Text.pack (show i))) ([1..numIds] :: [Int])

               else
                   if role /= Constructor && role /= StaticRole
                   then [PVar "this"]
                   else []
    in contextArgs ++ paramNames

generateProperty :: SemanticModel -> [SResource] -> SResource -> SProperty -> [HsDecl]
generateProperty model allRes res prop =
    let
        findMethod mName = find (\m -> methodName m == mName) (methods res)

        getter = propRead prop >>= findMethod
        setter = propWrite prop >>= findMethod
        sizeGetter = propSize prop >>= findMethod

        ty = propType prop

        genWrapper m role typeHint retType errType =
             let mapping = case methodMapping m of
                     StandardMapping  -> inferCFunctionMapping allRes res m
                     CustomMapping cm -> cm

                 cFunc = cFunctionName mapping
                 cImportName = cFunc
                 isPure = null (argMapping mapping)

                 ffiSig = generateFFISignature model allRes res mapping typeHint isPure
                 ffiDecl = HsForeignImport
                    {
                      foreignCName = cFunc
                    , foreignHsName = cImportName
                    , foreignType = ffiSig
                    , foreignPure = isPure
                    }

                 wrapperName = idToHaskellCamel (cFunctionName mapping) -- Derived from C name
                 wrapperSig = generateWrapperSignature model allRes res (inputs m) retType errType role
                 wrapperBody = generateWrapperBody model allRes res (inputs m) retType mapping cImportName role

                 sigDecl = HsFunSig wrapperName wrapperSig
                 bindDecl = HsFunBind wrapperName (collectBindArgs model allRes res (inputs m) role) wrapperBody
             in [ffiDecl, sigDecl, bindDecl]

        genFunc _ Nothing = []
        genFunc typeHint (Just m) =
             let role = GetterRole -- Assume getter for simple property access
             in genWrapper m role typeHint typeHint (methodErrorType m)

        genSetter (Just m) =
             let role = SetterRole
                 retType = case methodMapping m of
                     StandardMapping  -> I.inferCFunctionMapping allRes res m
                     CustomMapping cm -> cm
                 cRet = cReturnType retType
             in genWrapper m role cRet cRet (methodErrorType m)
        genSetter _ = []

    in genFunc ty getter ++ genSetter setter ++ genFunc SSizeT sizeGetter

generateFFISignature :: SemanticModel -> [SResource] -> SResource -> CFunctionMapping -> SType -> Bool -> HsType
generateFFISignature model allRes res mapping typeHint isPure =
    let args = argMapping mapping
        params = map (sourceToHaskellType model allRes res (cSemParams mapping) (cErrorType mapping) typeHint) args

        hasBuffer = any (\arg -> case arg of BufferPtr _ -> True; _ -> False) args
        hasErrorPtr = any (\arg -> case arg of ErrorPtr -> True; _ -> False) args

        retType = if hasBuffer then
                      if hasErrorPtr then TyCon "Bool" else TyUnit
                  else if hasErrorPtr then toHsType allRes typeHint
                  else case cErrorType mapping of
                      Just e  -> TyCon (idToHaskell e) -- Returns Error Enum
                      Nothing -> toHsType allRes typeHint

        finalRet = if isPure then retType else TyApp (TyCon "IO") retType

    in foldr TyFun finalRet params

sourceToHaskellType :: SemanticModel -> [SResource] -> SResource -> [SParameter] -> Maybe Text -> SType -> CArgSource -> HsType
sourceToHaskellType model allRes res semParams mErr typeHint src = case src of
    ThisObject _ -> toHsType allRes (SHandle (resourceName res))
    PathObject n _ ->
        let tyName = I.resolvePathTarget allRes res n
        in toHsType allRes (SHandle tyName)
    PathId n ->
        let tyName = I.resolvePathTarget allRes res n
        in case find (\r -> resourceName r == tyName) allRes of
            Just r -> case resourceType r of
                ResId t -> toHsType allRes t
                _       -> TyCon "Word32"
            Nothing -> TyCon "Word32"
    SemanticArg i ->
        if i < length semParams
        then toHsType allRes (paramType (semParams !! i))
        else TyApp (TyCon "Ptr") TyUnit
    ErrorPtr -> case mErr of
        Just e ->
             let semName = case find (\en -> enumName en == e) (enums model) of
                     Just en -> enumSemanticName en
                     Nothing -> e
             in TyApp (TyCon "CErr") (TyCon (idToHaskell semName))
        Nothing -> TyApp (TyCon "CErr") TyUnit
    BufferPtr _ -> case typeHint of
        SList t -> TyApp (TyCon "Ptr") (toHsType allRes t)
        _       -> TyCon "CString"
    BufferSize -> TyCon "CSize"
    UserData -> TyApp (TyCon "Ptr") TyUnit
    Constant _ -> TyCon "Int32"
toHsType :: [SResource] -> SType -> HsType
toHsType _ SVoid              = TyUnit
toHsType _ SBool              = TyCon "CBool"
toHsType _ (SInt 8)           = TyCon "Int8"
toHsType _ (SInt 16)          = TyCon "Int16"
toHsType _ (SInt 32)          = TyCon "Int32"
toHsType _ (SInt 64)          = TyCon "Int64"
toHsType _ (SUInt 8)          = TyCon "Word8"
toHsType _ (SUInt 16)         = TyCon "Word16"
toHsType _ (SUInt 32)         = TyCon "Word32"
toHsType _ (SUInt 64)         = TyCon "Word64"
toHsType _ SSizeT             = TyCon "CSize"
toHsType _ SString            = TyCon "CString"
toHsType _ SBytes             = TyCon "CString"
toHsType _ (SFixedBytes _ _)  = TyCon "CString"
toHsType _ (SFixedList t _ _) = TyApp (TyCon "Ptr") (toHsType [] t)
toHsType _ (SEnum n)          = TyParen (TyApp (TyCon "CEnum") (TyCon (idToHaskell n)))
toHsType _ (SHandle "void")   = TyApp (TyCon "Ptr") TyUnit
toHsType _ (SHandle n)        = TyCon (idToHaskell n <> "Ptr")
toHsType _ (SCallback n)      = TyApp (TyCon "FunPtr") (TyCon (idToHaskell n))
toHsType _ (SResourceId n)    = TyCon (idToHaskell n)
toHsType _ (SList t)          = TyApp (TyCon "Ptr") (toHsType [] t)
toHsType _ _                  = TyApp (TyCon "Ptr") TyUnit

idToHaskell :: Text -> Text
idToHaskell = idToHaskell' Casing.toPascal

idToHaskellCamel :: Text -> Text
idToHaskellCamel = idToHaskell' Casing.toCamel

idToHaskell' :: (Casing.Identifier String -> String) -> Text -> Text
idToHaskell' casing =
    Text.pack
    . casing
    . Casing.Identifier
    . Casing.unIdentifier
    . Casing.fromSnake
    . Text.unpack

getImports :: SemanticModel -> HsDecl -> [Text]
getImports _ _ = []

-- Wrapper Generation Helpers

safeName :: Text -> Text
safeName "type"   = "type_"
safeName "data"   = "data_"
safeName "class"  = "class_"
safeName "case"   = "case_"
safeName "do"     = "do_"
safeName "let"    = "let_"
safeName "where"  = "where_"
safeName "in"     = "in_"
safeName "of"     = "of_"
safeName "module" = "module_"
safeName "import" = "import_"
safeName "id"     = "id_"
safeName n        = n

toWrapperType :: [SResource] -> SType -> HsType
toWrapperType _ SBytes            = TyCon "ByteString"
toWrapperType _ SString           = TyCon "ByteString"
toWrapperType _ (SFixedBytes _ _) = TyCon "ByteString"
toWrapperType _ (SEnum n)         = TyCon (idToHaskell n)
toWrapperType _ SBool             = TyCon "Bool"
toWrapperType _ (SList t)         = TyList (toWrapperType [] t)
toWrapperType allRes t            = toHsType allRes t

generateWrapperSignature :: SemanticModel -> [SResource] -> SResource -> [SParameter] -> SType -> Maybe Text -> SMethodRole -> HsType
generateWrapperSignature model allRes res semParams retType mErr role =
    let
        paramTypes = map (\p -> toWrapperType allRes (paramType p)) semParams

        hierarchy = getHierarchy allRes res
        isIdRes = case resourceType res of { ResId _ -> True; _ -> False }

        contextArgs =
               if isIdRes
               then
                   let rootName = case hierarchy of
                           (n:_) -> n
                           []    -> ""
                       rootType = toHsType allRes (SHandle rootName)

                       relevantHierarchy = if role == Constructor || role == StaticRole
                                           then reverse (drop 1 (reverse (drop 1 hierarchy)))
                                           else drop 1 hierarchy

                       idArgs = map (\rName ->
                               case find (\r -> resourceName r == rName) allRes of
                                   Just r -> case resourceType r of
                                       ResId t -> toHsType allRes t
                                       _       -> TyCon "Word32"
                                   Nothing -> TyCon "Word32"
                               ) relevantHierarchy

                   in rootType : idArgs

               else
                   if role /= Constructor && role /= StaticRole
                   then [toHsType allRes (SHandle (resourceName res))]
                   else []

        wrapperRetType = toWrapperType allRes retType

        ret = case mErr of
            Just err -> TyApp (TyApp (TyCon "Either") (TyCon (resolveErrorType model err))) wrapperRetType
            Nothing -> wrapperRetType

    in foldr TyFun (TyApp (TyCon "IO") ret) (contextArgs ++ paramTypes)

resolveErrorType :: SemanticModel -> Text -> Text
resolveErrorType model cName =
    case find (\e -> enumName e == cName) (enums model) of
        Just e  -> idToHaskell (enumSemanticName e)
        Nothing -> idToHaskell cName

checkSizeFnArgs :: [SResource] -> Text -> Bool
checkSizeFnArgs allRes name =
    let
        checkMap m = cFunctionName m == name && not (null (argMapping m))

        checkMethod r m =
            let mapping = case methodMapping m of
                    CustomMapping cm -> cm
                    StandardMapping  -> inferCFunctionMapping allRes r m
            in checkMap mapping

        checkProperty r p =
            let getMethod mName = find (\m -> methodName m == mName) (methods r)
                methods' = catMaybes [ propRead p >>= getMethod
                                     , propWrite p >>= getMethod
                                     , propSize p >>= getMethod
                                     ]
            in any (checkMethod r) methods'

    in any (\r -> any (checkMethod r) (methods r) || any (checkProperty r) (properties r)) allRes

hasCFunction :: [SResource] -> Text -> Bool
hasCFunction allRes name =
    let
        checkMap m = cFunctionName m == name

        checkMethod r m =
            let mapping = case methodMapping m of
                    CustomMapping cm -> cm
                    StandardMapping  -> inferCFunctionMapping allRes r m
            in checkMap mapping

        checkProperty r p =
            let getMethod mName = find (\m -> methodName m == mName) (methods r)
                methods' = catMaybes [ propRead p >>= getMethod
                                     , propWrite p >>= getMethod
                                     , propSize p >>= getMethod
                                     ]
            in any (checkMethod r) methods'

    in any (\r -> any (checkMethod r) (methods r) || any (checkProperty r) (properties r)) allRes

generateWrapperBody :: SemanticModel -> [SResource] -> SResource -> [SParameter] -> SType -> CFunctionMapping -> Text -> SMethodRole -> HsExpr
generateWrapperBody model allRes res semParams retType mapping ffiName role =
    let
        args = argMapping mapping

        paramNames = zipWith (\p i -> (paramName p, safeName (idToHaskellCamel (paramName p)), i)) semParams ([0..] :: [Int])
        pName i = case find (\(n, _, _) -> n == paramName (semParams !! i)) paramNames of
             Just (_, s, _) -> s
             Nothing        -> "undefined"

        mkWrapper (name, SBytes, i) body =
             EApp (EApp (EVar "BS.useAsCStringLen") (EVar name))
                  (ELam [PTuple [PVar ("ptr" <> Text.pack (show i)), PVar ("len" <> Text.pack (show i))]] body)
        mkWrapper (name, SString, i) body =
             EApp (EApp (EVar "BS.useAsCString") (EVar name))
                  (ELam [PVar ("ptr" <> Text.pack (show i))] body)
        mkWrapper (name, SFixedBytes _ _, i) body =
             EApp (EApp (EVar "BS.useAsCString") (EVar name))
                  (ELam [PVar ("ptr" <> Text.pack (show i))] body)
        mkWrapper _ body = body

        wrappers = map (\(_, s, i) -> (s, paramType (semParams !! i), i)) paramNames
        wrappedCall = foldr mkWrapper innerBody wrappers

        -- Buffer handling
        isBufferPtr (BufferPtr _) = True
        isBufferPtr _             = False
        hasBuffer = any isBufferPtr args

        bufferVar = "outPtr"

        -- Context args logic
        hierarchy = getHierarchy allRes res
        isIdRes = case resourceType res of { ResId _ -> True; _ -> False }

        contextArgNames = if isIdRes
             then
                 let relevantHierarchy = if role == Constructor || role == StaticRole
                                         then reverse (drop 1 (reverse (drop 1 hierarchy)))
                                         else drop 1 hierarchy
                     numIds = length relevantHierarchy
                 in "this" : map (\i -> "id" <> Text.pack (show i)) ([1..numIds] :: [Int])
             else
                 if role /= Constructor && role /= StaticRole
                 then ["this"]
                 else []

        contextArgExprs = map EVar contextArgNames

        -- Size call args
        sizeCallArgs = contextArgExprs ++ (if hasErrorPtr then [EVar "nullPtr"] else [])

        (innerBody, _) = if hasBuffer
            then
               let
                   (sizeStmt, sizeVar) = case (cSizeFunctionName mapping, retType) of
                       (Just sizeFn, _) ->
                           let haskellName = idToHaskellCamel sizeFn

                               checkProp propToCheck resToCheck =
                                   let getMethod mName = find (\m -> methodName m == mName) (methods resToCheck)

                                       methods' = catMaybes [ propRead propToCheck >>= getMethod
                                                            , propWrite propToCheck >>= getMethod
                                                            , propSize propToCheck >>= getMethod
                                                            ]

                                       extract m = case methodMapping m of
                                           CustomMapping cm -> idToHaskellCamel (cFunctionName cm) == haskellName
                                           _ -> False
                                   in any extract methods'

                               isMethodName = any (\resToCheck ->
                                       any (\m -> idToHaskellCamel (methodName m) == haskellName) (methods resToCheck) ||
                                       any (\p -> checkProp p resToCheck) (properties resToCheck)
                                   ) allRes

                               targetName = if isMethodName then haskellName <> "_Const" else haskellName

                               isInConstants = any (\c -> constantName c == sizeFn) (constants model)

                               takesArgs = checkSizeFnArgs allRes sizeFn

                           in if sizeFn == "TOX_ADDRESS_SIZE"
                              then (SLet [HsFunBind "size" [] (EVar "tox_address_size")], "size")
                              else if takesArgs
                              then (SGenerator (PVar "size") (foldl EApp (EVar sizeFn) sizeCallArgs), "size")
                              else if isInConstants
                                   then (SLet [HsFunBind "size" [] (EVar ("FFI.Tox.Types." <> targetName))], "size")
                                   else (SLet [HsFunBind "size" [] (EVar sizeFn)], "size")
                       (Nothing, SFixedBytes sizeName _) ->
                           let haskellName = idToHaskellCamel sizeName
                               isMethodName = any (\resToCheck ->
                                       any (\m -> idToHaskellCamel (methodName m) == haskellName) (methods resToCheck) ||
                                       any (\p -> checkProp p resToCheck) (properties resToCheck)
                                   ) allRes

                               checkProp propToCheck resToCheck =
                                   let getMethod mName = find (\m -> methodName m == mName) (methods resToCheck)

                                       methods' = catMaybes [ propRead propToCheck >>= getMethod
                                                            , propWrite propToCheck >>= getMethod
                                                            , propSize propToCheck >>= getMethod
                                                            ]

                                       extract m = case methodMapping m of
                                           CustomMapping cm -> idToHaskellCamel (cFunctionName cm) == haskellName
                                           _ -> False
                                   in any extract methods'

                               targetName = if isMethodName then haskellName <> "_Const" else haskellName
                           in if sizeName == "TOX_ADDRESS_SIZE"
                              then (SLet [HsFunBind "size" [] (EVar "tox_address_size")], "size")
                              else (SLet [HsFunBind "size" [] (EVar ("FFI.Tox.Types." <> targetName))], "size")
                       _ -> (SLet [HsFunSig "size" (TyCon "CSize"), HsFunBind "size" [] (ELit (LInt 0))], "size")

                   allocApp = EApp (EVar "allocaArray") (EApp (EVar "fromIntegral") (EVar sizeVar))
                   allocLam = ELam [PVar bufferVar] bodyWithErr

                   callArgs = resolveArgs args (-1) True

                   packCode = case retType of
                       SList (SEnum _) ->
                           [ SGenerator (PVar "raw") (EApp (EApp (EVar "peekArray") (EApp (EVar "fromIntegral") (EVar sizeVar))) (EVar bufferVar))
                           , SLet [HsFunBind "val" [] (EApp (EApp (EVar "map") (EVar "fromCEnum")) (EVar "raw"))]
                           ]
                       SList _ ->
                           [ SGenerator (PVar "val") (EApp (EApp (EVar "peekArray") (EApp (EVar "fromIntegral") (EVar sizeVar))) (EVar bufferVar)) ]
                       _ ->
                           [ SGenerator (PVar "val") (EApp (EVar "BS.packCStringLen") (ETuple [EVar bufferVar, EApp (EVar "fromIntegral") (EVar sizeVar)])) ]

                   resReturn = if hasErrorPtr
                               then EApp (EVar "return") (EApp (EVar "Right") (EVar "val"))
                               else EApp (EVar "return") (EVar "val")

                   bodyWithErr = EDo $
                       if hasErrorPtr
                       then [ SGenerator (PVar "res") (callExpr callArgs)
                            , SExpr $ ECase (EVar "res")
                                [ HsAlt (PCon "Left" [PVar "err"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "err")))
                                , HsAlt (PCon "Right" [PWildCard]) (EDo (packCode ++ [SExpr resReturn]))
                                ]
                            ]
                       else [ SGenerator (PVar "_") (callExpr callArgs) ] ++ packCode ++
                            [ SExpr resReturn ]

               in (EDo [sizeStmt, SExpr (EApp allocApp allocLam)], callArgs)
            else
               let
                   callArgs = resolveArgs args (-1) False
                   baseCall = callExpr callArgs

                   convert expr =
                       if False -- isPure is always False for Raw
                       then expr
                       else if hasErrorPtr
                       then case retType of
                           SEnum _ -> expr
                           SBool   -> expr
                           SInt _  -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SUInt _ -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SSizeT  -> EApp (EApp (EVar "fmap") (EApp (EVar "fmap") (EVar "fromIntegral"))) expr
                           SString -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           SFixedBytes _ _ -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           SBytes -> EInfixApp expr ">>=" (ELam [PVar "r"] (ECase (EVar "r")
                                         [ HsAlt (PCon "Left" [PVar "e"]) (EApp (EVar "return") (EApp (EVar "Left") (EVar "e")))
                                         , HsAlt (PCon "Right" [PVar "s"]) (EInfixApp (EVar "Right") "<$>" (EApp (EVar "BS.packCString") (EVar "s")))
                                         ]))
                           _ -> expr
                       else case retType of
                           SEnum _ -> expr
                           SBool   -> expr
                           SInt _  -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SUInt _ -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SSizeT  -> EApp (EApp (EVar "fmap") (EVar "fromIntegral")) expr
                           SString -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           SFixedBytes _ _ -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           SBytes -> EInfixApp expr ">>=" (EVar "BS.packCString")
                           _ -> expr

               in (convert baseCall, callArgs)

        resolveArgs [] _ _ = []
        resolveArgs (arg:rest) lastIdx useBuffer =
             case arg of
                 SemanticArg i ->
                     let val = mapArg arg
                         nextIdx = if isBytes (semParams !! i) then i else lastIdx
                     in val : resolveArgs rest nextIdx useBuffer
                 BufferSize ->
                     let val = EParen (EApp (EVar "fromIntegral") (EVar ("len" <> Text.pack (show lastIdx))))
                     in val : resolveArgs rest lastIdx useBuffer
                 BufferPtr _ -> (if useBuffer then EVar bufferVar else EVar "undefined") : resolveArgs rest lastIdx useBuffer
                 _ -> mapArg arg : resolveArgs rest lastIdx useBuffer

        isBytes p = case paramType p of
            SBytes          -> True
            SFixedBytes _ _ -> True
            SString         -> True
            _               -> False

        mapArg (ThisObject _) = EVar "this"
        mapArg (PathObject _ _) = EVar "this"
        mapArg (PathId n) = EVar ("id" <> Text.pack (show n))
        mapArg (SemanticArg i) =
             let name = pName i
                 ty = paramType (semParams !! i)
             in case ty of
                 SBytes -> EParen (EApp (EVar "castPtr") (EVar ("ptr" <> Text.pack (show i))))
                 SString -> EVar ("ptr" <> Text.pack (show i))
                 SFixedBytes _ _ -> EParen (EApp (EVar "castPtr") (EVar ("ptr" <> Text.pack (show i))))
                 SEnum _ -> EVar name
                 SBool   -> EVar name
                 SInt _ -> EParen (EApp (EVar "fromIntegral") (EVar name))
                 SUInt _ -> EParen (EApp (EVar "fromIntegral") (EVar name))
                 _ -> EVar name
        mapArg ErrorPtr = EParen (EApp (EVar "castPtr") (EVar "errPtr"))
        mapArg _ = EVar "undefined"

        hasErrorPtr = any isErrorPtr args
        isErrorPtr ErrorPtr = True
        isErrorPtr _        = False

        callExpr argsList =
            let f = EVar ffiName
                apply = foldl EApp f (map (\a -> if a == EVar "undefined" then EVar "errPtr" else a) argsList)
            in if hasErrorPtr
               then EApp (EVar "callErrFun") (ELam [PVar "errPtr"] apply)
               else foldl EApp f (map (\a -> if a == EVar "undefined" then EVar "nullPtr" else a) argsList)

    in wrappedCall
