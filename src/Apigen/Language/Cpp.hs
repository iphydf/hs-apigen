{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.Cpp (generate) where

import qualified Apigen.Inference           as I
import           Apigen.Language.Cpp.AST
import qualified Apigen.Language.Cpp.Pretty as Pretty
import           Apigen.Semantic            (CArgSource (..),
                                             CFunctionMapping (..),
                                             SCallbackTypeModel (..),
                                             SConstantModel (..),
                                             SEnumModel (..), SEvent (..),
                                             SIdTypeModel (..), SLocation (..),
                                             SMapping (..), SMethod (..),
                                             SMethodRole (..), SParameter (..),
                                             SProperty (..), SResource (..),
                                             SResourceType (..),
                                             SResultStrategy (..), SType (..),
                                             SVariant (..), SVariantMember (..),
                                             SemanticModel (..))
import qualified Apigen.Semantic            as S
import qualified Apigen.Types               as T
import           Data.Char                  (isDigit, isUpper)
import qualified Data.List                  as List
import           Data.Maybe                 (catMaybes, fromMaybe, isJust,
                                             isNothing, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

renderCppType :: CppType -> Text
renderCppType TyVoid = "void"
renderCppType TyBool = "bool"
renderCppType (TyInt 8) = "int8_t"
renderCppType (TyInt 16) = "int16_t"
renderCppType (TyInt 32) = "int32_t"
renderCppType (TyInt 64) = "int64_t"
renderCppType (TyUInt 8) = "uint8_t"
renderCppType (TyUInt 16) = "uint16_t"
renderCppType (TyUInt 32) = "uint32_t"
renderCppType (TyUInt 64) = "uint64_t"
renderCppType TySizeT = "size_t"
renderCppType TyString = "std::string"
renderCppType TyBytes = "std::vector<uint8_t>"
renderCppType (TyVector t) = "std::vector<" <> renderCppType t <> ">"
renderCppType (TyArray t s) = "std::array<" <> renderCppType t <> ", " <> s <> ">"
renderCppType (TyUserDefined t) = t
renderCppType (TyPointer t) = renderCppType t <> "*"
renderCppType (TyReference t) = renderCppType t <> "&"
renderCppType (TyConst t) = "const " <> renderCppType t
renderCppType (TyRValueReference t) = renderCppType t <> "&&"
renderCppType (TySpan t) = "std::span<" <> renderCppType t <> ">"
renderCppType (TyResult t e) = "Result<" <> renderCppType t <> ", " <> renderCppType e <> ">"
renderCppType (TyFunction ret params) = renderCppType ret <> "(" <> Text.intercalate ", " (map renderCppType params) <> ")"
renderCppType _ = "void" -- Fallback

generate :: SemanticModel -> Text
generate model = Pretty.render $ Pretty.plain $ Pretty.renderAST $ generateAST model

generateAST :: SemanticModel -> [CppDecl]
generateAST (SemanticModel enums_ _constants_ idTys _callbacks resources_ variants_ cp _) =
    let allRes = List.sortOn (\res -> (isJust (parent res), resourceName res)) resources_
        apiName = Text.toUpper cp
        headerGuard = apiName <> "_CPP_API_H"
        namespaceName = Text.toLower cp

        hasEvents = any (not . null . events) resources_

        (res_decls_list, res_defs_list) = unzip $ map (generateResource cp allRes idTys enums_ variants_) allRes
        res_decls = concat res_decls_list
        res_defs = concat res_defs_list

    in [ HeaderGuard headerGuard $
          [ Include "cstdint" True
          , Include "string" True
          , Include "vector" True
          , Include "array" True
          , Include "algorithm" True
          ] ++ [Include "map" True | hasEvents] ++
          [ Include "span" True
          , Include "string_view" True
          , Include "utility" True
          , Include "optional" True
          , Include "variant" True
          , Include "new" True
          , Include "cstdlib" True
          , Include "functional" True
          , Include "tox_result.h" False
          , Namespace namespaceName $
            concatMap generateForwardDecl allRes ++
            map generateIdType idTys ++
            map (generateEnum cp) enums_ ++
            map (generateVariant cp allRes) variants_ ++
            map generateCallbackType _callbacks ++
            res_decls ++
            res_defs ++
            [ CommentDecl "" ]
          ]
       ]

generateForwardDecl :: SResource -> [CppDecl]
generateForwardDecl res =
    let name = resourceName res
        isTmpl = case resourceType res of ResId _ -> True; _ -> False
    in if isTmpl
       then [ TemplateDecl ["typename ToxT"] (ForwardDecl (name <> "Handle"))
            , TypedefDecl (TyUserDefined (name <> "Handle<Tox>")) name
            , TypedefDecl (TyUserDefined (name <> "Handle<const Tox>")) ("Const" <> name)
            ]
       else [ForwardDecl name]

generateCallbackType :: SCallbackTypeModel -> CppDecl
generateCallbackType (SCallbackTypeModel name cName_ _) =
    TypedefDecl (TyUserDefined ("::" <> cName_)) name

generateIdType :: SIdTypeModel -> CppDecl
generateIdType (SIdTypeModel name cName_ _ty) =
    Class name
        [ MemberDecl Public (TyUserDefined ("::" <> cName_)) "value"
        , ConstructorDecl Public [] [("value", "0")] (Just [])
        , ConstructorDecl Public [CppParam (TyUserDefined ("::" <> cName_)) "v"] [("value", "v")] (Just [])
        , MethodDecl Public ("operator " <> "::" <> cName_) (TyUserDefined "") [] T.ConstThis (Just [Return "value"])
        , MethodDecl Public "operator==" TyBool [CppParam (TyConst (TyReference (TyUserDefined name))) "other"] T.ConstThis (Just [Return "value == other.value"])
        , MethodDecl Public "operator!=" TyBool [CppParam (TyConst (TyReference (TyUserDefined name))) "other"] T.ConstThis (Just [Return "value != other.value"])
        ]

generateEnum :: Text -> SEnumModel -> CppDecl
generateEnum _ (SEnumModel _ semName members) =
    Enum semName (map (fixEnumMember . Text.toUpper . snd) members)
  where
    fixEnumMember "NULL"   = "NULL_PTR"
    fixEnumMember "RIVATE" = "PRIVATE" -- Fix bad prefix stripping?
    fixEnumMember "UBLIC"  = "PUBLIC"   -- Fix bad prefix stripping?
    fixEnumMember m        = m

generateVariant :: Text -> [SResource] -> SVariant -> CppDecl
generateVariant cp allRes (SVariant name _ vMembers) =
    let variantTypes = map (\m -> toCppType cp allRes (SHandle (memberType m))) vMembers
        vDeclName = name <> "Variant"
    in TypedefDecl (TyUserDefined ("std::variant<" <> Text.intercalate ", " (map renderCppType variantTypes) <> ">")) vDeclName

toCName :: Text -> [SIdTypeModel] -> [SEnumModel] -> SType -> Text
toCName _ _ enumsList (SEnum n) =
    case List.find (\e -> enumSemanticName e == n) enumsList of
        Just e  -> enumName e
        Nothing -> n -- Fallback to semantic name if not found? Or handle prefixing here?
toCName cp idTys _ (SResourceId n) =
    case List.find (\m -> idName m == n) idTys of
        Just m  -> idCName m
        Nothing -> if (cp <> "_") `Text.isPrefixOf` n then n else cp <> "_" <> n
toCName _ _ _ (SHandle n) = n
toCName _ _ _ _ = ""

generateResource :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> [SVariant] -> SResource -> ([CppDecl], [CppDecl])
generateResource cp allRes idTys enumsList variants_ res =
    let name = resourceName res
        resType = resourceType res

        isTmpl = case resType of ResId _ -> True; _ -> False
        className = if isTmpl then name <> "Handle" else name

        (prop_decls, prop_defs) = unzip $ map (generateProperty cp allRes idTys enumsList res) (properties res)

        -- Filter out methods that are already handled by properties
        propertyMethods = concatMap (\p -> catMaybes [propRead p, propWrite p, propSize p]) (properties res)
        standaloneMethods = filter (\m -> methodName m `notElem` propertyMethods) (methods res)

        (meth_decls, meth_defs) = unzip $ map (\m -> generateMethod cp allRes idTys enumsList res m isTmpl) standaloneMethods

        (var_decls, var_defs) = generateVariantMethod cp allRes enumsList variants_ res

        (reg_decls, reg_defs) = generateEventHandling cp allRes idTys enumsList res

        -- Private members
        members = case resType of
            ResHandle -> generateHandleMembers cp allRes res prop_decls meth_decls var_decls reg_decls
            ResId idT -> generateIdMembers cp allRes res idT prop_decls meth_decls reg_decls

        allDefs = concat prop_defs ++ concat meth_defs ++ var_defs ++ reg_defs

        finalDefs = if isTmpl
                    then map (\case MethodDef _ n r p c b -> MethodDef (className <> "<ToxT>") n r p c b
                                    x -> x) allDefs
                    else allDefs

        resDecl = if isTmpl
                  then [ TemplateDecl ["typename ToxT"] (Class className members) ]
                  else [Class className members]

        resDefs = if isTmpl
                  then map (TemplateDecl ["typename ToxT"]) finalDefs
                  else finalDefs

    in (resDecl, resDefs)

mkVariantBranch :: Text -> [SResource] -> [SEnumModel] -> Text -> SVariantMember -> CppStmt
mkVariantBranch cp allRes enumsList vEnum (SVariantMember mName mType mGetter) =
    let fixEnumMember "NULL" = "NULL_PTR"
        fixEnumMember m      = m

        enumVal = case List.find (\e -> enumSemanticName e == vEnum) enumsList of
            Just e -> case List.find (\(_, sem) -> sem == mName) (enumMembers e) of
                Just (_, semVal) -> vEnum <> "::" <> fixEnumMember (Text.toUpper semVal)
                Nothing     -> "/* Unknown enum member " <> mName <> " */"
            Nothing -> "/* Unknown enum " <> vEnum <> " */"

        wrapper = renderCppType (toCppType cp allRes (SHandle mType))
        call = "::" <> mGetter <> "(instance_)"
    in If ("type == " <> enumVal) [Return (wrapper <> "(" <> call <> ")")] []

generateProperty :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> ([CppMember], [CppDecl])
generateProperty cp allRes idTys enumsList res prop =
    let findMethod mName = List.find (\m -> methodName m == mName) (methods res)

        getter = case propRead prop of
            Just gName -> findMethod gName
            Nothing    -> Nothing

        setter = case propWrite prop of
            Just sName -> findMethod sName
            Nothing    -> Nothing

        sizeGetter = case propSize prop of
            Just sName -> findMethod sName
            Nothing    -> Nothing

        ty = case getter of
            Just g  -> toCppType cp allRes (output g)
            Nothing -> case setter of
                Just s  -> if not (null (inputs s)) then toCppType cp allRes (paramType (last (inputs s))) else TyVoid
                Nothing -> TyVoid

        (getter_decl, getter_def) = generatePropertyGetter cp allRes idTys enumsList res prop getter sizeGetter ty
        (setter_decl, setter_def) = generatePropertySetter cp allRes idTys enumsList res prop setter ty

    in (getter_decl ++ setter_decl, getter_def ++ setter_def)

generatePropertyGetter :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> Maybe SMethod -> Maybe SMethod -> CppType -> ([CppMember], [CppDecl])
generatePropertyGetter _ _ _ _ _ _ Nothing _ _ = ([], [])
generatePropertyGetter cp allRes idTys enumsList res prop (Just g) sizeGetter ty =
    let name = propName prop
        resName = resourceName res
        mapping = case methodMapping g of
                S.CustomMapping c -> c
                S.StandardMapping -> I.inferCFunctionMapping allRes res g

        isInstanceSource (ThisObject _)   = True
        isInstanceSource (PathObject _ _) = True
        isInstanceSource (PathId _)       = True
        isInstanceSource _                = False
        isStatic = not (any isInstanceSource (argMapping mapping))
        accessSpec = if isStatic then Static Public else Public

        semParams = case output g of
            SBytes          -> []
            SFixedBytes _ _ -> []
            _               -> []

        cArgs = map (toCArgExpr cp allRes idTys enumsList res (argMapping mapping) semParams False isStatic) (zip [0..] (argMapping mapping))
        callExpr = "::" <> cFunctionName mapping <> "(" <> Text.intercalate ", " cArgs <> ")"
        errTy = cErrorType mapping
        setup = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                in [Expr ("::" <> et <> " error = " <> errOk)]
            Nothing -> []

        errCheck = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                    semEt = case List.find (\e -> enumName e == et) enumsList of
                        Just e  -> enumSemanticName e
                        Nothing -> et
                in [If ("error != " <> errOk) [Return ("static_cast<" <> semEt <> ">(error)")] []]
            Nothing -> []

        semTy = output g

        body = generatePropertyGetterBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck semTy callExpr isStatic

        finalRet = if isJust errTy
                   then let et = fromMaybe "void" errTy
                            semEt = case List.find (\e -> enumName e == et) enumsList of
                                Just e  -> enumSemanticName e
                                Nothing -> et
                        in TyResult ty (TyUserDefined semEt)
                   else ty

        cns = if isStatic then T.MutableThis
              else if isResourceWrapper cp allRes semTy then T.MutableThis
              else deriveConstness (argMapping mapping)

    in ([MethodDecl accessSpec ("get_" <> name) finalRet [] cns Nothing], [MethodDef resName ("get_" <> name) finalRet [] cns body])

generatePropertyGetterBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> CFunctionMapping -> Maybe SMethod -> [CppStmt] -> [CppStmt] -> SType -> Text -> Bool -> [CppStmt]
generatePropertyGetterBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck semTy callExpr isStatic =
    case semTy of
        SBytes -> generateBytesPropertyBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck isStatic
        SList elemTy -> generateListPropertyBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck elemTy isStatic
        SFixedBytes sizer _ -> generateFixedPropertyBody cp allRes idTys enumsList res mapping setup errCheck (Right sizer) (SFixedBytes sizer False) isStatic
        SFixedList elemTy sizer _ -> generateFixedPropertyBody cp allRes idTys enumsList res mapping setup errCheck (Left (elemTy, sizer)) semTy isStatic
        _ -> generateSimplePropertyBody cp allRes enumsList mapping setup errCheck semTy callExpr

generateBytesPropertyBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> CFunctionMapping -> Maybe SMethod -> [CppStmt] -> [CppStmt] -> Bool -> [CppStmt]
generateBytesPropertyBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck isStatic =
    let name = propName prop
        resName = resourceName res
        isInstanceSource (ThisObject _)   = True
        isInstanceSource (PathObject _ _) = True
        isInstanceSource (PathId _)       = True
        isInstanceSource _                = False

        sizeExpr = case sizeGetter of
            Just sMapMethod ->
                    let sMap = case methodMapping sMapMethod of
                            S.CustomMapping c -> c
                            S.StandardMapping -> I.inferCFunctionMapping allRes res sMapMethod
                        isStaticSize = not (any isInstanceSource (argMapping sMap))
                    in "::" <> cFunctionName sMap <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res (argMapping sMap) [] False isStaticSize) (zip [0..] (argMapping sMap))) <> ")"
            Nothing ->
                case findSizeConstant (propConstants prop) of
                    Just c -> c
                    Nothing -> error $ "Unknown size for property " ++ show name ++ " in " ++ show resName

        hasBufferPtr = any (\case BufferPtr _ -> True; _ -> False) (argMapping mapping)
        cCall = "::" <> cFunctionName mapping <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res (argMapping mapping) [] False isStatic) (zip [0..] (argMapping mapping))) <> ")"
        isBoolRet = cReturnType mapping == S.SBool
    in if hasBufferPtr
    then if isBoolRet
            then setup ++
            [ Expr ("size_t size = " <> sizeExpr)
            , Expr ("std::vector<uint8_t> result(size)")
            , Expr ("bool ok = " <> cCall)
            ] ++ [If "!ok" errCheck []] ++
            [ Return "result"
            ]
            else setup ++
            [ Expr ("size_t size = " <> sizeExpr)
            , Expr ("std::vector<uint8_t> result(size)")
            , Expr cCall
            ] ++ errCheck ++
            [ Return "result"
            ]
    else setup ++
        [ Expr ("size_t size = " <> sizeExpr)
        , Expr ("auto ptr = " <> cCall)
        ] ++ errCheck ++
        [ Return "std::vector<uint8_t>(ptr, ptr + size)"
        ]

generateListPropertyBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> CFunctionMapping -> Maybe SMethod -> [CppStmt] -> [CppStmt] -> SType -> Bool -> [CppStmt]
generateListPropertyBody cp allRes idTys enumsList res prop mapping sizeGetter setup errCheck elemTy isStatic =
    let name = propName prop
        resName = resourceName res
        isInstanceSource (ThisObject _)   = True
        isInstanceSource (PathObject _ _) = True
        isInstanceSource (PathId _)       = True
        isInstanceSource _                = False

        (rawTyStr, needsCast) = case elemTy of
                SResourceId _ -> ("uint32_t", True) -- Assuming IDs are uint32_t
                _             -> (renderCppType (toRawCppType cp allRes elemTy), False)

        resTyStr = renderCppType (toCppType cp allRes elemTy)

        sizeExpr = case sizeGetter of
            Just sMapMethod ->
                    let sMap = case methodMapping sMapMethod of
                            S.CustomMapping c -> c
                            S.StandardMapping -> I.inferCFunctionMapping allRes res sMapMethod
                        isStaticSize = not (any isInstanceSource (argMapping sMap))
                    in "::" <> cFunctionName sMap <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res (argMapping sMap) [] False isStaticSize) (zip [0..] (argMapping sMap))) <> ")"
            Nothing -> error $ "Unknown size for property " ++ show name ++ " in " ++ show resName

        elemRes = case elemTy of
            SResourceId n -> List.find (\r -> case resourceType r of ResId (SResourceId i) -> i == n; _ -> False) allRes
            _ -> Nothing

        parentName = case elemRes of
            Just r  -> fromMaybe "Tox" (lifecycleParent r)
            Nothing -> "Tox"

        ctorArg = if parentName == resourceName res then "*this" else "core_"

        convert = case elemTy of
            SResourceId _ -> "final_result.emplace_back(" <> ctorArg <> ", v)"
            SEnum _ -> "final_result.push_back(static_cast<" <> resTyStr <> ">(v))"
            _ -> "final_result.push_back(v)"

        cCallArgs = map (toCArgExpr cp allRes idTys enumsList res (argMapping mapping) [] False isStatic) (zip [0..] (argMapping mapping))

        fixCast (src, arg) = case src of
            BufferPtr _ | needsCast -> "reinterpret_cast<" <> toCName cp idTys enumsList elemTy <> "*>(" <> arg <> ")"
            _ -> arg

        cCallArgsFixed = map fixCast (zip (argMapping mapping) cCallArgs)
        cCall = "::" <> cFunctionName mapping <> "(" <> Text.intercalate ", " cCallArgsFixed <> ")"
        isBoolRet = cReturnType mapping == S.SBool

    in if isBoolRet
        then setup ++
        [ Expr ("size_t size = " <> sizeExpr)
        , Expr ("std::vector<" <> rawTyStr <> "> result(size)")
        , Expr ("bool ok = " <> cCall)
        ] ++ [If "!ok" errCheck []] ++
        [ Expr ("std::vector<" <> resTyStr <> "> final_result")
        , Expr ("final_result.reserve(size)")
        , Expr ("do { for (const auto& v : result) { " <> convert <> "; } } while(0)")
        , Return "final_result"
        ]
        else setup ++
        [ Expr ("size_t size = " <> sizeExpr)
        , Expr ("std::vector<" <> rawTyStr <> "> result(size)")
        , Expr cCall
        ] ++ errCheck ++
        [ Expr ("std::vector<" <> resTyStr <> "> final_result")
        , Expr ("final_result.reserve(size)")
        , Expr ("do { for (const auto& v : result) { " <> convert <> "; } } while(0)")
        , Return "final_result"
        ]

generateFixedPropertyBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> CFunctionMapping -> [CppStmt] -> [CppStmt] -> Either (SType, Text) Text -> SType -> Bool -> [CppStmt]
generateFixedPropertyBody cp allRes idTys enumsList res mapping setup errCheck sizerOrElem _ isStatic =
    let cCall = "::" <> cFunctionName mapping <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res (argMapping mapping) [] False isStatic) (zip [0..] (argMapping mapping))) <> ")"

        isBoolRet = cReturnType mapping == S.SBool

        (resTyStr, sizer) = case sizerOrElem of
            Right s -> ("uint8_t", s)
            Left (elemTy, s) -> (renderCppType (toCppType cp allRes elemTy), s)

        arrayTy = "std::array<" <> resTyStr <> ", " <> sizer <> ">"
        vectorTy = "std::vector<" <> resTyStr <> ">"

    in if isConstantSize sizer
        then if isBoolRet
            then setup ++
                    [ Expr (arrayTy <> " result")
                    , Expr ("bool ok = " <> cCall)
                    ] ++ [If "!ok" errCheck []] ++
                    [ Return "result"
                    ]
            else setup ++
                    [ Expr (arrayTy <> " result")
                    , Expr cCall
                    ] ++ errCheck ++
                    [ Return "result"
                    ]
        else if isBoolRet
            then setup ++
                    [ Expr (vectorTy <> " result(" <> sizer <> ")")
                    , Expr ("bool ok = " <> cCall)
                    ] ++ [If "!ok" errCheck []] ++
                    [ Return "result"
                    ]
            else setup ++
                    [ Expr (vectorTy <> " result(" <> sizer <> ")")
                    , Expr cCall
                    ] ++ errCheck ++
                    [ Return "result"
                    ]

generateSimplePropertyBody :: Text -> [SResource] -> [SEnumModel] -> CFunctionMapping -> [CppStmt] -> [CppStmt] -> SType -> Text -> [CppStmt]
generateSimplePropertyBody cp allRes _ mapping setup errCheck semTy callExpr =
    let errTy = cErrorType mapping

        castedResult = case semTy of
                SEnum n -> "static_cast<" <> n <> ">(result)"
                SResourceId n ->
                    let cppTy = toCppType cp allRes (SResourceId n)
                        isWrapper = case cppTy of
                            TyUserDefined tName -> isJust (List.find (\r -> resourceName r == tName) allRes)
                            _ -> False
                    in if isWrapper
                        then (case cppTy of TyUserDefined t -> t; _ -> n) <> "(*this, result)"
                        else "static_cast<" <> (case cppTy of TyUserDefined t -> t; _ -> n) <> ">(result)"
                SHandle n -> (case toCppType cp allRes (SHandle n) of TyUserDefined t -> t; _ -> n) <> "(result)"
                _ -> "result"
    in case errTy of
        Just _ ->
            setup ++
            [ Expr ("auto result = " <> callExpr) ] ++
            errCheck ++
            [ Return castedResult ]
        Nothing ->
            let castedCall = case semTy of
                    SHandle n ->
                        if "_cb" `Text.isSuffixOf` Text.toLower n || n == "void"
                        then callExpr
                        else (case toCppType cp allRes (SHandle n) of TyUserDefined t -> t; _ -> n) <> "(" <> callExpr <> ")"
                    SEnum n -> "static_cast<" <> n <> ">(" <> callExpr <> ")"
                    SResourceId n ->
                        let cppTy = toCppType cp allRes (SResourceId n)
                            isWrapper = case cppTy of
                                TyUserDefined tName -> isJust (List.find (\r -> resourceName r == tName) allRes)
                                _ -> False
                        in if isWrapper
                            then (case cppTy of TyUserDefined t -> t; _ -> n) <> "(*this, " <> callExpr <> ")"
                            else "static_cast<" <> (case cppTy of TyUserDefined t -> t; _ -> n) <> ">(" <> callExpr <> ")"
                    _ -> callExpr
            in [Return castedCall]

generatePropertySetter :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SProperty -> Maybe SMethod -> CppType -> ([CppMember], [CppDecl])
generatePropertySetter _ _ _ _ _ _ Nothing _ = ([], [])
generatePropertySetter cp allRes idTys enumsList res prop (Just s) ty =
    let name = propName prop
        resName = resourceName res
        mapping = case methodMapping s of
                S.CustomMapping c -> c
                S.StandardMapping -> I.inferCFunctionMapping allRes res s

        isInstanceSource (ThisObject _)   = True
        isInstanceSource (PathObject _ _) = True
        isInstanceSource (PathId _)       = True
        isInstanceSource _                = False
        isStatic = not (any isInstanceSource (argMapping mapping))
        accessSpec = if isStatic then Static Public else Public

        semTy = paramType (last (inputs s)) -- Assuming last param is the value

        isComplex TyBytes       = True
        isComplex TyString      = True
        isComplex (TyVector _)  = True
        isComplex (TyArray _ _) = True
        isComplex _             = False

        paramTy = if isComplex ty then TyConst (TyReference ty) else ty

        cArgs = map (toCArgExpr cp allRes idTys enumsList res (argMapping mapping) [SParameter name semTy T.ConstThis []] False isStatic) (zip [0..] (argMapping mapping))
        callExpr = "::" <> cFunctionName mapping <> "(" <> Text.intercalate ", " cArgs <> ")"
        errTy = cErrorType mapping
        body = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                    semEt = case List.find (\e -> enumName e == et) enumsList of
                        Just e  -> enumSemanticName e
                        Nothing -> et
                in [ Expr ("::" <> et <> " error = " <> errOk)
                   , Expr callExpr
                   , If ("error != " <> errOk) [Return ("static_cast<" <> semEt <> ">(error)")] [Return "{}"]
                   ]
            Nothing -> [Expr callExpr]
        ret = if isJust errTy
              then let et = fromMaybe "void" errTy
                       semEt = case List.find (\e -> enumName e == et) enumsList of
                           Just e  -> enumSemanticName e
                           Nothing -> et
                   in TyResult TyVoid (TyUserDefined semEt)
              else TyVoid
    in ([MethodDecl accessSpec ("set_" <> name) ret [CppParam paramTy name] T.MutableThis Nothing], [MethodDef resName ("set_" <> name) ret [CppParam paramTy name] T.MutableThis body])

tyToVector :: SType -> Text -> Text -> Text
tyToVector (SFixedBytes sizer _) _ expr = "std::vector<uint8_t>(" <> expr <> ", " <> expr <> " + " <> sizer <> ")"
tyToVector _ "" expr = "std::vector<uint8_t>(" <> expr <> ", " <> expr <> " + 0 /* FIXME: unknown size */)"
tyToVector _ sizeExpr expr = "std::vector<uint8_t>(" <> expr <> ", " <> expr <> " + " <> sizeExpr <> ")"

generateMethod :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> Bool -> ([CppMember], [CppDecl])
generateMethod cp allRes idTys enumsList res meth isTmpl =
    let methodRawName = methodName meth
        role = methodRole meth
        isStaticRole = role `elem` [Constructor, StaticRole]
        strippedName = fromMaybe methodRawName (Text.stripPrefix (cPrefix res) methodRawName)
        name = if strippedName == "new" then "make"
               else if strippedName == "delete" then "delete_"
               else if strippedName == "default" then "default_"
               else strippedName
        ret = toCppType cp allRes (output meth)
        errTy = methodErrorType meth
        resStrat = methodResultStrategy meth
        mapping = case methodMapping meth of
            S.CustomMapping c -> c
            S.StandardMapping -> I.inferCFunctionMapping allRes res meth

        -- Include PathObjects and PathIds in C++ params
        extraParams = mapMaybe toExtraParam (argMapping mapping)

        toExtraParam src = case src of
            PathObject 0 _ | (case resourceType res of ResId _ -> True; _ -> False) && not isStaticRole -> Nothing
            PathObject 0 _ ->
                let tyName = I.resolvePathTarget allRes res 0
                in Just $ CppParam (TyReference (TyUserDefined tyName)) "parent"
            PathObject _ _ | (case resourceType res of ResId _ -> True; _ -> False) && not isStaticRole -> Nothing
            PathObject n _ ->
                let tyName = I.resolvePathTarget allRes res n
                in Just $ CppParam (TyReference (TyUserDefined tyName)) ("parent_" <> Text.pack (show n))
            PathId n | n == (length (I.getHierarchy allRes res) - 1) && (case resourceType res of ResId _ -> True; _ -> False) && not isStaticRole -> Nothing
            PathId n | (case resourceType res of ResId _ -> True; _ -> False) && n < (length (I.getHierarchy allRes res) - 1) && not isStaticRole -> Nothing
            PathId n ->
                let tyName = I.resolvePathTarget allRes res n
                    ty = toRawCppType cp allRes (getIdTypeFor allRes tyName)
                in Just $ CppParam ty ("path_id_" <> Text.pack (show n))
            _ -> Nothing

        isSpecialGetter = case output meth of
            SList (SResourceId _) -> True
            SList (SHandle _)     -> True -- Only if it's a wrapper?
            _                     -> False

        params = if isSpecialGetter then [] else extraParams ++ map (toCppParam cp allRes) (inputs meth)

        finalRet = if isSpecialGetter
                   then case output meth of
                       SList t -> TyVector (toCppType cp allRes t)
                       _       -> ret
                   else if isJust errTy
                   then let et = fromMaybe "void" errTy
                            semEt = case List.find (\e -> enumName e == et) enumsList of
                                Just e  -> enumSemanticName e
                                Nothing -> et
                            t = if resStrat == IgnoreReturn && ret == TyVoid then TyVoid else ret
                        in TyResult t (TyUserDefined semEt)
                   else ret

        resName = resourceName res
        mBody = Just $ generateMethodBody cp allRes idTys enumsList res meth mapping isStaticRole isSpecialGetter isTmpl

        cns = if isStaticRole then T.MutableThis
              else if isSpecialGetter then T.MutableThis
              else if isResourceWrapper cp allRes (output meth) then T.MutableThis
              else deriveConstness (argMapping mapping)

    in case role of
        Constructor ->
            let retTy = if isJust errTy
                        then let et = fromMaybe "void" errTy
                                 semEt = case List.find (\e -> enumName e == et) enumsList of
                                     Just e  -> enumSemanticName e
                                     Nothing -> et
                             in TyResult (TyUserDefined resName) (TyUserDefined semEt)
                        else TyUserDefined resName
                decl = MethodDecl (Static Public) name retTy params cns Nothing
                def = case mBody of
                        Just [Deleted] -> []
                        Just b -> [MethodDef resName name retTy params cns b]
                        Nothing -> []
            in ([decl], def)
        Destructor ->
            let body = case mBody of
                        Just b  -> b
                        Nothing -> []

                decl = DestructorDecl Public Nothing
                def = [DestructorDef resName body]
            in ([decl], def)
        StaticRole ->
            let decl = MethodDecl (Static Public) name finalRet params cns Nothing
                def = case mBody of
                        Just b -> [MethodDef resName name finalRet params cns b]
                        Nothing -> []
            in ([decl], def)
        RegistrarRole -> ([], []) -- Handled by generateEvent
        _ ->
            let decl = MethodDecl Public name finalRet params cns Nothing
                def = case mBody of
                        Just [Deleted] -> []
                        Just b -> [MethodDef resName name finalRet params cns b]
                        Nothing -> []
            in ([decl], def)

generateMethodBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> CFunctionMapping -> Bool -> Bool -> Bool -> [CppStmt]
generateMethodBody cp allRes idTys enumsList res meth mapping isStaticMethod isSpecialGetter isTmpl =
    if isSpecialGetter then
        generateSpecialGetterBody cp allRes idTys enumsList res meth mapping
    else
        case methodRole meth of
            Constructor -> generateConstructorBody cp allRes idTys enumsList res meth mapping isStaticMethod
            Destructor -> generateDestructorBody cp allRes idTys enumsList res meth mapping isStaticMethod
            _ -> generateStandardMethodBody cp allRes idTys enumsList res meth mapping isStaticMethod isTmpl

generateSpecialGetterBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> CFunctionMapping -> [CppStmt]
generateSpecialGetterBody cp allRes idTys enumsList _ meth mapping =
    let cFuncName = cFunctionName mapping
        sizeFunc = cFuncName <> "_size"
        sizeCall = "::" <> sizeFunc <> "(instance_)"

        (rawTyStr, resTyStr, convert) = case output meth of
            SList t@(SResourceId _) ->
                ("uint32_t", renderCppType (toCppType cp allRes t), "final_result.emplace_back(*this, v)")
            SList t@(SHandle _) ->
                (renderCppType (toCParamType cp allRes idTys enumsList t), renderCppType (toCppType cp allRes t), "final_result.emplace_back(v)")
            SList t@(SEnum _) ->
                (renderCppType (toCParamType cp allRes idTys enumsList t), renderCppType (toCppType cp allRes t), "final_result.push_back(static_cast<" <> renderCppType (toCppType cp allRes t) <> ">(v))")
            _ -> ("uint32_t", "Unknown", "final_result.push_back(v)")

    in [ Expr ("size_t size = " <> sizeCall)
       , Expr ("std::vector<" <> rawTyStr <> "> result(size)")
       , Expr ("::" <> cFuncName <> "(instance_, result.data())")
       , Expr ("std::vector<" <> resTyStr <> "> final_result")
       , Expr ("final_result.reserve(size)")
       , Expr ("do { for (const auto& v : result) { " <> convert <> "; } } while(0)")
       , Return "final_result"
       ]

generateConstructorBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> CFunctionMapping -> Bool -> [CppStmt]
generateConstructorBody cp allRes idTys enumsList res meth mapping isStaticMethod =
    let cFuncName = cFunctionName mapping
        args = argMapping mapping
        semParams = inputs meth

        cArgs = map (toCArgExpr cp allRes idTys enumsList res args semParams True isStaticMethod) (zip [0..] args)
        callExpr = "::" <> cFuncName <> "(" <> Text.intercalate ", " cArgs <> ")"

        errTy = methodErrorType meth
        resName' = resourceName res
        ctorCall = if case resourceType res of ResId _ -> True; _ -> False
                   then let parentIsCore = case lifecycleParent res of
                                            Just p -> case List.find (\r -> resourceName r == p) allRes of
                                                Just pr -> isRoot pr
                                                Nothing -> False
                                            Nothing -> True -- Default to core if no parent?
                            coreArg = if isStaticMethod
                                      then if parentIsCore then "parent" else "parent.core_"
                                      else "core_"
                        in resName' <> "(" <> coreArg <> ", parent, result)"
                   else resName' <> "(result)"

        setup = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                in [Expr ("::" <> et <> " error = " <> errOk)]
            Nothing -> []

    in if isJust errTy && isStaticMethod
    then
        let errOk = "static_cast<::" <> fromMaybe "void" errTy <> ">(" <> Text.toUpper (fromMaybe "void" errTy <> "_OK") <> ")"
            semErrTy = fromMaybe "void" (methodErrorType meth)
            errType = case List.find (\e -> enumName e == semErrTy) enumsList of
                Just e  -> enumSemanticName e
                Nothing -> semErrTy
        in setup ++ [Expr ("auto result = " <> callExpr), If ("error != " <> errOk) [Return ("static_cast<" <> errType <> ">(error)")] [Return ctorCall]]
    else if isStaticMethod
    then [Expr ("auto result = " <> callExpr), Return ctorCall]
    else
        let member = if case resourceType res of ResId _ -> True; _ -> False then "id_" else "instance_"
            errOk = "static_cast<::" <> fromMaybe "void" errTy <> ">(" <> Text.toUpper (fromMaybe "void" errTy <> "_OK") <> ")"
        in if isJust errTy
           then setup ++ [Expr (member <> " = " <> callExpr), If ("error != " <> errOk) [Return ""] []]
           else [Expr (member <> " = " <> callExpr)]

generateDestructorBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> CFunctionMapping -> Bool -> [CppStmt]
generateDestructorBody cp allRes idTys enumsList res meth mapping isStaticMethod =
    let cFuncName = cFunctionName mapping
        args = argMapping mapping
        semParams = inputs meth
        cArgs = map (toCArgExpr cp allRes idTys enumsList res args semParams True isStaticMethod) (zip [0..] args)
        callExpr = "::" <> cFuncName <> "(" <> Text.intercalate ", " cArgs <> ")"
    in [Expr callExpr]

generateStandardMethodBody :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SMethod -> CFunctionMapping -> Bool -> Bool -> [CppStmt]
generateStandardMethodBody cp allRes idTys enumsList res meth mapping isStaticMethod isTmpl =
    let cFuncName = cFunctionName mapping
        args = argMapping mapping
        semParams = inputs meth

        -- Generate C-style arguments
        cArgs = map (toCArgExpr cp allRes idTys enumsList res args semParams True isStaticMethod) (zip [0..] args)

        callExpr = "::" <> cFuncName <> "(" <> Text.intercalate ", " cArgs <> ")"

        errTy = methodErrorType meth
        resStrat = methodResultStrategy meth

        cns = if isStaticMethod then T.MutableThis
              else if isResourceWrapper cp allRes (output meth) then T.MutableThis
              else deriveConstness (argMapping mapping)

        constCheck = if isTmpl && cns == T.MutableThis && not isStaticMethod
                     then [Expr "static_assert(!std::is_const<ToxT>::value, \"Cannot call mutable method on const handle\")"]
                     else []

        setup = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                in [Expr ("::" <> et <> " error = " <> errOk)]
            Nothing -> []
        errCheck = case errTy of
            Just et ->
                let errOk = "static_cast<::" <> et <> ">(" <> Text.toUpper (et <> "_OK") <> ")"
                    semEt = case List.find (\e -> enumName e == et) enumsList of
                        Just e  -> enumSemanticName e
                        Nothing -> et
                in [If ("error != " <> errOk) [Return ("static_cast<" <> semEt <> ">(error)")] []]
            Nothing -> []

        call = case resStrat of
                ReturnIsValue ->
                    let castedCall val = case output meth of
                            SHandle n ->
                                if "_cb" `Text.isSuffixOf` Text.toLower n || n == "void"
                                then val
                                else (case toCppType cp allRes (SHandle n) of TyUserDefined t -> t; _ -> n) <> "(" <> val <> ")"
                            SEnum n -> "static_cast<" <> n <> ">(" <> val <> ")"
                            SResourceId n ->
                                let cls = case toCppType cp allRes (SResourceId n) of TyUserDefined t -> t; _ -> n
                                in cls <> "(*this, " <> val <> ")"
                            _ -> val
                    in if output meth == SBytes
                       then let sizeExpr = case cSizeFunctionName mapping of
                                              Just sn -> "::" <> sn <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res args semParams True isStaticMethod) (zip [0..] args)) <> ")"
                                              Nothing -> error $ "Unknown size for method " ++ show (methodName meth) ++ " in " ++ show (resourceName res)
                                hasBufferPtr = any (\case BufferPtr _ -> True; _ -> False) args
                            in if hasBufferPtr
                               then [ Expr ("size_t size = " <> sizeExpr)
                                    ] ++ errCheck ++
                                    [ Expr ("std::vector<uint8_t> result(size)")
                                    , Expr callExpr
                                    ] ++ errCheck ++
                                    [ Return "result"
                                    ]
                               else [Expr ("auto result = " <> callExpr)] ++ errCheck ++ [Return $ tyToVector (output meth) sizeExpr "result"]
                       else case output meth of
                           SFixedBytes sizerRaw _ ->
                               let sizer = sanitizeSizeExpr semParams sizerRaw
                                   resTyStr = "std::array<uint8_t, " <> sizer <> ">"
                                   hasBufferPtr = any (\case BufferPtr _ -> True; _ -> False) args
                               in if isConstantSize sizer
                                  then if hasBufferPtr
                                       then [ Expr (resTyStr <> " result")
                                            , Expr callExpr
                                            ] ++ errCheck ++
                                            [ Return "result"
                                            ]
                                       else [Expr ("auto result = " <> callExpr)] ++ errCheck ++
                                            [ Expr (resTyStr <> " arr_result")
                                            , Expr ("std::copy_n(result, " <> sizer <> ", arr_result.begin())")
                                            , Return "arr_result"
                                            ]
                                  else if hasBufferPtr
                                       then [ Expr ("std::vector<uint8_t> result(" <> sizer <> ")")
                                            , Expr callExpr
                                            ] ++ errCheck ++
                                            [ Return "result"
                                            ]
                                       else [Expr ("auto result = " <> callExpr)] ++ errCheck ++ [Return $ tyToVector (output meth) sizer "result"]
                           SFixedList elemTy sizerRaw _ ->
                               let sizer = sanitizeSizeExpr semParams sizerRaw
                                   resTyStr = renderCppType (TyArray (toCppType cp allRes elemTy) sizer)
                                   hasBufferPtr = any (\case BufferPtr _ -> True; _ -> False) args
                               in if isConstantSize sizer
                                  then if hasBufferPtr
                                       then [ Expr (resTyStr <> " result")
                                            , Expr callExpr
                                            ] ++ errCheck ++
                                            [ Return "result"
                                            ]
                                       else [Expr ("auto result = " <> callExpr)] ++ errCheck ++
                                            [ Expr (resTyStr <> " arr_result")
                                            , Expr ("std::copy_n(result, " <> sizer <> ", arr_result.begin())")
                                            , Return "arr_result"
                                            ]
                                  else if hasBufferPtr
                                       then [ Expr ("std::vector<" <> renderCppType (toCppType cp allRes elemTy) <> "> result(" <> sizer <> ")")
                                            , Expr callExpr
                                            ] ++ errCheck ++
                                            [ Return "result"
                                            ]
                                       else [Expr ("auto result = " <> callExpr)] ++ errCheck ++
                                            [ Expr ("std::vector<" <> renderCppType (toCppType cp allRes elemTy) <> "> vec_result(" <> sizer <> ")")
                                            , Expr ("std::copy_n(result, " <> sizer <> ", vec_result.begin())")
                                            , Return "vec_result"
                                            ]
                           _ -> if output meth == SVoid
                            then [Expr callExpr] ++ errCheck
                            else [Expr ("auto result = " <> callExpr)] ++ errCheck ++ [Return (castedCall "result")]
                IgnoreReturn ->
                    let errOk = "static_cast<::" <> fromMaybe "void" errTy <> ">(" <> Text.toUpper (fromMaybe "void" errTy <> "_OK") <> ")"
                        semErrTy = fromMaybe "void" (methodErrorType meth)
                        errType = case List.find (\e -> enumName e == semErrTy) enumsList of
                            Just e  -> enumSemanticName e
                            Nothing -> semErrTy
                        captureResult = if output meth /= SVoid then "auto result = " else ""
                        returnResult = if output meth /= SVoid then "result" else "{}"
                    in [Expr (captureResult <> callExpr), If ("error != " <> errOk) [Return ("static_cast<" <> errType <> ">(error)")] [Return returnResult]]
                ReturnIsResult ->
                    let castedResult = case output meth of
                            SEnum n -> "static_cast<" <> n <> ">(result)"
                            SResourceId n ->
                                let cls = case toCppType cp allRes (SResourceId n) of TyUserDefined t -> t; _ -> n
                                in cls <> "(*this, result)"
                            SHandle n -> (case toCppType cp allRes (SHandle n) of TyUserDefined t -> t; _ -> n) <> "(result)"
                            _ -> "result"
                        errOk = "static_cast<::" <> fromMaybe "void" errTy <> ">(" <> Text.toUpper (fromMaybe "void" errTy <> "_OK") <> ")"
                        semErrTy = fromMaybe "void" (methodErrorType meth)
                        errType = case List.find (\e -> enumName e == semErrTy) enumsList of
                            Just e  -> enumSemanticName e
                            Nothing -> semErrTy
                    in if output meth == SVoid
                       then [Expr callExpr, If ("error != " <> errOk) [Return ("static_cast<" <> errType <> ">(error)")] [Return "{}"]]
                       else [Expr ("auto result = " <> callExpr), If ("error != " <> errOk) [Return ("static_cast<" <> errType <> ">(error)")] [Return castedResult]]
                _ ->
                    let castedCall = case output meth of
                            SHandle n ->
                                                                if "_cb" `Text.isSuffixOf` Text.toLower n || n == "void"
                                                                then callExpr
                                                                else case List.find (\r -> cName r == n) allRes of
                                                                       Just r -> resourceName r <> "(" <> callExpr <> ")"
                                                                       Nothing -> n <> "(" <> callExpr <> ")"
                            SEnum n ->
                                if isResourceWrapper cp allRes (SResourceId n)
                                then
                                    let cls = n
                                    in cls <> "(*this, " <> callExpr <> ")"
                                else "static_cast<" <> n <> ">(" <> callExpr <> ")"
                            SResourceId n ->
                                let cppTy = toCppType cp allRes (SResourceId n)
                                    isWrapper = case cppTy of
                                        TyUserDefined tName -> isJust (List.find (\r -> resourceName r == tName) allRes)
                                        _ -> False
                                in if isWrapper
                                   then
                                       let cls = case cppTy of TyUserDefined t -> t; _ -> n
                                       in cls <> "(*this, " <> callExpr <> ")"
                                   else "static_cast<" <> (case cppTy of TyUserDefined t -> t; _ -> n) <> ">(" <> callExpr <> ")"
                            _ -> callExpr
                    in if output meth == SBytes || isFixedBytes (output meth)
                       then let sizeExpr = case cSizeFunctionName mapping of
                                              Just sn -> "::" <> sn <> "(" <> Text.intercalate ", " (map (toCArgExpr cp allRes idTys enumsList res args semParams True isStaticMethod) (zip [0..] args)) <> ")"
                                              Nothing -> ""
                            in [Expr ("auto result = " <> callExpr), Return $ tyToVector (output meth) sizeExpr "result"]
                       else if output meth == SVoid
                       then [Expr callExpr]
                       else [Return castedCall]

    in constCheck ++ setup ++ call

toCArgExpr :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> [CArgSource] -> [SParameter] -> Bool -> Bool -> (Int, CArgSource) -> Text
toCArgExpr cp allRes idTys enumsList res allArgs semParams isMethod isStaticMethod (i, src) =
    case src of
        ThisObject _ -> "instance_"
        PathObject 0 _ ->
            if isStaticMethod then "parent.instance_"
            else if case resourceType res of ResId _ -> True; _ -> False
            then "core_.instance_"
            else "instance_"
        PathObject n _ ->
            let name' = if n == 0 then "core_" else if n == 1 then "parent_" else "path_id_" <> Text.pack (show (n-1)) <> "_"
                pName = if n == 0 then "parent" else "parent_" <> Text.pack (show n)
            in if isStaticMethod then pName <> ".instance_"
               else if case resourceType res of ResId _ -> True; _ -> False then name' <> ".instance_"
               else if isMethod then pName <> ".instance_"
               else name' <> ".instance_"
        PathId n ->
            let numParents = length (I.getHierarchy allRes res) - 1
                name' = if n == numParents then "id_"
                        else if n == 0 then "core_.id_" -- Probably not used as PathId
                        else "path_id_" <> Text.pack (show n) <> "_"
                pName = if n == numParents then "id" else "path_id_" <> Text.pack (show n)
            in if isStaticMethod then pName <> ".value"
               else if case resourceType res of ResId _ -> True; _ -> False then name' <> ".value"
               else if isMethod then pName <> ".value"
               else name' <> ".value"
        SemanticArg n ->
            if n >= 0 && n < length semParams then
                let p = semParams !! n
                in case paramType p of
                    SBytes -> "const_cast<uint8_t*>(" <> (paramName p) <> ".data())"
                    SFixedBytes _ _ -> "const_cast<uint8_t*>(" <> (paramName p) <> ".data())"
                    SList _ -> (paramName p) <> ".data()"
                    SFixedList _ _ _ -> (paramName p) <> ".data()"
                    SString -> (paramName p) <> ".data()"
                    SCallback _ -> paramName p
                    SHandle n' ->
                        if n' == "void"
                        then paramName p
                        else case List.find (\r -> cName r == n') allRes of
                            Just r -> case resourceType r of
                                ResHandle -> (paramName p) <> ".instance_"
                                ResId _   -> (paramName p) <> ".id_.value"
                            Nothing -> (paramName p) <> ".instance_" -- Fallback
                    SEnum n' -> "static_cast<::" <> toCName cp idTys enumsList (SEnum n') <> ">(" <> paramName p <> ")"
                    SResourceId n' ->
                        let cast = "static_cast<::" <> toCName cp idTys enumsList (SResourceId n') <> ">"
                            acc = case List.find (\res' -> case resourceType res' of ResId (SResourceId i') -> i' == n'; _ -> False) allRes of
                                Just _  -> ".id_.value"
                                Nothing -> ".value"
                        in cast <> "(" <> paramName p <> acc <> ")"
                    _ -> paramName p
            else "0"
        BufferSize ->
            if i > 0 && (i - 1) < length allArgs then
                case allArgs !! (i - 1) of
                    SemanticArg n ->
                        if n >= 0 && n < length semParams
                        then (paramName (semParams !! n)) <> ".size()"
                        else "0"
                    PathObject 0 _ ->
                        if isStaticMethod then "parent.instance_"
                        else if case resourceType res of ResId _ -> True; _ -> False
                        then "parent_.instance_"
                        else "instance_"
                    _ -> "0"
            else "0"
        ErrorPtr -> "&error"
        BufferPtr _ -> "result.data()"
        UserData -> "this"
        Constant v -> Text.pack (show v)

toCppParam :: Text -> [SResource] -> SParameter -> CppParam
toCppParam cp allRes (SParameter n t _ _) =
    let ty = case t of
            SBytes          -> TySpan (TyConst (TyUInt 8))
            SFixedBytes _ _ -> TySpan (TyConst (TyUInt 8))
            SFixedList elemTy _ _ -> TySpan (TyConst (toCppType cp allRes elemTy))
            SString         -> TyConst (TyReference TyString)
            SHandle h ->
                if h == "void"
                then toCppType cp allRes (SHandle h)
                else let rName = case List.find (\res -> cName res == h) allRes of
                                     Just r  -> resourceName r
                                     Nothing -> h
                     in if isResourceWrapper cp allRes t
                        then TyReference (TyConst (TyUserDefined rName))
                        else TyReference (TyUserDefined rName)
            SResourceId _ ->
                let cppTy = toCppType cp allRes t
                in if isResourceWrapper cp allRes t
                   then TyReference (TyConst cppTy)
                   else cppTy
            _ -> toCppType cp allRes t
    in CppParam ty n

generateEvent :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> SEvent -> ([CppMember], [CppMember], [CppDecl], [CppStmt])
generateEvent cp allRes idTys enumsList res ev =
    let name = "callback_" <> eventName ev

        -- High level params
        stdCbParams = map (toCppParam cp allRes) (eventParams ev)

        -- Virtual method in Handler
        virtMethodName = "on_" <> eventName ev
        virtMethod = MethodDecl (Virtual Public) virtMethodName TyVoid stdCbParams T.MutableThis (Just [])

        -- C static wrapper params
        (cParams, callArgs) = unzip $ map (expandCParams cp allRes idTys enumsList "wrapper" (eventParams ev)) (eventParams ev)
        cParamsFlat = concat cParams

        -- Add handle and user_data to C params
        handleTy = TyPointer (TyUserDefined ("::" <> cName res))
        userDataTy = TyPointer TyVoid
        cParamsFull = [CppParam handleTy "handle"] ++ cParamsFlat ++ [CppParam userDataTy "user_data"]

        staticCbName = eventName ev <> "_cb_static"
        staticDecl = MethodDecl (Static Private) staticCbName TyVoid cParamsFull T.MutableThis Nothing

        -- Static callback body
        staticBody =
            [ Expr ("auto* wrapper = static_cast<" <> resourceName res <> "*>(user_data)")
            , If ("wrapper->handler_")
                [ Expr ("wrapper->handler_->" <> virtMethodName <> "(" <> Text.intercalate ", " (concat callArgs) <> ")") ]
                []
            ]

        -- Registration statement
        regStmt = Expr ("::" <> cPrefix res <> name <> "(instance_, " <> staticCbName <> if eventHasUserData ev then ", this)" else ")")

    in ([virtMethod], [staticDecl], [MethodDef (resourceName res) staticCbName TyVoid cParamsFull T.MutableThis staticBody], [regStmt])

expandCParams :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> Text -> [SParameter] -> SParameter -> ([CppParam], [Text])
expandCParams cp allRes idTys enumsList wrapperName allParams (SParameter n t _ _) =
   case t of
       SBytes | n `elem` ["y", "u", "v"] ->
           let size = case n of
                   "y" -> "std::max<int32_t>(width, std::abs(ystride)) * height"
                   "u" -> "std::max<int32_t>(width/2, std::abs(ustride)) * (height/2)"
                   "v" -> "std::max<int32_t>(width/2, std::abs(vstride)) * (height/2)"
                   _   -> "0"
           in ( [CppParam (TyPointer (TyConst (TyUInt 8))) n]
              , [ "std::vector<uint8_t>(" <> n <> ", " <> n <> " + (" <> size <> "))" ]
              )
       SBytes ->
           ( [CppParam (TyPointer (TyConst (TyUInt 8))) n, CppParam TySizeT (n <> "_len")]
           , [ "std::vector<uint8_t>(" <> n <> ", " <> n <> " + " <> n <> "_len)" ]
           )
       SFixedBytes sizer _ ->
           ( [CppParam (TyPointer (TyConst (TyUInt 8))) n]
           , [ "std::span<const uint8_t>(" <> n <> ", " <> sizer <> ")" ]
           )
       SFixedList elemTy sizer _ ->
           let cElemTy = toCParamType cp allRes idTys enumsList elemTy
               cppElemTy = toCppType cp allRes elemTy
               spanTy = "std::span<const " <> renderCppType cppElemTy <> ">"
           in ( [CppParam (TyPointer (TyConst cElemTy)) n]
              , [ spanTy <> "(" <> n <> ", " <> sizer <> ")" ]
              )
       SList elemTy ->
           let cElemTy = toCParamType cp allRes idTys enumsList elemTy
               cppElemTy = toCppType cp allRes elemTy
               vecTy = renderCppType (TyVector cppElemTy)
           in ( [CppParam (TyPointer (TyConst cElemTy)) n, CppParam TySizeT (n <> "_len")]
              , [ vecTy <> "(" <> n <> ", " <> n <> " + " <> n <> "_len)" ]
              )
       SString ->
           ( [CppParam (TyPointer (TyConst (TyUserDefined "char"))) n, CppParam TySizeT (n <> "_len")]
           , [ "std::string(" <> n <> ", " <> n <> "_len)" ]
           )
       _ ->
           let cTy = toCParamType cp allRes idTys enumsList t
               arg = case t of
                   SResourceId rName ->
                       let res = List.find (\r -> case resourceType r of
                                   ResId (SResourceId i) -> i == rName
                                   _                     -> False) allRes
                       in case res of
                           Just r | case resourceType r of ResId _ -> True; _ -> False ->
                               constructResource cp allRes wrapperName allParams r n
                           _ ->
                               let resType = renderCppType (toCppType cp allRes t)
                               in resType <> "(*" <> wrapperName <> ", " <> n <> ")"
                   SEnum _ -> "static_cast<" <> renderCppType (toCppType cp allRes t) <> ">(" <> n <> ")"
                   SBool -> n
                   _ -> n
           in ([CppParam cTy n], [arg])

isConstantSize :: Text -> Bool
isConstantSize t = Text.all (\c -> isUpper c || isDigit c || c == '_') t && not (Text.null t)

toCParamType :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SType -> CppType
toCParamType cp allRes idTys enumsList t =
    case t of
        SResourceId _ -> TyUInt 32
        SEnum _       -> TyUserDefined ("::" <> toCName cp idTys enumsList t)
        SList elemTy  -> toCParamType cp allRes idTys enumsList elemTy
        _             -> toRawCppType cp allRes t

toCppType :: Text -> [SResource] -> SType -> CppType
toCppType cp allRes = \case
    SVoid -> TyVoid
    SBool -> TyBool
    SInt 8 -> TyInt 8
    SInt 16 -> TyInt 16
    SInt 32 -> TyInt 32
    SInt 64 -> TyInt 64
    SUInt 8 -> TyUInt 8
    SUInt 16 -> TyUInt 16
    SUInt 32 -> TyUInt 32
    SUInt 64 -> TyUInt 64
    SSizeT -> TySizeT
    SString -> TyString
    SBytes -> TyBytes
    SFixedBytes s _ -> if isConstantSize s then TyArray (TyUInt 8) s else TyBytes
    SFixedList t s _ -> if isConstantSize s then TyArray (toCppType cp allRes t) s else TyVector (toCppType cp allRes t)
    SEnum n -> TyUserDefined n
    SCallback n -> TyPointer (TyUserDefined n)
    SHandle n ->
        if n == "void"
        then TyPointer TyVoid
        else case List.find (\res -> cName res == n) allRes of
            Just r  -> TyUserDefined (resourceName r)
            Nothing -> TyUserDefined n
    SResourceId n ->
        case List.find (\res -> case resourceType res of ResId (SResourceId i) -> i == n; _ -> False) allRes of
            Just r  -> TyUserDefined (resourceName r)
            Nothing -> TyUserDefined n
    SList t -> TyVector (toCppType cp allRes t)
    SInt b -> TyInt b
    SUInt b -> TyUInt b

constructResource :: Text -> [SResource] -> Text -> [SParameter] -> SResource -> Text -> Text
constructResource cp allRes wrapperName allParams targetRes targetIdName =
    let getPath res = case lifecycleParent res of
            Just pName -> case List.find (\r -> resourceName r == pName) allRes of
                Just p  -> getPath p ++ [resourceName res]
                Nothing -> [resourceName res]
            Nothing -> [resourceName res]
        path = getPath targetRes
    in doConstruct (reverse path)
  where
    doConstruct [] = "*" <> wrapperName
    doConstruct [_] = "*" <> wrapperName
    doConstruct (resName : parents) =
        let parentObj = doConstruct parents
            res = fromMaybe targetRes (List.find (\r -> resourceName r == resName) allRes)

            idArg = if resName == resourceName targetRes then targetIdName
                    else findIdParamName resName allParams

            clsName = renderCppType (toCppType cp allRes (SHandle (resourceName res)))
        in clsName <> "(" <> parentObj <> ", " <> idArg <> ")"

    findIdParamName resName params =
        let res = fromMaybe targetRes (List.find (\r -> resourceName r == resName) allRes)
            targetIdType = case resourceType res of ResId t -> Just t; _ -> Nothing
            expectedName = Text.toLower (cName res) <> "_number"
        in case targetIdType of
            Just t -> case List.find (\p -> paramType p == t) params of
                Just p -> paramName p
                Nothing -> case List.find (\p -> paramName p == expectedName) params of
                    Just p  -> paramName p
                    Nothing -> "/* Missing ID for " <> resName <> " */"
            Nothing -> "/* No ID for " <> resName <> " */"

toRawCppType :: Text -> [SResource] -> SType -> CppType
toRawCppType cp allRes = \case
    SResourceId n ->
        case List.find (\r -> cName r == n) allRes of
            Just r -> case resourceType r of
                ResId t -> toRawCppType cp allRes t
                _       -> TyUserDefined n
            Nothing -> TyUserDefined n
    t -> toCppType cp allRes t

isFixedBytes :: SType -> Bool
isFixedBytes (SFixedBytes _ _) = True
isFixedBytes _                 = False

isResourceWrapper :: Text -> [SResource] -> SType -> Bool
isResourceWrapper _ allRes t = case t of
    SResourceId n         -> isKnownResource n || isKnownResourceById n
    SList (SResourceId n) -> isKnownResource n || isKnownResourceById n
    SHandle n             -> isKnownResourceHandle n
    _                     -> False
    where
      isKnownResource n = isJust $ List.find (\res -> resourceName res == n || cName res == n) allRes
      isKnownResourceHandle n = isJust $ List.find (\res -> cName res == n) allRes
      isKnownResourceById n = isJust $ List.find (\res -> case resourceType res of
          ResId (SResourceId i) -> i == n
          _                     -> False) allRes

deriveConstness :: [CArgSource] -> T.Constness
deriveConstness args =
    if any isMutable args then T.MutableThis else T.ConstThis
  where
    isMutable (ThisObject T.MutableThis)   = True
    isMutable (PathObject _ T.MutableThis) = True
    isMutable _                            = False

findSizeConstant :: [Text] -> Maybe Text
findSizeConstant cs = List.find (\c -> "_SIZE" `Text.isSuffixOf` c || "_LENGTH" `Text.isSuffixOf` c) cs

getIdTypeFor :: [SResource] -> Text -> SType
getIdTypeFor allRes rn =
    case List.find (\r -> resourceName r == rn) allRes of
        Just r' -> case resourceType r' of
            ResId t -> t
            _       -> SResourceId (rn <> "Id")
        Nothing -> SResourceId (rn <> "Id")

generateTrait :: Text -> [SResource] -> SResource -> S.SResourceTrait -> [CppMember]
generateTrait cp allRes res (S.Iterable elemTy smName gmName) =
    let elemCppTy = toCppType cp allRes elemTy
        iterName = "iterator"
        name = resourceName res
    in
    [ CommentMember "Iterators"
    , NestedDecl $ Class iterName
        [ MemberDecl Public (TyPointer (TyConst (TyUserDefined name))) "container"
        , MemberDecl Public (TyUInt 32) "index"
        , MethodDecl Public "operator*" elemCppTy [] T.ConstThis (Just [Return ("container->" <> gmName <> "(index)")])
        , MethodDecl Public "operator!=" TyBool [CppParam (TyConst (TyReference (TyUserDefined iterName))) "other"] T.ConstThis (Just [Return "index != other.index"])
        , MethodDecl Public "operator++" (TyReference (TyUserDefined iterName)) [] T.MutableThis (Just [Expr "++index", Return "*this"])
        ]
    , MethodDecl Public "begin" (TyUserDefined iterName) [] T.ConstThis (Just [Return (iterName <> "{this, 0}")])
    , MethodDecl Public "end" (TyUserDefined iterName) [] T.ConstThis (Just [Return (iterName <> "{this, " <> smName <> "()}")])
    ]

generateVariantMethod :: Text -> [SResource] -> [SEnumModel] -> [SVariant] -> SResource -> ([CppMember], [CppDecl])
generateVariantMethod cp allRes enumsList variants_ res =
    let variantInfo = List.find (\v -> variantName v == resourceName res) variants_
    in case variantInfo of
        Just (SVariant vName vEnum vMembers) ->
            let retTy = TyOptional (TyUserDefined (vName <> "Variant"))
                mName = "get_variant"
                cns = T.ConstThis
                body = [Expr ("auto type = get_type()")] ++ map (mkVariantBranch cp allRes enumsList vEnum) vMembers ++ [Return "std::nullopt"]
            in ([MethodDecl Public mName retTy [] cns Nothing], [MethodDef (resourceName res) mName retTy [] cns body])
        Nothing -> ([], [])

generateEventHandling :: Text -> [SResource] -> [SIdTypeModel] -> [SEnumModel] -> SResource -> ([CppMember], [CppDecl])
generateEventHandling cp allRes idTys enumsList res =
    let (ev_handler_methods, ev_static_decls, ev_static_defs, ev_reg_stmts) = List.unzip4 $ map (generateEvent cp allRes idTys enumsList res) (events res)
        name = resourceName res
    in if null (events res) then ([], [])
       else
           let handlerClass =
                   NestedDecl $ Class "Handler" $
                       [ DestructorDecl (Virtual Public) (Just []) ] ++
                       concat ev_handler_methods

               handlerMember = [ MemberDecl Private (TyPointer (TyUserDefined "Handler")) "handler_ = nullptr" ]

               setHandlerDecl = MethodDecl Public "set_handler" TyVoid [CppParam (TyPointer (TyUserDefined "Handler")) "handler"] T.MutableThis Nothing

               setHandlerBody =
                   [ Expr "handler_ = handler" ] ++ concat ev_reg_stmts

               setHandlerDef = MethodDef name "set_handler" TyVoid [CppParam (TyPointer (TyUserDefined "Handler")) "handler"] T.MutableThis setHandlerBody

           in ([handlerClass] ++ handlerMember ++ concat ev_static_decls ++ [setHandlerDecl], concat ev_static_defs ++ [setHandlerDef])

generateHandleMembers :: Text -> [SResource] -> SResource -> [[CppMember]] -> [[CppMember]] -> [CppMember] -> [CppMember] -> [CppMember]
generateHandleMembers cp allRes res prop_decls meth_decls var_decls reg_decls =
    let name = resourceName res
        rawName = cName res
        isConst = all (\m -> methodConstness m == T.ConstThis) (methods res)
        instTy = if isConst
                 then TyPointer (TyConst (TyUserDefined ("::" <> rawName)))
                 else TyPointer (TyUserDefined ("::" <> rawName))

        registerSelf = []

        iterators = concatMap (generateTrait cp allRes res) (S.traits res)

    in concat prop_decls
    ++ concat meth_decls
    ++ var_decls
    ++ iterators
    ++ reg_decls
    ++ [ ConstructorDecl Public [] [("instance_", "nullptr")] (Just [])
       , ConstructorDecl Public [CppParam instTy "instance"] [("instance_", "instance")] (Just registerSelf)
       , CommentMember "No copying"
       , ConstructorDecl Private [CppParam (TyConst (TyReference (TyUserDefined name))) ""] [] (Just [Deleted])
       , MethodDecl Private ("operator=") (TyReference (TyUserDefined name)) [CppParam (TyConst (TyReference (TyUserDefined name))) ""] T.MutableThis (Just [Deleted])
       , CommentMember "Move support"
       , ConstructorDecl Public [CppParam (TyRValueReference (TyUserDefined name)) "other"] [("instance_", "other.instance_")] (Just (registerSelf ++ [Expr "other.instance_ = nullptr"]))
       , MethodDecl Public "operator=" (TyReference (TyUserDefined name)) [CppParam (TyRValueReference (TyUserDefined name)) "other"] T.MutableThis (Just ([If "this != &other" ([Expr "std::swap(instance_, other.instance_)"] ++ registerSelf) [], Return "*this"]))
       , MemberDecl Public instTy "instance_"
       ]

generateIdMembers :: Text -> [SResource] -> SResource -> SType -> [[CppMember]] -> [[CppMember]] -> [CppMember] -> [CppMember]
generateIdMembers cp allRes res idT prop_decls meth_decls reg_decls =
    let name = resourceName res
        isTmpl = True
        className = if isTmpl then name <> "Handle" else name

        parentTy = fromMaybe "void" (lifecycleParent res)

        parentKind = case List.find (\r -> resourceName r == parentTy) allRes of
            Just r  -> resourceType r
            Nothing -> ResHandle

        isIdResource n = isJust $ List.find (\r -> resourceName r == n && (case resourceType r of ResId _ -> True; _ -> False)) allRes

        resolveParentTy n = if isTmpl
                            then if n == "Tox" || n == "Core" then "ToxT"
                                 else if isIdResource n then n <> "Handle<ToxT>"
                                 else n
                            else n

        parentStorageType = case parentKind of
            ResHandle -> TyReference (TyUserDefined (resolveParentTy parentTy))
            _         -> TyUserDefined (resolveParentTy parentTy)

        parentParamType = case parentKind of
            ResHandle -> TyReference (TyUserDefined (resolveParentTy parentTy))
            _         -> TyConst (TyReference (TyUserDefined (resolveParentTy parentTy)))

        idTy = toRawCppType cp allRes idT
        path = I.getHierarchy allRes res
        numParents = length path - 1
        pathIds = [ (toRawCppType cp allRes (getIdTypeFor allRes (I.resolvePathTarget allRes res i)), "path_id_" <> Text.pack (show i))
                  | i <- [1..numParents-1] ]

        headMay []    = Nothing
        headMay (x:_) = Just x

        coreTyName = if isTmpl then "ToxT" else fromMaybe "Core" (headMay path)

        ctorParams = [CppParam (TyReference (TyUserDefined coreTyName)) "core"
                     , CppParam parentParamType "parent"]
                     ++ [CppParam ty pName | (ty, pName) <- pathIds]
                     ++ [CppParam idTy "id"]

        ctorInits = [("core_", "core"), ("parent_", "parent")]
                    ++ [(pName <> "_", pName) | (_, pName) <- pathIds]
                    ++ [("id_", "id")]

        pathMembers = [MemberDecl Public ty (pName <> "_") | (ty, pName) <- pathIds]

        opName = case idTy of TyUserDefined n -> n; _ -> "uint32_t"

        mkPathInit (i, (_, pName)) =
            if i == numParents - 1
            then (pName <> "_", "parent.id_")
            else (pName <> "_", "parent." <> pName <> "_")

        convCtorParams = [CppParam parentParamType "parent", CppParam idTy "id"]

        parentIsCore = parentTy == (fromMaybe "Core" (headMay path))
        coreInit = if parentIsCore then "parent" else "parent.core_"

        convCtorInits = [("core_", coreInit)
                        , ("parent_", "parent")]
                        ++ map mkPathInit (zip [1..] pathIds)
                        ++ [("id_", "id")]

        convCtor = if parentTy /= "void"
                   then [ConstructorDecl Public convCtorParams convCtorInits (Just [])]
                   else []

        tmplCopyCtor = if isTmpl
                       then [ TemplateMemberDecl ["typename U"] $
                              ConstructorDecl Public [CppParam (TyConst (TyReference (TyUserDefined (className <> "<U>")))) "other"]
                              ([("core_", "other.core_"), ("parent_", "other.parent_")]
                               ++ map (\(_, pName) -> (pName <> "_", "other." <> pName <> "_")) pathIds
                               ++ [("id_", "other.id_")])
                              (Just [])
                            ]
                       else []

    in concat prop_decls
       ++ concat meth_decls
       ++ reg_decls
       ++ [ ConstructorDecl Public ctorParams ctorInits (Just [])
          ] ++ convCtor ++ tmplCopyCtor ++
          [ MethodDecl Public ("operator " <> opName) (TyUserDefined "") [] T.ConstThis (Just [Return "id_"])
          , MemberDecl Public (TyReference (TyUserDefined coreTyName)) "core_"
          , MemberDecl Public parentStorageType "parent_"
          ] ++ pathMembers ++ [ MemberDecl Public idTy "id_" ]

sanitizeSizeExpr :: [SParameter] -> Text -> Text
sanitizeSizeExpr params expr =
    List.foldl' replaceParam expr params
  where
    replaceParam e p =
        case paramType p of
            SBytes  -> replaceLen (paramName p) e
            SString -> replaceLen (paramName p) e
            _       -> e

    replaceLen name e =
        let strategies = [ (name <> "_len", name <> ".size()")
                         , (name <> "_length", name <> ".size()")
                         ]
        in List.foldl' (\acc (search, repl) -> Text.replace search repl acc) e strategies
