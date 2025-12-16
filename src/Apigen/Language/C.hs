{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.C (generate) where

import qualified Apigen.Inference       as I
import           Apigen.Semantic        (CArgSource (..), CFunctionMapping (..),
                                         SCallbackTypeModel (..),
                                         SConstantModel (..), SEnumModel (..),
                                         SEvent (..), SIdTypeModel (..),
                                         SLocation (..), SMethod (..),
                                         SParameter (..), SProperty (..),
                                         SResource (..), SResourceType (..),
                                         SType (..), SemanticModel (..))
import qualified Apigen.Semantic        as S
import qualified Apigen.Types           as T
import           Data.Fix               (Fix (..))
import qualified Data.List              as List
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Language.Cimple        (CommentStyle (..), Lexeme (..),
                                         LexemeClass (..), LiteralType (..),
                                         Node, NodeF (..), Nullability (..),
                                         Scope (..))
import qualified Language.Cimple        as Cimple
import qualified Language.Cimple.Pretty as Pretty

l :: LexemeClass -> Text -> Lexeme Text
l = L (Cimple.AlexPn 0 0 0)

generate :: SemanticModel -> Text
generate model = Pretty.render $ Pretty.plain $ Pretty.ppTranslationUnit $ generateAST model

generateAST :: SemanticModel -> [Node (Lexeme Text)]
generateAST (SemanticModel enums_ constants_ idTypes_ callbacks_ resources_ _variants cp _) =
    let allRes = resources_
        apiName = Text.toUpper cp
        headerGuard = apiName <> "_API_H"
        -- Forward declare all handle resources
        handleForwardDecls = [ Fix $ Typedef (Fix $ TyStruct (l IdSueType (S.cName r))) (l IdSueType (S.cName r)) []
                             | r <- allRes, case resourceType r of S.ResHandle -> True; _ -> False ]
    in [ Fix $ Comment Regular (l CmtStart "/*") [l CmtWord " Auto-generated API headers "] (l CmtEnd "*/")
    , Fix $ PreprocIfndef (l IdVar headerGuard)
        [ Fix $ Group $
          [ Fix $ PreprocDefine (l IdVar headerGuard)
          , Fix $ PreprocInclude (l LitString "<stdint.h>")
          , Fix $ PreprocInclude (l LitString "<stdbool.h>")
          , Fix $ PreprocInclude (l LitString "<stddef.h>")
          ]
          ++ map generateConstant constants_
          ++ map generateEnum enums_
          ++ handleForwardDecls
          ++ map (generateCallbackTypedef idTypes_ allRes) callbacks_
          ++ concatMap (generateResource idTypes_ allRes) resources_
        ]
        (Fix $ Group [])
    ]

generateConstant :: SConstantModel -> Node (Lexeme Text)
generateConstant (SConstantModel name val) =
    Fix $ PreprocDefineConst (l IdVar name) (Fix $ LiteralExpr Int (l LitInteger (Text.pack (show val))))

generateCallbackTypedef :: [SIdTypeModel] -> [SResource] -> S.SCallbackTypeModel -> Node (Lexeme Text)
generateCallbackTypedef ids allRes (S.SCallbackTypeModel _ cNm params) =
    let pNodes = map (generateParamDecl ids allRes) params
    in Fix $ TypedefFunction $ Fix $ FunctionPrototype (Fix $ TyStd (l KwVoid "void")) (l IdFuncType cNm) pNodes



generateEnum :: SEnumModel -> Node (Lexeme Text)
generateEnum (SEnumModel name _ members) =
    Fix $ EnumDecl (l IdSueType name)
                   [Fix $ Enumerator (l IdConst (fst m)) Nothing | m <- members]
                   (l IdSueType name)

generateResource :: [SIdTypeModel] -> [SResource] -> SResource -> [Node (Lexeme Text)]
generateResource ids allRes res =
    let idTypedef = case resourceType res of
            S.ResId (S.SResourceId idNm) ->
                 let cNm = case List.find ((== idNm) . S.idName) ids of
                                Just m  -> S.idCName m
                                Nothing -> idNm -- Fallback, shouldn't happen if consistent
                 in [ Fix $ Typedef (Fix $ TyStd (l IdStdType "uint32_t")) (l IdSueType cNm) [] ]
            _ -> []
        props = concatMap (generateProperty ids allRes res) (properties res)
        meths = map (generateMethod ids allRes res) (methods res)
        evs = map (generateEvent ids allRes res) (events res)
    in idTypedef ++ props ++ meths ++ evs

generateProperty :: [SIdTypeModel] -> [SResource] -> SResource -> SProperty -> [Node (Lexeme Text)]
generateProperty _ _ _ _ = []

generateMethod :: [SIdTypeModel] -> [SResource] -> SResource -> SMethod -> Node (Lexeme Text)
generateMethod ids allRes res meth =
    let cmap = case methodMapping meth of
            S.CustomMapping c -> c
            S.StandardMapping -> I.inferCFunctionMapping allRes res meth
    in generateCFunction ids allRes res cmap (inferReturnType cmap (output meth)) (Just (output meth))

generateEvent :: [SIdTypeModel] -> [SResource] -> SResource -> SEvent -> Node (Lexeme Text)
generateEvent ids allRes res ev =
    let prefix = if Text.null (cPrefix res) then resourceName res <> "_callback_" else cPrefix res <> "callback_"
        args = [S.ThisObject T.MutableThis, S.SemanticArg 0] ++ [S.UserData | eventHasUserData ev]
        cmap = S.CFunctionMapping (prefix <> eventName ev) args Nothing Nothing Nothing [SParameter "callback" (SCallback (cCallback ev)) T.MutableThis []] SVoid
    in generateCFunction ids allRes res cmap SVoid Nothing

generateCFunction :: [SIdTypeModel] -> [SResource] -> SResource -> CFunctionMapping -> SType -> Maybe SType -> Node (Lexeme Text)
generateCFunction ids allRes res (CFunctionMapping name args _ _ mErr semParams cRet) _ bufferType =
    Fix $ FunctionDecl Global $ Fix $ FunctionPrototype (toCType ids allRes cRet) (l IdFuncType name) (generateArgs ids allRes res mErr args semParams bufferType)

generateArgs :: [SIdTypeModel] -> [SResource] -> SResource -> Maybe Text -> [CArgSource] -> [SParameter] -> Maybe SType -> [Node (Lexeme Text)]
generateArgs ids allRes res mErr args semParams bufferType =
    if null args
    then [ Fix $ TyStd (l KwVoid "void") ]
    else map (generateArg ids allRes res mErr semParams bufferType) (zip [0..] args)

generateArg :: [SIdTypeModel] -> [SResource] -> SResource -> Maybe Text -> [SParameter] -> Maybe SType -> (Int, CArgSource) -> Node (Lexeme Text)
generateArg ids allRes res mErr semParams bufferType (i, src) =
    let pName = inferParamName ids allRes res src semParams i
    in case src of
        ThisObject cns ->
            Fix $ VarDecl (identityTypeFor ids allRes (resourceName res) cns) (l IdVar pName) []
        PathObject n cns ->
            let tyName = I.resolvePathTarget allRes res n
            in Fix $ VarDecl (identityTypeFor ids allRes tyName cns) (l IdVar pName) []
        PathId n ->
            let tyName = I.resolvePathTarget allRes res n
            in Fix $ VarDecl (identityTypeFor ids allRes (tyName <> "_Number") T.MutableThis) (l IdVar pName) []
        SemanticArg n ->
            if n < length semParams then
                let p = semParams !! n
                    ty = paramType p
                    cns = paramConstness p
                in case ty of
                    SFixedBytes sizer _ ->
                        let base = toCType ids allRes ty
                            cns' = if cns == T.ConstThis then Fix (TyConst base) else base
                            dim = Fix $ VarExpr (l IdVar sizer)
                        in Fix $ VarDecl cns' (l IdVar pName) [Fix $ DeclSpecArray NullabilityUnspecified (Just dim)]
                    SFixedList t sizer _ ->
                        let base = toCType ids allRes t
                            cns' = if cns == T.ConstThis then Fix (TyConst base) else base
                            dim = Fix $ VarExpr (l IdVar sizer)
                        in Fix $ VarDecl cns' (l IdVar pName) [Fix $ DeclSpecArray NullabilityUnspecified (Just dim)]
                    _ ->
                        let base = toCType ids allRes ty
                            final = if cns == T.ConstThis
                                    then case base of
                                        Fix (TyPointer t) -> Fix (TyPointer (Fix (TyConst t)))
                                        _                 -> Fix (TyConst base)
                                    else base
                        in Fix $ VarDecl final (l IdVar pName) []
            else Fix $ VarDecl (Fix $ TyPointer (Fix $ TyStd (l KwVoid "void"))) (l IdVar pName) []
        ErrorPtr ->
            let errTy = fromMaybe "void" mErr
            in Fix $ VarDecl (Fix $ TyPointer (Fix $ TyUserDefined (l IdSueType errTy))) (l IdVar pName) []
        BufferPtr mSizer ->
            let base = case bufferType of
                    Just (SList t) -> toCType ids allRes t
                    Just (SFixedList t _ _) -> toCType ids allRes t
                    _ -> Fix $ TyStd (l IdStdType "uint8_t")
            in case mSizer of
                Just sizer ->
                    let dim = Fix $ VarExpr (l IdVar sizer)
                    in Fix $ VarDecl base (l IdVar pName) [Fix $ DeclSpecArray NullabilityUnspecified (Just dim)]
                Nothing    -> Fix $ VarDecl (Fix $ TyPointer base) (l IdVar pName) []
        BufferSize -> Fix $ VarDecl (Fix $ TyStd (l IdStdType "size_t")) (l IdVar pName) []
        UserData -> Fix $ VarDecl (Fix $ TyPointer (Fix $ TyStd (l KwVoid "void"))) (l IdVar pName) []
        Constant v ->
            let var = Fix $ VarDecl (Fix $ TyStd (l IdStdType "int")) (l IdVar pName) []
                comment = Fix $ Comment Regular (l CmtStart "/*") [l CmtWord (Text.pack (show v))] (l CmtEnd "*/")
            in Fix $ Commented comment var

identityTypeFor :: [SIdTypeModel] -> [SResource] -> Text -> T.Constness -> Node (Lexeme Text)
identityTypeFor ids allRes name cns =
    let numName = name <> "_Number"
        idNm = name <> "_Id"
        findIdCName sName = fmap S.idCName $ List.find ((== sName) . S.idName) ids
        findResCName sName = fmap S.cName $ List.find ((== sName) . S.resourceName) allRes
    in case findIdCName name of
        Just cNm -> Fix $ TyUserDefined (l IdSueType cNm) -- name was already an ID name?
        Nothing -> case findIdCName numName of
            Just cNm -> Fix $ TyUserDefined (l IdSueType cNm)
            Nothing -> case findIdCName idNm of
                Just cNm -> Fix $ TyUserDefined (l IdSueType cNm)
                Nothing ->
                    let cNm = fromMaybe name (findResCName name)
                        base = Fix $ TyUserDefined (l IdSueType cNm)
                        base' = if cns == T.ConstThis then Fix (TyConst base) else base
                    in Fix $ TyPointer base'

inferReturnType :: CFunctionMapping -> SType -> SType
inferReturnType cmap semRet =
    if any isBufferPtr (argMapping cmap)
    then SBool
    else semRet
  where
    isBufferPtr (BufferPtr _) = True
    isBufferPtr _             = False

inferParamName :: [SIdTypeModel] -> [SResource] -> SResource -> CArgSource -> [SParameter] -> Int -> Text
inferParamName ids allRes res src semParams i =
    case src of
        ThisObject _    -> I.deriveHandleName allRes "" (resourceName res)
        PathObject n _  ->
            let tyName = I.resolvePathTarget allRes res n
            in I.deriveHandleName allRes "" tyName
        PathId n ->
            let tyName = I.resolvePathTarget allRes res n
                suffix = if any (\m -> S.idName m == tyName <> "_Number") ids then "_number" else "_id"
            in I.deriveHandleName allRes "" (tyName <> suffix)
        SemanticArg n   -> if n < length semParams then paramName (semParams !! n) else "err"
        ErrorPtr        -> "error"
        BufferPtr _     -> "data" -- Usually overwritten by property name in generateProperty
        BufferSize      -> "length"
        UserData        -> "user_data"
        Constant _      -> "const_" <> Text.pack (show i)


toCType :: [SIdTypeModel] -> [SResource] -> SType -> Node (Lexeme Text)
toCType ids allRes = \case
    SVoid -> Fix $ TyStd (l KwVoid "void")
    SBool -> Fix $ TyStd (l IdStdType "bool")
    SInt 8 -> Fix $ TyStd (l IdStdType "int8_t")
    SInt 16 -> Fix $ TyStd (l IdStdType "int16_t")
    SInt 32 -> Fix $ TyStd (l IdStdType "int32_t")
    SInt 64 -> Fix $ TyStd (l IdStdType "int64_t")
    SUInt 8 -> Fix $ TyStd (l IdStdType "uint8_t")
    SUInt 16 -> Fix $ TyStd (l IdStdType "uint16_t")
    SUInt 32 -> Fix $ TyStd (l IdStdType "uint32_t")
    SUInt 64 -> Fix $ TyStd (l IdStdType "uint64_t")
    SSizeT -> Fix $ TyStd (l IdStdType "size_t")
    SString -> Fix $ TyPointer (Fix $ TyStd (l IdStdType "char"))
    SBytes -> Fix $ TyPointer (Fix $ TyStd (l IdStdType "uint8_t"))
    SFixedBytes _ _ -> Fix $ TyStd (l IdStdType "uint8_t")
    SFixedList t _ _ -> toCType ids allRes t
    SEnum n -> Fix $ TyUserDefined (l IdSueType n)
    SHandle n ->
        let cNm = case List.find ((== n) . S.resourceName) allRes of
                      Just r  -> S.cName r
                      Nothing -> n
        in Fix $ TyPointer (Fix $ TyUserDefined (l IdSueType cNm))
    SResourceId n ->
        let cNm = case List.find ((== n) . S.idName) ids of
                      Just m  -> S.idCName m
                      Nothing -> n
        in Fix $ TyUserDefined (l IdSueType cNm)
    SList (SUInt 8) -> Fix $ TyPointer (Fix $ TyStd (l IdStdType "uint8_t"))
    SList t -> Fix $ TyPointer (toCType ids allRes t)
    SCallback n -> Fix $ TyPointer (Fix $ TyUserDefined (l IdSueType n))
    _ -> Fix $ TyPointer (Fix $ TyStd (l KwVoid "void"))

generateParamDecl :: [SIdTypeModel] -> [SResource] -> SParameter -> Node (Lexeme Text)
generateParamDecl ids allRes (SParameter n t cns _) =
    case t of
        SFixedBytes sizer _ ->
             let base = toCType ids allRes t
                 base' = if cns == T.ConstThis then Fix (TyConst base) else base
                 dim = Fix $ VarExpr (l IdVar sizer)
             in Fix $ VarDecl base' (l IdVar n) [Fix $ DeclSpecArray NullabilityUnspecified (Just dim)]
        SFixedList t' sizer _ ->
             let base' = toCType ids allRes t'
                 base'' = if cns == T.ConstThis then Fix (TyConst base') else base'
                 dim = Fix $ VarExpr (l IdVar sizer)
             in Fix $ VarDecl base'' (l IdVar n) [Fix $ DeclSpecArray NullabilityUnspecified (Just dim)]
        _ ->
            let base = toCType ids allRes t
                base' = if cns == T.ConstThis
                        then case base of
                            Fix (TyPointer p) -> Fix (TyPointer (Fix (TyConst p)))
                            _                 -> Fix (TyConst base)
                        else base
            in Fix $ VarDecl base' (l IdVar n) []
