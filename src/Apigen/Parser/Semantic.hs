{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Apigen.Parser.Semantic (toSemantic, withContext, flattenName, getLoc) where

import qualified Apigen.Facts               as Facts
import qualified Apigen.Inference           as I
import qualified Apigen.Knowledge           as Knowledge
import           Apigen.Model               (isEnumNamer, nameIdsAfterTypes)
import           Apigen.Parser.AST          (Module (..))
import qualified Apigen.Parser.AST          as T
import           Apigen.Parser.SymbolTable  (Name)
import qualified Apigen.Semantic            as S
import           Apigen.SizeExpr            (SizeExpr (..), renderSizeExpr,
                                             sizeExprVars, sizeName)
import qualified Apigen.Tox                 as Tox
import qualified Apigen.Types               as T
import           Control.Applicative        ((<|>))
import           Control.Monad              (mfilter, when)
import           Control.Monad.Trans.Writer (Writer, runWriter, tell)
import           Data.Bits                  ((.&.))
import           Data.Char                  (isUpper, toLower)
import           Data.Function              (on)
import qualified Data.List                  as List
import           Data.Maybe                 (catMaybes, fromMaybe, isJust,
                                             isNothing, listToMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (AlexPosn (..), Lexeme (..),
                                             LexemeClass (..), lexemeText)
import           Text.Read                  (readMaybe)

type DDecl = (T.Decl (Lexeme Name), FilePath)

toSemantic :: Bool -> T.Model (Lexeme Name) -> S.SemanticModel
toSemantic strict model0 =
    let model = rewriteArrayTypedefs model0
        declared = [ (d, moduleFile m) | m <- T.modelMods model, d <- T.moduleDecls m ]
        unbound = unboundReason (gatherResourceNames declared) [ flattenName n | (T.Enumeration _ (L _ _ n) _, _) <- declared ]
        (left, simplifiedDecls) = List.partition (isJust . unbound . fst) declared
        diag0 = [ S.SDiagnostic S.Warning (Just (getLoc fp d)) (n <> ": " <> why)
                | (d, fp) <- left, Just n <- [getDeclName d], Just why <- [unbound d] ]

        enums_ = gatherEnums toSemanticName simplifiedDecls
        constants_ = gatherConstants simplifiedDecls

        allIdNames = gatherIdNames simplifiedDecls
        allCallbackNames = gatherCallbackNames simplifiedDecls
        allScalars = gatherScalars simplifiedDecls
        names = TypeNames allIdNames allCallbackNames [ (n, t) | (n, t, _) <- allScalars ]
        allResNames = gatherResourceNames simplifiedDecls
        cp = commonPrefix allResNames
        toSemanticName = semanticName cp (librarySuffixes cp simplifiedDecls)

        idTypes_ = [ S.SIdTypeModel (toSemanticName n) n (S.SUInt 32) | n <- allIdNames ]
        scalarTypes_ = [ S.SScalarTypeModel (toSemanticName n) n t (Text.pack fp) | (n, t, fp) <- allScalars ]

        (callbacks_, diag4) = runWriter $ gatherCallbackTypes toSemanticName names simplifiedDecls

        resDeclFiles = resourceDeclFiles simplifiedDecls

        skeletons = [ buildSkeleton cp toSemanticName allResNames simplifiedDecls resDeclFiles name | name <- allResNames ]

        (assignedDecls, diag6) = runWriter $
            mapM (\d -> do
                      r <- bestResourceForDecl toSemanticName skeletons resDeclFiles names d
                      return (r, d))
                 simplifiedDecls

        (resources_, diag3) = runWriter $ mapM (fillResource strict enums_ constants_ names assignedDecls skeletons cp toSemanticName) skeletons

        (variants_, diag5) = runWriter $ gatherVariants resources_ enums_

        -- @typedef uint8_t X[SIZE]@: size constant to the Rust newtype name.
        arrayTypes_ =
            [ (sc, toSemanticName (flattenName nm))
            | m <- T.modelMods model0
            , T.ArrayTypeDecl (L _ _ nm) sz <- T.moduleDecls m
            , Just sc <- [sizeExprOf sz >>= sizeName]
            ]

        allDiagnostics = diag0 ++ diag3 ++ diag4 ++ diag5 ++ diag6
        sem = S.SemanticModel (List.nub $ List.sortOn S.enumName enums_)
                        (List.nub $ List.sortOn S.constantName constants_)
                        (List.nub $ List.sortOn S.idName idTypes_)
                        (List.nub $ List.sortOn S.scalarName scalarTypes_)
                        (List.nub $ List.sortOn S.cbName callbacks_)
                        (List.nub $ List.sortOn S.resourceName resources_)
                        (List.nub $ List.sortOn S.variantName variants_)
                        (List.nub $ List.sortOn fst arrayTypes_)
                        cp
                        allDiagnostics
                        S.emptyKnowledge
                        S.emptyTargets
                        S.emptyContext
    in nameIdsAfterTypes sem { S.knowledge = Knowledge.infer sem, S.targets = Tox.targets }

-- | A model with the enums and callback typedefs another one declares besides
-- its own as its context: the model of the public headers and
-- @tox_private.h@ for the public headers'.
withContext :: S.SemanticModel -> S.SemanticModel -> S.SemanticModel
withContext ctx m = m
    { S.context = S.SContext
        { S.contextEnums     = [ e | e <- S.enums ctx, S.enumName e `notElem` map S.enumName (S.enums m) ]
        , S.contextCallbacks = [ c | c <- S.callbacks ctx, S.cbName c `notElem` map S.cbName (S.callbacks m) ]
        }
    }

-- | Rewrite @typedef uint8_t X[SIZE]@ into sized byte arrays (otherwise
-- they'd be taken for enums).
rewriteArrayTypedefs :: T.Model (Lexeme Name) -> T.Model (Lexeme Name)
rewriteArrayTypedefs m =
    m { T.modelMods = map rwMod (T.modelMods m) }
  where
    defs =
        [ (flattenName nm, sz)
        | md <- T.modelMods m
        , T.ArrayTypeDecl (L _ _ nm) sz <- T.moduleDecls md
        ]
    rwMod md = md { T.moduleDecls =
        map rwDecl [ d | d <- T.moduleDecls md, not (isArrayTypeDecl d) ] }
    isArrayTypeDecl T.ArrayTypeDecl{} = True
    isArrayTypeDecl _                 = False
    rwDecl = \case
        T.Function ret nm ps rn nps  -> T.Function (rwTy ret) nm (map rwDecl ps) rn nps
        T.CallbackTypeDecl ret nm ps nl -> T.CallbackTypeDecl (rwTy ret) nm (map rwDecl ps) nl
        T.Var ty nm                  -> T.Var (rwTy ty) nm
        d                            -> d
    rwTy t = case t of
        T.Typename (L _ _ nm)
            | Just sz <- lookup (flattenName nm) defs ->
                T.SizedArrayType (T.BuiltinType (T.UInt T.B8)) sz
        T.ConstType inner -> T.ConstType (rwTy inner)
        _ -> t

resourceDeclFiles :: [DDecl] -> [(Text, FilePath)]
resourceDeclFiles = List.nub . mapMaybe entry
  where
    entry (T.TypeDecl (L _ _ n), fp)     = Just (flattenName n, fp)
    entry (T.ClassDecl (L _ _ n) _, fp)  = Just (flattenName n, fp)
    entry (T.ScalarTypeDecl (L _ _ n) _, fp)   = (\r -> (r, fp)) <$> idOwner (flattenName n)
    entry _ = Nothing

-- | Why a function can't be bound, given the structs and the enums.
unboundReason :: [Text] -> [Text] -> T.Decl (Lexeme Name) -> Maybe Text
unboundReason structs enumNames = \case
    T.Function ret _ params _ _
        -- Structs only go by pointer, as handles.
        | any byValue (ret : map varType params) -> Just "structs passed by value aren't bound"
        -- An enum only comes out through the last parameter, as the error.
        | any enumOut (dropEnd1 params) -> Just "enums given back before the last parameter aren't bound yet"
    _ -> Nothing
  where
    byValue (T.Typename (L _ _ n)) = flattenName n `elem` structs
    byValue _                      = False
    enumOut (T.Var (T.PointerType (L _ _ n)) _) = flattenName n `elem` enumNames
    enumOut _                                   = False
    dropEnd1 xs = take (length xs - 1) xs

declSignatureTypeNames :: T.Decl (Lexeme Name) -> [Text]
declSignatureTypeNames = \case
    T.Function ret _ params _ _ -> mapMaybe getTypeName (ret : map varType params)
    T.CallbackTypeDecl ret _ params _ -> mapMaybe getTypeName (ret : map varType params)
    _ -> []

-- | Resource a declaration belongs to. Only one whose type is in the
-- signature, or declared in the same header, can get it.
bestResourceForDecl :: (Text -> Text) -> [S.SResource] -> [(Text, FilePath)] -> TypeNames -> DDecl -> Writer [S.SDiagnostic] Text
bestResourceForDecl toSem allSkeletons resDeclFiles names d@(decl, fp) =
    case getDeclName decl of
        Nothing -> return ""
        Just dName ->
            let dNameLower = Text.toLower dName
                sigTypes = declSignatureTypeNames decl

                identityTypeNames r =
                    let c = S.cName r
                    in [c, c <> "_Number", c <> "_Id"]

                typeReachable r = any (`elem` sigTypes) (identityTypeNames r)

                resFiles r = [ f | (rn, f) <- resDeclFiles, rn == S.cName r ]
                fileReachable r = fp `elem` resFiles r

                prefixMatches r =
                    let rCNameLower = Text.toLower (S.cName r)
                    in (rCNameLower <> "_") `Text.isPrefixOf` dNameLower

                -- A handle in the signature beats a resource only sharing the header
                -- (toxav_add_av_groupchat takes a Tox). Handle-less functions
                -- (tox_pass_encrypt) fall back to their file's only resource.
                handleReachable r = S.resourceType r == S.ResHandle && typeReachable r
                onlyInFile = case filter fileReachable allSkeletons of
                    [r] -> [r]
                    _   -> []
                candidates = case filter (not . null)
                        [ filter (\r -> prefixMatches r && typeReachable r) allSkeletons
                        , filter handleReachable allSkeletons
                        , filter (\r -> prefixMatches r && fileReachable r) allSkeletons
                        , onlyInFile
                        ] of
                    c : _ -> c
                    []    -> []

                score res =
                    let baseScore = declIdentityMatchScore allSkeletons res d
                        returnsRes = if declReturnsResourceIdentity toSem names res d then 10 else 0
                    in (baseScore + returnsRes, Text.length (S.cName res), negate (Text.length (S.resourceName res)))

                -- E.g. tox_log_level.h declares no resource.
                rootRes = listToMaybe (filter S.isRoot allSkeletons)
            in if not (null candidates)
               then return $ S.resourceName $ List.maximumBy (compare `on` score) candidates
               else case rootRes of
                   Just r -> return $ S.resourceName r
                   Nothing
                       | null allSkeletons -> return ""
                       | otherwise -> do
                           tell [S.SDiagnostic S.Error (Just (getLoc fp decl))
                                    (dName <> ": no resource is reachable for this declaration")]
                           return ""
declIdentityMatchScore :: [S.SResource] -> S.SResource -> DDecl -> Int
declIdentityMatchScore allSkeletons res (T.Function _ _ params _ _, _) =
    matchDeclParams allSkeletons res params
declIdentityMatchScore allSkeletons res (T.CallbackTypeDecl _ _ params _, _) =
    matchDeclParams allSkeletons res params
declIdentityMatchScore _ _ _ = 0

matchDeclParams :: [S.SResource] -> S.SResource -> [T.Decl (Lexeme Name)] -> Int
matchDeclParams allSkeletons res params =
    let collapsed = collapseParams params
        path = I.getHierarchy allSkeletons res
        -- Arguments follow the hierarchy: root, child, grandchild, ...
        matchCount (rName:restH) (p:restP) =
            case List.find ((== rName) . S.resourceName) allSkeletons of
                Just r -> if isIdentityOf r p then 1 + matchCount restH restP else 0
                Nothing -> 0
        matchCount _ _ = 0

        fullMatch = matchCount path collapsed
        selfMatch = if S.resourceType res == S.ResHandle
                    then matchCount [S.resourceName res] collapsed
                    else 0

        score = max fullMatch selfMatch
        required = if selfMatch > fullMatch then 1 else length path
    in if score < required then 0 else score
  where
    isIdentityOf res' (T.Var (T.Typename (L _ _ n)) _) =
        let fn = flattenName n
            name = S.cName res'
            semName = S.resourceName res'
        in fn == name || idOwner fn == Just name || semName <> "Id" == fn
    -- E.g. @const Tox *tox@, so functions taking only the root handle
    -- (tox_conference_get_chatlist) don't score zero everywhere.
    isIdentityOf res' (T.Var (T.PointerType (L _ _ n)) _) =
        flattenName n == S.cName res'
    isIdentityOf res' (T.Var (T.ConstPointerType (L _ _ n)) _) =
        flattenName n == S.cName res'
    isIdentityOf _ _ = False

declReturnsResourceIdentity :: (Text -> Text) -> TypeNames -> S.SResource -> DDecl -> Bool
declReturnsResourceIdentity toSem names res (T.Function ret _ _ _ _, fp) =
    let tRet = fst $ runWriter $ toSType toSem names (S.SLocation (Text.pack fp) 0 0) ret
    in case tRet of
        S.SHandle h     -> h == S.resourceName res
        S.SResourceId i -> S.resourceName res `elem` idOwners i
        _               -> False
declReturnsResourceIdentity _ _ _ _ = False

gatherEnums :: (Text -> Text) -> [DDecl] -> [S.SEnumModel]
gatherEnums toSem = mapMaybe mkEnum
  where
    mkEnum (T.Enumeration _ (L _ _ name) mems, fp) =
        let n = flattenName name
            members = mapMaybe enumMemberName mems
            -- Whole segments only: PUBLIC/PRIVATE share a @P@.
            rawCp = commonPrefix (map fst members)
            cp' = case Text.findIndex (== '_') (Text.reverse rawCp) of
                Just i  -> Text.dropEnd i rawCp
                Nothing -> rawCp
            semMembers =
                [ S.SEnumMember m (fromMaybe m (Text.stripPrefix cp' m)) v
                | ((m, _), v) <- zip members (memberValues n members)
                ]
            explicit = all (isJust . snd) members
        in Just $ S.SEnumModel n (toSem n) semMembers (kind n explicit semMembers) (Text.pack fp)
    mkEnum _ = Nothing

    kind n explicit members
        | "_Err_" `Text.isInfixOf` n && any ((== "OK") . S.enumMemberName) members = S.ErrorEnum
        | explicit && isFlags (map S.enumMemberValue members) = S.FlagsEnum
        | n `elem` map snd Facts.enumValuedParams = S.OpenEnum
        | otherwise = S.ClosedEnum

    -- Flags: explicit values, each 0 or a power of 2, not just 0 to 2.
    isFlags vs = all (\v -> v == 0 || v .&. (v - 1) == 0) vs && any (>= 4) vs

    enumMemberName (T.EnumMember (L _ _ name) mval) = Just (flattenName name, mval)
    enumMemberName _                                = Nothing

    -- An initializer we can't evaluate (@1 << 5@) is an error, not a guess.
    memberValues enumN = go' 0
      where
        go' _ [] = []
        go' next ((m, mval) : rest) =
            let v = case mval of
                    Nothing -> next
                    Just expr -> case evalIntExpr expr of
                        Just v' -> v'
                        Nothing -> error $
                            "gatherEnums: cannot evaluate initializer of "
                                <> Text.unpack m <> " in " <> Text.unpack enumN
            in v : go' (v + 1) rest

    evalIntExpr (T.IntVal (L _ _ v)) = readMaybe (Text.unpack (flattenName v))
    evalIntExpr (T.Paren e)          = evalIntExpr e
    evalIntExpr _                    = Nothing

gatherConstants :: [DDecl] -> [S.SConstantModel]
gatherConstants decls = mapMaybe toConstant decls
  where
    toConstant (T.Define (L _ _ name) val, _) = do
        v <- evaluate val
        return $ S.SConstantModel (flattenName name) v
    toConstant _ = Nothing

    definedConstants = mapMaybe (\(d, _) -> case d of
        T.Define (L _ _ name) val -> Just (flattenName name, val)
        _                         -> Nothing) decls

    evaluate (T.IntVal (L _ _ v)) = readMaybe (Text.unpack (flattenName v))
    evaluate (T.Add l r)          = (+) <$> evaluate l <*> evaluate r
    evaluate (T.Sub l r)          = (-) <$> evaluate l <*> evaluate r
    evaluate (T.Mul l r)          = (*) <$> evaluate l <*> evaluate r
    evaluate (T.Div l r)          = div <$> evaluate l <*> evaluate r
    evaluate (T.Paren e)          = evaluate e
    evaluate (T.Sizeof ty)        = evaluateSize ty
    evaluate (T.Ref (L _ _ name)) =
        let n = flattenName name
        in case List.lookup n definedConstants of
             Just val -> evaluate val
             Nothing  -> Nothing
    evaluate _                    = Nothing

    evaluateSize (T.BuiltinType (T.UInt T.B16)) = Just 2
    evaluateSize (T.BuiltinType (T.UInt T.B32)) = Just 4
    evaluateSize _                              = Nothing

-- | The names a type reference can be besides an enum or a struct.
data TypeNames = TypeNames
    { tnIds       :: [Text]
    , tnCallbacks :: [Text]
    , tnScalars   :: [(Text, S.SType)]  -- ^ Units: @Toxav_Bit_Rate@ is a @SUInt 32@.
    }

-- | The typedef'd numbers that aren't ids, and their headers.
gatherScalars :: [DDecl] -> [(Text, S.SType, FilePath)]
gatherScalars decls = List.nub
    [ (n, builtin b, fp)
    | (T.ScalarTypeDecl (L _ _ nm) b, fp) <- decls
    , let n = flattenName nm
    , isNothing (idOwner n)
    ]
  where
    builtin b = case b of
        T.SInt bs -> S.SInt (fromBitSize bs)
        T.UInt bs -> S.SUInt (fromBitSize bs)
        _         -> S.SSizeT

gatherIdNames :: [DDecl] -> [Text]
gatherIdNames decls = List.nub $ mapMaybe (toIdName . fst) decls
  where
    toIdName (T.ScalarTypeDecl (L _ _ name) _) = mfilter (isJust . idOwner) (Just (flattenName name))
    toIdName (T.TypeDecl (L _ _ name))   = mfilter (isJust . idOwner) (Just (flattenName name))
    toIdName _                           = Nothing

gatherCallbackNames :: [DDecl] -> [Text]
gatherCallbackNames decls = List.nub $ mapMaybe (toCbName . fst) decls
  where
    toCbName (T.CallbackTypeDecl _ (L _ _ name) _ _) = Just (flattenName name)
    toCbName _                                     = Nothing

gatherResourceNames :: [DDecl] -> [Text]
gatherResourceNames decls = List.nub $ mapMaybe (toResName . fst) decls
  where
    -- A struct is a resource when functions use it, so internal types only
    -- named as struct members (Memory) aren't.
    toResName (T.TypeDecl (L _ _ name)) = mfilter used (Just (flattenName name))
    toResName (T.ClassDecl (L _ _ name) _) = mfilter used (Just (flattenName name))
    toResName (T.ScalarTypeDecl (L _ _ name) _) = idOwner (flattenName name)
    toResName _ = Nothing
    used n = n `elem` concatMap (declSignatureTypeNames . fst) decls
      || any ((Text.toLower n <> "_") `Text.isPrefixOf`) functionNames
    functionNames = [ Text.toLower (flattenName f) | (T.Function _ (L _ _ f) _ _ _, _) <- decls ]

gatherCallbackTypes :: (Text -> Text) -> TypeNames -> [DDecl] -> Writer [S.SDiagnostic] [S.SCallbackTypeModel]
gatherCallbackTypes toSem names decls = mapMaybeM toCallback decls
  where
    toCallback (d@(T.CallbackTypeDecl ret (L _ _ name) _ _), fp) | ret /= T.BuiltinType T.Void = do
        tell [S.SDiagnostic S.Warning (Just (getLoc fp d))
                 (flattenName name <> ": callbacks returning a value aren't bound yet")]
        return Nothing
    toCallback (T.CallbackTypeDecl _ (L _ _ name) params _, fp) =
        let n = flattenName name
            semName = toSem n

        in do
           let stripped = collapseParams params

           semParams <- mapM (\d -> do
                 let loc = getLoc fp d
                 case d of
                     T.Var ty (L _ _ pName) -> do
                        t <- toSType toSem names loc ty
                        let cns = getConstness ty
                        return (parameter (flattenName pName) t cns False)
                     _ -> return $ S.SParameter "unknown" S.SVoid T.MutableThis [] False
                 ) stripped

           return $ Just $ S.SCallbackTypeModel semName n semParams (Text.pack fp)
    toCallback (T.Function (T.CallbackType (L _ _ _name)) _ _params _ _, _fp) =
       return Nothing
    toCallback _ = return Nothing


-- | Header a resource lives in: the declaring file that also has its
-- functions, else the first. The C backend emits it there too.
resourcePrimaryFile :: [DDecl] -> [(Text, FilePath)] -> Text -> FilePath
resourcePrimaryFile allDecls resDeclFiles name =
    let declFiles = [ f | (rn, f) <- resDeclFiles, rn == name ]
        prefix = Text.toLower name <> "_"
        prefixFiles = List.nub
            [ fp
            | (decl, fp) <- allDecls
            , fp `elem` declFiles
            , Just dn <- [getDeclName decl]
            , prefix `Text.isPrefixOf` Text.toLower dn
            ]
    in case prefixFiles ++ declFiles of
        (f:_) -> f
        []    -> ""

buildSkeleton :: Text -> (Text -> Text) -> [Text] -> [DDecl] -> [(Text, FilePath)] -> Text -> S.SResource
buildSkeleton cPrefix toSemanticName allResNames allDecls resDeclFiles name =
    let mParent = findParent name allResNames allDecls

        resType = if isIdResource name allDecls
                  then case findIdentifier toSemanticName name allDecls of
                      Just t  -> S.ResId t
                      Nothing -> S.ResHandle
                  else S.ResHandle

        semantic = toSemanticName name
        semanticParent = fmap toSemanticName mParent
        isRoot = Text.toLower name == Text.toLower cPrefix
        prefix = Text.toLower name <> "_"
        resFile = Text.pack (resourcePrimaryFile allDecls resDeclFiles name)

    in (emptyResource semantic name resType)
        { S.cPrefix      = prefix
        , S.isRoot       = isRoot
        , S.parent       = semanticParent
        , S.resourceFile = resFile
        }

-- | A resource with nothing in it yet.
emptyResource :: Text -> Text -> S.SResourceType -> S.SResource
emptyResource name cName resType = S.SResource
    { S.resourceName    = name
    , S.cName           = cName
    , S.resourceType    = resType
    , S.cPrefix         = ""
    , S.isRoot          = False
    , S.parent          = Nothing
    , S.lifecycleParent = Nothing
    , S.resourceFile    = ""
    , S.properties      = []
    , S.methods         = []
    , S.traits          = []
    , S.events          = []
    }

fillResource :: Bool -> [S.SEnumModel] -> [S.SConstantModel] -> TypeNames -> [(Text, DDecl)] -> [S.SResource] -> Text -> (Text -> Text) -> S.SResource -> Writer [S.SDiagnostic] S.SResource
fillResource strict enums_ constants_ names allDecls allSkeletons cp toSem res = do
    let name = S.resourceName res
        myDecls = [ d | (r, d) <- allDecls, r == name ]
        fullPath = I.getHierarchy allSkeletons res
        ctx = ResourceContext (S.resourceName res) (S.resourceType res) (S.parent res) (map snd allDecls) constants_ names fullPath strict (S.cName res) toSem cp

    evs <- gatherEvents ctx myDecls
    meths <- gatherMethods allSkeletons ctx myDecls

    let eventRegistrarNames = [ Text.toLower (ctxCName ctx) <> "_callback_" <> S.eventName ev | ev <- evs ]
    let meths' = filter (\m -> Text.toLower (S.methodName m) `notElem` eventRegistrarNames) meths

    let props = identifyProperties ctx meths'

    let meths'' = map (updateMethodWithSize allSkeletons res meths' props) meths'

    checkErrorEnums ctx enums_ (meths'', props)

    let mLifecycleParent = case List.find ((== S.Constructor) . S.methodRole) meths'' of
            Just ctor ->
                let ctorName = S.methodName ctor
                    ctorDecls = filter (isFuncNamed ctorName) myDecls
                    isConstArg = any isConstCtor ctorDecls
                    isConstCtor (T.Function _ _ (firstParam:_) _ _, _) =
                        case varType firstParam of
                            T.ConstPointerType _ -> True
                            T.ConstType _        -> True
                            _                    -> False
                    isConstCtor _ = False
                in if isConstArg
                   then S.parent res
                   else case S.inputs ctor of
                        (S.SParameter _ (S.SHandle h) _ _ _) : _ ->
                            if h /= S.resourceName res then Just h else Nothing
                        _ -> S.parent res
            Nothing -> S.parent res

    let traits_ = detectTraits (S.cPrefix res) meths'' props

    return $ res { S.properties = props, S.methods = meths'', S.traits = traits_, S.events = evs, S.lifecycleParent = mLifecycleParent, S.isRoot = S.isRoot res }

updateMethodWithSize :: [S.SResource] -> S.SResource -> [S.SMethod] -> [S.SProperty] -> S.SMethod -> S.SMethod
updateMethodWithSize allRes res allMeths props m =
    case List.find (\prop -> S.propRead prop == Just (S.methodName m)) props of
        Just p -> case S.propSize p of
            Just sizerName ->
                let realCName = case List.find (\sm -> S.methodName sm == sizerName) allMeths of
                        Just sm -> case S.methodMapping sm of
                            S.CustomMapping cm -> S.cFunctionName cm
                            S.StandardMapping -> S.cFunctionName (I.inferCFunctionMapping allRes res sm)
                        Nothing -> sizerName

                    newMap = case S.methodMapping m of
                        S.StandardMapping ->
                            let baseMap = I.inferCFunctionMapping [] res m
                            in S.CustomMapping (baseMap { S.cSizeFunctionName = Just realCName })
                        S.CustomMapping cm ->
                            S.CustomMapping (cm { S.cSizeFunctionName = Just realCName })
                in m { S.methodMapping = newMap }
            Nothing -> m
        Nothing -> m

data ResourceContext = ResourceContext
    {
        ctxName       :: Text
    ,   ctxType       :: S.SResourceType
    ,   ctxParent     :: Maybe Text
    ,   ctxAllDecls   :: [DDecl]
    ,   ctxConstants  :: [S.SConstantModel]
    ,   ctxNames      :: TypeNames
    ,   ctxPath       :: [Text]
    ,   ctxStrictMode :: Bool
    ,   ctxCName      :: Text
    ,   ctxToSemantic :: Text -> Text
    ,   ctxPrefix     :: Text
    }

identifyProperties :: ResourceContext -> [S.SMethod] -> [S.SProperty]
identifyProperties ctx methods =
    let
        stripPrefix name =
            let p = Text.toLower (ctxCName ctx) <> "_"
            in fromMaybe name (Text.stripPrefix p (Text.toLower name))

        -- The word before a dropped verb isn't said twice:
        -- @group_get_group_list@ is @group_list@.
        joinVerb p r = case reverse (filter (not . Text.null) (Text.splitOn "_" p)) of
            w : _ | (w <> "_") `Text.isPrefixOf` r -> Text.dropEnd (Text.length w + 1) p <> r
            _ -> p <> r

        analyzeName name =
            let stripped = stripPrefix name

                checkSize suffix dropLen =
                    if suffix `Text.isSuffixOf` stripped
                    then
                        let base = Text.dropEnd dropLen stripped
                            (p, r) = Text.breakOn "get_" base
                        in if not (Text.null r)
                           then Just ("size", joinVerb p (Text.drop 4 r))
                           else Just ("size", base)
                    else Nothing

            in case checkSize "_size" 5 of
                 Just s -> Just s
                 Nothing -> case checkSize "_length" 7 of
                     Just s -> Just s
                     Nothing ->
                        let (p, r) = Text.breakOn "get_" stripped
                        in if not (Text.null r) then Just ("get", joinVerb p (Text.drop 4 r))
                        else let (p', r') = Text.breakOn "set_" stripped
                             in if not (Text.null r') then Just ("set", joinVerb p' (Text.drop 4 r'))
                             else if "self_get_" `Text.isPrefixOf` stripped then Just ("get", "self_" <> Text.drop 9 stripped)
                             else if "self_set_" `Text.isPrefixOf` stripped then Just ("set", "self_" <> Text.drop 9 stripped)
                             else Nothing

        -- @tox_err_set_info_to_string@ names an enum's values, not a property.
        candidates = foldl (categorizeMethod analyzeName) []
            (filter (not . isEnumNamer) methods)

        categorizeMethod :: (Text -> Maybe (Text, Text)) -> [(Text, Maybe S.SMethod, Maybe S.SMethod, Maybe S.SMethod)] -> S.SMethod -> [(Text, Maybe S.SMethod, Maybe S.SMethod, Maybe S.SMethod)]
        categorizeMethod analyzer acc m =
            case analyzer (S.methodName m) of
                Just ("get", base) -> insertOrUpdate (clean base) (\(_, s, z) -> (Just m, s, z)) acc
                Just ("set", base) -> insertOrUpdate (clean base) (\(g, _, z) -> (g, Just m, z)) acc
                -- A size accessor describes a buffer even though it returns a
                -- scalar, so it pairs with @get_x_data@.
                Just ("size", base) -> insertOrUpdate (cleanData base) (\(g, s, _) -> (g, s, Just m)) acc
                _ -> acc
          where
            -- Only for buffers: @experimental_owned_data@ keeps its name.
            clean b = if "_data" `Text.isSuffixOf` b && isBufferMethod
                      then cleanData b else b
            cleanData b = if "_data" `Text.isSuffixOf` b
                          then Text.dropEnd 5 b else b
            isBufferMethod =
                isBufferType (S.output m) || any (isBufferType . S.paramType) (S.inputs m)

        insertOrUpdate key f [] = [(key, g, s, z) | let (g, s, z) = f (Nothing, Nothing, Nothing)]
        insertOrUpdate key f ((k, g, s, z):xs)
            | k == key = let (g', s', z') = f (g, s, z) in (k, g', s', z') : xs
            | otherwise = (k, g, s, z) : insertOrUpdate key f xs

    in mapMaybe (buildProperty ctx) candidates

buildProperty :: ResourceContext -> (Text, Maybe S.SMethod, Maybe S.SMethod, Maybe S.SMethod) -> Maybe S.SProperty
buildProperty ctx (baseName, mGet, mSet, mSize) =
    if isNothing mGet && isNothing mSet
    then Nothing
    else do
        let validGetter = case mGet of
                Just g  -> null (S.inputs g)
                Nothing -> True
            validSetter = case mSet of
                Just s  -> length (S.inputs s) == 1
                Nothing -> True

        if not (validGetter && validSetter) then Nothing else do

            let propType = case mGet of
                    Just g  -> S.output g
                    Nothing -> case mSet of
                        Just s -> if not (null (S.inputs s)) then S.paramType (last (S.inputs s)) else S.SVoid
                        Nothing -> S.SVoid

            let name = baseName
                -- Only for buffers: @experimental_owned_data@ keeps its name.
                cleanName = if "_data" `Text.isSuffixOf` name && isBufferType propType
                            then Text.dropEnd 5 name else name

                constants = findAssociatedConstants ctx cleanName

                err = case mGet of
                    Just g  -> S.methodErrorType g
                    Nothing -> case mSet of
                         Just s  -> S.methodErrorType s
                         Nothing -> Nothing

            return S.SProperty
                { S.propName      = cleanName
                , S.propType      = propType
                , S.propRead      = S.methodName <$> mGet
                , S.propWrite     = S.methodName <$> mSet
                , S.propSize      = S.methodName <$> mSize
                , S.propErrorType = err
                , S.propConstants = constants
                }

gatherMethods :: [S.SResource] -> ResourceContext -> [DDecl] -> Writer [S.SDiagnostic] [S.SMethod]
gatherMethods allRes ctx decls = mapMaybeM (toSMethod allRes ctx) decls

toSMethod :: [S.SResource] -> ResourceContext -> DDecl -> Writer [S.SDiagnostic] (Maybe S.SMethod)
toSMethod allRes ctx d@(T.Function ret (L _ _ name) params retNull nullPs, fp) =
    do
        let n = flattenName name
        let nulls = [ flattenName p | L _ _ p <- nullPs ]
        -- A predicate's bool is its result, not an error mirror.
        let isPredicate = "_is_" `Text.isInfixOf` n
        let isGetter = "_get_" `Text.isInfixOf` n || "self_get_" `Text.isInfixOf` n

        tRet <- toSType (ctxToSemantic ctx) (ctxNames ctx) (getLoc fp (fst d)) ret
        let initialRole = inferMethodRole ctx n tRet []

        (argMappings1, semParams1) <- partitionParams ctx nulls initialRole (collapseParams params) fp

        finalRet <- if any isBufferPtr argMappings1
                    then do
                        let collapsed = collapseParams params
                        bufferParam <- findBufferParam ctx collapsed fp
                        case bufferParam of
                            Just bt -> return $ case bt of
                                S.SHandle "uint8_t" -> S.SBytes
                                S.SHandle "char"    -> S.SBytes
                                _                   -> bt
                            Nothing -> return tRet
                    else return tRet

        let errType = findErrorType params
            role = inferMethodRole ctx n finalRet semParams1
            role' = if role == S.ActionRole && not (any isInstanceHandle argMappings1)
                    then S.StaticRole
                    else role

        (argMappings, semParams) <- partitionParams ctx nulls role' (collapseParams params) fp

        let cns = fromMaybe T.ConstThis $ listToMaybe $ mapMaybe (\case S.ThisObject c -> Just c; S.PathObject _ c -> Just c; _ -> Nothing) argMappings
            resStrat
                | isNothing errType = S.ReturnIsValue
                | tRet == S.SBool && finalRet == tRet && not isGetter && not isPredicate = S.IgnoreReturn
                | tRet == S.SBool && isPredicate = S.ReturnIsResult
                | S.SResourceId _ <- finalRet = S.ReturnIsResult
                | otherwise = S.ReturnIsValue

        let hasUserData = any (== S.UserData) argMappings
            cMap = S.CFunctionMapping
                { S.cFunctionName     = n
                , S.argMapping        = argMappings
                , S.returnMapping     = Nothing
                , S.cSizeFunctionName = Nothing
                , S.cErrorType        = errType
                , S.cSemParams        = semParams
                , S.cReturnType       = tRet
                , S.cArgNames         = [ flattenName pn | T.Var _ (L _ _ pn) <- params ]
                }
            (_, reasons) = checkStandardMapping allRes ctx role' cMap cns resStrat hasUserData finalRet

        when (ctxStrictMode ctx && not (null reasons)) $
            tell [S.SDiagnostic S.Warning (Just (getLoc fp (fst d))) (n <> ": non-standard mapping: " <> Text.intercalate ", " reasons)]

        return $ Just S.SMethod
            { S.methodName           = n
            , S.methodRole           = role'
            , S.inputs               = semParams
            , S.output               = finalRet
            , S.methodConstness      = cns
            , S.methodErrorType      = errType
            , S.methodResultStrategy = resStrat
            , S.methodMapping        = S.CustomMapping cMap
            , S.methodHasUserData    = hasUserData
            , S.methodConstants      = []
            , S.methodReturnNullable = retNull
            , S.methodFile           = Text.pack fp
            }
  where
    isBufferPtr (S.BufferPtr _) = True
    isBufferPtr _               = False

    findBufferParam c p f = do
        types <- mapM (paramType' c f) p
        let pairs = zip types p
            isParamConst (T.Var ty _)               = isParamConst ty
            isParamConst (T.SizedArrayType memTy _) = isConst memTy
            isParamConst other                      = isConst other
        return $ fmap fst $ List.find (\(t, decl) -> isBuf t decl && not (isParamConst decl)) pairs

    paramType' c f (T.Var ty _) = toSType (ctxToSemantic c) (ctxNames c) (getLoc f (fst d)) ty
    paramType' c f (T.SizedArrayType ty _) = toSType (ctxToSemantic c) (ctxNames c) (getLoc f (fst d)) ty
    paramType' _ _ _ = return S.SVoid

    isBuf t _ = isBufferType t || t `elem` [S.SHandle "uint8_t", S.SHandle "char"]

toSMethod _ _ _ = return Nothing

detectTraits :: Text -> [S.SMethod] -> [S.SProperty] -> [S.SResourceTrait]
detectTraits prefix meths props =
    let stripPrefix mName = fromMaybe mName (Text.stripPrefix prefix mName)
        isGet m = stripPrefix (S.methodName m) == "get" && length (S.inputs m) == 1
        isSize m = stripPrefix (S.methodName m) `elem` ["size", "get_size"] && null (S.inputs m)
        isSizeProp p = S.propName p == "size"

        getMethod = List.find isGet meths
        sizeMethod = List.find isSize meths
        sizeProp = List.find isSizeProp props

        foundSizeName = case sizeMethod of
            Just sm -> Just (stripPrefix (S.methodName sm))
            Nothing -> case sizeProp of
                Just _  -> Just "get_size"
                Nothing -> Nothing

    in case (getMethod, foundSizeName) of
        (Just gm, Just smName) ->
            [S.Iterable (S.output gm) smName (stripPrefix (S.methodName gm))]
        _ -> []

isBufferType :: S.SType -> Bool
isBufferType = \case
    S.SBytes      -> True
    S.SFixedBytes{} -> True
    S.SList{}     -> True
    S.SFixedList{} -> True
    _             -> False

isInstanceHandle :: S.CArgSource -> Bool
isInstanceHandle (S.ThisObject _) = True
isInstanceHandle (S.PathId _)     = True
isInstanceHandle _                = False

formatArgSource :: S.CArgSource -> Text
formatArgSource = \case
    S.ThisObject T.MutableThis   -> "this"
    S.ThisObject T.ConstThis     -> "const this"
    S.PathObject n T.MutableThis -> "pathObj" <> Text.pack (show n)
    S.PathObject n T.ConstThis   -> "const pathObj" <> Text.pack (show n)
    S.PathId n                 -> "pathId" <> Text.pack (show n)
    S.SemanticArg n            -> "arg" <> Text.pack (show n)
    S.ErrorPtr                 -> "error"
    S.BufferPtr (Just s)       -> "buffer(" <> renderSizeExpr s <> ")"
    S.BufferPtr Nothing        -> "buffer"
    S.BufferSize _             -> "size"
    S.UserData                 -> "userData"
    S.Constant v               -> "const(" <> Text.pack (show v) <> ")"

checkStandardMapping :: [S.SResource] -> ResourceContext -> S.SMethodRole -> S.CFunctionMapping -> T.Constness -> S.SResultStrategy -> Bool -> S.SType -> (S.SMapping, [Text])
checkStandardMapping allRes ctx role cmap cns resStrat hasUserData retTy =
    -- Only what 'I.getValidArgMappings' looks at.
    let dummyRes = (emptyResource (ctxName ctx) (ctxCName ctx) (ctxType ctx)) { S.parent = ctxParent ctx }
        dummyMethod = S.SMethod
            { S.methodName           = S.cFunctionName cmap
            , S.methodRole           = role
            , S.inputs               = S.cSemParams cmap
            , S.output               = retTy
            , S.methodConstness      = cns
            , S.methodErrorType      = S.cErrorType cmap
            , S.methodResultStrategy = resStrat
            , S.methodMapping        = S.StandardMapping
            , S.methodHasUserData    = hasUserData
            , S.methodConstants      = []
            , S.methodReturnNullable = False
            , S.methodFile           = ""
            }
        validMappings = I.getValidArgMappings allRes dummyRes dummyMethod
        fmt args = "(" <> Text.intercalate ", " (map formatArgSource args) <> ")"
    in case validMappings of
       (first:_) | S.argMapping cmap == first -> (S.StandardMapping, [])
       _ -> if S.argMapping cmap `elem` validMappings
       then (S.CustomMapping cmap, [])
       else (S.CustomMapping cmap, ["expected " <> Text.intercalate " or " (map fmt validMappings) <> " but found " <> fmt (S.argMapping cmap)])

inferMethodRole :: ResourceContext -> Text -> S.SType -> [S.SParameter] -> S.SMethodRole
inferMethodRole ctx name ret semParams =
    let cn = ctxCName ctx
        nameLower = Text.toLower name
        isMyHandle (S.SHandle h) = h == ctxName ctx
        isMyHandle _             = False
        isMyId (S.SResourceId i) = ctxName ctx `elem` idOwners i
        isMyId _                 = False
        isCallback (S.SCallback _) = True
        isCallback _               = False
        hasHandleOutParam = any (\p -> isMyHandle (S.paramType p)) semParams
    in if isMyHandle ret || isMyId ret || (hasHandleOutParam && not ("_equal" `Text.isSuffixOf` nameLower) && not ("_copy" `Text.isSuffixOf` nameLower)) then S.Constructor
       else if (Text.toLower cn <> "_kill") == nameLower || (Text.toLower cn <> "_free") == nameLower then S.Destructor
       else if any (isCallback . S.paramType) semParams then S.RegistrarRole
       -- Not @toxav_audio_iterate@, which runs only one part of it.
       else if (Text.toLower cn <> "_iterate") == nameLower then S.IterateRole
       else S.ActionRole

partitionParams :: ResourceContext -> [Text] -> S.SMethodRole -> [T.Decl (Lexeme Name)] -> FilePath -> Writer [S.SDiagnostic] ([S.CArgSource], [S.SParameter])
partitionParams ctx nulls role params fp = do
    let collapsed = collapseParams params
        numParams = length collapsed
        names = [ flattenName n | T.Var _ (L _ _ n) <- collapsed ]
    ((_, _, semParams), argMappings) <- mapAccumLM (\(pIdx, semIdx, semParams) d -> do
        (semIdx', sources) <- toCArg ctx role numParams pIdx semIdx fp d
        let isSemantic (S.SemanticArg _) = True
            isSemantic _                 = False
        semParam <- if any isSemantic sources then (:[]) <$> toSParam (ctxNames ctx) (ctxToSemantic ctx) fp names nulls d else return []
        return ((pIdx + 1, semIdx', reverse semParam ++ semParams), sources)
        ) (0, 0, []) collapsed
    return (concat argMappings, reverse semParams)

mapAccumLM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
mapAccumLM _ s [] = return (s, [])
mapAccumLM f s (x:xs) = do
    (s', y) <- f s x
    (s'', ys) <- mapAccumLM f s' xs
    return (s'', y:ys)

parameter :: Text -> S.SType -> T.Constness -> Bool -> S.SParameter
parameter name ty cns nullable = S.SParameter
    { S.paramName      = name
    , S.paramType      = ty
    , S.paramConstness = cns
    , S.paramConstants = []
    , S.paramNullable  = nullable
    }

-- | A parameter, with the names of its function's others.
toSParam :: TypeNames -> (Text -> Text) -> FilePath -> [Text] -> [Text] -> T.Decl (Lexeme Name) -> Writer [S.SDiagnostic] S.SParameter
toSParam names toSem fp params nulls d@(T.Var ty (L _ _ name)) = do
    let loc = getLoc fp d
    t <- toSType toSem names loc ty
    let n = flattenName name
    let cns = getConstness ty
        nullable = case t of
            S.SCallback _                      -> True
            -- A buffer with its length, or sized by other parameters: C reads
            -- none of it at size 0, so an empty one says what NULL would.
            S.SBytes                           -> False
            S.SList _                          -> False
            S.SFixedBytes size _ | sized size  -> False
            S.SFixedList _ size _ | sized size -> False
            _                                  -> n `elem` nulls
    return (parameter n t cns nullable)
  where
    sized = any (`elem` params) . sizeExprVars
toSParam _ _ _ _ _ _ = return $ S.SParameter "unknown" S.SVoid T.MutableThis [] False

toCArg :: ResourceContext -> S.SMethodRole -> Int -> Int -> Int -> FilePath -> T.Decl (Lexeme Name) -> Writer [S.SDiagnostic] (Int, [S.CArgSource])
toCArg ctx role numParams argIdx semIdx fp d = case d of
    -- An array C writes to is the output buffer. A length variable after it
    -- is its size.
    T.Var (T.SizedArrayType memTy sizer) _ -> do
        let (semIdx', arg)
                | isConst memTy = (semIdx + 1, S.SemanticArg semIdx)
                | otherwise     = (semIdx, S.BufferPtr (sizeExprOf sizer))
        size <- case sizer of
            T.Var lenTy _ -> (:[]) . S.BufferSize <$> toSType (ctxToSemantic ctx) (ctxNames ctx) (getLoc fp d) lenTy
            _             -> return []
        return (semIdx', arg : size)
    T.Var ty n -> do
        let loc = getLoc fp d
            toSem = ctxToSemantic ctx
        sTy <- toSType toSem (ctxNames ctx) loc ty
        let path = ctxPath ctx
            -- Already the model's names.
            sName = case sTy of
                S.SHandle h      -> h
                S.SResourceId n' -> n'
                _                -> ""

            mPathId = case sTy of
                S.SResourceId n' ->
                    let resNameFromId = fromMaybe n' $ List.find (`elem` path) (idOwners n' ++ [n'])
                    in List.elemIndex resNameFromId path
                _ -> Nothing

            isThis = role /= S.Constructor &&
                     (  (argIdx == 0 && ctxType ctx == S.ResHandle && sName == ctxName ctx)
                     || (case ctxType ctx of S.ResId _ -> mPathId == Just (length path - 1); _ -> False) )

            isPathObj0 = argIdx == 0 && not isThis && (List.elemIndex sName path == Just 0)

            isError = argIdx == numParams - 1 && case sTy of S.SHandle _ -> True; _ -> False
                      && any (`elem` snd (lexemeText n)) ["error", "err"]
            -- A callback's context, not one stored for later
            -- (@log_user_data@). No @void *@ is a buffer.
            isUserData = sTy == S.SHandle "void"
                         && snd (lexemeText n) `elem` [["user", "data"], ["userData"], ["userdata"]]
            isBuffer = not (isConst ty) && (sTy == S.SBytes || sTy == S.SHandle "uint8_t" || sTy == S.SHandle "char" || isList sTy)
            isList (S.SList _)          = True
            isList (S.SFixedList _ _ _) = True
            isList _                    = False
            cns = getConstness ty
        return $ if isThis then (semIdx, [case ctxType ctx of S.ResId _ -> S.PathId (length path - 1); S.ResHandle -> S.ThisObject cns])
           else if isPathObj0 then (semIdx, [S.PathObject 0 cns])
           else case mPathId of
               Just idx -> (semIdx, [S.PathId idx])
               Nothing -> if isError then (semIdx, [S.ErrorPtr])
                          else if isUserData then (semIdx, [S.UserData])
                          else if isBuffer then (semIdx, [S.BufferPtr Nothing])
                          else (semIdx + 1, [S.SemanticArg semIdx])
    _ -> return (semIdx, [S.Constant 0])

isConst :: T.Decl lexeme -> Bool
isConst = \case
    T.ConstType _ -> True
    T.ConstPointerType _ -> True
    T.ConstArrayType _ -> True
    T.Var ty _ -> isConst ty
    T.SizedArrayType ty _ -> isConst ty
    _ -> False

getConstness :: T.Decl lexeme -> T.Constness
getConstness d = if isConst d then T.ConstThis else T.MutableThis

type DEvent = (Text, (T.Decl (Lexeme Name), [T.Decl (Lexeme Name)]), DDecl)

gatherEvents :: ResourceContext -> [DDecl] -> Writer [S.SDiagnostic] [S.SEvent]
gatherEvents ctx decls =
    let funcs = mapMaybe getFunc decls
        callbackFuncs = filter (isCallbackFunc ctx) funcs
    in mapMaybeM (toSEvent ctx) callbackFuncs
  where
    getFunc d@(T.Function ret (L _ _ name) ps _ _, _) = Just (flattenName name, (ret, ps), d)
    getFunc _ = Nothing

isCallbackFunc :: ResourceContext -> DEvent -> Bool
isCallbackFunc ctx (name, _, _) =
    (Text.toLower (ctxCName ctx) <> "_callback_") `Text.isInfixOf` Text.toLower name

toSEvent :: ResourceContext -> DEvent -> Writer [S.SDiagnostic] (Maybe S.SEvent)
toSEvent ctx (name, (_, ps), (decl, fp)) =
    case ps of
        (_:T.Var (T.PointerType (L _ _ cbName)) _:rest) -> handleCallback cbName rest
        (_:T.Var (T.CallbackType (L _ _ cbName)) _:rest) -> handleCallback cbName rest
        _ -> return Nothing
  where
    handleCallback cbName rest =
            let cbDecls = filter (isCallbackTypedef (flattenName cbName) . fst) (ctxAllDecls ctx)
                isUserData (T.Var (T.BuiltinType T.VoidPtr) (L _ _ n)) =
                    snd n `elem` [["user", "data"], ["userData"]]
                isUserData _ = False
                hasUserData = any isUserData rest
            in if not (all isUserData rest)
               then do
                   tell [S.SDiagnostic S.Warning (Just (getLoc fp decl))
                            (name <> ": callback setters with more parameters aren't events yet")]
                   return Nothing
               else do
               let cbNameText = flattenName cbName
                   prefix = Text.toLower (ctxCName ctx) <> "_"
                   strippedCbName = if prefix `Text.isPrefixOf` cbNameText
                                    then Text.drop (Text.length prefix) cbNameText
                                    else cbNameText

                   cbDecls' = if null cbDecls
                              then filter (isCallbackTypedef strippedCbName . fst) (ctxAllDecls ctx)
                              else cbDecls

               case cbDecls' of
                (T.CallbackTypeDecl (T.BuiltinType T.Void) _ cbParams _, fp'):_ -> do
                    let collapsed = collapseParams cbParams
                    (_, semParams) <- partitionParams ctx [] S.RegistrarRole (stripCbParams collapsed) fp'
                    return $ Just S.SEvent
                        { S.eventName        = toEventName ctx name
                        , S.eventRegistrar   = name
                        , S.eventParams      = semParams
                        , S.cCallback        = flattenName cbName
                        , S.eventHasUserData = hasUserData
                        }
                _ -> return Nothing

    isCallbackTypedef targetName (T.CallbackTypeDecl _ (L _ _ n) _ _) = flattenName n == targetName
    isCallbackTypedef _ _ = False

    stripCbParams []       = []
    stripCbParams [_]      = []
    stripCbParams (_:rest) = init rest

toEventName :: ResourceContext -> Text -> Text
toEventName ctx name =
    let nameLower = Text.toLower name
        prefix = Text.toLower (ctxCName ctx) <> "_callback_"
    in if prefix `Text.isPrefixOf` nameLower
       then Text.drop (Text.length prefix) nameLower
       else nameLower

collapseParams :: [T.Decl (Lexeme Name)] -> [T.Decl (Lexeme Name)]
collapseParams [] = []
collapseParams (T.Var ty name : len@(T.Var lenTy (L _ _ lenName)) : ps)
    | isLength ty lenTy (flattenName lenName) =
        T.Var (T.SizedArrayType ty len) name : collapseParams ps
  where
    -- A @size_t@ after an array, or in new headers an integer named like a
    -- size after an array C writes: @bytes[], uint32_t bytes_size@.
    isLength T.ArrayType{} (T.BuiltinType T.SizeT) _ = True
    isLength T.ConstArrayType{} (T.BuiltinType T.SizeT) _ = True
    isLength T.ArrayType{} (T.BuiltinType (T.UInt _)) n = any (`Text.isSuffixOf` n) ["length", "size", "_len"]
    isLength _ _ _ = False
collapseParams (p:ps) = p : collapseParams ps

flattenName :: Name -> Text
flattenName (ns, n) = Text.intercalate "_" (ns ++ n)

toSType :: (Text -> Text) -> TypeNames -> S.SLocation -> T.Decl (Lexeme Name) -> Writer [S.SDiagnostic] S.SType
toSType toSem names loc = \case
    T.BuiltinType T.Void -> return S.SVoid
    T.BuiltinType T.VoidPtr -> return $ S.SHandle "void"
    T.BuiltinType T.Bool -> return S.SBool
    T.BuiltinType (T.SInt T.B8) -> return $ S.SInt 8
    T.BuiltinType (T.SInt T.B16) -> return $ S.SInt 16
    T.BuiltinType (T.SInt T.B32) -> return $ S.SInt 32
    T.BuiltinType (T.SInt T.B64) -> return $ S.SInt 64
    T.BuiltinType (T.UInt T.B8) -> return $ S.SUInt 8
    T.BuiltinType (T.UInt T.B16) -> return $ S.SUInt 16
    T.BuiltinType (T.UInt T.B32) -> return $ S.SUInt 32
    T.BuiltinType (T.UInt T.B64) -> return $ S.SUInt 64
    T.BuiltinType T.String -> return S.SString
    T.BuiltinType T.SizeT -> return S.SSizeT
    T.Typename (L _ _ name) ->
        let n = flattenName name
        in return $ case lookup n (tnScalars names) of
            _ | n `elem` idNames -> S.SResourceId (toSem n)
            Just t               -> S.SScalar (toSem n) t
            Nothing              -> S.SEnum (toSem n)
    T.PointerType (L _ _ name) ->
        let n = flattenName name
        in return $ if n `elem` idNames then S.SList (S.SResourceId (toSem n))
                    else if n `elem` cbNames then S.SCallback (toSem n)
                    else if n == "uint8_t" || n == "char" then S.SBytes
                    else S.SHandle (toSem n)
    T.ConstPointerType (L _ _ name) ->
        let n = flattenName name
        in return $ if n `elem` idNames then S.SList (S.SResourceId (toSem n))
                    else if n `elem` cbNames then S.SCallback (toSem n)
                    else if n == "uint8_t" || n == "char" then S.SBytes
                    else S.SHandle (toSem n)
    T.CallbackType (L _ _ name) -> return $ S.SCallback (toSem (flattenName name))
    T.ArrayType (T.UInt T.B8) -> return S.SBytes
    T.ArrayType (T.UInt bs) -> return $ S.SList (S.SUInt (fromBitSize bs))
    T.ArrayType (T.SInt bs) -> return $ S.SList (S.SInt (fromBitSize bs))
    T.ConstArrayType (T.UInt T.B8) -> return S.SBytes
    T.ConstArrayType (T.UInt bs) -> return $ S.SList (S.SUInt (fromBitSize bs))
    T.ConstArrayType (T.SInt bs) -> return $ S.SList (S.SInt (fromBitSize bs))
    T.UserArrayType (L _ _ name) ->
        let n = flattenName name
        in return $ S.SList (if n `elem` idNames then S.SResourceId (toSem n) else S.SEnum (toSem n))
    T.SizedArrayType ty sizer -> do
        sTy <- toSType toSem names loc ty
        let hasSize = case sizer of T.Var _ _ -> True; _ -> False
        case sizeExprOf sizer of
            Just n ->
                case sTy of
                    S.SBytes           -> return $ S.SFixedBytes n hasSize
                    S.SUInt 8          -> return $ S.SFixedBytes n hasSize
                    S.SList t          -> return $ S.SFixedList t n hasSize
                    S.SFixedList t _ _ -> return $ S.SFixedList t n hasSize
                    _                  -> return $ S.SFixedList sTy n hasSize
            Nothing -> return $ case sTy of
                S.SBytes           -> S.SBytes
                S.SUInt 8          -> S.SBytes
                S.SList _          -> sTy
                S.SFixedList t _ _ -> S.SList t
                S.SFixedBytes _ _  -> S.SBytes
                _                  -> S.SList sTy
    T.ConstType ty -> toSType toSem names loc ty
    x -> do
        tell [S.SDiagnostic S.Error (Just loc) ("unrecognized type: " <> Text.pack (show x))]
        return S.SVoid
  where
    idNames = tnIds names
    cbNames = tnCallbacks names


fromBitSize :: T.BitSize -> Int
fromBitSize T.B8  = 8
fromBitSize T.B16 = 16
fromBitSize T.B32 = 32
fromBitSize T.B64 = 64

findErrorType :: [T.Decl (Lexeme Name)] -> Maybe Text
findErrorType ps =
    case ps of
        [] -> Nothing
        _  -> isErrPtr (last ps)
  where
    isErrPtr (T.Var (T.PointerType (L _ _ n)) (L _ _ (_, ["error"]))) = Just (flattenName n)
    isErrPtr _                                                       = Nothing

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = mapMaybe id <$> mapM f xs

toSLocation :: FilePath -> Lexeme a -> S.SLocation
toSLocation fp (L (AlexPn _ l c) _ _) = S.SLocation (Text.pack fp) (fromIntegral l) (fromIntegral c)

getLoc :: FilePath -> T.Decl (Lexeme Name) -> S.SLocation
getLoc fp = \case
    T.Function _ (L p _ _) _ _ _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Method _ _ (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Var _ (L p _ _) -> toSLocation fp (L p IdVar ("" :: Text))
    T.Define (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Enumeration _ (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.TypeDecl (L p _ _) -> toSLocation fp (L p IdVar ("" :: Text))
    T.ClassDecl (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.ScalarTypeDecl (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.CallbackTypeDecl _ (L p _ _) _ _ -> toSLocation fp (L p IdVar ("" :: Text))
    _ -> S.SLocation (Text.pack fp) 0 0

gatherVariants :: [S.SResource] -> [S.SEnumModel] -> Writer [S.SDiagnostic] [S.SVariant]
gatherVariants resources enums = do
    let candidates = mapMaybe findVariantCandidate resources
    vs <- mapM (buildVariant resources enums) candidates
    return $ filter (not . null . S.variantMembers) vs

findVariantCandidate :: S.SResource -> Maybe (S.SResource, Text)
findVariantCandidate res =
    let typeGetterName = S.cPrefix res <> "get_type"
        mGetType = List.find (\m -> S.methodName m == typeGetterName) (S.methods res)
    in case mGetType of
        Just m -> case S.output m of
            S.SEnum eName -> Just (res, eName)
            _             -> Nothing
        Nothing -> Nothing

buildVariant :: [S.SResource] -> [S.SEnumModel] -> (S.SResource, Text) -> Writer [S.SDiagnostic] S.SVariant
buildVariant resources enums (res, enumName) = do
    let mEnum = List.find (\e -> S.enumSemanticName e == enumName) enums
    case mEnum of
        Nothing -> return $ S.SVariant (S.resourceName res) enumName []
        Just e -> do
            let members = S.enumMembers e
            let variantMembers = mapMaybe (matchMember resources res) members
            return $ S.SVariant (S.resourceName res) enumName variantMembers

matchMember :: [S.SResource] -> S.SResource -> S.SEnumMember -> Maybe S.SVariantMember
matchMember allRes res mem =
    let semEnumMember = S.enumMemberName mem
        getterName = S.cPrefix res <> "get_" <> camelToSnake semEnumMember
        mMethod = List.find (\m -> S.methodName m == getterName) (S.methods res)
    in case mMethod of
        Just m ->
            let cName = case S.methodMapping m of
                    S.CustomMapping cm -> S.cFunctionName cm
                    S.StandardMapping -> S.cFunctionName (I.inferCFunctionMapping allRes res m)
            in case S.output m of
                S.SHandle h     -> Just $ S.SVariantMember semEnumMember h cName
                S.SResourceId r -> Just $ S.SVariantMember semEnumMember r cName
                _               -> Nothing
        Nothing -> Nothing

camelToSnake :: Text -> Text
camelToSnake t =
    let s = Text.unpack t
    in if '_' `elem` s
       then Text.toLower t
       else Text.dropWhile (== '_') $ Text.pack $ go s
  where
    go [] = []
    go (c:cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs

checkErrorEnums :: ResourceContext -> [S.SEnumModel] -> ([S.SMethod], [S.SProperty]) -> Writer [S.SDiagnostic] ()
checkErrorEnums ctx allEnums (meths, props) = do
    let errTypes = List.nub $ mapMaybe S.methodErrorType meths ++ mapMaybe S.propErrorType props
    mapM_ checkEnum errTypes
  where
    checkEnum :: Text -> Writer [S.SDiagnostic] ()
    checkEnum et =
        case List.find ((== et) . S.enumName) allEnums of
            Just e ->
                if not (any ((== "OK") . S.enumMemberName) (S.enumMembers e))
                then tell [S.SDiagnostic S.Warning Nothing ("Error enum " <> et <> " for resource " <> ctxName ctx <> " is missing an OK value")]
                else return ()
            Nothing ->
                tell [S.SDiagnostic S.Warning Nothing ("Error type " <> et <> " for resource " <> ctxName ctx <> " not found in enums")]

-- | A C name without its library's prefix. The root library's goes:
-- @Tox_Err_Bootstrap@ is @Err_Bootstrap@. Another library keeps its own word:
-- @Toxav_Err_New@ is @Av_Err_New@ and @toxav_call_cb@ is @av_call_cb@. A
-- library's handle is named for it, as the root's: @ToxAV@ is @Tox_Av@.
semanticName :: Text -> [Text] -> Text -> Text
semanticName root libs n = fromMaybe n . listToMaybe $ mapMaybe library libs ++ mapMaybe stripRoot
    [ root <> "_", Text.toLower root <> "_", root ]
  where
    library w
        | Just rest <- Text.stripPrefix (root <> w <> "_") n = Just (capitalise w <> "_" <> rest)
        | Just rest <- Text.stripPrefix (Text.toLower root <> w <> "_") n = Just (w <> "_" <> rest)
        | Text.toLower n == Text.toLower (root <> w) = Just (root <> "_" <> capitalise w)
        | otherwise = Nothing
    -- Only a name going on after the prefix: @Tox@ stays.
    stripRoot p = mfilter (not . Text.null) (Text.stripPrefix p n) >>= \rest ->
        if p /= root || isUpper (Text.head rest) then Just rest else Nothing
    capitalise w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

-- | The words other libraries add to the root's in their functions:
-- @av@ for @toxav_new@.
librarySuffixes :: Text -> [DDecl] -> [Text]
librarySuffixes root decls = List.nub
    [ w | (d, _) <- decls, Just f <- [getDeclName d]
        , (word, rest) <- [Text.breakOn "_" f], not (Text.null rest)
        , Just w <- [Text.stripPrefix (Text.toLower root) word], not (Text.null w) ]

commonPrefix :: [Text] -> Text
commonPrefix [] = ""
commonPrefix [x] = x
commonPrefix (x:xs) = foldl prefix x xs
  where
    prefix a b = Text.pack $ map fst $ takeWhile (uncurry (==)) $ zip (Text.unpack a) (Text.unpack b)

isIdResource :: Text -> [DDecl] -> Bool
isIdResource name = not . null . idTypesOf name

findIdentifier :: (Text -> Text) -> Text -> [DDecl] -> Maybe S.SType
findIdentifier toSem name = fmap (S.SResourceId . toSem) . listToMaybe . idTypesOf name

-- | The id types naming a resource: @Tox_Friend_Number@ for @Tox_Friend@.
idTypesOf :: Text -> [DDecl] -> [Text]
idTypesOf name decls = [ n | (T.ScalarTypeDecl (L _ _ nm) _, _) <- decls, let n = flattenName nm, idOwner n == Just name ]

-- | The resource an id type names: @Tox_Friend_Number@ is @Tox_Friend@.
idOwner :: Text -> Maybe Text
idOwner n = Text.stripSuffix "_Number" n <|> Text.stripSuffix "_Id" n

-- | What a model name for an id may say it names: @Friend@ for
-- @Friend_Number@ or @FriendId@.
idOwners :: Text -> [Text]
idOwners n = mapMaybe (`Text.stripSuffix` n) ["_Number", "_Id", "Number", "Id"]

getDeclName :: T.Decl (Lexeme Name) -> Maybe Text
getDeclName = \case
    T.Function _ (L _ _ n) _ _ _ -> Just $ flattenName n
    T.Method _ _ (L _ _ n) _ -> Just $ flattenName n
    _ -> Nothing

findParent :: Text -> [Text] -> [DDecl] -> Maybe Text
findParent name allNames allDecls =
    let nameLower = Text.toLower name
        allNamesLower = map Text.toLower allNames

        -- A constructor's first parameter is the parent.
        isCtor (T.Function ret _ _ _ _) =
            case getTypeName ret of
                Just r  ->
                    let rLower = Text.toLower r
                    in r == name || rLower == nameLower
                Nothing -> False
        isCtor _ = False

        ctors = filter (isCtor . fst) allDecls
        parentFromCtor =
            let candidates = map checkCtor ctors
                checkCtor (T.Function _ _ (firstParam:_) _ _, _) =
                    case varType firstParam of
                        T.ConstPointerType _ -> Nothing
                        T.ConstType _        -> Nothing
                        ty                   -> getTypeName ty
                checkCtor _ = Nothing

            in if any isNothing candidates then Nothing else listToMaybe (catMaybes candidates)

        -- Or the name prefix (Parent_Child), if the constructors agree.
        possibleParents = filter (\n -> (n <> "_") `Text.isPrefixOf` nameLower && n /= nameLower) allNamesLower
        parentFromPrefix = case possibleParents of
           [] -> Nothing
           ps -> let pLower = List.maximumBy (compare `on` Text.length) ps
                     candidate = List.find ((== pLower) . Text.toLower) allNames
                 in case candidate of
                     Nothing -> Nothing
                     Just p ->
                         -- No constructor taking the parent means no child (Options).
                         let paramTypes = map (ctorFirstParamType . fst) ctors
                             ctorFirstParamType (T.Function _ _ (firstParam:_) _ _) = varType firstParam
                             ctorFirstParamType _ = T.BuiltinType T.Void

                             isParentArg (T.PointerType (L _ _ t))      = flattenName t == p
                             isParentArg (T.ConstPointerType (L _ _ t)) = flattenName t == p
                             isParentArg (T.Typename (L _ _ t))         = flattenName t == p
                             isParentArg _                              = False
                         in if null ctors
                            then Just p
                            else if any isParentArg paramTypes
                            then Just p
                            else Nothing

    in case parentFromCtor of
           Just p | p `elem` allNames -> Just p
           _                          -> parentFromPrefix

isFuncNamed :: Text -> DDecl -> Bool
isFuncNamed n (T.Function _ (L _ _ fn) _ _ _, _) = flattenName fn == n
isFuncNamed _ _                                  = False

getTypeName :: T.Decl (Lexeme Name) -> Maybe Text
getTypeName (T.PointerType (L _ _ n))      = Just (flattenName n)
getTypeName (T.ConstPointerType (L _ _ n)) = Just (flattenName n)
getTypeName (T.Typename (L _ _ n))         = Just (flattenName n)
getTypeName (T.ConstType ty)               = getTypeName ty
getTypeName _                              = Nothing

findAssociatedConstants :: ResourceContext -> Text -> [Text]
findAssociatedConstants ctx name =
    let n = Text.toUpper $ ctxCName ctx
        p = Text.toUpper name
        gp = Text.toUpper $ ctxPrefix ctx
        patterns = [n <> "_MAX_" <> p <> "_LENGTH", n <> "_MAX_" <> p <> "_SIZE", n <> "_" <> p <> "_SIZE", gp <> "_" <> p <> "_SIZE", gp <> "_" <> p <> "_LENGTH"]
    in List.sort [ S.constantName c | c <- ctxConstants ctx, any (`Text.isInfixOf` S.constantName c) patterns ]

-- | Buffer-size expression from a @[/*! ... */]@ comment, e.g.
-- @max(width, abs(ystride)) * height@.
sizeExprOf :: T.Decl (Lexeme Name) -> Maybe SizeExpr
sizeExprOf = \case
    T.Ref (L _ _ n)    -> Just (SizeVar (flattenName n))
    T.IntVal (L _ _ v) -> SizeNum <$> readMaybe (Text.unpack (flattenName v))
    T.Paren e          -> sizeExprOf e
    T.Abs e            -> SizeCall "abs" . pure <$> sizeExprOf e
    T.Max l r          -> SizeCall "max" <$> traverse sizeExprOf [l, r]
    T.Add l r          -> SizeOp '+' <$> sizeExprOf l <*> sizeExprOf r
    T.Sub l r          -> SizeOp '-' <$> sizeExprOf l <*> sizeExprOf r
    T.Mul l r          -> SizeOp '*' <$> sizeExprOf l <*> sizeExprOf r
    T.Div l r          -> SizeOp '/' <$> sizeExprOf l <*> sizeExprOf r
    _                  -> Nothing

varType :: T.Decl lexeme -> T.Decl lexeme
varType (T.Var ty _) = ty
varType d            = d
