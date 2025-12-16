{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Apigen.Parser.Semantic (toSemantic) where

import qualified Apigen.Inference           as I
import           Apigen.Parser.AST          (Decl (..), Model (..), Module (..))
import qualified Apigen.Parser.AST          as T
import qualified Apigen.Parser.Query        as Query
import           Apigen.Parser.SymbolTable  (Name, SId)
import qualified Apigen.Semantic            as S
import qualified Apigen.Types               as T
import           Control.Monad              (filterM, when)
import           Control.Monad.Trans.Writer (Writer, runWriter, tell)
import           Data.Char                  (isSpace, isUpper, toLower, toUpper)
import           Data.Fix                   (Fix (..))
import           Data.Function              (on)
import qualified Data.List                  as List
import           Data.Maybe                 (fromMaybe, isJust, isNothing,
                                             listToMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (AlexPosn (..), Lexeme (..),
                                             LexemeClass (..), lexemeText)
import           Text.Read                  (readMaybe)

type DDecl = (T.Decl (Lexeme Name), FilePath)

toSemantic :: Bool -> T.Model (Lexeme Name) -> S.SemanticModel
toSemantic strict model0 =
    let model = rewriteArrayTypedefs model0
        simplifiedDecls = [ (d, moduleFile m) | m <- T.modelMods model, d <- T.moduleDecls m ]

        (enums_, diag1) = runWriter $ gatherEnums toSemanticName simplifiedDecls
        (constants_, diag2) = runWriter $ gatherConstants simplifiedDecls

        allIdNames = gatherIdNames simplifiedDecls
        allCallbackNames = gatherCallbackNames simplifiedDecls
        allResNames = gatherResourceNames simplifiedDecls
        cp = commonPrefix allResNames

        -- Helper to convert C name to semantic name
        toSemanticName n =
            let cpLower = Text.toLower cp
                errP = cp <> "_Err_"
                stdP = cp <> "_"
                errPL = cpLower <> "_err_"
                stdPL = cpLower <> "_"

                stripped = if errP `Text.isPrefixOf` n then "Err_" <> Text.drop (Text.length errP) n
                           else if stdP `Text.isPrefixOf` n then Text.drop (Text.length stdP) n
                           else if errPL `Text.isPrefixOf` n then "Err_" <> Text.drop (Text.length errPL) n
                           else if stdPL `Text.isPrefixOf` n then Text.drop (Text.length stdPL) n
                           else if cp `Text.isPrefixOf` n then
                               let s = Text.drop (Text.length cp) n
                                   s' = if "_" `Text.isPrefixOf` s then Text.drop 1 s else s
                               in if not (Text.null s') && isUpper (Text.head s') then s' else n
                           else n

                final = stripped
            in final

        idTypes_ = [ S.SIdTypeModel (toSemanticName n) n (S.SUInt 32) | n <- allIdNames ]

        (callbacks_, diag4) = runWriter $ gatherCallbackTypes toSemanticName allResNames idTypes_ allCallbackNames simplifiedDecls

        -- Discover skeletons first to build a stable hierarchy
        skeletons = [ buildSkeleton cp allResNames simplifiedDecls name | name <- allResNames ]

        assignedDecls = [ (bestResourceForDecl cp toSemanticName skeletons allIdNames allCallbackNames d, d) | d <- simplifiedDecls ]

        (resources_, diag3) = runWriter $ mapM (fillResource strict enums_ constants_ allIdNames allCallbackNames assignedDecls skeletons cp toSemanticName) skeletons

        (variants_, diag5) = runWriter $ gatherVariants resources_ enums_ toSemanticName

        allDiagnostics = diag1 ++ diag2 ++ diag3 ++ diag4 ++ diag5
    in S.SemanticModel (List.nub $ List.sortOn S.enumName enums_)
                        (List.nub $ List.sortOn S.constantName constants_)
                        (List.nub $ List.sortOn S.idName idTypes_)
                        (List.nub $ List.sortOn S.cbName callbacks_)
                        (List.nub $ List.sortOn S.resourceName resources_)
                        (List.nub $ List.sortOn S.variantName variants_)
                        cp
                        allDiagnostics

-- | Rewrite references to @typedef uint8_t X[SIZE]@ types into explicit sized
-- byte arrays. Without this the extractor sees only an opaque type name and
-- mis-classifies these fixed-size key/address buffers as enums.
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
        T.Function ret nm ps     -> T.Function (rwTy ret) nm (map rwDecl ps)
        T.CallbackTypeDecl nm ps -> T.CallbackTypeDecl nm (map rwDecl ps)
        T.Var ty nm              -> T.Var (rwTy ty) nm
        d                        -> d
    rwTy t = case t of
        T.Typename (L _ _ nm)
            | Just sz <- lookup (flattenName nm) defs ->
                T.SizedArrayType (T.BuiltinType (T.UInt T.B8)) sz
        T.ConstType inner -> T.ConstType (rwTy inner)
        _ -> t

bestResourceForDecl :: Text -> (Text -> Text) -> [S.SResource] -> [Text] -> [Text] -> DDecl -> Text
bestResourceForDecl cp toSem allSkeletons allIdNames allCallbackNames d@(decl, _) =
    case getDeclName decl of
        Nothing -> ""
        Just dName ->
            let dNameLower = Text.toLower dName
                matchingRes = filter (\r ->
                    let rCNameLower = Text.toLower (S.cName r)
                    in (rCNameLower <> "_") `Text.isPrefixOf` dNameLower
                       || (rCNameLower == "tox" && "toxav_" `Text.isPrefixOf` dNameLower)
                    ) allSkeletons

                overrides = if "tox_pass_" `Text.isPrefixOf` dNameLower
                               || dNameLower `elem` ["tox_get_salt", "tox_is_data_encrypted"]
                            then filter (\r -> S.cName r == "Tox_Pass_Key") allSkeletons
                            else if dNameLower == "tox_conference_invite"
                            then filter (\r -> S.resourceName r == "Tox") allSkeletons
                            else []

                candidates = if not (null overrides) then overrides else matchingRes

                score res =
                    let baseScore = declIdentityMatchScore allSkeletons allIdNames res d
                        returnsRes = if declReturnsResourceIdentity cp toSem allIdNames allCallbackNames res d then 10 else 0
                    in (baseScore + returnsRes, Text.length (S.cName res), negate (Text.length (S.resourceName res)))
                best = List.maximumBy (compare `on` score) candidates
            in if null candidates then ""
               else S.resourceName best
declIdentityMatchScore :: [S.SResource] -> [Text] -> S.SResource -> DDecl -> Int
declIdentityMatchScore allSkeletons _ res (T.Function _ _ params, _) =
    matchDeclParams allSkeletons res params
declIdentityMatchScore allSkeletons _ res (T.CallbackTypeDecl (L _ _ _) params, _) =
    matchDeclParams allSkeletons res params
declIdentityMatchScore _ _ _ _ = 0

matchDeclParams :: [S.SResource] -> S.SResource -> [T.Decl (Lexeme Name)] -> Int
matchDeclParams allSkeletons res params =
    let collapsed = collapseParams params
        path = I.getHierarchy allSkeletons res
        -- For simplicity, just count how many path IDs are present in arguments
        -- Tox convention: arguments follow the hierarchy: Root, Child1, Child2, ...
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
        in fn == name || fn == name <> "_Number" || fn == name <> "_Id" || semName <> "Id" == fn
    -- A handle resource is identified by a pointer to its C type, e.g. the
    -- @const Tox *tox@ receiver. Without this, functions that take only the
    -- root handle (e.g. @tox_conference_get_chatlist@) score zero everywhere
    -- and get mis-attributed to the deepest prefix-matching resource.
    isIdentityOf res' (T.Var (T.PointerType (L _ _ n)) _) =
        flattenName n == S.cName res'
    isIdentityOf res' (T.Var (T.ConstPointerType (L _ _ n)) _) =
        flattenName n == S.cName res'
    isIdentityOf res' (T.Var _ (L _ _ n)) =
        let fn = flattenName n
            semName = S.resourceName res'
        in if semName == "Conference"
           then fn `elem` ["groupnumber", "conference_number", "group_number", "conference_number"]
           else False
    isIdentityOf _ _ = False

declReturnsResourceIdentity :: Text -> (Text -> Text) -> [Text] -> [Text] -> S.SResource -> DDecl -> Bool
declReturnsResourceIdentity _cp toSem idNames cbNames res (T.Function ret _ _, fp) =
    let tRet = fst $ runWriter $ toSType toSem idNames cbNames (S.SLocation (Text.pack fp) 0 0) ret
    in case tRet of
        S.SHandle h -> h == S.resourceName res
        S.SResourceId i -> i == S.resourceName res <> "Id" || i == S.resourceName res <> "_Number"
        _ -> False
declReturnsResourceIdentity _ _ _ _ _ _ = False




gatherEnums :: (Text -> Text) -> [DDecl] -> Writer [S.SDiagnostic] [S.SEnumModel]
gatherEnums toSem decls = pure $ mapMaybe mkEnum decls
  where
    mkEnum (T.Enumeration _ (L _ _ name) mems, _) =
        let n = flattenName name
            members = mapMaybe enumMemberName mems
            cp' = commonPrefix members
            semMembers = map (\m -> (m, fromMaybe m (Text.stripPrefix cp' m))) members
        in Just $ S.SEnumModel n (toSem n) semMembers
    mkEnum _ = Nothing

    enumMemberName (T.EnumMember (L _ _ name)) = Just (flattenName name)
    enumMemberName _                           = Nothing

gatherConstants :: [DDecl] -> Writer [S.SDiagnostic] [S.SConstantModel]
gatherConstants decls = pure $ mapMaybe toConstant decls
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

gatherIdNames :: [DDecl] -> [Text]
gatherIdNames decls = List.nub $ mapMaybe (toIdName . fst) decls
  where
    toIdName (T.IdTypeDecl (L _ _ name)) =
        let n = flattenName name
        in if "Number" `Text.isSuffixOf` n || "Id" `Text.isSuffixOf` n then Just n else Nothing
    toIdName (T.TypeDecl (L _ _ name)) =
        let n = flattenName name
        in if "Number" `Text.isSuffixOf` n || "Id" `Text.isSuffixOf` n then Just n else Nothing
    toIdName _                           = Nothing

gatherCallbackNames :: [DDecl] -> [Text]
gatherCallbackNames decls = List.nub $ mapMaybe (toCbName . fst) decls
  where
    toCbName (T.CallbackTypeDecl (L _ _ name) _) = Just (flattenName name)
    toCbName _                                   = Nothing

gatherResourceNames :: [DDecl] -> [Text]
gatherResourceNames decls = List.nub $ mapMaybe (toResName . fst) decls
  where
    toResName (T.TypeDecl (L _ _ name)) = Just $ flattenName name
    toResName (T.ClassDecl (L _ _ name) _) = Just $ flattenName name
    toResName (T.IdTypeDecl (L _ _ name)) =
        let n = flattenName name
            resName = if "Number" `Text.isSuffixOf` n
                      then Just (Text.dropEnd 7 n)
                      else if "Id" `Text.isSuffixOf` n
                      then Just (Text.dropEnd 3 n)
                      else Nothing
        in resName
    toResName _ = Nothing

gatherCallbackTypes :: (Text -> Text) -> [Text] -> [S.SIdTypeModel] -> [Text] -> [DDecl] -> Writer [S.SDiagnostic] [S.SCallbackTypeModel]
gatherCallbackTypes toSem _allResNames idTypes_ cbNames decls = mapMaybeM toCallback decls
  where
    toCallback (T.CallbackTypeDecl (L _ _ name) params, fp) =
        let n = flattenName name
            semName = toSem n
            -- Fallback toSemantic (simple stripping of prefix? or identity?)
            -- For types inside callbacks, we don't have full context. We use basic heuristics.
            -- This function is similar to `toSEvent` logic but more generic.
            simpleToSem = toSem
            idNames = map S.idCName idTypes_

        in do
           let stripped = collapseParams params

           semParams <- mapM (\d -> do
                 let loc = getLoc fp d
                 case d of
                     T.Var ty (L _ _ pName) -> do
                        t <- toSType simpleToSem idNames cbNames loc ty
                        let t' = if "toxav_" `Text.isPrefixOf` n && t == S.SResourceId "Friend_Number" then S.SUInt 32 else t
                        let cns = getConstness ty
                        return $ S.SParameter (flattenName pName) t' cns []
                     _ -> return $ S.SParameter "unknown" S.SVoid T.MutableThis []
                 ) stripped

           return $ Just $ S.SCallbackTypeModel semName n semParams
    toCallback (T.Function (T.CallbackType (L _ _ _name)) _ _params, _fp) =
       -- Handle function pointer typedefs that might be parsed as Function returning CallbackType?
       -- Or check if language-cimple parses `typedef void func(...)` as Function decl?
       -- But typedef keyword should change it.
       -- If `language-cimple` does not support function typedefs, we might be out of luck without patching it.
       return Nothing
    toCallback _ = return Nothing


buildSkeleton :: Text -> [Text] -> [DDecl] -> Text -> S.SResource
buildSkeleton cPrefix allResNames allDecls name =
    let mParent = findParent name allResNames allDecls

        -- Helper to convert C name to semantic name
        toSemanticName n =
            let stripped = fromMaybe n $ Text.stripPrefix cPrefix n
                stripped' = fromMaybe stripped $ Text.stripPrefix "_" stripped
            in if Text.null stripped' then n else stripped'

        resType = if isIdResource name allDecls
                  then case findIdentifier toSemanticName name allDecls of
                      Just t  -> S.ResId t
                      Nothing -> S.ResHandle -- Fallback? Should be strict?
                  else S.ResHandle

        semanticName = toSemanticName name
        semanticParent = fmap toSemanticName mParent
        isRoot = Text.toLower name == Text.toLower cPrefix
        prefix = Text.toLower name <> "_"

    in S.SResource semanticName name resType prefix isRoot semanticParent Nothing [] [] [] []

fillResource :: Bool -> [S.SEnumModel] -> [S.SConstantModel] -> [Text] -> [Text] -> [(Text, DDecl)] -> [S.SResource] -> Text -> (Text -> Text) -> S.SResource -> Writer [S.SDiagnostic] S.SResource
fillResource strict enums_ constants_ allIdNames allCallbackNames allDecls allSkeletons cp toSem res = do
    let name = S.resourceName res
        myDecls = [ d | (r, d) <- allDecls, r == name ]
        fullPath = I.getHierarchy allSkeletons res
        ctx = ResourceContext (S.resourceName res) (S.resourceType res) (S.parent res) (map snd allDecls) constants_ allIdNames allCallbackNames fullPath strict (S.cName res) toSem cp

    checkIdConvention ctx cp toSem

    evs <- gatherEvents ctx myDecls
    meths <- gatherMethods allSkeletons ctx myDecls

    let eventRegistrarNames = [ Text.toLower (ctxCName ctx) <> "_callback_" <> S.eventName ev | ev <- evs ]
    let meths' = filter (\m -> Text.toLower (S.methodName m) `notElem` eventRegistrarNames) meths

    let props = identifyProperties ctx meths'

    -- Update methods with size function info from properties
    let meths'' = map (updateMethodWithSize allSkeletons res meths' props) meths'

    checkErrorEnums ctx enums_ (meths'', props)

    let mLifecycleParent = case List.find ((== S.Constructor) . S.methodRole) meths'' of
            Just ctor ->
                let ctorName = S.methodName ctor
                    ctorDecls = filter (isFuncNamed ctorName) myDecls
                    isConstArg = any isConstCtor ctorDecls
                    isConstCtor (T.Function _ _ (firstParam:_), _) =
                        case varType firstParam of
                            T.ConstPointerType _ -> True
                            T.ConstType _        -> True
                            _                    -> False
                    isConstCtor _ = False
                in if isConstArg
                   then S.parent res
                   else case S.inputs ctor of
                        (S.SParameter _ (S.SHandle h) _ _) : _ ->
                            if h /= S.resourceName res then Just h else Nothing
                        _ -> S.parent res
            Nothing -> S.parent res

    let traits_ = detectTraits (S.cPrefix res) meths'' props

    return $ res { S.properties = props, S.methods = meths'', S.traits = traits_, S.events = evs, S.lifecycleParent = mLifecycleParent, S.isRoot = S.isRoot res }

updateMethodWithSize :: [S.SResource] -> S.SResource -> [S.SMethod] -> [S.SProperty] -> S.SMethod -> S.SMethod
updateMethodWithSize allRes res allMeths props m =
    -- Find if this method is a "read" method for any property
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
        ctxName          :: Text
    ,   ctxType          :: S.SResourceType
    ,   ctxParent        :: Maybe Text
    ,   ctxAllDecls      :: [DDecl]
    ,   ctxConstants     :: [S.SConstantModel]
    ,   ctxIdNames       :: [Text]
    ,   ctxCallbackNames :: [Text]
    ,   ctxPath          :: [Text]
    ,   ctxStrictMode    :: Bool
    ,   ctxCName         :: Text
    ,   ctxToSemantic    :: Text -> Text
    ,   ctxPrefix        :: Text
    }

identifyProperties :: ResourceContext -> [S.SMethod] -> [S.SProperty]
identifyProperties ctx methods =
    let
        stripPrefix name =
            let p = Text.toLower (ctxCName ctx) <> "_"
            in fromMaybe name (Text.stripPrefix p (Text.toLower name))

        analyzeName name =
            let stripped = stripPrefix name

                checkSize suffix dropLen =
                    if suffix `Text.isSuffixOf` stripped
                    then
                        let base = Text.dropEnd dropLen stripped
                            (p, r) = Text.breakOn "get_" base
                        in if not (Text.null r)
                           then Just ("size", p <> Text.drop 4 r)
                           else Just ("size", base)
                    else Nothing

            in case checkSize "_size" 5 of
                 Just s -> Just s
                 Nothing -> case checkSize "_length" 7 of
                     Just s -> Just s
                     Nothing ->
                        let (p, r) = Text.breakOn "get_" stripped
                        in if not (Text.null r) then Just ("get", p <> Text.drop 4 r)
                        else let (p', r') = Text.breakOn "set_" stripped
                             in if not (Text.null r') then Just ("set", p' <> Text.drop 4 r')
                             else if "self_get_" `Text.isPrefixOf` stripped then Just ("get", "self_" <> Text.drop 9 stripped)
                             else if "self_set_" `Text.isPrefixOf` stripped then Just ("set", "self_" <> Text.drop 9 stripped)
                             else Nothing

        candidates = foldl (categorizeMethod analyzeName) [] methods

        categorizeMethod :: (Text -> Maybe (Text, Text)) -> [(Text, Maybe S.SMethod, Maybe S.SMethod, Maybe S.SMethod)] -> S.SMethod -> [(Text, Maybe S.SMethod, Maybe S.SMethod, Maybe S.SMethod)]
        categorizeMethod analyzer acc m =
            case analyzer (S.methodName m) of
                Just ("get", base) -> insertOrUpdate (clean base) (\(_, s, z) -> (Just m, s, z)) acc
                Just ("set", base) -> insertOrUpdate (clean base) (\(g, _, z) -> (g, Just m, z)) acc
                Just ("size", base) -> insertOrUpdate (clean base) (\(g, s, _) -> (g, s, Just m)) acc
                _ -> acc
          where
            -- Collapse the @x_data@ getter of a size-then-get buffer onto base
            -- name @x@ — but only for an actual buffer accessor, so that a
            -- scalar like @experimental_owned_data@ keeps its name.
            clean b = if "_data" `Text.isSuffixOf` b && isBufferMethod
                      then Text.dropEnd 5 b else b
            isBufferMethod =
                isBufTy (S.output m) || any (isBufTy . S.paramType) (S.inputs m)
            isBufTy = \case
                S.SBytes -> True; S.SFixedBytes{} -> True
                S.SList{} -> True; S.SFixedList{} -> True; _ -> False

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
        -- Validate signatures
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
                -- Drop the @_data@ of a buffer's get_x_data accessor, but keep
                -- it on scalar names like @experimental_owned_data@.
                isBufferProp = case propType of
                    S.SBytes -> True; S.SFixedBytes{} -> True
                    S.SList{} -> True; S.SFixedList{} -> True; _ -> False
                cleanName = if "_data" `Text.isSuffixOf` name && isBufferProp
                            then Text.dropEnd 5 name else name

                constants = findAssociatedConstants ctx cleanName

                err = case mGet of
                    Just g  -> S.methodErrorType g
                    Nothing -> case mSet of
                         Just s  -> S.methodErrorType s
                         Nothing -> Nothing

            -- Filter out "get_size" property if it's just the size of another property
            let isOrphanSize = isNothing mGet && isNothing mSet && isJust mSize
            if isOrphanSize then Nothing else
                return $ S.SProperty cleanName propType (fmap S.methodName mGet) (fmap S.methodName mSet) (fmap S.methodName mSize) err constants

gatherMethods :: [S.SResource] -> ResourceContext -> [DDecl] -> Writer [S.SDiagnostic] [S.SMethod]
gatherMethods allRes ctx decls = mapMaybeM (toSMethod allRes ctx) decls

toSMethod :: [S.SResource] -> ResourceContext -> DDecl -> Writer [S.SDiagnostic] (Maybe S.SMethod)
toSMethod allRes ctx d@(T.Function ret (L _ _ name) params, fp) =
    do
        let n = flattenName name
        let isGetter = "_get_" `Text.isInfixOf` n || "self_get_" `Text.isInfixOf` n

        tRet <- toSType (ctxToSemantic ctx) (ctxIdNames ctx) (ctxCallbackNames ctx) (getLoc fp (fst d)) ret
        let initialRole = inferMethodRole ctx n tRet []

        (argMappings1, semParams1) <- partitionParams ctx n initialRole (collapseParams params) fp isGetter

        -- If we have a BufferPtr, we might need to adjust the return type
        finalRet <- if any isBufferPtr argMappings1
                    then do
                        -- Find the parameter that corresponds to BufferPtr
                        -- partitionParams logic is complex, so we re-scan to find the buffer param type
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
            -- Refine role: if it has no ThisObject or ParentObject, it's a StaticRole
            role' = if role == S.ActionRole && not (any isInstanceHandle argMappings1)
                    then S.StaticRole
                    else role

        -- Recalculate mappings with the final role (and isGetter)
        (argMappings, semParams) <- partitionParams ctx n role' (collapseParams params) fp isGetter

        let cns = fromMaybe T.ConstThis $ headMay $ mapMaybe (\case S.ThisObject c -> Just c; S.PathObject _ c -> Just c; _ -> Nothing) argMappings
            headMay []    = Nothing
            headMay (x:_) = Just x
            resStrat = if isJust errType then
                           if tRet == S.SBool && finalRet == tRet && not isGetter then S.IgnoreReturn
                           else case finalRet of
                               S.SResourceId _ -> S.ReturnIsResult
                               S.SHandle h -> if h == ctxName ctx <> "Id" then S.ReturnIsValue else S.ReturnIsValue -- Fix Logic
                               -- Wait, I'm copying old code context. I should just insert the function.
                               _ -> S.ReturnIsValue
                       else if role' == S.Constructor then S.ReturnIsValue
                       else S.ReturnIsValue

            -- For variable array return types without wrapping in Property logic, we just treat them as SBytes/SList
            -- The generator will have to handle manual size management if not a property.

        let hasUserData = any (== S.UserData) argMappings
            cMap = S.CFunctionMapping n argMappings Nothing Nothing errType semParams tRet
            (_, reasons) = checkStandardMapping allRes ctx role' cMap cns resStrat hasUserData finalRet

        when (ctxStrictMode ctx && not (null reasons)) $
            tell [S.SDiagnostic S.Warning (Just (getLoc fp (fst d))) (n <> ": non-standard mapping: " <> Text.intercalate ", " reasons)]

        -- We always use CustomMapping to preserve the exact inferred mapping
        return $ Just $ S.SMethod n role' semParams finalRet cns errType resStrat (S.CustomMapping cMap) hasUserData []
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

    paramType' c f (T.Var ty _) = toSType (ctxToSemantic c) (ctxIdNames c) (ctxCallbackNames c) (getLoc f (fst d)) ty
    paramType' c f (T.SizedArrayType ty _) = toSType (ctxToSemantic c) (ctxIdNames c) (ctxCallbackNames c) (getLoc f (fst d)) ty
    paramType' _ _ _ = return S.SVoid

    isBuf S.SBytes _              = True
    isBuf (S.SFixedBytes _ _) _   = True
    isBuf (S.SList _) _           = True
    isBuf (S.SFixedList _ _ _) _  = True
    isBuf (S.SHandle "uint8_t") _ = True
    isBuf (S.SHandle "char") _    = True
    isBuf _ _                     = False

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
    S.BufferPtr (Just s)       -> "buffer(" <> s <> ")"
    S.BufferPtr Nothing        -> "buffer"
    S.BufferSize               -> "size"
    S.UserData                 -> "userData"
    S.Constant v               -> "const(" <> Text.pack (show v) <> ")"

checkStandardMapping :: [S.SResource] -> ResourceContext -> S.SMethodRole -> S.CFunctionMapping -> T.Constness -> S.SResultStrategy -> Bool -> S.SType -> (S.SMapping, [Text])
checkStandardMapping allRes ctx role cmap cns resStrat hasUserData retTy =
    let dummyRes = S.SResource (ctxName ctx) (ctxCName ctx) (ctxType ctx) "" False (ctxParent ctx) Nothing [] [] [] []
        dummyMethod = S.SMethod (S.cFunctionName cmap) role (S.cSemParams cmap) retTy cns (S.cErrorType cmap) resStrat S.StandardMapping hasUserData []
        validMappings = I.getValidArgMappings allRes dummyRes dummyMethod
        fmt args = "(" <> Text.intercalate ", " (map formatArgSource args) <> ")"
    in case validMappings of
       (first:_) | S.argMapping cmap == first -> (S.StandardMapping, [])
       _ -> if S.argMapping cmap `elem` validMappings
       then (S.CustomMapping cmap, [])
       else (S.CustomMapping cmap, ["expected " <> Text.intercalate " or " (map fmt validMappings) <> " but found " <> fmt (S.argMapping cmap)])

inferMethodRole :: ResourceContext -> Text -> S.SType -> [S.SParameter] -> S.SMethodRole
inferMethodRole ctx name ret semParams =
    let _n = Text.toLower $ ctxName ctx
        cn = ctxCName ctx
        nameLower = Text.toLower name
        isMyHandle (S.SHandle h) = h == ctxName ctx
        isMyHandle _             = False
        isMyId (S.SResourceId i) =
            let candidates = [ Text.stripSuffix "Number" i >>= Text.stripSuffix "_"
                             , Text.stripSuffix "Id" i >>= Text.stripSuffix "_"
                             , Text.stripSuffix "Number" i
                             , Text.stripSuffix "Id" i ]
            in i == ctxName ctx <> "Id" || any (== Just (ctxName ctx)) candidates
        isMyId _                 = False
        isCallback (S.SCallback _) = True
        isCallback _               = False
        hasHandleOutParam = any (\p -> isMyHandle (S.paramType p)) semParams
    in if isMyHandle ret || isMyId ret || (hasHandleOutParam && not ("_equal" `Text.isSuffixOf` nameLower)) then S.Constructor
       else if (Text.toLower cn <> "_kill") == nameLower || (Text.toLower cn <> "_free") == nameLower then S.Destructor
       else if any (isCallback . S.paramType) semParams then S.RegistrarRole
       else S.ActionRole

partitionParams :: ResourceContext -> Text -> S.SMethodRole -> [T.Decl (Lexeme Name)] -> FilePath -> Bool -> Writer [S.SDiagnostic] ([S.CArgSource], [S.SParameter])
partitionParams ctx funcName role params fp isGetterFunc = do
    let collapsed = collapseParams params
        numParams = length collapsed
    ((_, _, semParams), argMappings) <- mapAccumLM (\(pIdx, semIdx, semParams) d -> do
        (semIdx', sources) <- toCArg ctx funcName role numParams pIdx semIdx fp isGetterFunc d
        let isSemantic (S.SemanticArg _) = True
            isSemantic _                 = False
        semParam <- if any isSemantic sources then (:[]) <$> toSParam (ctxIdNames ctx) (ctxCallbackNames ctx) (ctxToSemantic ctx) fp d (ctxCName ctx == "ToxAV") else return []
        return ((pIdx + 1, semIdx', reverse semParam ++ semParams), sources)
        ) (0, 0, []) collapsed
    return (concat argMappings, reverse semParams)

mapAccumLM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
mapAccumLM _ s [] = return (s, [])
mapAccumLM f s (x:xs) = do
    (s', y) <- f s x
    (s'', ys) <- mapAccumLM f s' xs
    return (s'', y:ys)

toSParam :: [Text] -> [Text] -> (Text -> Text) -> FilePath -> T.Decl (Lexeme Name) -> Bool -> Writer [S.SDiagnostic] S.SParameter
toSParam idNames cbNames toSem fp d@(T.Var ty (L _ _ name)) forceIntFriendId = do
    let loc = getLoc fp d
    t <- toSType toSem idNames cbNames loc ty
    let t' = if forceIntFriendId && t == S.SResourceId "Friend_Number" then S.SUInt 32 else t
    let n = flattenName name
    let cns = getConstness ty
    return $ S.SParameter n t' cns []
toSParam _ _ _ _ _ _ = return $ S.SParameter "unknown" S.SVoid T.MutableThis []

toCArg :: ResourceContext -> Text -> S.SMethodRole -> Int -> Int -> Int -> FilePath -> Bool -> T.Decl (Lexeme Name) -> Writer [S.SDiagnostic] (Int, [S.CArgSource])
toCArg ctx funcName role numParams argIdx semIdx fp isGetterFunc d = case d of
    T.Var (T.SizedArrayType memTy sizer@(T.Var _ _)) _ ->
        let isInput = isConst memTy
            isBufArg = isGetterFunc || not isInput
            argType = if isBufArg && not isInput then S.BufferPtr (getSizerName sizer) else S.SemanticArg semIdx
            newSemIdx = if isBufArg && not isInput then semIdx else semIdx + 1
        in return (newSemIdx, [argType, S.BufferSize])
    T.Var (T.SizedArrayType memTy sizer) _ ->
        let isInput = isConst memTy
            isBufArg = isGetterFunc || not isInput
            argType = if isBufArg && not isInput then S.BufferPtr (getSizerName sizer) else S.SemanticArg semIdx
            newSemIdx = if isBufArg && not isInput then semIdx else semIdx + 1
        in return (newSemIdx, [argType])
    T.Var ty n -> do
        let loc = getLoc fp d
            allIdNames = ctxIdNames ctx
            toSem = ctxToSemantic ctx
        sTy <- toSType toSem allIdNames (ctxCallbackNames ctx) loc ty
        let path = ctxPath ctx
            -- Convert handle/resource C name to semantic name for comparison
            (h, sName) = case sTy of
                S.SHandle h'     -> (h', toSem h')
                S.SResourceId n' -> ("", n') -- Already semantic
                _                -> ("", "")

            -- Correct PathId detection
            mPathId = case sTy of
                S.SResourceId n' ->
                    let candidates = [ Text.stripSuffix "Number" n' >>= Text.stripSuffix "_"
                                     , Text.stripSuffix "Id" n' >>= Text.stripSuffix "_"
                                     , Text.stripSuffix "Number" n'
                                     , Text.stripSuffix "Id" n'
                                     , Just n' ]
                        resNameFromId = fromMaybe n' $ List.find (`elem` path) (mapMaybe id candidates)
                    in List.elemIndex resNameFromId path
                _ -> Nothing

            -- Identity components
            isThis = role /= S.Constructor &&
                     (  (argIdx == 0 && ctxType ctx == S.ResHandle && (sName == ctxName ctx || h == ctxCName ctx))
                     || (case ctxType ctx of S.ResId _ -> mPathId == Just (length path - 1); _ -> False) )

            isPathObj0 = argIdx == 0 && not isThis && (List.elemIndex sName path == Just 0)

            isError = argIdx == numParams - 1 && case sTy of S.SHandle _ -> True; _ -> False
                      && (any (`elem` [["error"], ["err"]]) [snd (lexemeText n)] || "error" `List.elem` snd (lexemeText n) || "err" `List.elem` snd (lexemeText n))
            isUserData = case sTy of S.SHandle "void" -> True; _ -> False
                         && (["user", "data"] `List.isSuffixOf` snd (lexemeText n) || ["userData"] `List.isSuffixOf` snd (lexemeText n) || ["userdata"] `List.isSuffixOf` snd (lexemeText n))
                         && funcName /= "tox_options_set_log_user_data"
            isBuffer = (isGetterFunc || not (isConst ty)) && (sTy == S.SBytes || sTy == S.SHandle "uint8_t" || sTy == S.SHandle "char" || sTy == S.SHandle "void" || isList sTy)
                       && not (isConst ty)
                       && funcName /= "tox_options_set_log_user_data"
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
    getFunc d@(T.Function ret (L _ _ name) ps, _) = Just (flattenName name, (ret, ps), d)
    getFunc _ = Nothing

isCallbackFunc :: ResourceContext -> DEvent -> Bool
isCallbackFunc ctx (name, _, _) =
    (Text.toLower (ctxCName ctx) <> "_callback_") `Text.isInfixOf` Text.toLower name

toSEvent :: ResourceContext -> DEvent -> Writer [S.SDiagnostic] (Maybe S.SEvent)
toSEvent ctx (name, (_, ps), (_, _fp)) =
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
            in do
               -- Try to match by stripping common prefix from cbName if direct match fails
               let cbNameText = flattenName cbName
                   prefix = Text.toLower (ctxCName ctx) <> "_"
                   strippedCbName = if prefix `Text.isPrefixOf` cbNameText
                                    then Text.drop (Text.length prefix) cbNameText
                                    else cbNameText

                   cbDecls' = if null cbDecls
                              then filter (isCallbackTypedef strippedCbName . fst) (ctxAllDecls ctx)
                              else cbDecls

               case cbDecls' of
                (T.CallbackTypeDecl _ cbParams, fp'):_ -> do
                    let collapsed = collapseParams cbParams
                    (_, semParams) <- partitionParams ctx "" S.RegistrarRole (stripCbParams collapsed) fp' False
                    return $ Just $ S.SEvent (toEventName ctx name) semParams (flattenName cbName) hasUserData
                _ -> return Nothing

    isCallbackTypedef targetName (T.CallbackTypeDecl (L _ _ n) _) = flattenName n == targetName
    isCallbackTypedef _ _ = False

    -- Strip the first (handle) and last (user_data) parameters from callbacks
    stripCbParams []       = []
    stripCbParams [_]      = []
    stripCbParams (_:rest) = init rest

toEventName :: ResourceContext -> Text -> Text
toEventName ctx name =
    let _n = Text.toLower $ ctxName ctx
        nameLower = Text.toLower name
        prefix = Text.toLower (ctxCName ctx) <> "_callback_"
    in if prefix `Text.isPrefixOf` nameLower
       then Text.drop (Text.length prefix) nameLower
       else nameLower

collapseParams :: [T.Decl (Lexeme Name)] -> [T.Decl (Lexeme Name)]
collapseParams [] = []
collapseParams (T.Var ty name : len@(T.Var (T.BuiltinType T.SizeT) _) : ps)
    | isArray ty = T.Var (T.SizedArrayType ty len) name : collapseParams ps
  where
    isArray T.ArrayType{}      = True
    isArray T.ConstArrayType{} = True
    isArray _                  = False
collapseParams (p:ps) = p : collapseParams ps

flattenName :: Name -> Text
flattenName (ns, n) = Text.intercalate "_" (ns ++ n)

toSType :: (Text -> Text) -> [Text] -> [Text] -> S.SLocation -> T.Decl (Lexeme Name) -> Writer [S.SDiagnostic] S.SType
toSType toSem idNames cbNames loc = \case
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
        in return $ if n `elem` idNames then S.SResourceId (toSem n) else S.SEnum (toSem n)
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
        sTy <- toSType toSem idNames cbNames loc ty
        let hasSize = case sizer of T.Var _ _ -> True; _ -> False
        case getSizerName sizer of
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
    T.ConstType ty -> toSType toSem idNames cbNames loc ty
    x -> do
        tell [S.SDiagnostic S.Error (Just loc) ("unrecognized type: " <> Text.pack (show x))]
        return S.SVoid

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
    T.Function _ (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Method _ _ (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Var _ (L p _ _) -> toSLocation fp (L p IdVar ("" :: Text))
    T.Define (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.Enumeration _ (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.TypeDecl (L p _ _) -> toSLocation fp (L p IdVar ("" :: Text))
    T.ClassDecl (L p _ _) _ -> toSLocation fp (L p IdVar ("" :: Text))
    T.IdTypeDecl (L p _ _) -> toSLocation fp (L p IdVar ("" :: Text))
    _ -> S.SLocation (Text.pack fp) 0 0

gatherVariants :: [S.SResource] -> [S.SEnumModel] -> (Text -> Text) -> Writer [S.SDiagnostic] [S.SVariant]
gatherVariants resources enums toSem = do
    let candidates = mapMaybe findVariantCandidate resources
    tell [S.SDiagnostic S.Warning Nothing ("Found " <> Text.pack (show (length candidates)) <> " variant candidates")]
    vs <- mapM (buildVariant resources enums toSem) candidates
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

buildVariant :: [S.SResource] -> [S.SEnumModel] -> (Text -> Text) -> (S.SResource, Text) -> Writer [S.SDiagnostic] S.SVariant
buildVariant resources enums _ (res, enumName) = do
    let mEnum = List.find (\e -> S.enumSemanticName e == enumName) enums
    case mEnum of
        Nothing -> return $ S.SVariant (S.resourceName res) enumName []
        Just e -> do
            let members = S.enumMembers e
            let variantMembers = mapMaybe (matchMember resources res) members
            return $ S.SVariant (S.resourceName res) enumName variantMembers

matchMember :: [S.SResource] -> S.SResource -> (Text, Text) -> Maybe S.SVariantMember
matchMember allRes res (_, semEnumMember) =
    let getterName = S.cPrefix res <> "get_" <> camelToSnake semEnumMember
        mMethod = List.find (\m -> S.methodName m == getterName) (S.methods res)
    in case mMethod of
        Just m ->
            let cName = case S.methodMapping m of
                    S.CustomMapping cm -> S.cFunctionName cm
                    S.StandardMapping -> S.cFunctionName (I.inferCFunctionMapping allRes res m)
            in case S.output m of
                S.SHandle h -> Just $ S.SVariantMember semEnumMember h cName
                S.SResourceId r -> Just $ S.SVariantMember semEnumMember r cName
                _ -> Nothing
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
                if not (any (\(_, s) -> s == "OK") (S.enumMembers e))
                then tell [S.SDiagnostic S.Warning Nothing ("Error enum " <> et <> " for resource " <> ctxName ctx <> " is missing an OK value")]
                else return ()
            Nothing ->
                tell [S.SDiagnostic S.Warning Nothing ("Error type " <> et <> " for resource " <> ctxName ctx <> " not found in enums")]

checkIdConvention :: ResourceContext -> Text -> (Text -> Text) -> Writer [S.SDiagnostic] ()
checkIdConvention ctx _cp _toSem =
    let name = ctxCName ctx
        badIds = [ flattenName n | (T.IdTypeDecl (L _ _ n), _) <- ctxAllDecls ctx
                                 , let fn = flattenName n
                                 , name `Text.isPrefixOf` fn
                                 , not ("Number" `Text.isSuffixOf` fn || "Id" `Text.isSuffixOf` fn) ]
    in mapM_ (\bad -> tell [S.SDiagnostic S.Warning Nothing ("ID type for " <> name <> " does not follow convention: " <> bad)]) badIds

commonPrefix :: [Text] -> Text
commonPrefix [] = ""
commonPrefix [x] = x
commonPrefix (x:xs) = foldl prefix x xs
  where
    prefix a b = Text.pack $ map fst $ takeWhile (uncurry (==)) $ zip (Text.unpack a) (Text.unpack b)

isIdResource :: Text -> [DDecl] -> Bool
isIdResource name decls = any (isIdDecl . fst) decls
  where
    isIdDecl (T.IdTypeDecl (L _ _ n)) = flattenName n `elem` [name <> "_Number", name <> "_Id"]
    isIdDecl _                        = False

findIdentifier :: (Text -> Text) -> Text -> [DDecl] -> Maybe S.SType
findIdentifier toSem name decls =
    case filter (isIdDecl . fst) decls of
        (T.IdTypeDecl (L _ _ n), _):_ -> Just $ S.SResourceId (toSem (flattenName n))
        _                             -> Nothing
  where
        isIdDecl (T.IdTypeDecl (L _ _ n)) = flattenName n `elem` [name <> "_Number", name <> "_Id"]
        isIdDecl _                        = False

getDeclName :: T.Decl (Lexeme Name) -> Maybe Text
getDeclName = \case
    T.Function _ (L _ _ n) _ -> Just $ flattenName n
    T.Method _ _ (L _ _ n) _ -> Just $ flattenName n
    _ -> Nothing

findParent :: Text -> [Text] -> [DDecl] -> Maybe Text
findParent name allNames allDecls =
    let nameLower = Text.toLower name
        allNamesLower = map Text.toLower allNames

        -- Special case for AV
        isAV = name == "AV" || name == "ToxAV"

        -- Method 1: Constructor inference (functions returning the resource handle/ID)
        isCtor (T.Function ret _ _) =
            case getTypeName ret of
                Just r  ->
                    let rLower = Text.toLower r
                    in r == name || rLower == nameLower || rLower == Text.toLower ("Tox_" <> name)
                Nothing -> False
        isCtor _ = False

        ctors = filter (isCtor . fst) allDecls
        parentFromCtor =
            let candidates = map checkCtor ctors
                checkCtor (T.Function _ _ (firstParam:_), _) =
                    case varType firstParam of
                        T.ConstPointerType _ -> Nothing
                        T.ConstType _        -> Nothing
                        ty                   -> getTypeName ty
                checkCtor _ = Nothing

                headMay (x:_) = Just x
                headMay []    = Nothing
            in if any isNothing candidates then Nothing else headMay (mapMaybe id candidates)

        -- Method 2: Naming convention (Parent_Child) -> Verify with Constructor
        possibleParents = filter (\n -> (n <> "_") `Text.isPrefixOf` nameLower && n /= nameLower) allNamesLower
        parentFromPrefix = case possibleParents of
           [] -> Nothing
           ps -> let pLower = List.maximumBy (compare `on` Text.length) ps
                     candidate = List.find ((== pLower) . Text.toLower) allNames
                 in case candidate of
                     Nothing -> Nothing
                     Just p ->
                         -- Verify if constructor takes Parent* as first arg
                         -- If no constructor exists, fallback to prefix (weak inference)
                         -- If constructor exists but doesn't take Parent, it's NOT a child (e.g. Options)
                         let paramTypes = map (ctorFirstParamType . fst) ctors
                             ctorFirstParamType (T.Function _ _ (firstParam:_)) = varType firstParam
                             ctorFirstParamType _ = T.BuiltinType T.Void

                             isParentArg (T.PointerType (L _ _ t))      = flattenName t == p
                             isParentArg (T.ConstPointerType (L _ _ t)) = flattenName t == p
                             isParentArg (T.Typename (L _ _ t))         = flattenName t == p
                             isParentArg _                              = False
                         in if null ctors
                            then Just p -- No constructor, assume naming implies hierarchy?
                            else if any isParentArg paramTypes
                            then Just p
                            else Nothing

    in case parentFromCtor of
           Just _ | name == "File" -> Just "Friend"
           Just p | p `elem` allNames -> Just p
           _                          -> if isAV then Just "Tox" else parentFromPrefix

isFuncNamed :: Text -> DDecl -> Bool
isFuncNamed n (T.Function _ (L _ _ fn) _, _) = flattenName fn == n
isFuncNamed _ _                              = False

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

-- | Render a buffer-size expression from a @[/*! ... */]@ comment. Handles
-- arithmetic and the @abs@/@max@ calls used by the video-frame size
-- expressions (e.g. @max(width, abs(ystride)) * height@).
getSizerName :: T.Decl (Lexeme Name) -> Maybe Text
getSizerName (T.Ref (L _ _ n))    = Just (flattenName n)
getSizerName (T.IntVal (L _ _ v)) = Just (flattenName v)
getSizerName (T.Paren e)          = (\n -> "(" <> n <> ")") <$> getSizerName e
getSizerName (T.Abs e)            = (\n -> "abs(" <> n <> ")") <$> getSizerName e
getSizerName (T.Sizeof e)         = (\n -> "sizeof(" <> n <> ")") <$> getSizerName e
getSizerName (T.Max l r)          = do
    l' <- getSizerName l
    r' <- getSizerName r
    return $ "max(" <> l' <> ", " <> r' <> ")"
getSizerName (T.Add l r)          = binOp "+" l r
getSizerName (T.Sub l r)          = binOp "-" l r
getSizerName (T.Mul l r)          = binOp "*" l r
getSizerName (T.Div l r)          = binOp "/" l r
getSizerName _                    = Nothing

binOp :: Text -> T.Decl (Lexeme Name) -> T.Decl (Lexeme Name) -> Maybe Text
binOp op l r = do
    l' <- getSizerName l
    r' <- getSizerName r
    return $ l' <> " " <> op <> " " <> r'

-- Helper because T.Var contains the type in 'ty' field
varType :: T.Decl lexeme -> T.Decl lexeme
varType (T.Var ty _) = ty
varType d            = d
