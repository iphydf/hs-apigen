{-# LANGUAGE OverloadedStrings #-}
module Apigen.Inference
    ( deriveHandleName
    , findRoot
    , getHierarchy
    , getValidArgMappings
    , inferCFunctionMapping
    , resolvePathTarget
    ) where

import           Apigen.Semantic (CArgSource (..), CFunctionMapping (..),
                                  SMethod (..), SMethodRole (..),
                                  SParameter (..), SResource (..),
                                  SResourceType (..), SResultStrategy (..),
                                  SType (..))
import           Apigen.Types    (Constness (..))
import           Data.List       (find, sortOn)
import           Data.Maybe      (isJust, isNothing)
import           Data.Text       (Text)
import qualified Data.Text       as Text

-- | Infers the idiomatic handle name for a resource.
-- This logic was moved from Language.C to be shared across generators.
deriveHandleName :: [SResource] -> Text -> Text -> Text
deriveHandleName allRes parentName t =
    let roots = [ resourceName r | r <- allRes, isNothing (parent r) ]
        prefix = parentName <> "_"
        t' = if prefix `Text.isPrefixOf` t then Text.drop (Text.length prefix) t
             else stripRoots roots t
    in Text.toLower t'
  where
    stripRoots roots name =
        case find (\r -> (r <> "_") `Text.isPrefixOf` name) (sortOn (negate . Text.length) roots) of
            Just r -> Text.drop (Text.length r + 1) name
            Nothing ->
                case find (\r -> (r /= name && r `Text.isPrefixOf` name)) roots of
                    Just r  -> Text.drop (Text.length r) name
                    Nothing -> name

-- | Finds the root resource in the ownership hierarchy.
findRoot :: [SResource] -> SResource -> Text
findRoot allRes res = case getHierarchy allRes res of
    []    -> error "Apigen.Inference.findRoot: empty hierarchy"
    (x:_) -> x

-- | Retrieves the hierarchy of resource names from root to child.
getHierarchy :: [SResource] -> SResource -> [Text]
getHierarchy allRes res =
    case parent res of
        Just pName ->
            case find ((== pName) . resourceName) allRes of
                Just pRes -> getHierarchy allRes pRes ++ [resourceName res]
                Nothing   -> [pName, resourceName res]
        Nothing -> [resourceName res]

-- | Returns a list of valid C argument mappings following various Tox conventions.
getValidArgMappings :: [SResource] -> SResource -> SMethod -> [[CArgSource]]
getValidArgMappings allRes res meth =
    let role = methodRole meth
        semParams = inputs meth
        errType = methodErrorType meth
        cns = methodConstness meth
        resStrat = methodResultStrategy meth
        path = getHierarchy allRes res
        numParents = length path - 1

        semArgsList = expand semParams 0

        -- Identity components
        root = [PathObject 0 cns | numParents > 0]
        parents = [PathId i | (i, _) <- zip [0..] path, i > 0 && i < numParents]
        self = [PathId numParents]

        isIdRes = case resourceType res of
            ResId _ -> True
            _       -> False

        -- Standard actions/getters/setters Patterns
        basePatterns = concatMap (\semArgs -> case role of
            Constructor -> [root ++ semArgs, [ThisObject cns] ++ semArgs, semArgs]
            Destructor  -> [[ThisObject MutableThis]]
            RegistrarRole -> [[ThisObject MutableThis, SemanticArg 0] ++ [UserData | methodHasUserData meth]]
            StaticRole -> [semArgs, root ++ semArgs]
            _ ->
                let thisArgs = if isIdRes
                               then root ++ parents ++ self
                               else [ThisObject cns]

                    -- Pattern 1: Standard Prefix
                    p1 = thisArgs ++ semArgs

                    -- Pattern 2: Interleaved First Semantic Arg (e.g. Tox_Friend_Number)
                    p2 = case (thisArgs, semArgs) of
                        (r:restId, firstSem:restSem) ->
                            case firstSem of
                                SemanticArg _ -> [r, firstSem] ++ restId ++ restSem
                                _             -> []
                        _ -> []

                    -- Pattern 3: Collection (Parent Identity only)
                    p3 = if isIdRes && numParents > 0
                         then (root ++ parents) ++ semArgs
                         else []
                in filter (not . null) [p1, p2, p3]) semArgsList

        basePatternsWithOutput = concatMap (\p ->
            let out = output meth
                bufs = case out of
                    SFixedBytes n _  -> [[BufferPtr (Just n)]]
                    SFixedList _ n _ -> [[BufferPtr (Just n)]]
                    SBytes           -> [[BufferPtr Nothing]]
                    SList _          -> [[BufferPtr Nothing]]
                    SString          -> [[BufferPtr Nothing]]
                    _                -> []
            in p : [ p ++ b | b <- bufs ]
            ) basePatterns

        -- Apply UserData and ErrorPtr to all patterns
        withUserData = map (\p -> if methodHasUserData meth && UserData `notElem` p then p ++ [UserData] else p) basePatternsWithOutput
        withError = map (\p -> if isJust errType && resStrat /= ReturnIsErrorCode then p ++ [ErrorPtr] else p) withUserData

    in withError

-- | Helper to expand semantic parameters to C arguments (handling BufferSize)
expand :: [SParameter] -> Int -> [[CArgSource]]
expand [] _ = [[]]
expand (SParameter _ ty _ _ : ps) i =
    let rest = expand ps (i + 1)
        current = case ty of
            SBytes           -> [[SemanticArg i, BufferSize], [SemanticArg i]]
            SFixedBytes _ b  -> [[SemanticArg i] ++ [BufferSize | b]]
            SFixedList _ _ b -> [[SemanticArg i] ++ [BufferSize | b]]
            -- SString is NUL-terminated in this project
            _                -> [[SemanticArg i]]
    in [ c ++ r | c <- current, r <- rest ]

-- | Reconstructs a C function mapping from semantic metadata following Tox conventions.
inferCFunctionMapping :: [SResource] -> SResource -> SMethod -> CFunctionMapping
inferCFunctionMapping allRes res meth =
    let args = case getValidArgMappings allRes res meth of
            []    -> error $ "Apigen.Inference.inferCFunctionMapping: no valid mappings for " ++ Text.unpack (methodName meth)
            (x:_) -> x

        cRet = case methodResultStrategy meth of
            ReturnIsErrorCode -> case methodErrorType meth of
                Just e  -> SEnum e
                Nothing -> SVoid
            IgnoreReturn -> SBool
            ReturnIsValue -> if any isBufferPtr args then SBool else output meth
            ReturnIsResult -> if any isBufferPtr args then SBool else output meth

    in CFunctionMapping (methodName meth) args Nothing Nothing (methodErrorType meth) (inputs meth) cRet
  where
    isBufferPtr (BufferPtr _) = True
    isBufferPtr _             = False

-- | Resolves the resource name at a specific index in the hierarchy path.
resolvePathTarget :: [SResource] -> SResource -> Int -> Text
resolvePathTarget allRes res n =
    let path = getHierarchy allRes res
    in if n >= 0 && n < length path then path !! n else resourceName res

