{-# LANGUAGE OverloadedStrings #-}
-- | Everything Tox-specific the generators need, worked out once for all of
-- them. Most of it follows from the headers' standards. The rest is facts
-- the headers don't say yet, each with how a header could.
--
-- Generators ask this module, not the C names.
module Apigen.Knowledge
    ( infer
      -- * Libraries
    , libraryWords
    , rootLibrary
    , libraryHandles
      -- * Resources
    , resourceKind
    , isView
    , isViewList
    , isSettings
    , keepsHandler
    , borrowed
    , handedOutBy
    , variantTags
    , idScope
      -- * Functions
    , endsId
    , functionOwners
    , namedFor
    , lengthOf
    , enumParam
    , failureReturn
    , secretArrays
    , arrayPrefixes
    , minimumSize
    , eventsVariant
    , handlesOfKind
    , rootEvent
    , rootSettings
    , registeredCallbacks
    , isLoopFunction
    , isRawPointer
    , alwaysOnOptions
    , frames
    , frameIn
      -- * Facts
    , factReport
    ) where

import qualified Apigen.Facts    as Facts
import           Apigen.Semantic
import           Apigen.SizeExpr (sizeExprVars)
import           Control.Monad   (guard)
import           Data.List       (find, isPrefixOf, nub, sortOn, tails)
import           Data.Maybe      (isJust, isNothing, listToMaybe, mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text

-- | The model's knowledge, from the model and the facts.
infer :: SemanticModel -> SKnowledge
infer model = SKnowledge
    { kLibraries      = libs
    , kResourceKinds  = [ (resourceName r, kindOf r) | r <- res ]
    , kBorrows        = [ (resourceName r, SKnown p FromStructure)
                        | r <- res, resourceType r == ResHandle, not (isRoot r)
                        , any ((== Constructor) . methodRole) (methods r)
                        , Just p <- [parent r] ]
    , kHandlerKeepers = [ SKnown (resourceName r) FromStructure
                        | r <- res, resourceType r == ResHandle, not (isRoot r), not (null (events r)) ]
    , kHandedOutBy    = nub $
        [ (memberType m, SKnown (variantName v) FromStructure) | v <- variants model, m <- variantMembers v ]
        ++ [ (e, SKnown (resourceName c) FromStructure) | c <- res, Iterable (SHandle e) _ _ <- traits c ]
    , kVariantTags    = [ SKnown (enumName e) FromStructure
                        | v <- variants model, Just e <- [findEnum (variantTypeEnum v)] ]
    , kIdScopes       = [ (resourceName r, SKnown s FromStructure) | r <- res, Just s <- [idScopeOf r] ]
    , kIdEnders       = [ SKnown f (FromFact "a naming standard for functions ending their id")
                        | f <- Facts.idDestructors ]
    -- A root function taking another resource's id acts on it, the last
    -- id's if more: tox_file_control's friend and file are the file's.
    , kFunctionOwners = [ (methodName m, SKnown (resourceName o) FromStructure)
                        | r <- res, isRoot r, m <- methods r
                        , SResourceId i : _ <- [reverse [ paramType p | p <- inputs m, isId (paramType p) ]]
                        , Just o <- [find ((== ResId (SResourceId i)) . resourceType) res] ]
    , kLengths        = concatMap lengths [ cm | r <- res, m <- methods r, CustomMapping cm <- [methodMapping m] ]
    , kEnumParams     = [ (k, SKnown e (FromFact "a typedef of the integer named for the enum"))
                        | (k, e) <- Facts.enumValuedParams ]
    , kFailureReturns = [ (f, SKnown v (FromFact "an error enum parameter"))
                        | (f, v) <- Facts.failureReturns ]
    , kSecretArrays   = [ SKnown a (FromFact "a naming standard or qualifier for secrets")
                        | a <- Facts.secretArrays ]
    , kArrayPrefixes  = [ (a, SKnown p (FromFact "a typedef of the array as its parts"))
                        | (a, p) <- Facts.arrayPrefixes ]
    , kMinimumSizes   = [ (k, SKnown v (FromFact "a minimum-size qualifier on the array"))
                        | (k, v) <- Facts.minimumSizes ]
    -- Iterating, how long to wait before it, and the root's setup of its
    -- list of views (tox_events_init).
    , kLoopFunctions  = nub $
        [ SKnown f FromNaming | f <- functionNames
        , any (`Text.isSuffixOf` f) ["_iterate", "_iteration_interval"] ]
        ++ [ SKnown (methodName m) FromStructure
           | r <- res, isRoot r, m <- methods r
           , Just o <- [find (\o -> cPrefix o `Text.isPrefixOf` methodName m && o /= r) res]
           , knownValue (kindOf o) == ViewList ]
    , kFrames         = [ SKnown f FromStructure | f <- inferFrames (map libWord libs) model ]
    }
  where
    res = resources model
    findEnum n = find (\e -> enumSemanticName e == n || enumName e == n) (enums model)

    libs = [ SLibrary w (resourceName <$> find ((== w <> "_") . cPrefix) res) | w <- words' ]
    words' = sortOn (/= root) . nub $ mapMaybe firstWord functionNames
    root = maybe "" (Text.toLower . Text.dropWhileEnd (== '_') . cPrefix) (find isRoot res)
    functionNames = [ methodName m | r <- res, m <- methods r ] ++ map cbCName (callbacks model)
    firstWord n = case Text.breakOn "_" n of
        (w, rest) | not (Text.null w), not (Text.null rest) -> Just (Text.toLower w)
        _ -> Nothing

    isId (SResourceId _) = True
    isId _               = False

    -- A root child's id is only unique within another's if every parameter
    -- list with it has the other's id just before: a file number follows its
    -- friend's.
    idScopeOf r = do
        ResId (SResourceId i) <- Just (resourceType r)
        guard (parent r == (resourceName <$> find isRoot res))
        let before ps = listToMaybe [ paramType a | (a, b) <- zip ps (drop 1 ps), paramType b == SResourceId i ]
            holding = [ ps | ps <- paramLists, SResourceId i `elem` map paramType ps ]
        [Just (SResourceId j)] <- Just (nub (map before holding))
        owner <- find (\o -> resourceType o == ResId (SResourceId j) && parent o == parent r) res
        return (resourceName owner, j)
    paramLists = [ inputs m | r <- res, m <- methods r ] ++ map cbParams (callbacks model)

    views = [ memberType m | v <- variants model, m <- variantMembers v ] ++ map variantName (variants model)
    kindOf r
        | isRoot r = SKnown RootHandle FromStructure
        | ResId _ <- resourceType r = SKnown IdResource FromStructure
        | resourceName r `elem` views = SKnown View FromStructure
        | or [ e `elem` views | Iterable (SHandle e) _ _ <- traits r ] = SKnown ViewList FromStructure
        | any ((== Just (resourceName r)) . libHandle) libs = SKnown LibraryHandle FromNaming
        | isNothing (parent r)
        , any ((`elem` [Constructor, Destructor]) . methodRole) (methods r)
        , any (isJust . propWrite) (properties r) = SKnown SettingsHandle FromStructure
        | isNothing (parent r)
        , any ((== Constructor) . methodRole) (methods r) = SKnown StandaloneHandle FromStructure
        | otherwise = SKnown OtherHandle FromStructure

    -- A length right after the buffer it's the length of.
    lengths cm =
        [ ((cFunctionName cm, len), SKnown (paramName p) FromStructure)
        | ((SemanticArg k, _), (BufferSize, len)) <- zip args (drop 1 args)
        , Just p <- [listToMaybe (drop k (cSemParams cm))] ]
      where
        args = zip (argMapping cm) (cArgNames cm)

-- | The libraries' words, lower case, the root's first: @tox@, @toxav@.
libraryWords :: SemanticModel -> [Text]
libraryWords = map libWord . kLibraries . knowledge

-- | The root's library word.
rootLibrary :: SemanticModel -> Text
rootLibrary = maybe "" libWord . listToMaybe . kLibraries . knowledge

-- | The handles of the libraries other than the root's.
libraryHandles :: SemanticModel -> [SResource]
libraryHandles model =
    [ r | l <- drop 1 (kLibraries (knowledge model)), Just h <- [libHandle l]
        , Just r <- [find ((== h) . resourceName) (resources model)] ]

resourceKind :: SemanticModel -> SResource -> SResourceKind
resourceKind model r = maybe OtherHandle knownValue (lookup (resourceName r) (kResourceKinds (knowledge model)))

isView, isViewList, isSettings :: SemanticModel -> SResource -> Bool
isView model r = resourceKind model r == View
isViewList model r = resourceKind model r == ViewList
isSettings model r = resourceKind model r == SettingsHandle

-- | Whether a handle keeps a handler for its callbacks, like ToxAV.
keepsHandler :: SemanticModel -> SResource -> Bool
keepsHandler model r = resourceName r `elem` map knownValue (kHandlerKeepers (knowledge model))

-- | What a handle can't outlive: the parent it's made from.
borrowed :: SemanticModel -> SResource -> Maybe Text
borrowed model r = knownValue <$> lookup (resourceName r) (kBorrows (knowledge model))

-- | The resource giving this one out: a view's variant or list.
handedOutBy :: SemanticModel -> Text -> Maybe Text
handedOutBy model n = knownValue <$> lookup n (kHandedOutBy (knowledge model))

-- | The enums saying which member of a variant a value is.
variantTags :: SemanticModel -> [Text]
variantTags = map knownValue . kVariantTags . knowledge

-- | The resource and id a resource's own id is only unique within: a
-- @File@'s, its @Friend@'s @Friend_Number@.
idScope :: SemanticModel -> Text -> Maybe (Text, Text)
idScope model n = knownValue <$> lookup n (kIdScopes (knowledge model))

-- | Whether a function's id names nothing after it.
endsId :: SemanticModel -> Text -> Bool
endsId model f = f `elem` map knownValue (kIdEnders (knowledge model))

-- | Functions the model has on the root that act on another resource, as
-- @(resource, function)@.
functionOwners :: SemanticModel -> [(Text, Text)]
functionOwners model = [ (knownValue r, f) | (f, r) <- kFunctionOwners (knowledge model) ]

-- | The resource a root function is named for, by its prefix: @tox_file_@
-- is the file's.
namedFor :: SemanticModel -> Text -> Maybe Text
namedFor model f = resourceName <$> find ((`Text.isPrefixOf` f) . cPrefix) others
  where
    others = sortOn (negate . Text.length . cPrefix) [ r | r <- resources model, not (isRoot r) ]

-- | The parameter a function's C length parameter is the length of.
lengthOf :: SemanticModel -> Text -> Text -> Maybe Text
lengthOf model f len = knownValue <$> lookup (f, len) (kLengths (knowledge model))

enumParam :: SemanticModel -> (Text, Text) -> Maybe Text
enumParam model k = knownValue <$> lookup k (kEnumParams (knowledge model))

failureReturn :: SemanticModel -> Text -> Maybe Integer
failureReturn model f = knownValue <$> lookup f (kFailureReturns (knowledge model))

secretArrays :: SemanticModel -> [Text]
secretArrays = map knownValue . kSecretArrays . knowledge

arrayPrefixes :: SemanticModel -> [(Text, Text)]
arrayPrefixes model = [ (a, knownValue p) | (a, p) <- kArrayPrefixes (knowledge model) ]

minimumSize :: SemanticModel -> (Text, Text) -> Maybe Text
minimumSize model k = knownValue <$> lookup k (kMinimumSizes (knowledge model))

-- | The variant of views, like the events.
eventsVariant :: SemanticModel -> Maybe SVariant
eventsVariant model = find isViews (variants model)
  where
    isViews v = any (\r -> resourceName r == variantName v && isView model r) (resources model)

-- | The root's event of this name: @friend_name@.
rootEvent :: SemanticModel -> Text -> Maybe SEvent
rootEvent model name = listToMaybe [ e | r <- handlesOfKind model RootHandle, e <- events r, eventName e == name ]

-- | The resources of a kind.
handlesOfKind :: SemanticModel -> SResourceKind -> [SResource]
handlesOfKind model k = filter ((== k) . resourceKind model) (resources model)

-- | The settings the root's constructor takes (there may be others).
rootSettings :: SemanticModel -> Maybe SResource
rootSettings model = listToMaybe
    [ s | root <- filter isRoot (resources model), m <- methods root, methodRole m == Constructor
        , p <- inputs m, s <- handlesOfKind model SettingsHandle, paramType p == SHandle (resourceName s) ]

-- | The callback types a resource's functions register, like the
-- settings' log callback.
registeredCallbacks :: SemanticModel -> SResource -> [SCallbackTypeModel]
registeredCallbacks model r = nub
    [ cb | m <- methods r, methodRole m == RegistrarRole, p <- inputs m, SCallback n <- [paramType p]
         , cb <- callbacks model, cbName cb == n || cbCName cb == n ]

-- | The settings' options every binding turns on.
alwaysOnOptions :: SemanticModel -> [SProperty]
alwaysOnOptions model =
    [ p | r <- handlesOfKind model SettingsHandle, p <- properties r, propName p `elem` Facts.alwaysOnOptions ]

-- | Whether a function drives a handle's event loop, so a binding calls it
-- itself.
isLoopFunction :: SemanticModel -> Text -> Bool
isLoopFunction model f = f `elem` map knownValue (kLoopFunctions (knowledge model))

-- | A C callback or a @void *@: no binding can type it.
isRawPointer :: SType -> Bool
isRawPointer (SCallback _)    = True
isRawPointer (SHandle "void") = True
isRawPointer _                = False

-- | The facts: what the headers should say, and how they could.
factReport :: SemanticModel -> Text
factReport model = Text.unlines $ concat
    [ section "Id scopes" [ (r <> ": " <> f <> " " <> t, s) | (r, SKnown (f, t) s) <- kIdScopes k ]
    , section "Functions ending their id" [ (v, s) | SKnown v s <- kIdEnders k ]
    , section "Functions on the root acting on another resource" [ (f <> " acts on " <> v, s) | (f, SKnown v s) <- kFunctionOwners k ]
    , section "Enum-valued integers" [ (f <> " " <> p <> ": " <> v, s) | ((f, p), SKnown v s) <- kEnumParams k ]
    , section "Integer failure returns" [ (f <> ": " <> tshow v, s) | (f, SKnown v s) <- kFailureReturns k ]
    , section "Secret arrays" [ (v, s) | SKnown v s <- kSecretArrays k ]
    , section "Array prefixes" [ (a <> " starts with " <> v, s) | (a, SKnown v s) <- kArrayPrefixes k ]
    , section "Minimum sizes" [ (f <> " " <> p <> ": " <> v, s) | ((f, p), SKnown v s) <- kMinimumSizes k ]
    ]
  where
    k = knowledge model
    tshow :: Show a => a -> Text
    tshow = Text.pack . show
    section title entries = case [ (e, how) | (e, FromFact how) <- entries ] of
        []  -> []
        fs  -> ("# " <> title) : [ "- " <> e <> " (" <> how <> ")" | (e, how) <- fs ] ++ [""]

-- | Frames: runs of parameters a function and a callback share, holding an
-- array that the run's numbers size, as audio or video frames go out and
-- come in. Named for the words the two names share: @toxav_audio_send_frame@
-- and @toxav_audio_receive_frame_cb@ hold an @Audio_Frame@.
inferFrames :: [Text] -> SemanticModel -> [(Text, [Text])]
inferFrames libWords model =
    [ (name, run)
    | run <- nub (map fst candidates)
    , let names = [ n | (r, n) <- candidates, r == run ]
    , name : _ <- [sortOn (negate . length . Text.splitOn "_") names]
    ]
  where
    candidates =
        [ (run, Text.intercalate "_" (map capital words'))
        | r <- resources model, m <- methods r
        , c <- callbacks model
        , Just run <- [frameRun (inputs m) (cbParams c)]
        , let words' = commonWords (nameWords (methodName m)) (nameWords (cbCName c))
        , not (null words')
        ]
    nameWords n = [ w | w <- Text.splitOn "_" (Text.toLower n), w `notElem` libWords, w /= "cb" ]
    capital w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

-- | The longest run both have, from the first array or number its arrays
-- are sized by.
frameRun :: [SParameter] -> [SParameter] -> Maybe [Text]
frameRun ps qs = listToMaybe
    [ drop start run
    | run <- sortOn (negate . length) (commonRuns (map paramName ps) (map paramName qs))
    , let sized = [ (paramName p, sizeExprVars s) | p <- ps, paramName p `elem` run, Just s <- [sizeOf (paramType p)] ]
    , not (null sized)
    , all (\(_, vs) -> not (null vs) && all (`elem` run) vs) sized
    , let used = map fst sized ++ concatMap snd sized
    , start : _ <- [[ i | (i, n) <- zip [0 :: Int ..] run, n `elem` used ]]
    , length (drop start run) >= 2
    ]
  where
    sizeOf t = case t of
        SFixedList _ s _ -> Just s
        SFixedBytes s _  -> Just s
        _                -> Nothing

-- | Every run of two or more both lists have, at any place.
commonRuns :: Eq a => [a] -> [a] -> [[a]]
commonRuns xs ys = nub
    [ run
    | xt <- tails xs, yt <- tails ys
    , let run = map fst (takeWhile (uncurry (==)) (zip xt yt))
    , length run >= 2
    ]

-- | The words both have, in order.
commonWords :: [Text] -> [Text] -> [Text]
commonWords (x : xs) ys
    | x `elem` ys = x : commonWords xs (drop 1 (dropWhile (/= x) ys))
    | otherwise   = commonWords xs ys
commonWords [] _ = []

-- | The frames, by name: @Audio_Frame@ and its parameters.
frames :: SemanticModel -> [(Text, [Text])]
frames = map knownValue . kFrames . knowledge

-- | A frame these parameters hold, by name, where its run starts, and how
-- many parameters it takes with the numbers its arrays' sizes use after it
-- (a received video frame's strides).
frameIn :: SemanticModel -> [SParameter] -> Maybe (Text, Int, Int)
frameIn model ps = listToMaybe
    [ (name, i, length run + length extra)
    | (name, run) <- frames model
    , (i, rest) <- zip [0 ..] (tails ps)
    , run `isPrefixOf` map paramName rest
    , let after = drop (length run) rest
          sizeVars = concat [ sizeExprVars s | p <- take (length run) rest, Just s <- [sizeOf (paramType p)] ]
          extra = takeWhile ((`elem` sizeVars) . paramName) after
    ]
  where
    sizeOf t = case t of
        SFixedList _ s _ -> Just s
        SFixedBytes s _  -> Just s
        _                -> Nothing
