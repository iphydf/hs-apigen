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
    , privateEnums
    , lengthOf
    , enumParam
    , failureReturn
    , secretArrays
    , arrayPrefixes
    , minimumSize
    , eventsVariant
    , handlesOfKind
    , registeredCallbacks
    , isLoopFunction
    , isRawPointer
      -- * Facts
    , factReport
    ) where

import qualified Apigen.Facts    as Facts
import           Apigen.Semantic
import           Data.List       (find, nub, sortOn)
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
    , kIdScopes       = [ (r, SKnown i (FromFact "file functions take the friend number first; a doc or type pairing them would say it"))
                        | (r, i) <- Facts.scopedIds ]
    , kIdEnders       = [ SKnown f (FromFact "a naming standard for functions ending their id")
                        | f <- Facts.idDestructors ]
    -- A root function taking another resource's id acts on it, the last
    -- id's if more: tox_file_control's friend and file are the file's.
    , kFunctionOwners = [ (methodName m, SKnown (resourceName o) FromStructure)
                        | r <- res, isRoot r, m <- methods r
                        , SResourceId i : _ <- [reverse [ paramType p | p <- inputs m, isId (paramType p) ]]
                        , Just o <- [find ((== ResId (SResourceId i)) . resourceType) res] ]
    , kPrivateEnums   = [ SKnown e (FromFact "parse tox_private.h")
                        | e <- Facts.privateEnums ]
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

-- | The id (field, type) a resource's own id is only unique within.
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

-- | A callback parameter's type, where it's better than C's.
privateEnums :: SemanticModel -> [(Text, [Text])]
privateEnums = map knownValue . kPrivateEnums . knowledge

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

-- | The resources of a kind.
handlesOfKind :: SemanticModel -> SResourceKind -> [SResource]
handlesOfKind model k = filter ((== k) . resourceKind model) (resources model)

-- | The callback types a resource's functions register, like the
-- settings' log callback.
registeredCallbacks :: SemanticModel -> SResource -> [SCallbackTypeModel]
registeredCallbacks model r = nub
    [ cb | m <- methods r, methodRole m == RegistrarRole, p <- inputs m, SCallback n <- [paramType p]
         , cb <- callbacks model, cbName cb == n || cbCName cb == n ]

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
    , section "Enums from unread headers" [ (fst v, s) | SKnown v s <- kPrivateEnums k ]
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
