{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | jvm-toxcore-c conventions the c-toxcore headers don't express (yet).
module Apigen.Language.Jvm.Conventions
    ( -- * Interface annotations & factories
      interfaceExtraMembers
    , interfaceGenericParams

      -- * Impl-class extras
    , implExtraMembers

      -- * Synthetic exceptions
    , syntheticExceptionFor
    , syntheticExceptionEnums

      -- * Error enums and options
    , errorEnumIsReachable
    , optionsWireFields

      -- * Hand-curated callback documentation

      -- * Exception class extras
    , exceptionExtraCodes

      -- * Constants extras
    , constantsExtraMembers

      -- * Sealed groupings (ToxOptions)
    , SealedGroup (..)
    , sealedGroups
    , lookupSealedGroup
    , sealedGroupAccess

      -- * JNI extras
    , cppExtraNativeRefs
    , jniExtraNatives
    , jniExtraJavaImports

      -- * Public-API filtering
    , isLifecycleConstructor
    , isHiddenFromPublicApi
    , interfaceAllowsStatics
    , interfaceSelfResource
    , hasNoJniShim

      -- * Constants
    , isCryptoConstant

    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Jvm.Model (Subsystem (..), avResourceNames,
                                            classPrefix, coreResourceNames,
                                            jniPrefix, native, standalonePrefix,
                                            subsystems)
import           Apigen.Language.Jvm.Names (camelCase, pascalCase)
import           Apigen.Model              (declaredElsewhere, returnsBorrowed)
import           Apigen.Namespace          (namespaces, resourceNamespace)
import           Apigen.Semantic
import qualified Data.List                 as List
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

--------------------------------------------------------------------------------
-- Interface annotations & factories
--------------------------------------------------------------------------------

-- | A subsystem's hand-written extras, by any of its classes' names.
extrasOf :: SemanticModel -> Text -> SJvmExtras
extrasOf model cls = maybe noJvmExtras (\s -> fromMaybe noJvmExtras (lookup (subWord s) (jvmExtras (targets model)))) (ownerOf model cls)

-- | The subsystem a class is of: @ToxCoreJni@ is the root's.
ownerOf :: SemanticModel -> Text -> Maybe Subsystem
ownerOf model cls = List.find (\s -> any (\suffix -> subClass s <> suffix == cls) ["", "Impl", "Jni", "Constants"]) (subsystems model)

interfaceExtraMembers :: SemanticModel -> Text -> [Text]
interfaceExtraMembers model = jxInterfaceMembers . extrasOf model

-- | Interface type parameters: each @ToxCrypto@ impl picks its @PassKey@.
interfaceGenericParams :: SemanticModel -> Text -> [Text]
interfaceGenericParams model = jxInterfaceGenerics . extrasOf model

-- | A handle's finalizer, and what the subsystem adds.
implExtraMembers :: SemanticModel -> Text -> [Text]
implExtraMembers model cls =
    [ "    protected fun finalize() {\n\
      \        runCatching {\n\
      \            " <> jni <> "." <> native model s Destructor <> "(instanceNumber)\n\
      \            " <> jni <> "." <> fin <> "(instanceNumber)\n\
      \        }\n\
      \    }"
    | Just s <- [ownerOf model cls], Just fin <- [finalizeNative model s], let jni = subClass s <> "Jni" ]
    ++ jxImplMembers (extrasOf model cls)

-- | Kill first (it's idempotent): @toxFinalize@ throws on a live instance,
-- and a finalizer's throw gets swallowed. A standalone handle has none.
finalizeNative :: SemanticModel -> Subsystem -> Maybe Text
finalizeNative model s
    | subKind s `elem` [RootHandle, LibraryHandle] =
        (<> "Finalize") <$> resourceNamespace (namespaces model) (subHandle s)
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Constants extras
--------------------------------------------------------------------------------

constantsExtraMembers :: SemanticModel -> Text -> [Text]
constantsExtraMembers model = jxConstants . extrasOf model

--------------------------------------------------------------------------------
-- Sealed groupings (ToxOptions)
--------------------------------------------------------------------------------

-- | C fields that become one Kotlin field of a sealed type: @proxy_type@,
-- @proxy_host@ and @proxy_port@ become @proxy: ProxyOptions.Type@.
data SealedGroup = SealedGroup
    { sealedGroupFieldName :: Text    -- ^ On @ToxOptions@.
    , sealedGroupClass     :: Text    -- ^ @ProxyOptions@.
    , sealedGroupTypeName  :: Text
    , sealedGroupDefault   :: Text
    , sealedGroupCFields   :: [Text]
    } deriving (Show, Eq)

-- | @Tox_Options@ fields with a common first word @X@, if there are 2 or
-- more and one is an enum @X_type@.
sealedGroups :: SemanticModel -> [SealedGroup]
sealedGroups model = case K.handlesOfKind model SettingsHandle of
    [] -> []
    options : _ ->
        [ SealedGroup
            { sealedGroupFieldName = camelCase prefix
            , sealedGroupClass     = pascalCase prefix <> "Options"
            , sealedGroupTypeName  = pascalCase prefix <> "Options.Type"
            , sealedGroupDefault   = pascalCase prefix <> "Options.None"
            , sealedGroupCFields   = map propName groupProps
            }
        | (prefix, groupProps) <- groupedByPrefix options
        , isQualifiedGroup prefix groupProps
        ]
  where

    -- Without the log callback and its user data.
    groupedByPrefix options =
        let visible = filter (visiblePropType . propType)
                            (properties options)
            firstToken p = case Text.splitOn "_" (propName p) of
                (t : _) -> t
                []      -> propName p
            keyOf p = (firstToken p, [p])
            merged = List.foldl' insertOrAppend [] (map keyOf visible)
            insertOrAppend acc (k, ps) =
                case List.lookup k acc of
                    Just existing -> (k, existing ++ ps) : filter ((/= k) . fst) acc
                    Nothing       -> acc ++ [(k, ps)]
        in merged

    visiblePropType (SHandle _)   = False
    visiblePropType (SCallback _) = False
    visiblePropType _             = True

    isQualifiedGroup prefix ps =
        length ps >= 2
        && any (\p -> propName p == prefix <> "_type"
                   && isEnumType (propType p)) ps

    isEnumType (SEnum _) = True
    isEnumType _         = False

lookupSealedGroup :: SemanticModel -> Text -> Maybe SealedGroup
lookupSealedGroup model pname =
    case filter (\g -> pname `elem` sealedGroupCFields g) (sealedGroups model) of
        (g : _) -> Just g
        []      -> Nothing

-- | A grouped C field on its group: @proxy.proxyHost@.
sealedGroupAccess :: SemanticModel -> Text -> Maybe Text
sealedGroupAccess model cField = (\g -> sealedGroupFieldName g <> "." <> camelCase cField) <$> lookupSealedGroup model cField

--------------------------------------------------------------------------------
-- JNI extras
--------------------------------------------------------------------------------

-- | The natives a JNI class has besides the generated ones: the finalizer
-- and what the subsystem adds by hand.
jniExtraNatives :: SemanticModel -> Text -> [SJvmNative]
jniExtraNatives model cls =
    [ SJvmNative fin "void" [("int", "instanceNumber")] Nothing | Just fin <- [finalizeOf model cls] ]
    ++ jxNatives (extrasOf model cls)

jniExtraJavaImports :: SemanticModel -> Text -> [Text]
jniExtraJavaImports model = jxJavaImports . extrasOf model

finalizeOf :: SemanticModel -> Text -> Maybe Text
finalizeOf model cls = ownerOf model cls >>= finalizeNative model

-- | @natives.h@ pairs no resource gives.
cppExtraNativeRefs :: SemanticModel -> Text -> [(Text, Text)]
cppExtraNativeRefs model = jxNativeRefs . extrasOf model

--------------------------------------------------------------------------------
-- Exception class extras
--------------------------------------------------------------------------------

exceptionExtraCodes :: SemanticModel -> Text -> [(Text, Maybe Text)]
exceptionExtraCodes model errEnum =
    [ (codeMember, Just (codeKdoc ownerName))
    | (methodNm, synth) <- syntheticExceptionMethods model
    , synth == errEnum
    , let ownerName = ownerResourceName model methodNm
    , let codeMember = Text.toUpper ownerName <> "_NOT_FOUND"
    ]
  where
    codeKdoc r = "The " <> Text.toLower r
              <> " number passed did not designate a valid "
              <> Text.toLower r <> "."

--------------------------------------------------------------------------------
-- Synthetic exceptions
--------------------------------------------------------------------------------

-- | Exception for a public function that fills a byte buffer and returns
-- @bool@ with no @Tox_Err_*@, so failing throws instead of tripping a value
-- class's size check.
syntheticExceptionFor :: SemanticModel -> Text -> Maybe Text
syntheticExceptionFor model mn =
    case [ enumNm | (m, enumNm) <- syntheticExceptionMethods model, m == mn ] of
        (e : _) -> Just e
        []      -> Nothing

syntheticExceptionEnums :: SemanticModel -> [SEnumModel]
syntheticExceptionEnums model =
    [ SEnumModel
        { enumName         = enumNm
        , enumSemanticName = stripToxErrPrefix enumNm
        , enumMembers      = []
        , enumKind         = ErrorEnum
        , enumFile         = file
        }
    | (mn, enumNm) <- syntheticExceptionMethods model
    , file <- take 1
        [ resourceFile r
        | r <- resources model
        , any ((== mn) . methodName) (methods r)
        ]
    ]
  where
    stripToxErrPrefix n = case Text.stripPrefix (classPrefix model <> "_") n of
        Just rest -> rest
        Nothing   -> n

syntheticExceptionMethods :: SemanticModel -> [(Text, Text)]
syntheticExceptionMethods model =
    [ (methodName m, deriveEnumName (methodName m))
    | r <- resources model
    , resourceName r `elem` publicApiResources model
    , m <- methods r
    , needsSyntheticException m
    ]
  where
    needsSyntheticException m =
        methodErrorType m == Nothing
        && methodRole m /= StaticRole
        && cReturnTypeOf m == Just SBool
        && isByteOutput (output m)

    cReturnTypeOf m = case methodMapping m of
        CustomMapping cmap -> Just (cReturnType cmap)
        _                  -> Nothing

    isByteOutput (SFixedBytes _ _) = True
    isByteOutput SBytes            = True
    isByteOutput _                 = False

    -- @tox_hash@'s is @Tox_Err_Hash@.
    errPrefix = classPrefix model <> "_Err_"
    deriveEnumName mn = case Text.stripPrefix (K.rootLibrary model <> "_") mn of
        Just rest -> errPrefix <> Text.intercalate "_" (map pascalCase (Text.splitOn "_" rest))
        Nothing   -> errPrefix <> pascalCase mn

ownerResourceName :: SemanticModel -> Text -> Text
ownerResourceName model mn =
    case [ r | r <- resources model
             , resourceName r `elem` publicApiResources model
             , m <- methods r
             , methodName m == mn
         ] of
        (r : _) -> resourceName r
        []      -> classPrefix model

--------------------------------------------------------------------------------
-- Error enums and options
--------------------------------------------------------------------------------

-- | Whether a public method throws this error enum. The C++ @HANDLE@s use it
-- too (a @HANDLE@ for a missing Kotlin class fails at runtime).
errorEnumIsReachable :: SemanticModel -> Text -> Bool
errorEnumIsReachable model name = name `elem`
    [ ref
    | r <- resources model
    , m <- methods r
    , not (isHiddenFromPublicApi model (methodName m))
    , Just ref <- [methodErrorType m]
    ]

-- | @Tox_Options@ fields @toxNew@ takes: the Kotlin call, the Java native and
-- the C++ setters. Not C's locking, which the C++ always turns on.
optionsWireFields :: SemanticModel -> [SProperty]
optionsWireFields model =
    case K.handlesOfKind model SettingsHandle of
        [] -> []
        options : _ ->
            [ p
            | p <- properties options
            , visible (propType p)
            , p `notElem` K.lockingOptions model
            ]
  where
    visible (SHandle _)   = False
    visible (SCallback _) = False
    visible _             = True

--------------------------------------------------------------------------------
-- Public-API filtering
--------------------------------------------------------------------------------

-- | The resources of the public API: the core and the AV ones. Not the
-- settings (Kotlin has @ToxOptions@) or the standalone handles.
publicApiResources :: SemanticModel -> [Text]
publicApiResources model = coreResourceNames model ++ avResourceNames model

-- | Interfaces with static functions. Not @interfaceSelfResource == Nothing@,
-- as that's also true for unknown names.
interfaceAllowsStatics :: SemanticModel -> Text -> Bool
interfaceAllowsStatics model iface = any ((== StandaloneHandle) . subKind) (ownerOf model iface)

-- | Resource an interface stands for, whose handle its methods don't take.
interfaceSelfResource :: SemanticModel -> Text -> Maybe Text
interfaceSelfResource model iface = listToMaybe
    [ resourceName (subHandle s) | s <- subsystems model, subClass s == iface, subKind s `elem` [RootHandle, LibraryHandle] ]

-- | The Impl classes' own constructors: the root's and the other
-- libraries' handles'. Not a standalone one's, like @tox_pass_key_derive@.
isLifecycleConstructor :: SemanticModel -> Text -> Bool
isLifecycleConstructor model n = n `elem`
    [ methodName m | r <- resources model
    , K.resourceKind model r `elem` [RootHandle, LibraryHandle]
    , m <- methods r, methodRole m == Constructor ]

-- | No JNI native: toxav.h's old group calls (they work on a Tox),
-- @toxav_get_tox@ (a Tox the ToxAV doesn't own), and the separate audio
-- and video loops.
hasNoJniShim :: SemanticModel -> SMethod -> Bool
hasNoJniShim model m = any shimless [ r | r <- resources model, m `elem` methods r ]
  where
    shimless r = declaredElsewhere r m || returnsBorrowed r m || partLoop r
    partLoop r =
        any (`Text.isSuffixOf` methodName m) ["_iterate", "_iteration_interval"]
        && methodName m `notElem` map (cPrefix r <>) ["iterate", "iteration_interval"]

isHiddenFromPublicApi :: SemanticModel -> Text -> Bool
isHiddenFromPublicApi model n =
    n `elem` jvmHidden (targets model)
    || setsUpEvents
    || returnsOthers
    || isOptionsHidden model n
    || isFriendPollingGetter model n
    || isPairedSizeGetter model n
    || isCallbackSetterOnOptionsHandle model n
  where
    -- The root's set-up of its list of events: @tox_events_init@.
    setsUpEvents = or
        [ cPrefix v `Text.isPrefixOf` n
        | r <- resources model, isRoot r, m <- methods r, methodName m == n
        , v <- K.handlesOfKind model ViewList ]
    -- A handle the caller doesn't own: @toxav_get_tox@.
    returnsOthers = or
        [ True | r <- resources model, m <- methods r, methodName m == n, returnsBorrowed r m
        , SHandle h <- [output m]
        , o <- resources model, resourceName o == h, K.resourceKind model o `elem` [RootHandle, LibraryHandle] ]

-- | Kotlin has @ToxOptions@, which @toxNew@ gets as its fields: the
-- settings' functions are hidden, apart from setting a property.
isOptionsHidden :: SemanticModel -> Text -> Bool
isOptionsHidden model n = or
    [ n `notElem` [ w | p <- properties r, Just w <- [propWrite p] ]
    | r <- K.handlesOfKind model SettingsHandle, m <- methods r, methodName m == n ]

-- | A getter of a resource whose state comes from events, with an event of
-- its own: @tox_friend_get_name@ and @tox_friend_name_cb@.
isFriendPollingGetter :: SemanticModel -> Text -> Bool
isFriendPollingGetter model methodN = or
    [ True
    | (resName, kept) <- jvmEventOnly (targets model)
    , r <- resources model, resourceName r == resName
    , Just stateName <- [Text.stripPrefix (cPrefix r <> "get_") methodN]
    , stateName `notElem` kept
    , cPrefix r <> stateName <> "_cb" `elem` map cbCName (callbacks model) ]

isPairedSizeGetter :: SemanticModel -> Text -> Bool
isPairedSizeGetter model methodN =
    case Text.stripSuffix "_size" methodN of
        Just base
            -> base `elem` allMethodNames
        Nothing
            -> False
  where
    allMethodNames =
        [ methodName m
        | r <- resources model
        , m <- methods r
        ]

-- | Setters for options @ToxOptions@ has no field for (@log_user_data@).
isCallbackSetterOnOptionsHandle :: SemanticModel -> Text -> Bool
isCallbackSetterOnOptionsHandle model methodN =
    case Text.stripPrefix setPrefix methodN of
        Just propRaw ->
            case [ propType p
                 | r <- resources model
                 , K.isSettings model r
                 , p <- properties r
                 , propName p == propRaw
                 ] of
                (SHandle _   : _) -> True
                (SCallback _ : _) -> True
                _                 -> False
        Nothing -> False
  where
    setPrefix = maybe "" ((<> "set_") . cPrefix) (listToMaybe (K.handlesOfKind model SettingsHandle))


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Constants in @ToxCryptoConstants@: toxencryptsave.h's, and the hash
-- length.
isCryptoConstant :: SemanticModel -> Text -> Bool
isCryptoConstant model n =
    Text.toUpper (standalonePrefix model) `Text.isPrefixOf` n || n `elem` jvmStandaloneConstants (targets model)
