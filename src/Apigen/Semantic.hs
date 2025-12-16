{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Semantic where

import           Apigen.SizeExpr (SizeExpr)
import           Apigen.Types    (Constness)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

data SType
    = SVoid
    | SBool
    | SInt Int           -- ^ Bit size (8, 16, 32, 64)
    | SUInt Int          -- ^ Bit size
    | SSizeT             -- ^ C size_t type
    | SString            -- ^ High-level UTF-8 string
    | SBytes             -- ^ High-level byte array
    | SFixedBytes SizeExpr Bool -- ^ Fixed-size byte array, flag for explicit size arg
    | SFixedList SType SizeExpr Bool -- ^ Fixed-size list of any type, flag for explicit size arg
    | SEnum Text         -- ^ Reference to an enum name
    | SHandle Text       -- ^ Opaque C handle
    | SCallback Text     -- ^ Function pointer / Callback
    | SResourceId Text   -- ^ Semantic ID of a resource (e.g., "FriendId")
    | SScalar Text SType -- ^ A typedef'd number, a unit: @Av_Bit_Rate@ over @SUInt 32@.
    | SList SType        -- ^ Homogeneous collection
    deriving (Show, Eq, Generic)

instance ToJSON SType
instance FromJSON SType

data CArgSource
    = ThisObject Constness -- ^ The primary instance pointer
    | PathObject Int Constness -- ^ Pointer to the N-th owning resource in hierarchy (0 = Root)
    | PathId Int           -- ^ The N-th identifier in hierarchy path
    | SemanticArg Int    -- ^ Maps to the N-th high-level argument
    | ErrorPtr           -- ^ Automatically handled output error pointer
    | BufferPtr (Maybe SizeExpr) -- ^ Output buffer (e.g., for getters), optional fixed size
    | BufferSize         -- ^ Length parameter (maps to length of SBytes/SString)
    | UserData           -- ^ Closure context pointer for callbacks
    | Constant Int       -- ^ Literal value required by the C API
    deriving (Show, Eq, Generic)

instance ToJSON CArgSource
instance FromJSON CArgSource

data SMethodRole
    = Constructor   -- ^ Creates a new resource instance
    | Destructor    -- ^ Destroys a resource instance
    | GetterRole    -- ^ Retrieves a property or state
    | SetterRole    -- ^ Modifies a property or state
    | ActionRole    -- ^ Performs a general operation
    | RegistrarRole -- ^ Registers a callback/event handler
    | StaticRole    -- ^ Global utility or constant provider
    | IterateRole   -- ^ Drives an event loop, flushing accumulated events to the binding
    deriving (Show, Eq, Generic)

instance ToJSON SMethodRole
instance FromJSON SMethodRole

data SMapping
    = StandardMapping -- ^ Follows the resource/role convention
    | CustomMapping CFunctionMapping -- ^ Irregular mapping requiring explicit detail
    deriving (Show, Eq, Generic)

instance ToJSON SMapping
instance FromJSON SMapping

data CFunctionMapping = CFunctionMapping
    { cFunctionName     :: Text
    , argMapping        :: [CArgSource]     -- ^ Positional mapping for C arguments
    , returnMapping     :: Maybe CArgSource   -- ^ How the C return maps to semantic result
    , cSizeFunctionName :: Maybe Text       -- ^ Function to call to get buffer size (for size-then-get)
    , cErrorType        :: Maybe Text       -- ^ The C error enum name
    , cSemParams        :: [SParameter]     -- ^ Semantic parameters for this C call
    , cReturnType       :: SType            -- ^ Original C return type
    , cArgNames         :: [Text]           -- ^ The C parameters' names, one per 'argMapping'
    } deriving (Show, Eq, Generic)

instance ToJSON CFunctionMapping
instance FromJSON CFunctionMapping

-- | How the C return value relates to error enums.
data SResultStrategy
    = IgnoreReturn          -- ^ High-level API ignores the C return value
    | ReturnIsValue         -- ^ C return value is the semantic result
    | ReturnIsErrorCode     -- ^ C return value IS the error code itself
    | ReturnIsResult        -- ^ C return value is the success payload of a Result<T, E>
    deriving (Show, Eq, Generic)

instance ToJSON SResultStrategy
instance FromJSON SResultStrategy

data SMethod = SMethod
    { methodName           :: Text
    , methodRole           :: SMethodRole       -- ^ Semantic intent
    , inputs               :: [SParameter]
    , output               :: SType
    , methodConstness      :: Constness         -- ^ Derived from 'ThisObject' constness
    , methodErrorType      :: Maybe Text        -- ^ Linked Error Enum for exceptions/results
    , methodResultStrategy :: SResultStrategy  -- ^ How to interpret return vs error
    , methodMapping        :: SMapping          -- ^ How to call this in C
    , methodHasUserData    :: Bool              -- ^ Whether the C function takes UserData
    , methodConstants      :: [Text]            -- ^ Related #defines
    , methodReturnNullable :: Bool              -- ^ C return type was `_Nullable`
    , methodFile           :: Text              -- ^ Header file declaring it
    } deriving (Show, Eq, Generic)

instance ToJSON SMethod
instance FromJSON SMethod

data SParameter = SParameter
    { paramName      :: Text
    , paramType      :: SType
    , paramConstness :: Constness
    , paramConstants :: [Text]            -- ^ Related #defines
    , paramNullable  :: Bool              -- ^ A NULL pointer means "none"
    } deriving (Show, Eq, Generic)

instance ToJSON SParameter
instance FromJSON SParameter

data SProperty = SProperty
    { propName      :: Text
    , propType      :: SType
    , propRead      :: Maybe Text        -- ^ Name of the getter method
    , propWrite     :: Maybe Text        -- ^ Name of the setter method
    , propSize      :: Maybe Text        -- ^ Name of the size method (if any/separate)
    , propErrorType :: Maybe Text
    , propConstants :: [Text]            -- ^ Related #defines
    } deriving (Show, Eq, Generic)

instance ToJSON SProperty
instance FromJSON SProperty

data SResourceType
    = ResHandle
    | ResId SType -- ^ The ID type
    deriving (Show, Eq, Generic)

instance ToJSON SResourceType
instance FromJSON SResourceType

data SResource = SResource
    { resourceName    :: Text
    , cName           :: Text             -- ^ Original C type name
    , resourceType    :: SResourceType    -- ^ Handle-based or ID-based
    , cPrefix         :: Text             -- ^ C function prefix (e.g., "resource_")
    , isRoot          :: Bool             -- ^ True if this is the root context object (e.g. Tox)
    , parent          :: Maybe Text       -- ^ Hierarchical owner
    , lifecycleParent :: Maybe Text       -- ^ Resource required for creation (RAII dependency)
    , resourceFile    :: Text             -- ^ Header file declaring this resource's type
    , properties      :: [SProperty]
    , methods         :: [SMethod]
    , traits          :: [SResourceTrait] -- ^ Resource capabilities
    , events          :: [SEvent]         -- ^ High-level callbacks
    } deriving (Show, Eq, Generic)

instance ToJSON SResource
instance FromJSON SResource

data SResourceTrait
    = Iterable { iterElement :: SType, iterSize :: Text, iterAccessor :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON SResourceTrait
instance FromJSON SResourceTrait

data SEvent = SEvent
    { eventName        :: Text
    , eventRegistrar   :: Text              -- ^ The C function setting its callback:
                                            -- @tox_callback_friend_name@.
    , eventParams      :: [SParameter]
    , cCallback        :: Text              -- ^ The C typedef name for the callback
    , eventHasUserData :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON SEvent
instance FromJSON SEvent

data SVariant = SVariant
    { variantName     :: Text
    , variantTypeEnum :: Text
    , variantMembers  :: [SVariantMember]
    } deriving (Show, Eq, Generic)

instance ToJSON SVariant
instance FromJSON SVariant

data SVariantMember = SVariantMember
    { memberName   :: Text
    , memberType   :: Text
    , memberGetter :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON SVariantMember
instance FromJSON SVariantMember

data SemanticModel = SemanticModel
    { enums        :: [SEnumModel]
    , constants    :: [SConstantModel]
    , idTypes      :: [SIdTypeModel]    -- ^ Mapping of semantic IDs to C types
    , scalarTypes  :: [SScalarTypeModel] -- ^ The other typedef'd numbers: units.
    , callbacks    :: [SCallbackTypeModel] -- ^ C Callback typedefs
    , resources    :: [SResource]
    , variants     :: [SVariant]
    , arrayTypes   :: [(Text, Text)]   -- ^ @(sizeConstant, semanticTypeName)@ for
                                       -- each @typedef uint8_t X[SIZE]@ array type.
    , commonPrefix :: Text             -- ^ Common prefix shared by resources/functions
    , diagnostics  :: [SDiagnostic]
    , knowledge    :: SKnowledge       -- ^ What the generators need to know, and why.
    , targets      :: STargets         -- ^ Where each binding's code goes.
    , context      :: SContext         -- ^ What headers read only to look names up in declare.
    } deriving (Show, Eq, Generic)

-- | The enums and callback typedefs of headers apigen reads without binding
-- them: @tox_private.h@'s, for the public bindings.
data SContext = SContext
    { contextEnums     :: [SEnumModel]
    , contextCallbacks :: [SCallbackTypeModel]
    } deriving (Show, Eq, Generic)

instance ToJSON SContext
instance FromJSON SContext

emptyContext :: SContext
emptyContext = SContext [] []

-- | The binding repos' layout: names the C API can't give, set by the
-- library's own layer.
data STargets = STargets
    { haskellModules         :: Text            -- ^ The safe API's root module: @Network.Tox.C@.
    , haskellRawModules      :: Text            -- ^ The raw bindings' root module: @FFI.Tox@.
    , haskellModuleDocs      :: [(Text, Text)]  -- ^ What a safe module is for, by its last word.
    , headerModuleNames      :: [(Text, Text)]  -- ^ A header's word where its name doesn't split:
                                           -- @toxencryptsave.h@ is @EncryptSave@.
    , includeDir             :: Text            -- ^ Where the headers are installed: @tox@.
    , pythonPackage          :: Text            -- ^ The Python package: @pytox@.
    , cppNamespace           :: Text            -- ^ The C++ namespace, and its headers' directory: @tox@.
    , dartPackage            :: Text            -- ^ The Dart package: @toxcore@.
    , dartLibrary            :: (Text, Text)    -- ^ The hand-written module and class loading the C library.
    , dartFfi                :: (Text, Text)    -- ^ ffigen's module and class.
    , dartKeyClasses         :: [(Text, Text)]  -- ^ The hand-written classes of array types, by model name.
    , rustHandWritten        :: ([Text], [Text])  -- ^ Functions the Rust crate's core and safe
                                             -- layers write themselves, though their
                                             -- shapes are standard.
    , rustWrappers           :: [Text]          -- ^ Functions the Rust resource wrappers leave to it.
    , rustPrivateEnums       :: [Text]          -- ^ Context enums the Rust crate's own code uses.
    , rustCorePaths          :: [(Text, Text)]  -- ^ How a hand-written handle struct reaches its core
                                           -- one, by resource: @inner.core@.
    , rustWrapperNotes       :: [(Text, [Text])] -- ^ What a resource wrapper's file says it leaves
                                            -- to the hand-written code.
    , jvmPackage             :: Text            -- ^ The JVM package: @im.tox.tox4j@.
    , jvmNativeLibrary       :: Text            -- ^ The JNI library: @tox4j-c@.
    , jvmSubsystems          :: [(Text, Text)]  -- ^ A subsystem's word where its header directory's
                                           -- doesn't say it: @crypto@ for @toxencryptsave@.
    , jvmHidden              :: [Text]          -- ^ Functions the JVM API leaves out.
    , jvmEventOnly           :: [(Text, [Text])] -- ^ Resources whose state comes only from events,
                                            -- and the getters they keep anyway.
    , jvmStandaloneConstants :: [Text]     -- ^ The root's constants a standalone subsystem has.
    , jvmCallbackDocs        :: [(Text, [Text])] -- ^ More doc for a callback.
    , jvmExtras              :: [(Text, SJvmExtras)] -- ^ Hand-written members, by subsystem word.
    } deriving (Show, Eq, Generic)

-- | What a JVM subsystem's hand-written code adds to its generated classes.
data SJvmExtras = SJvmExtras
    { jxInterfaceGenerics :: [Text]          -- ^ The interface's type parameters.
    , jxInterfaceMembers  :: [Text]
    , jxImplMembers       :: [Text]
    , jxConstants         :: [Text]
    , jxJavaNatives       :: [Text]          -- ^ Declarations in the JNI class.
    , jxJavaImports       :: [Text]
    , jxHeaderNatives     :: [Text]          -- ^ Their javah declarations.
    , jxNativeRefs        :: [(Text, Text)]  -- ^ Natives and the C functions they call.
    } deriving (Show, Eq, Generic)

instance ToJSON SJvmExtras
instance FromJSON SJvmExtras

noJvmExtras :: SJvmExtras
noJvmExtras = SJvmExtras [] [] [] [] [] [] [] []

instance ToJSON STargets
instance FromJSON STargets

emptyTargets :: STargets
emptyTargets = STargets "" "" [] [] "" "" "" "" ("", "") ("", "") [] ([], []) [] [] [] [] "" "" [] [] [] [] [] []

instance ToJSON SemanticModel
instance FromJSON SemanticModel

data SCallbackTypeModel = SCallbackTypeModel
    { cbName   :: Text
    , cbCName  :: Text
    , cbParams :: [SParameter]
    , cbFile   :: Text              -- ^ Header file declaring it
    } deriving (Show, Eq, Generic)

instance ToJSON SCallbackTypeModel
instance FromJSON SCallbackTypeModel

data SIdTypeModel = SIdTypeModel
    { idName  :: Text
    , idCName :: Text                  -- ^ Original C type name
    , idType  :: SType                  -- ^ Underlying integer type (e.g., SUInt 32)
    } deriving (Show, Eq, Generic)

instance ToJSON SIdTypeModel
instance FromJSON SIdTypeModel

-- | @typedef uint32_t Toxav_Bit_Rate;@
data SScalarTypeModel = SScalarTypeModel
    { scalarName  :: Text
    , scalarCName :: Text
    , scalarType  :: SType
    , scalarFile  :: Text   -- ^ The header declaring it.
    } deriving (Show, Eq, Generic)

instance ToJSON SScalarTypeModel
instance FromJSON SScalarTypeModel

data SEnumModel = SEnumModel
    { enumName         :: Text
    , enumSemanticName :: Text
    , enumMembers      :: [SEnumMember]
    , enumKind         :: SEnumKind
    , enumFile         :: Text      -- ^ Header file declaring it
    } deriving (Show, Eq, Generic)

instance ToJSON SEnumModel
instance FromJSON SEnumModel

data SEnumKind
    = ErrorEnum   -- ^ A function's error, with an OK member
    | ClosedEnum  -- ^ One of its members
    | OpenEnum    -- ^ Its members, or a value a client defines
    | FlagsEnum   -- ^ Any combination of its members' bits
    deriving (Show, Eq, Generic)

instance ToJSON SEnumKind
instance FromJSON SEnumKind

-- | One C enumerator, with its real C value (not its position: see
-- @Toxav_Friend_Call_State@'s bit flags).
data SEnumMember = SEnumMember
    { enumMemberCName :: Text -- ^ C enumerator name (e.g. TOX_FILE_KIND_DATA)
    , enumMemberName  :: Text -- ^ Semantic name with prefix stripped (e.g. DATA)
    , enumMemberValue :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SEnumMember
instance FromJSON SEnumMember

data SConstantModel = SConstantModel
    { constantName  :: Text
    , constantValue :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SConstantModel
instance FromJSON SConstantModel

data SDiagnostic = SDiagnostic
    { severity :: SIsError
    , location :: Maybe SLocation
    , message  :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON SDiagnostic
instance FromJSON SDiagnostic

data SIsError = Warning | Error deriving (Show, Eq, Generic, Ord)

instance ToJSON SIsError
instance FromJSON SIsError

data SLocation = SLocation
    { locFile   :: Text
    , locLine   :: Int
    , locColumn :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SLocation
instance FromJSON SLocation

-- | Where something the generators know comes from. Facts are what the
-- headers don't say yet: each says how a header could.
data SSource
    = FromTypedef       -- ^ A C type says it.
    | FromNaming        -- ^ A naming standard of the headers says it.
    | FromStructure     -- ^ The declarations' shape says it.
    | FromFact Text     -- ^ Only we know it, and how a header could say it.
    deriving (Show, Eq, Generic)

instance ToJSON SSource
instance FromJSON SSource

data SKnown a = SKnown
    { knownValue  :: a
    , knownSource :: SSource
    } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (SKnown a)
instance FromJSON a => FromJSON (SKnown a)

-- | What a resource is to a binding.
data SResourceKind
    = RootHandle        -- ^ The root, like Tox.
    | LibraryHandle     -- ^ Another library's handle, like ToxAV.
    | SettingsHandle    -- ^ Settings to fill in, like Tox_Options.
    | ViewList          -- ^ A list of views, like Tox_Events.
    | View              -- ^ A view of what C holds, like an event.
    | StandaloneHandle  -- ^ A handle a client makes on its own, like Tox_Pass_Key.
    | OtherHandle       -- ^ Any other handle.
    | IdResource        -- ^ Something an id names, like a friend.
    deriving (Show, Eq, Generic)

instance ToJSON SResourceKind
instance FromJSON SResourceKind

-- | A library: the word its names start with, and its handle.
data SLibrary = SLibrary
    { libWord   :: Text         -- ^ @tox@, @toxav@.
    , libHandle :: Maybe Text   -- ^ The resource its functions start with just its word.
    } deriving (Show, Eq, Generic)

instance ToJSON SLibrary
instance FromJSON SLibrary

-- | Everything Tox-specific the generators need, in one place. Most of it
-- is inferred from the headers' standards, the rest is facts.
data SKnowledge = SKnowledge
    { kLibraries      :: [SLibrary]                         -- ^ The root's first.
    , kResourceKinds  :: [(Text, SKnown SResourceKind)]
    , kBorrows        :: [(Text, SKnown Text)]              -- ^ A resource, and what it can't outlive.
    , kHandlerKeepers :: [SKnown Text]                      -- ^ Handles keeping a handler for their callbacks.
    , kHandedOutBy    :: [(Text, SKnown Text)]              -- ^ A resource only another gives out.
    , kVariantTags    :: [SKnown Text]                      -- ^ Enums saying which member of a variant a value is.
    , kIdScopes       :: [(Text, SKnown (Text, Text))]      -- ^ A resource, and the resource and id its own is only unique within.
    , kIdEnders       :: [SKnown Text]                      -- ^ Functions after which their id names nothing.
    , kFunctionOwners :: [(Text, SKnown Text)]              -- ^ Functions on the root that act on another resource.
    , kLengths        :: [((Text, Text), SKnown Text)]      -- ^ A function's C length parameter, and the parameter it's the length of.
    , kEnumParams     :: [((Text, Text), SKnown Text)]      -- ^ Integer parameters holding an enum's values.
    , kFailureReturns :: [(Text, SKnown Integer)]           -- ^ Functions returning this integer on failure.
    , kSecretArrays   :: [SKnown Text]                      -- ^ Array types holding secrets.
    , kArrayPrefixes  :: [(Text, SKnown Text)]              -- ^ An array type, and the array type it starts with.
    , kMinimumSizes   :: [((Text, Text), SKnown Text)]      -- ^ Inputs C reads at least this much of.
    , kLoopFunctions  :: [SKnown Text]                      -- ^ Functions driving a handle's event loop, which a binding calls itself.
    , kFrames         :: [SKnown (Text, [Text])]            -- ^ Frames: a name, and the parameters holding one.
    } deriving (Show, Eq, Generic)

instance ToJSON SKnowledge
instance FromJSON SKnowledge

emptyKnowledge :: SKnowledge
emptyKnowledge = SKnowledge [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
