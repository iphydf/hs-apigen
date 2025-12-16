{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Semantic where

import           Apigen.Types (Constness)
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data SType
    = SVoid
    | SBool
    | SInt Int           -- ^ Bit size (8, 16, 32, 64)
    | SUInt Int          -- ^ Bit size
    | SSizeT             -- ^ C size_t type
    | SString            -- ^ High-level UTF-8 string
    | SBytes             -- ^ High-level byte array
    | SFixedBytes Text Bool -- ^ Fixed-size byte array, flag for explicit size arg
    | SFixedList SType Text Bool -- ^ Fixed-size list of any type, flag for explicit size arg
    | SEnum Text         -- ^ Reference to an enum name
    | SHandle Text       -- ^ Opaque C handle
    | SCallback Text     -- ^ Function pointer / Callback
    | SResourceId Text   -- ^ Semantic ID of a resource (e.g., "FriendId")
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
    | BufferPtr (Maybe Text) -- ^ Output buffer (e.g., for getters), optional fixed size
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
    , callbacks    :: [SCallbackTypeModel] -- ^ C Callback typedefs
    , resources    :: [SResource]
    , variants     :: [SVariant]
    , arrayTypes   :: [(Text, Text)]   -- ^ @(sizeConstant, semanticTypeName)@ for
                                       -- each @typedef uint8_t X[SIZE]@ array type.
    , commonPrefix :: Text             -- ^ Common prefix shared by resources/functions
    , diagnostics  :: [SDiagnostic]
    , knowledge    :: SKnowledge       -- ^ What the generators need to know, and why.
    } deriving (Show, Eq, Generic)

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
    , kIdScopes       :: [(Text, SKnown (Text, Text))]      -- ^ A resource, and the id (field, type) its own is only unique within.
    , kIdEnders       :: [SKnown Text]                      -- ^ Functions after which their id names nothing.
    , kFunctionOwners :: [(Text, SKnown Text)]              -- ^ Functions on the root that act on another resource.
    , kPrivateEnums   :: [SKnown (Text, [Text])]            -- ^ Enums from a header we don't read, with their members.
    , kLengths        :: [((Text, Text), SKnown Text)]      -- ^ A function's C length parameter, and the parameter it's the length of.
    , kEnumParams     :: [((Text, Text), SKnown Text)]      -- ^ Integer parameters holding an enum's values.
    , kFailureReturns :: [(Text, SKnown Integer)]           -- ^ Functions returning this integer on failure.
    , kSecretArrays   :: [SKnown Text]                      -- ^ Array types holding secrets.
    , kArrayPrefixes  :: [(Text, SKnown Text)]              -- ^ An array type, and the array type it starts with.
    , kMinimumSizes   :: [((Text, Text), SKnown Text)]      -- ^ Inputs C reads at least this much of.
    , kLoopFunctions  :: [SKnown Text]                      -- ^ Functions driving a handle's event loop, which a binding calls itself.
    } deriving (Show, Eq, Generic)

instance ToJSON SKnowledge
instance FromJSON SKnowledge

emptyKnowledge :: SKnowledge
emptyKnowledge = SKnowledge [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
