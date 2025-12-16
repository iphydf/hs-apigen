{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apigen.Semantic where

import           Apigen.Types (Constness)
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | High-level semantic types.
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

-- | Parameter directionality and role.
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

-- | High-level semantic role of a method.
data SMethodRole
    = Constructor   -- ^ Creates a new resource instance
    | Destructor    -- ^ Destroys a resource instance
    | GetterRole    -- ^ Retrieves a property or state
    | SetterRole    -- ^ Modifies a property or state
    | ActionRole    -- ^ Performs a general operation
    | RegistrarRole -- ^ Registers a callback/event handler
    | StaticRole    -- ^ Global utility or constant provider
    deriving (Show, Eq, Generic)

instance ToJSON SMethodRole
instance FromJSON SMethodRole

-- | Strategy for mapping a high-level call to its C implementation.
data SMapping
    = StandardMapping -- ^ Follows the resource/role convention
    | CustomMapping CFunctionMapping -- ^ Irregular mapping requiring explicit detail
    deriving (Show, Eq, Generic)

instance ToJSON SMapping
instance FromJSON SMapping

-- | Explicit mapping of a high-level call to its C implementation.
data CFunctionMapping = CFunctionMapping
    { cFunctionName     :: Text
    , argMapping        :: [CArgSource]     -- ^ Positional mapping for C arguments
    , returnMapping     :: Maybe CArgSource   -- ^ How the C return maps to semantic result
    , cSizeFunctionName :: Maybe Text       -- ^ Function to call to get buffer size (for size-then-get)
    , cErrorType        :: Maybe Text       -- ^ The C error enum name
    , cSemParams        :: [SParameter]     -- ^ Semantic parameters for this C call
    , cReturnType       :: SType            -- ^ Original C return type
    } deriving (Show, Eq, Generic)

instance ToJSON CFunctionMapping
instance FromJSON CFunctionMapping

-- | Strategy for handling the relationship between C return value and error enums.
data SResultStrategy
    = IgnoreReturn          -- ^ High-level API ignores the C return value
    | ReturnIsValue         -- ^ C return value is the semantic result
    | ReturnIsErrorCode     -- ^ C return value IS the error code itself
    | ReturnIsResult        -- ^ C return value is the success payload of a Result<T, E>
    deriving (Show, Eq, Generic)

instance ToJSON SResultStrategy
instance FromJSON SResultStrategy

-- | A high-level Method.
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
    } deriving (Show, Eq, Generic)

instance ToJSON SMethod
instance FromJSON SMethod

data SParameter = SParameter
    { paramName      :: Text
    , paramType      :: SType
    , paramConstness :: Constness
    , paramConstants :: [Text]            -- ^ Related #defines
    } deriving (Show, Eq, Generic)

instance ToJSON SParameter
instance FromJSON SParameter

-- | A high-level Property.
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

-- | Resource Classification
data SResourceType
    = ResHandle
    | ResId SType -- ^ The ID type
    deriving (Show, Eq, Generic)

instance ToJSON SResourceType
instance FromJSON SResourceType

-- | A semantic Resource (Object).
data SResource = SResource
    { resourceName    :: Text
    , cName           :: Text             -- ^ Original C type name
    , resourceType    :: SResourceType    -- ^ Handle-based or ID-based
    , cPrefix         :: Text             -- ^ C function prefix (e.g., "resource_")
    , isRoot          :: Bool             -- ^ True if this is the root context object (e.g. Tox)
    , parent          :: Maybe Text       -- ^ Hierarchical owner
    , lifecycleParent :: Maybe Text       -- ^ Resource required for creation (RAII dependency)
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

-- | Complete API Model.
data SemanticModel = SemanticModel
    { enums        :: [SEnumModel]
    , constants    :: [SConstantModel]
    , idTypes      :: [SIdTypeModel]    -- ^ Mapping of semantic IDs to C types
    , callbacks    :: [SCallbackTypeModel] -- ^ C Callback typedefs
    , resources    :: [SResource]
    , variants     :: [SVariant]
    , commonPrefix :: Text             -- ^ Common prefix shared by resources/functions
    , diagnostics  :: [SDiagnostic]
    } deriving (Show, Eq, Generic)

instance ToJSON SemanticModel
instance FromJSON SemanticModel

data SCallbackTypeModel = SCallbackTypeModel
    { cbName   :: Text
    , cbCName  :: Text
    , cbParams :: [SParameter]
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
    , enumMembers      :: [(Text, Text)] -- ^ (C Name, Semantic Name)
    } deriving (Show, Eq, Generic)

instance ToJSON SEnumModel
instance FromJSON SEnumModel

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
