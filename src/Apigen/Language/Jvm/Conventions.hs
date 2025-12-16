{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Hardcoded jvm-toxcore-c conventions that don't (yet) live in the
-- c-toxcore headers.
--
-- This module is the **staging ground** for knowledge that should
-- eventually be expressed in the c-toxcore header annotations or
-- typedefs themselves. Centralising it here keeps the cost of moving
-- it later down to "delete this module, populate the same tables from
-- parsed header annotations" — one focused refactor rather than
-- unwinding inline checks scattered across the generator.
--
-- Each table below maps a piece of c-toxcore reality (param name,
-- callback param name, method name, etc.) to a jvm-toxcore-c ergonomic
-- convention (Kotlin wrapper class, naming override, etc.).
module Apigen.Language.Jvm.Conventions
    ( -- * Parameter wrapping
      paramWrapperClass
    , paramWrapperFor
    , paramWrapperImport

      -- * Generated numeric value classes
    , ValueClassSpec (..)
    , ValueClassBacking (..)
    , generatedValueClasses
    , paramNamesForWrapper

      -- * Return wrapping
    , returnWrapperClass
    , returnWrapperImport

      -- * Interface annotations & factories
    , interfaceExtraMembers
    , interfaceGenericParams

      -- * Impl-class extras
    , implExtraMembers

      -- * Synthetic exceptions
    , syntheticExceptionFor
    , syntheticExceptionEnums

      -- * Variable-size byte wrappers
    , varByteWrappers

      -- * Error-enum reachability
    , errorEnumIsReachable
    , isOpenEnum
    , optionsWireFields

      -- * Hand-curated callback documentation
    , callbackKdocExtra

      -- * Method renames
    , kotlinMethodNameOverride

      -- * Bool-return classification
    , boolReturnsValue

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
    , jniExtraJavaDecls
    , jniExtraJavaImports
    , jniExtraHeaderDecls

      -- * Public-API filtering
    , isLifecycleConstructor
    , isHiddenFromPublicApi
    , interfaceAllowsStatics
    , interfaceSelfResource

    ) where

import qualified Apigen.Semantic
import           Apigen.Semantic (SCallbackTypeModel (..), SMethod (..),
                                  SParameter (..), SResource (..), SType (..),
                                  SemanticModel (..))
import qualified Data.List       as List
import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text

--------------------------------------------------------------------------------
-- Parameter wrapping
--------------------------------------------------------------------------------

-- | Small AV-side vocabulary: numeric param names that map to a Kotlin
-- value/enum class. Used by both 'paramWrapperFor' (as the AV-vocab
-- fallback) and 'returnWrapperClass' (via 'returnAvVocabMatch'). Each
-- entry is kept because c-toxcore exposes the value as a raw
-- @uint32_t@ / @uint16_t@ rather than a named typedef — when c-toxcore
-- gains typedefs like @Toxav_Bit_Rate@, this table goes away.
--
-- The legacy 'toxav_audio_data_cb' callback names its sample count as
-- @samples@ and sampling rate as @sample_rate@; the modern callback
-- uses @sample_count@ and @sampling_rate@. Map both names to the
-- modern wrapper so Kotlin clients see one type per concept.
paramWrapperClass :: Text -> Maybe Text
paramWrapperClass = (`lookup` paramWrapperTable)

-- | The c-param-name → wrapper-class association behind
-- 'paramWrapperClass'. Kept as an explicit list (not a bare @case@) so
-- 'generatedValueClasses' can reverse-map a class back to the C param
-- names that feed it — the two stay in lockstep from one source.
paramWrapperTable :: [(Text, Text)]
paramWrapperTable =
    [ ("audio_bit_rate", "BitRate")
    , ("video_bit_rate", "BitRate")
    , ("bit_rate",       "BitRate")
    , ("sample_count",   "SampleCount")
    , ("samples",        "SampleCount")
    , ("channels",       "AudioChannels")
    , ("sampling_rate",  "SamplingRate")
    , ("sample_rate",    "SamplingRate")
    , ("width",          "Width")
    , ("height",         "Height")
    , ("port",           "Port")
    ]

-- | The C param names that 'paramWrapperTable' maps to a given wrapper
-- class. Used by 'renderValueClass' to discover the C scalar width
-- from the model.
paramNamesForWrapper :: Text -> [Text]
paramNamesForWrapper cls = [n | (n, c) <- paramWrapperTable, c == cls]

-- | How a generated numeric value class backs its single @value@ field.
data ValueClassBacking
    = SignedWithBound  -- ^ Kotlin @Int@ + a @require@ range check + a
                       -- @fromInt@ clamping factory (Width, Height). JNI
                       -- has no unsigned types, so the wire type is
                       -- @Int@; the bound keeps an out-of-range value
                       -- from truncating silently as it crosses JNI.
    | UnsignedExact    -- ^ The unsigned Kotlin type that exactly covers
                       -- the C width — @UShort@ for @uint16_t@ (Port).
                       -- No runtime check: the type itself is the bound.
    deriving (Show, Eq)

-- | A numeric value class the JVM binding wraps but c-toxcore exposes
-- only as a raw integer — there is no typedef for it to ride
-- 'Apigen.Semantic.idTypes' like @Tox_Friend_Number@ does. Each is
-- emitted by 'renderValueClass' from this spec plus the C scalar width
-- discovered in the model, so a header re-typing (e.g. @width@ widening
-- to @uint32_t@) tracks into the generated bound instead of going
-- silently stale. When c-toxcore adds a typedef, drop the entry.
--
-- This covers only the structurally-derivable classes. The semantic
-- ones (BitRate sentinels, AudioChannels\/SamplingRate\/AudioLength
-- value sets) carry constants found in no header and stay hand-written
-- until toxav.h grows the corresponding enums.
data ValueClassSpec = ValueClassSpec
    { vcClassName :: Text             -- ^ Kotlin class name (= wrapper class)
    , vcPackage   :: Text             -- ^ Kotlin subpackage, e.g. @"av.data"@
    , vcBacking   :: ValueClassBacking
    , vcDoc       :: Maybe Text       -- ^ Optional one-line KDoc for the class
    }

generatedValueClasses :: [ValueClassSpec]
generatedValueClasses =
    [ ValueClassSpec "Width"  "av.data"   SignedWithBound Nothing
    , ValueClassSpec "Height" "av.data"   SignedWithBound Nothing
    -- @Port@ is a core network type; its hand-written file lived under
    -- @core.data@ and 'paramWrapperImport' keeps it there.
    , ValueClassSpec "Port"   "core.data" UnsignedExact
        (Just "IP_Port stores an IP datastructure with a port.")
    ]

-- | Method-aware param wrapping. For byte-array params, derives the
-- wrapper class name from the method name + param name via
-- 'deriveByteWrapper'. For numeric AV params, falls through to
-- 'paramWrapperClass'.
--
-- The empty method name is a contract from
-- 'Kotlin.Common.renderParamType' meaning "callback-context: only
-- name-only conventions apply." In that case only 'paramWrapperClass'
-- runs — derivation needs a method name to know the resource hierarchy.
paramWrapperFor :: Text {- C method name -} -> SParameter -> Maybe Text
paramWrapperFor cMethod p
    | Text.null cMethod = paramWrapperClass pname
    -- Crypto buffers (@passphrase@, @plaintext@, @ciphertext@) and
    -- option struct byte fields (@savedata@) are generic byte arrays
    -- in the JVM API — wrapping them adds noise without static-safety
    -- benefit. Keep them as raw @ByteArray@. The matching
    -- 'publicApiResources' list excludes @Pass_Key@ and @Options@
    -- from the wrapper-file walk so this and that filter stay in
    -- lockstep.
    | "tox_pass_"    `Text.isPrefixOf` cMethod = paramWrapperClass pname
    | "tox_options_" `Text.isPrefixOf` cMethod = paramWrapperClass pname
    | otherwise = case paramWrapperClass pname of
        -- AV vocabulary fires regardless of type — its job is to wrap
        -- *numeric* params named @bit_rate@\/@width@\/etc. into typed
        -- value classes. The vocabulary list itself is the type gate.
        Just cls -> Just cls
        -- Byte-array derivation only fires for variable-size byte
        -- buffers. Fixed-size byte typedefs are already wrapped by
        -- their @arrayTypes@ entry via @renderType@; numeric scalars
        -- (the @state@, @peer_number@, @audio_enabled@ params on
        -- callbacks) stay as raw @Int@\/@Boolean@.
        Nothing  -> case paramType p of
            SBytes -> deriveByteWrapper cMethod pname
            _      -> Nothing
  where
    pname = paramName p

-- | Fully qualified Kotlin import path for a wrapper class name. AV
-- vocabulary wrappers ('paramWrapperClass') live under @av.data@;
-- everything else under @core.data@. Single source of truth is
-- 'paramWrapperClass' itself — any name produced by that table is an
-- AV wrapper regardless of which method emitted it.
paramWrapperImport :: Text -> Text
paramWrapperImport cls
    | cls `elem` avVocabularyClassNames = "im.tox.tox4j.av.data." <> cls
    | otherwise                          = "im.tox.tox4j.core.data." <> cls

-- | The distinct class names that live under @av.data@ rather than
-- @core.data@. AV value-class wrappers (BitRate, SampleCount,
-- AudioChannels, SamplingRate, Width, Height) live in the AV
-- subpackage; @Port@ — though produced by 'paramWrapperClass' too
-- — is a core network type and the hand-written file lives under
-- @core.data@.
avVocabularyClassNames :: [Text]
avVocabularyClassNames =
    [ "BitRate"
    , "SampleCount"
    , "AudioChannels"
    , "SamplingRate"
    , "Width"
    , "Height"
    ]

--------------------------------------------------------------------------------
-- Byte-wrapper derivation
--------------------------------------------------------------------------------

-- | Derive a Kotlin wrapper class name from a method (or callback) name
-- and a byte-array parameter name. Returns 'Nothing' when no resource
-- chain can be identified — leaves the type as plain @ByteArray@.
--
-- The algorithm:
--
--   1. Strip the @_cb@ callback suffix and the
--      @tox_self_@\/@toxav_@\/@tox_@ subsystem prefix.
--   2. Tokenise on @_@.
--   3. Peel leading tokens that name a known resource (@friend@,
--      @conference@, @group@, @file@, plus the @peer@\/@offline@
--      sub-resource extensions). That's the resource chain.
--   4. Drop a known verb (@set@\/@get@\/@send@\/@add@\/@recv@\/…) from
--      the remainder; what's left is the "object" of the method.
--   5. If the object is empty, fall back to the parameter name — for
--      methods like @tox_friend_add@ where the verb is the whole tail.
--   6. Wrapper = @\"Tox\"@ + PascalCase of (resource chain ++ object).
--
-- Examples:
--
-- @
-- tox_self_set_name(name)              → ToxName
-- tox_self_set_status_message(...)     → ToxStatusMessage
-- tox_friend_send_message(message)     → ToxFriendMessage
-- tox_friend_add(message)              → ToxFriendMessage  (verb-only tail)
-- tox_friend_message_cb(message)       → ToxFriendMessage  (callback shape)
-- tox_friend_send_lossy_packet(data)   → ToxFriendLossyPacket
-- tox_conference_peer_get_name()       → ToxConferencePeerName
-- tox_group_set_password(password)     → ToxGroupPassword
-- @
deriveByteWrapper :: Text -> Text -> Maybe Text
deriveByteWrapper rawMethod paramName =
    let method   = stripCallbackSuffix rawMethod
        body    = stripSubsystemPrefix method
        tokens  = filter (not . Text.null) (Text.splitOn "_" body)
        (resourceChain, restTokens) = splitResourceChain tokens
        objTokens   = stripLeadingVerbs restTokens
        -- Decide whether to derive the subject from the param name or
        -- from the method's leftover (object) tokens:
        --
        --  * Returns have no param name — use the object tokens.
        --  * Generic param names (@data@, @value@, @bytes@,
        --    @plaintext@, @ciphertext@) carry no semantic — use the
        --    method's object tokens (which name the data type, e.g.
        --    @lossy_packet@).
        --  * When the param name matches the *last* object token
        --    (e.g. @status_message_cb@'s @message@ param), the object
        --    tokens are the more specific phrase — use them. This
        --    preserves the distinction between @ToxStatusMessage@ and
        --    @ToxFriendMessage@ that pure-param-name derivation would
        --    collapse.
        --  * Otherwise the param name is the concrete data type and
        --    wins.
        paramMatchesLastObj = not (null objTokens)
                           && not (Text.null paramName)
                           && last objTokens == paramName
        useObjTokens = Text.null paramName
                    || (paramName `elem` genericParamNames && not (null objTokens))
                    || paramMatchesLastObj
        subjectToks
            | useObjTokens = objTokens
            | otherwise    = Text.splitOn "_" paramName
        subjectToks' = case subjectToks of
            [] -> Text.splitOn "_" paramName
            xs -> xs
        all_        = collapseRedundantResource resourceChain subjectToks'
        nonEmpty    = filter (not . Text.null) all_
    in if null nonEmpty
         then Nothing
         else Just ("Tox" <> Text.concat (map capitalize nonEmpty))
  where
    stripCallbackSuffix m = case Text.stripSuffix "_cb" m of
        Just rest -> rest
        Nothing   -> m

    stripSubsystemPrefix m = case mapMaybe (`Text.stripPrefix` m) subsystemPrefixes of
        (rest : _) -> rest
        []         -> m

    -- Order matters: longest first. @tox_self_@ before @tox_@ so the
    -- @self_@ collapses. @tox_pass_@ before @tox_@ so the @pass_@
    -- collapses (the encryptsave subsystem owns the @Pass_Key@
    -- resource).
    subsystemPrefixes = ["tox_self_", "tox_pass_", "toxav_", "tox_"]

    capitalize t = case Text.uncons (Text.toLower t) of
        Nothing      -> ""
        Just (c, cs) -> Text.cons (toUpperAscii c) cs

    toUpperAscii c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise            = c

-- | Param names that carry no semantic — when these are the param name
-- on a method whose body has object tokens, prefer the object tokens.
-- E.g. @tox_friend_send_lossy_packet(data)@ uses @data@ as a generic
-- name and the object tokens @lossy_packet@ name the type.
genericParamNames :: [Text]
genericParamNames =
    [ "data", "value", "bytes", "buffer"
    , "plaintext", "ciphertext"
    ]

-- | Peel leading tokens that name a resource. Recognises the bare
-- resources plus the @peer@\/@offline@ extensions used by conference
-- and group sub-resources (@conference_peer@, @conference_offline_peer@,
-- @group_peer@). The @self@ token after a resource is silently
-- collapsed: @tox_group_self_set_name@ derives the same wrapper as
-- @tox_group_set_name@ would — both reference a peer name in a group.
splitResourceChain :: [Text] -> ([Text], [Text])
splitResourceChain = go []
  where
    go acc [] = (reverse acc, [])
    go acc (t : rest)
        | t `elem` baseResources                = go (t : acc) rest
        | t `elem` subResources, not (null acc) = go (t : acc) rest
        | t == "self",           not (null acc) = go acc rest -- skip silently
        | otherwise                              = (reverse acc, t : rest)
    baseResources = ["friend", "conference", "group", "file"]
    subResources  = ["peer", "offline"]

-- | Strip ALL leading verb tokens. The list mirrors the C-API naming
-- convention: action verbs (@set@\/@get@\/@send@\/@add@\/@recv@\/…)
-- plus the no-prefix @by@ used by look-up methods
-- (@tox_friend_by_public_key@). Methods whose body starts with a
-- non-verb token (predicate-shaped @tox_friend_exists@, data-named
-- callback shapes like @tox_friend_message@) leave the body intact.
--
-- Multi-strip handles compound verb prefixes like @invite_accept@ —
-- both tokens are verbs, neither names a data type, so both are
-- stripped and the wrapper name falls back to the parameter name.
stripLeadingVerbs :: [Text] -> [Text]
stripLeadingVerbs (v : rest) | v `elem` knownVerbs = stripLeadingVerbs rest
stripLeadingVerbs ts = ts

knownVerbs :: [Text]
knownVerbs =
    [ "set", "get", "send", "add", "recv"
    , "new", "delete", "free", "init"
    , "by", "kill", "iterate", "accept"
    , "invite", "join", "leave", "exit"
    , "kick", "answer", "call", "request"
    , "reconnect", "load", "dispatch"
    ]

-- | When the first subject token starts with the last resource token,
-- the resource is already named in the parameter (e.g. @file_send@'s
-- @filename@ already contains @file@). Drop the redundant resource so
-- @tox_file_send(filename)@ → @ToxFilename@, not @ToxFileFilename@.
collapseRedundantResource :: [Text] -> [Text] -> [Text]
collapseRedundantResource resourceChain subjectToks =
    case (reverse resourceChain, subjectToks) of
        (lastR : initR, firstS : _)
            | lastR `Text.isPrefixOf` firstS ->
                reverse initR ++ subjectToks
        _ -> resourceChain ++ subjectToks

--------------------------------------------------------------------------------
-- Return wrapping
--------------------------------------------------------------------------------

-- | If a method's return value should be wrapped in a Kotlin value class
-- on the public interface, return the wrapper's simple Kotlin name.
--
-- Routes to the AV vocabulary first when the method name's suffix
-- matches a vocabulary key (e.g. @tox_self_get_udp_port@ ends in
-- @_port@ → 'Port'). Otherwise derives from the method's resource +
-- subject the same way 'paramWrapperFor' does for byte-array params.
returnWrapperClass :: Text -> SType -> Maybe Text
returnWrapperClass methodName returnType = case returnType of
    -- Byte-array variable-size returns: derive a wrapper from the
    -- method's resource + object word.
    SBytes -> deriveByteWrapper methodName ""
    -- Numeric returns: consult the AV vocabulary. @tox_self_get_udp_port@
    -- returns @uint16_t@ and the @_port@ suffix → 'Port'. Setters
    -- ending in @_bit_rate@ etc. return @bool@ (or void) so they
    -- don't reach this branch.
    SUInt _ -> returnAvVocabMatch methodName
    SInt _  -> returnAvVocabMatch methodName
    SSizeT  -> returnAvVocabMatch methodName
    -- Other returns (handle types, enums, fixed-size typedefs, bool)
    -- defer to the existing renderType logic.
    _       -> Nothing

-- | Match a method's name suffix against an AV vocabulary key. Used by
-- return-wrapping to route @tox_self_get_udp_port@ → 'Port' and any
-- future AV getter to its vocabulary class.
returnAvVocabMatch :: Text -> Maybe Text
returnAvVocabMatch methodName =
    let suffixes = avVocabularyKeys
        matches = [ cls
                  | key <- suffixes
                  , ("_" <> key) `Text.isSuffixOf` methodName
                  , Just cls <- [paramWrapperClass key]
                  ]
    in case matches of
        (c : _) -> Just c
        []      -> Nothing
  where
    -- Mirrors the keys recognised by 'paramWrapperClass'. Listed
    -- explicitly so we don't have to enumerate all method names to
    -- find suffix candidates.
    avVocabularyKeys =
        [ "audio_bit_rate", "video_bit_rate", "bit_rate"
        , "sample_count", "samples"
        , "channels", "sampling_rate", "sample_rate"
        , "width", "height", "port"
        ]

-- | Fully qualified Kotlin import path for a return-wrapper class.
-- Routes AV vocabulary names to @av.data@; everything else to @core.data@.
returnWrapperImport :: Text -> SType -> Maybe Text
returnWrapperImport name returnType = case returnWrapperClass name returnType of
    Just c  -> Just (paramWrapperImport c)
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Interface annotations & factories
--------------------------------------------------------------------------------

-- | Extra interface members to emit verbatim into the interface body
-- (after the C-derived signatures). Each entry is one multi-line text
-- block. Used for hand-curated derived properties and factory shapes
-- that don't map to a single C function — e.g. ToxCore's @load@
-- factory and the @List<ToxFriendNumber>@-of-@IntArray@ wrappers.
interfaceExtraMembers :: Text -> [Text]
interfaceExtraMembers = \case
    "ToxCore" ->
        [ "    override fun close(): Unit"
        ]
    -- @tox_hash@ is a standalone utility (not bound to a Tox_Pass_Key
    -- resource), so it isn't picked up by the resource walker. We
    -- attach it to the interface verbatim. The remaining PassKey
    -- equality/serialise helpers from the old ByteArray design are
    -- dropped: a handle-based PassKey has no public serialisation in
    -- the C API.
    "ToxCrypto" ->
        [ "    fun hash(data: ByteArray): ByteArray"
        ]
    _ -> []

-- | Generic type parameters to attach to the interface declaration.
-- @ToxCrypto@ is generic over the @PassKey@ representation — the JNI
-- impl uses @ByteArray@, but a pure-Kotlin impl might choose a richer
-- type. Keeping the parameter abstract lets each impl decide.
interfaceGenericParams :: Text -> [Text]
interfaceGenericParams = \case
    "ToxCrypto" -> ["PassKey"]
    _           -> []

-- | Per-Impl-class verbatim extras: implementations of the
-- 'interfaceExtraMembers' factories and any other Impl-level helpers
-- (JVM @finalize()@ cleanup hooks, type-narrowing @create()@
-- factories). Each entry is one multi-line block placed inside the
-- Impl class body. The hand-curated factories have no C counterpart,
-- so the Impl emitter can't derive their bodies from the model.
implExtraMembers :: Text -> [Text]
implExtraMembers = \case
    -- finalize runs only if the user forgot to close(). The C-side
    -- instance manager throws @IllegalStateException("Leaked Tox
    -- instance #N")@ from @toxFinalize@ when the slot still holds a
    -- live instance, and the JVM finalizer swallows the throw —
    -- the slot would be permanently lost. Calling the (idempotent)
    -- kill first releases the C instance so finalize cleanly puts
    -- the slot back on the freelist. The runCatching swallows any
    -- residual failure because finalizers must never escape.
    "ToxCoreImpl" ->
        [ "    protected fun finalize() {\n\
          \        runCatching {\n\
          \            ToxCoreJni.toxKill(instanceNumber)\n\
          \            ToxCoreJni.toxFinalize(instanceNumber)\n\
          \        }\n\
          \    }"
        ]
    "ToxAvImpl" ->
        [ "    protected fun finalize() {\n\
          \        runCatching {\n\
          \            ToxAvJni.toxavKill(instanceNumber)\n\
          \            ToxAvJni.toxavFinalize(instanceNumber)\n\
          \        }\n\
          \    }"
        ]
    -- Implements the matching @hash@ entry from 'interfaceExtraMembers'
    -- ("ToxCrypto"): @tox_hash@ lives under the Tox resource in the
    -- model, so the Impl's resource walker doesn't reach it either.
    "ToxCryptoImpl" ->
        [ "    override fun hash(data: ByteArray): ByteArray = ToxCryptoJni.toxHash(data)"
        ]
    _ -> []

--------------------------------------------------------------------------------
-- Constants extras
--------------------------------------------------------------------------------

-- | Additional members for a generated @*Constants@ object that don't
-- come from C @#define@s. Currently used for jvm-toxcore-c's
-- convenience defaults (@DEFAULT_PROXY_PORT@, @DEFAULT_START_PORT@,
-- @DEFAULT_TCP_PORT@, @DEFAULT_END_PORT@) which are typed
-- @UShort@ and consumed by @ToxOptions@ as field defaults. Each
-- entry is a multi-line Kotlin block emitted verbatim after the
-- C-derived constants.
constantsExtraMembers :: Text -> [Text]
constantsExtraMembers = \case
    "ToxCoreConstants" ->
        [ "    /** Default port for HTTP proxies. */"
        , "    const val DEFAULT_PROXY_PORT: UShort = 8080u"
        , ""
        , "    /** Default start port for Tox UDP sockets. */"
        , "    const val DEFAULT_START_PORT: UShort = 33445u"
        , ""
        , "    /** Default end port for Tox UDP sockets. */"
        , "    val DEFAULT_END_PORT: UShort = (DEFAULT_START_PORT + 100u).toUShort()"
        , ""
        , "    /** Default port for Tox TCP relays. A value of 0 means disabled. */"
        , "    const val DEFAULT_TCP_PORT: UShort = 0u"
        ]
    -- Libsodium primitive sizes used by jvm-toxcore-c clients but not
    -- exposed in @toxencryptsave.h@. Values match the libsodium
    -- defaults the C library was built against.
    "ToxCryptoConstants" ->
        [ "    const val PUBLIC_KEY_LENGTH = 32"
        , "    const val SECRET_KEY_LENGTH = 32"
        , "    const val SHARED_KEY_LENGTH = 32"
        , "    const val NONCE_LENGTH = 24"
        , ""
        , "    const val ZERO_BYTES = 32"
        , "    const val BOX_ZERO_BYTES = 16"
        ]
    _ -> []

--------------------------------------------------------------------------------
-- Sealed groupings (ToxOptions)
--------------------------------------------------------------------------------

-- | A sealed-interface grouping: multiple flat C fields collapse
-- into one Kotlin field whose type is a sealed-interface @Type@.
-- e.g. @proxy_type@, @proxy_host@, @proxy_port@ → @proxy: ProxyOptions.Type@.
--
-- 'sealedGroupAccess' below maps each C field name to the Kotlin
-- accessor expression (relative to the @options@ root) so the
-- @ToxCoreImpl@ init block can write
-- @options.proxy.proxyAddress@ instead of @options.proxyHost@.
data SealedGroup = SealedGroup
    { sealedGroupFieldName  :: Text    -- ^ Kotlin field on @ToxOptions@.
    , sealedGroupTypeName   :: Text    -- ^ Fully-qualified Kotlin type.
    , sealedGroupDefault    :: Text    -- ^ Default-value expression.
    , sealedGroupCFields    :: [Text]  -- ^ C field names this group covers.
    } deriving (Show, Eq)

-- | Derive the sealed-interface groupings from the Tox_Options
-- properties. A group is any set of properties sharing a leading
-- @<X>_@ token where one of them is named @<X>_type@ and is enum-typed
-- (that's the discriminator) and at least one other property shares
-- the same prefix (the data fields).
--
-- The Kotlin field name on the parent @ToxOptions@ data class and the
-- sealed-interface type name are derived from the prefix:
--
-- @
-- prefix \"proxy\"    → field \"proxy\",    type \"ProxyOptions.Type\"
-- prefix \"savedata\" → field \"saveData\", type \"SaveDataOptions.Type\"
-- @
--
-- The @savedata@ case routes through 'compoundPrefixSplit' which
-- treats single-token C names as multi-word Kotlin names where the
-- binding has chosen camelCase'd readability over verbatim C. The
-- one entry there is the only manual knowledge this derivation
-- needs.
sealedGroups :: SemanticModel -> [SealedGroup]
sealedGroups model = case List.find ((== "Options") . Apigen.Semantic.resourceName) (resources model) of
    Nothing -> []
    Just options ->
        [ SealedGroup
            { sealedGroupFieldName = camelizeCompound prefix
            , sealedGroupTypeName  = pascalizeCompound prefix <> "Options.Type"
            , sealedGroupDefault   = pascalizeCompound prefix <> "Options.None"
            , sealedGroupCFields   = map propNameOf groupProps
            }
        | (prefix, groupProps) <- groupedByPrefix options
        , isQualifiedGroup prefix groupProps
        ]
  where
    propNameOf p = Apigen.Semantic.propName p

    -- Group properties by the first underscore-separated token of
    -- their C name. Excludes Handle/Callback fields (log callback +
    -- user data) so they don't accidentally form a phantom group.
    groupedByPrefix options =
        let visible = filter (visiblePropType . Apigen.Semantic.propType)
                            (Apigen.Semantic.properties options)
            firstToken p = case Text.splitOn "_" (Apigen.Semantic.propName p) of
                (t : _) -> t
                []      -> Apigen.Semantic.propName p
            keyOf p = (firstToken p, [p])
            merged = List.foldl' insertOrAppend [] (map keyOf visible)
            insertOrAppend acc (k, ps) =
                case List.lookup k acc of
                    Just existing -> (k, existing ++ ps) : filter ((/= k) . fst) acc
                    Nothing       -> acc ++ [(k, ps)]
        in merged

    visiblePropType (Apigen.Semantic.SHandle _)   = False
    visiblePropType (Apigen.Semantic.SCallback _) = False
    visiblePropType _                              = True

    -- A group "qualifies" if it has a @<prefix>_type@ enum-typed
    -- discriminator and at least one other field.
    isQualifiedGroup prefix ps =
        length ps >= 2
        && any (\p -> Apigen.Semantic.propName p == prefix <> "_type"
                   && isEnumType (Apigen.Semantic.propType p)) ps

    isEnumType (Apigen.Semantic.SEnum _) = True
    isEnumType _                          = False

    camelizeCompound p = case compoundPrefixSplit p of
        Just split -> camelCaseLocal split
        Nothing    -> p   -- single word, already camelCase
    pascalizeCompound p = case compoundPrefixSplit p of
        Just split -> pascalCaseLocal split
        Nothing    -> pascalCaseLocal p

-- | Map a single-token C prefix to the multi-word form the JVM
-- binding camelCase'd it as. One entry today; extend if c-toxcore
-- introduces more compounds whose Kotlin form splits them.
--
-- @\"savedata\"@ → @\"save_data\"@ — the C name is one token but the
-- Kotlin API exposes @saveData@ / @SaveDataOptions@ for readability.
compoundPrefixSplit :: Text -> Maybe Text
compoundPrefixSplit "savedata" = Just "save_data"
compoundPrefixSplit _           = Nothing

camelCaseLocal :: Text -> Text
camelCaseLocal t = case Text.splitOn "_" t of
    []           -> ""
    (h : rest)   -> Text.toLower h <> Text.concat (map capFirstLocal rest)

pascalCaseLocal :: Text -> Text
pascalCaseLocal = Text.concat . map capFirstLocal . Text.splitOn "_"

capFirstLocal :: Text -> Text
capFirstLocal s = case Text.uncons (Text.toLower s) of
    Nothing      -> ""
    Just (c, cs) -> Text.cons (toUpperA c) cs

toUpperA :: Char -> Char
toUpperA c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
    | otherwise            = c

-- | Map a C property name to its sealed group, if any. Walks the
-- derived 'sealedGroups' rather than a parallel table.
lookupSealedGroup :: SemanticModel -> Text -> Maybe SealedGroup
lookupSealedGroup model pname =
    case filter (\g -> pname `elem` sealedGroupCFields g) (sealedGroups model) of
        (g : _) -> Just g
        []      -> Nothing

-- | Kotlin accessor path for a grouped C field, relative to the
-- @options@ root. e.g. @proxy_host@ → @proxy.proxyAddress@ (note
-- the field rename inside @ProxyOptions.Type@).
sealedGroupAccess :: Text -> Maybe Text
sealedGroupAccess cField = case cField of
    "proxy_type"    -> Just "proxy.proxyType"
    "proxy_host"    -> Just "proxy.proxyAddress"
    "proxy_port"    -> Just "proxy.proxyPort"
    "savedata_type" -> Just "saveData.kind"
    "savedata"      -> Just "saveData.data"
    _               -> Nothing

--------------------------------------------------------------------------------
-- JNI extras
--------------------------------------------------------------------------------

-- | Extra @static native@ declarations appended to a generated
-- @*Jni.java@ file. Keyed by class name (@ToxCoreJni@/@ToxAvJni@/…).
-- Each entry is one line, including the @\    @ indent.
--
-- The model-driven generator produces a @tox_new(Tox_Options *)@
-- variant; the hand-written ToxCoreImpl calls a flat 11-arg variant
-- (@boolean ipv6Enabled, boolean udpEnabled, …, byte[] saveData@)
-- declared here and implemented in @lifecycle.cpp@. The matching
-- @tox_new@ entry is excluded from generation via 'byName' in
-- Apigen.Language.Jvm.Java so the hand-written decl below is the
-- only one. The @toxFinalize@ pair is similar — it has no C-API
-- counterpart; it's a JVM-side cleanup hook.
jniExtraJavaDecls :: Text -> [Text]
jniExtraJavaDecls = \case
    "ToxCoreJni" ->
        [ "    static native void toxFinalize(int instanceNumber);"
        ]
    "ToxAvJni" ->
        [ "    static native void toxavFinalize(int instanceNumber);"
        ]
    -- @tox_get_salt@, @tox_is_data_encrypted@, and @tox_hash@ are
    -- StaticRole utilities; the JNI emitter skips StaticRole by
    -- default, so the matching @native@ decls are listed here.
    "ToxCryptoJni" ->
        [ "    static native byte[] toxGetSalt(byte[] ciphertext) throws ToxGetSaltException;"
        , "    static native boolean toxIsDataEncrypted(byte[] data);"
        , "    static native byte[] toxHash(byte[] data);"
        ]
    _ -> []

-- | Imports required by 'jniExtraJavaDecls'.
jniExtraJavaImports :: Text -> [Text]
jniExtraJavaImports = \case
    "ToxCryptoJni" ->
        [ "im.tox.tox4j.crypto.exceptions.ToxGetSaltException"
        ]
    _ -> []

-- | Matching @JNIEXPORT@ declarations appended to the generated
-- @generated\/im_tox_tox4j_impl_jni_<Class>.h@ header. javah would
-- have emitted these from the @native@ decls above; we emit them
-- by hand so the same hand-written .cpp implementations link.
jniExtraHeaderDecls :: Text -> [Text]
jniExtraHeaderDecls = \case
    "ToxCoreJni" ->
        [ "JNIEXPORT void JNICALL Java_im_tox_tox4j_impl_jni_ToxCoreJni_toxFinalize"
        , "  (JNIEnv *, jclass, jint);"
        ]
    "ToxAvJni" ->
        [ "JNIEXPORT void JNICALL Java_im_tox_tox4j_impl_jni_ToxAvJni_toxavFinalize"
        , "  (JNIEnv *, jclass, jint);"
        ]
    "ToxCryptoJni" ->
        [ "JNIEXPORT jbyteArray JNICALL Java_im_tox_tox4j_impl_jni_ToxCryptoJni_toxGetSalt"
        , "  (JNIEnv *, jclass, jbyteArray);"
        , "JNIEXPORT jboolean JNICALL Java_im_tox_tox4j_impl_jni_ToxCryptoJni_toxIsDataEncrypted"
        , "  (JNIEnv *, jclass, jbyteArray);"
        , "JNIEXPORT jbyteArray JNICALL Java_im_tox_tox4j_impl_jni_ToxCryptoJni_toxHash"
        , "  (JNIEnv *, jclass, jbyteArray);"
        ]
    _ -> []

-- | Extra kdoc text to attach to a generated callback method when
-- c-toxcore's header has nothing to say about it. The list is one
-- line per kdoc body line (the @/**@ and @*/@ wrappers are added
-- by the renderer). Empty list means "fall through to the C-derived
-- kdoc, if any."
callbackKdocExtra :: Text -> [Text]
callbackKdocExtra = \case
    -- c-toxcore's @toxav_audio_data_cb@ has no doxygen on the
    -- typedef itself; document the naming divergence so callers
    -- aren't confused that this uses @samples@/@sampleRate@ while
    -- @audioReceiveFrame@ uses @sampleCount@/@samplingRate@.
    "toxav_audio_data_cb" ->
        [ "The legacy AV-groupchat audio callback. The Kotlin parameter"
        , "types are the same as @audioReceiveFrame@ (`SampleCount`,"
        , "`SamplingRate`); the C names diverge (`samples`/`sample_rate`"
        , "here vs `sample_count`/`sampling_rate` there) because the"
        , "two C callback typedefs were introduced separately."
        ]
    _ -> []

-- | Extra @(jniMethodName, cFunctionName)@ pairs to interleave into
-- the generated @cpp/Tox*/generated/natives.h@. Mirrors the
-- 'jniExtraJavaDecls' / 'jniExtraHeaderDecls' pairs for JNI methods
-- that don't have a natural resource owner (here: @tox_hash@ lives
-- on @Tox@ but is bound to @ToxCryptoJni@). The renderer sorts by
-- the C function name alongside the model-derived methods.
cppExtraNativeRefs :: Text -> [(Text, Text)]
cppExtraNativeRefs = \case
    "ToxCryptoJni" ->
        [ ("toxHash", "tox_hash")
        ]
    _ -> []

--------------------------------------------------------------------------------
-- Exception class extras
--------------------------------------------------------------------------------

-- | Additional @Code@ enumerators for an exception class that don't
-- come from the C error enum. Synthetic exceptions (those generated
-- by 'syntheticExceptionEnums' for methods with no @Tox_Err_*@) get
-- one automatic code per resource derived as
-- @\"<RESOURCE>_NOT_FOUND\"@ — matching the c-toxcore convention for
-- "thing didn't exist" failures.
exceptionExtraCodes :: SemanticModel -> Text -> [(Text, Maybe Text)]
exceptionExtraCodes model enumName =
    [ (codeMember, Just (codeKdoc resourceName))
    | (methodNm, synth) <- syntheticExceptionMethods model
    , synth == enumName
    , let resourceName = ownerResourceName model methodNm
    , let codeMember = Text.toUpper resourceName <> "_NOT_FOUND"
    ]
  where
    codeKdoc r = "The " <> Text.toLower r
              <> " number passed did not designate a valid "
              <> Text.toLower r <> "."

--------------------------------------------------------------------------------
-- Synthetic exceptions
--------------------------------------------------------------------------------

-- | C functions whose only failure mode is a return-false / NULL but
-- which have no @Tox_Err_*@ enum get a synthesized exception class so
-- the Kotlin side sees a typed throw rather than an
-- @IllegalArgumentException@ from a value-class init block.
--
-- The structural rule: the C function returns @bool@ (no error enum),
-- has a byte-array output buffer that the caller expects to be
-- populated, and is reachable from the public API. The Kotlin caller
-- wants a clean exception when the buffer can't be populated; the
-- alternative is the buffer being empty and tripping a downstream
-- @require(value.size == N)@.
syntheticExceptionFor :: SemanticModel -> Text -> Maybe Text
syntheticExceptionFor model mn =
    case [ enumNm | (m, enumNm) <- syntheticExceptionMethods model, m == mn ] of
        (e : _) -> Just e
        []      -> Nothing

-- | Synthetic 'SEnumModel's for methods identified by
-- 'syntheticExceptionMethods'. The generator fabricates the enum
-- shape so the exception emitter can produce the matching
-- @ToxXException@ class. Members are supplied by
-- 'exceptionExtraCodes'; @enumMembers@ stays empty here.
syntheticExceptionEnums :: SemanticModel -> [Apigen.Semantic.SEnumModel]
syntheticExceptionEnums model =
    [ Apigen.Semantic.SEnumModel
        { Apigen.Semantic.enumName         = enumNm
        , Apigen.Semantic.enumSemanticName = stripToxErrPrefix enumNm
        , Apigen.Semantic.enumMembers      = []
        }
    | (_, enumNm) <- syntheticExceptionMethods model
    ]
  where
    stripToxErrPrefix n = case Text.stripPrefix "Tox_" n of
        Just rest -> rest
        Nothing   -> n

-- | Methods that need a synthesized exception. Walks every public-API
-- resource looking for the structural shape:
--
--   * cReturnType is 'SBool' (the C function signals success/failure)
--   * methodErrorType is 'Nothing' (no @Tox_Err_*@ to thread)
--   * methodRole is not 'StaticRole' (free helpers like @tox_hash@
--     don't surface typed throws)
--   * output is a byte buffer ('SBytes' or 'SFixedBytes') — the
--     caller expects the buffer to be populated, and an empty
--     buffer would otherwise trip a downstream value-class init
--     check.
--
-- Returns @(C method name, synthetic enum name)@ pairs. The enum
-- name is derived from the method name by replacing the @tox_@
-- prefix with @Tox_Err_@ and PascalCase'ing each token.
syntheticExceptionMethods :: SemanticModel -> [(Text, Text)]
syntheticExceptionMethods model =
    [ (Apigen.Semantic.methodName m, deriveEnumName (Apigen.Semantic.methodName m))
    | r <- Apigen.Semantic.resources model
    , Apigen.Semantic.resourceName r `elem` publicApiResources
    , m <- Apigen.Semantic.methods r
    , needsSyntheticException m
    ]
  where
    needsSyntheticException m =
        Apigen.Semantic.methodErrorType m == Nothing
        && Apigen.Semantic.methodRole m /= Apigen.Semantic.StaticRole
        && cReturnTypeOf m == Just Apigen.Semantic.SBool
        && isByteOutput (Apigen.Semantic.output m)

    cReturnTypeOf m = case Apigen.Semantic.methodMapping m of
        Apigen.Semantic.CustomMapping cmap -> Just (Apigen.Semantic.cReturnType cmap)
        _                                   -> Nothing

    isByteOutput (Apigen.Semantic.SFixedBytes _ _) = True
    isByteOutput Apigen.Semantic.SBytes            = True
    isByteOutput _                                  = False

    deriveEnumName mn = case Text.stripPrefix "tox_" mn of
        Just rest -> "Tox_Err_" <> Text.intercalate "_" (map pascalCaseLocal (Text.splitOn "_" rest))
        Nothing   -> "Tox_Err_" <> pascalCaseLocal mn

-- | Identify which resource a method "lives on" by C method name.
-- Used for synthetic-exception code naming
-- (@\"<RESOURCE>_NOT_FOUND\"@). Walks the resource list and returns
-- the resource whose @cPrefix@ best matches the method name.
ownerResourceName :: SemanticModel -> Text -> Text
ownerResourceName model mn =
    case [ r | r <- Apigen.Semantic.resources model
             , Apigen.Semantic.resourceName r `elem` publicApiResources
             , m <- Apigen.Semantic.methods r
             , Apigen.Semantic.methodName m == mn
         ] of
        (r : _) -> Apigen.Semantic.resourceName r
        []      -> "Tox"

--------------------------------------------------------------------------------
-- Variable-size byte wrappers
--------------------------------------------------------------------------------

-- | Wrapper classes for variable-size @ByteArray@ values that cross
-- the JNI boundary. C-toxcore exposes these as @(const uint8_t *,
-- size_t)@ pairs; the JVM side wraps them in a typed class so the
-- API surface distinguishes a @ToxFriendMessage@ from a
-- @ToxConferenceMessage@ even though both are just bytes.
--
-- Returned as @(className, kdoc summary)@ pairs. The generator
-- emits each entry as a plain @class@ with content-based @equals@\/
-- @hashCode@ and a payload-eliding @toString@ — the same shape as
-- the fixed-size variants from 'arrayTypes', minus the
-- @require(value.size == K)@ block (no fixed length here).
--
-- Adding a new wrapper here is one entry; the matching
-- 'paramWrapperClass' \/ 'paramWrapperFor' bindings still need to be
-- updated separately to thread the type through the right
-- callbacks\/methods.
-- | True if any non-hidden method's @methodErrorType@ references
-- the given error-enum name. Used by both the Kotlin exception
-- generator (to skip the exception class) and the C++ @errors.cpp@
-- @HANDLE@ generator (to skip the matching template
-- specialization). Sharing the predicate here keeps the two sides
-- in lockstep — divergence would leak either a Kotlin class
-- without a C++ throw site (harmless dead code) or a C++ HANDLE
-- whose JNI ClassLoader lookup would fail at runtime against the
-- now-missing Kotlin class.
errorEnumIsReachable :: Apigen.Semantic.SemanticModel -> Text -> Bool
errorEnumIsReachable model name = name `elem`
    [ ref
    | r <- Apigen.Semantic.resources model
    , m <- Apigen.Semantic.methods r
    , not (isHiddenFromPublicApi model (Apigen.Semantic.methodName m))
    , Just ref <- [Apigen.Semantic.methodErrorType m]
    ]

-- | The Tox_Options fields that cross the JNI boundary inside the
-- serialized proto @Options@ message. One shared enumeration drives
-- all three sides — the proto schema (Proto.hs), the Kotlin builder
-- in ToxCoreImpl's init block (Kotlin.Impl), and the generated C++
-- @set_options_from_proto@ (Cpp.hs) — so a new c-toxcore Options
-- field regenerates the full pipeline and the sides cannot drift.
-- Handle- and callback-typed properties (log callback plumbing)
-- don't cross the boundary, same filter the old per-field JNI
-- setters used.
optionsWireFields :: Apigen.Semantic.SemanticModel -> [Apigen.Semantic.SProperty]
optionsWireFields model =
    case List.find ((== "Options") . Apigen.Semantic.resourceName) (Apigen.Semantic.resources model) of
        Nothing -> []
        Just options ->
            [ p
            | p <- Apigen.Semantic.properties options
            , visible (Apigen.Semantic.propType p)
            ]
  where
    visible (Apigen.Semantic.SHandle _)   = False
    visible (Apigen.Semantic.SCallback _) = False
    visible _                             = True

-- | True if the enum is an /open enumeration/: a non-error enum that no
-- method, property, event or callback signature ever mentions. C marks
-- these by typing the corresponding parameters @uint32_t@ instead of
-- the enum ("clients can invent their own file kinds") — so the enum
-- type never appears in any signature, and that absence is the
-- structural signal. A closed Kotlin @enum class@ would make user-
-- defined values unrepresentable; the Kotlin backend emits an @object@
-- of @const val@ Ints instead. Today this matches exactly
-- @Tox_File_Kind@.
isOpenEnum :: Apigen.Semantic.SemanticModel -> Apigen.Semantic.SEnumModel -> Bool
isOpenEnum model e =
    not isErrEnum && not (any mentionsEnum signatureTypes)
  where
    names =
        [ Apigen.Semantic.enumName e
        , Apigen.Semantic.enumSemanticName e
        ]
    isErrEnum =
        "Tox_Err_" `Text.isPrefixOf` Apigen.Semantic.enumName e
            || "Toxav_Err_" `Text.isPrefixOf` Apigen.Semantic.enumName e
    signatureTypes =
        [ t
        | r <- Apigen.Semantic.resources model
        , m <- Apigen.Semantic.methods r
        , t <- Apigen.Semantic.output m
                : map Apigen.Semantic.paramType (Apigen.Semantic.inputs m)
        ]
            ++ [ Apigen.Semantic.propType p
               | r <- Apigen.Semantic.resources model
               , p <- Apigen.Semantic.properties r
               ]
            ++ [ Apigen.Semantic.paramType p
               | r <- Apigen.Semantic.resources model
               , ev <- Apigen.Semantic.events r
               , p <- Apigen.Semantic.eventParams ev
               ]
            ++ [ Apigen.Semantic.paramType p
               | cb <- Apigen.Semantic.callbacks model
               , p <- Apigen.Semantic.cbParams cb
               ]
    mentionsEnum t = case t of
        Apigen.Semantic.SEnum n        -> n `elem` names
        Apigen.Semantic.SList t'       -> mentionsEnum t'
        Apigen.Semantic.SFixedList t' _ _ -> mentionsEnum t'
        _                              -> False

-- | Compute the set of variable-size byte-array wrapper classes the
-- generator needs to emit for this model.
--
-- Walks only methods that are surfaced on the public API — methods on
-- public-API resources (Tox/Friend/Conference/etc.), not hidden by
-- 'isHiddenFromPublicApi'. Walks every callback (those that route
-- through the dispatcher all surface). Collects whatever
-- 'deriveByteWrapper' / 'returnWrapperClass' produces for each
-- byte-array param/return.
--
-- Filters out:
--   * AV vocabulary names (BitRate, Width, …) — those classes are
--     hand-written under @av.data@, not generated here.
--   * Names that already correspond to a fixed-size typedef in
--     'arrayTypes' — those wrappers are emitted by
--     @Kotlin.renderFixedBytes@.
--
-- Returned @(className, kdoc)@ pairs are sorted for deterministic
-- regeneration. Kdoc text is generic; concrete prose isn't derivable.
varByteWrappers :: SemanticModel -> [(Text, Text)]
varByteWrappers model =
    let publicMethods =
            [ (r, m)
            | r <- resources model
            , resourceName r `elem` publicApiResources
            , m <- methods r
            , not (isHiddenFromPublicApi model (methodName m))
            ]
        fromMethodParams =
            [ cls
            | (_, m) <- publicMethods
            , p      <- inputs m
            , isVarByteParam (paramType p)
            , Just cls <- [paramWrapperFor (methodName m) p]
            ]
        fromMethodReturns =
            [ cls
            | (_, m) <- publicMethods
            , isVarByteParam (output m)
            , Just cls <- [returnWrapperClass (methodName m) (output m)]
            ]
        fromCallbackParams =
            [ cls
            | cb <- callbacks model
            , p  <- cbParams cb
            , isVarByteParam (paramType p)
            , Just cls <- [paramWrapperFor (cbCName cb) p]
            ]
        wrappers = List.sort . List.nub
            . filter (not . isAvVocabulary)
            . filter (not . isFixedTypedef)
            $ fromMethodParams ++ fromMethodReturns ++ fromCallbackParams
    in [(cls, defaultKdoc cls) | cls <- wrappers]
  where
    isVarByteParam SBytes = True
    isVarByteParam _      = False

    fixedClassNames = [ "Tox" <> pascalCaseLocal sname
                      | (_, sname) <- arrayTypes model
                      ]
    isFixedTypedef cls = cls `elem` fixedClassNames

    isAvVocabulary cls = cls `elem` avVocabularyClassNames

    defaultKdoc cls = "A typed byte array for " <> cls <> "."

    pascalCaseLocal = Text.concat . map cap . Text.splitOn "_"
    cap s = case Text.uncons (Text.toLower s) of
        Nothing      -> ""
        Just (c, cs) -> Text.cons (toUpper c) cs
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise            = c

-- | Resources whose methods take part in the public Kotlin interface
-- and therefore should produce typed byte-array wrappers when their
-- params are byte arrays. Excludes:
--
--   * @Options@ — its setters are JNI-only plumbing called from
--     @ToxCoreImpl@'s init block (the Kotlin @ToxOptions@ data class
--     is the user-facing API).
--   * @Pass_Key@ — its byte-array params (@plaintext@, @ciphertext@,
--     @passphrase@) are generic crypto buffers; wrapping adds noise
--     without static-safety benefit.
publicApiResources :: [Text]
publicApiResources =
    [ "Tox"
    , "Friend"
    , "Conference"
    , "Conference_Peer"
    , "Conference_Offline_Peer"
    , "Group"
    , "Group_Peer"
    , "File"
    , "AV"
    ]

--------------------------------------------------------------------------------
-- Method renames
--------------------------------------------------------------------------------

-- | Override the default camel-cased C method name with a more
-- Kotlin-idiomatic verb-first form. Keyed by the C function name.
-- e.g. @tox_self_set_status@ ('setStatus' makes more sense than the
-- default @selfSetStatus@). Hand-written jvm-toxcore-c API expressed
-- these by hand; codifying them here lets the generator match.
kotlinMethodNameOverride :: Text -> Maybe Text
kotlinMethodNameOverride = \case
    -- Default rule: match C naming order. @tox_friend_add@ →
    -- @friendAdd@, @tox_file_get_file_id@ → @fileGetFileId@,
    -- @toxav_audio_set_bit_rate@ → @audioSetBitRate@. The
    -- previous verb-first overrides (@addFriend@, @setAudioBitRate@)
    -- were inconsistent with the noun-first majority
    -- (@friendByPublicKey@, @friendExists@, every @file_*@) and the
    -- C calling convention. Stripping them produces uniform names
    -- with no special cases.
    --
    -- Crypto exceptions remain because they target the implicit
    -- receiver (the PassKey itself) rather than a sub-resource.
    "tox_pass_key_encrypt"         -> Just "encrypt"
    "tox_pass_key_decrypt"         -> Just "decrypt"
    -- Free-standing crypto helpers strip the @tox_@ prefix because
    -- there's no sub-resource path.
    "tox_get_salt"                 -> Just "getSalt"
    "tox_is_data_encrypted"        -> Just "isDataEncrypted"
    "tox_hash"                     -> Just "hash"
    _                              -> Nothing

--------------------------------------------------------------------------------
-- Bool-return classification
--------------------------------------------------------------------------------

-- | True if a @bool@-returning method with an error type should
-- propagate the @bool@ to the caller (rather than collapsing to
-- @void@ with the error as the only outcome).
--
-- The common shape is "the @bool@ is success\/failure and the error
-- is the failure reason" — that collapses to @void+throws@ on the
-- JVM side. The exception is predicate-shaped methods (@_is_@ \/
-- @_has_@ \/ @_exists@), where the @bool@ /is/ the answer the
-- caller wanted; the error covers an out-of-band failure
-- (FRIEND_NOT_FOUND etc.) that's orthogonal to the boolean result.
--
-- A small explicit allowlist covers the handful of named methods
-- that follow the predicate semantics without matching the
-- predicate naming convention. @tox_friend_get_typing@ is the
-- canonical case: returns whether the friend is typing, but the
-- name is a getter rather than a predicate.
boolReturnsValue :: Text -> Bool
boolReturnsValue n =
    isPredicateName n || n `elem` namedBoolGetters
  where
    isPredicateName s =
        "_is_"    `Text.isInfixOf` s
            || "_has_"   `Text.isInfixOf` s
            || "_exists" `Text.isSuffixOf` s
    namedBoolGetters =
        [ "tox_friend_get_typing"
        ]

--------------------------------------------------------------------------------
-- Public-API filtering
--------------------------------------------------------------------------------

-- | True for interfaces that expose StaticRole methods (free-standing
-- helpers, no Tox handle). Only @ToxCrypto@ has this shape — it's
-- effectively a static-method collection in the hand-written API
-- (clients call @cipher.hash(data)@ etc., where @cipher@ is an
-- otherwise-empty instance). Core and AV always carry instance state,
-- so their static-shaped methods stay off the interface.
-- | True for interfaces that expose @StaticRole@ methods. Only the
-- interfaces that are *intentionally* static-style collections
-- qualify — currently @ToxCrypto@ alone. The bare predicate
-- @interfaceSelfResource == Nothing@ would also fire for empty\/
-- unknown names (used by the Impl emitter when walking the model),
-- which would incorrectly let StaticRole methods through into
-- @ToxCoreImpl@. Keep the explicit allowlist.
interfaceAllowsStatics :: Text -> Bool
interfaceAllowsStatics = \case
    "ToxCrypto" -> True
    _           -> False

-- | The resource that an interface itself represents (its receiver).
-- Methods on the self resource don't need an explicit handle arg —
-- the receiver is implicit. Methods on *other* handle resources (e.g.
-- ToxCrypto operating on @Pass_Key@) take the handle as a parameter.
--
-- @Nothing@ means the interface has no self resource (ToxCrypto is a
-- static-style collection).
--
-- The three JVM-binding interface names map to specific c-toxcore
-- subsystem roots; the binding chooses @ToxCore@ as the Kotlin name
-- for the Tox subsystem (not @Tox@) because @ToxCore@ disambiguates
-- from the older @Tox@ class in the previous binding generation.
-- Listed explicitly because the mapping isn't derivable from the
-- interface name alone (ToxCore→Tox, not Core).
interfaceSelfResource :: Text -> Maybe Text
interfaceSelfResource = \case
    "ToxCore"   -> Just "Tox"
    "ToxAv"     -> Just "AV"
    "ToxCrypto" -> Nothing
    _           -> Nothing

-- | True for "lifecycle root" Constructors that are owned by the
-- Impl class's primary constructor and therefore aren't surfaced on
-- the public interface. @tox_new@ is wired by @ToxCoreImpl(opts)@;
-- @toxav_new@ by @ToxAvImpl(tox)@. Each is the subsystem-root
-- constructor for the corresponding @Tox*Impl@ class.
--
-- The model can't distinguish these from sub-resource constructors
-- like @tox_friend_add@ or @tox_pass_key_derive@ that *do* belong on
-- the public interface — those create owned sub-handles, while the
-- two below create subsystem-level singletons. Pass_Key for instance
-- has parent @Nothing@ but the JVM binding exposes @passKeyDerive@
-- on the ToxCrypto interface as the user-facing factory.
isLifecycleConstructor :: Text -> Bool
isLifecycleConstructor n =
    n `elem` [ "tox_new", "toxav_new" ]

-- | Decide whether a C method name is hidden from the public JVM
-- API. Combines three architectural statements with a small manual
-- carve-out list:
--
-- (1) JVM exposes Tox_Options as a Kotlin data class. Hide every
--     @tox_options_get_*@ (clients read fields off the data class)
--     and the C-builder helpers @tox_options_copy@ \/ @tox_options_default@.
--     Setters for SHandle\/SCallback properties (@log_user_data@) are
--     also hidden — they have no Kotlin representation.
--
-- (2) JVM is event-driven. Hide a getter @tox_<R>_get_<S>@ when a
--     matching callback @tox_<R>_<S>_cb@ exists. Friend\/conference
--     \/group state arrives via the event dispatcher; the polling
--     getters would tempt callers to race the event loop.
--
-- (3) Length getters are redundant with byte-array returns. Hide any
--     @tox_<X>_size@ method when @tox_<X>@ itself exists in the
--     model — the Kotlin @ByteArray.size@ already exposes the
--     length.
--
-- The remaining manual carve-outs are genuinely binding-side choices
-- with no structural signal: internal init helpers
-- (@tox_events_init@), high-level wrapper variants the binding
-- replaces with finer-grained calls (@tox_pass_encrypt@\/@decrypt@),
-- the legacy AV-groupchat bridge (@toxav_get_tox@), and reverse-
-- lookup convenience methods (@tox_file_by_id@\/@tox_group_by_id@)
-- the JVM API doesn't surface.
isHiddenFromPublicApi :: SemanticModel -> Text -> Bool
isHiddenFromPublicApi model n =
    n `elem` manualCarveOuts
    || isOptionsHidden n
    || isFriendPollingGetter model n
    || isPairedSizeGetter model n
    || isCallbackSetterOnOptionsHandle model n
  where
    -- Genuinely binding-side carve-outs. The friend-polling rule
    -- below collapses what used to be a 6-entry friend-state list;
    -- conference and group getters stay exposed (clients legitimately
    -- read current state, the callbacks only fire on changes).
    manualCarveOuts =
        [ "tox_events_init"
        , "tox_pass_encrypt"
        , "tox_pass_decrypt"
        , "toxav_get_tox"
        , "tox_file_by_id"
        , "tox_group_by_id"
        , "tox_self_get_friend_list_size"
        -- @tox_self_get_connection_status@ is the JVM binding's
        -- one self-state polling exception: clients consume the
        -- self-connection-status callback rather than poll. No
        -- hand-written JNI shim exists for this method; the matching
        -- callback delivers the same value.
        , "tox_self_get_connection_status"
        -- @tox_group_get_group_list@ surfaces an enumeration of
        -- joined NGC groups. The binding exposes joined-group state
        -- through the @group_join@ event flow and clients track it
        -- locally; no hand-written shim exists.
        , "tox_group_get_group_list"
        ]

-- | (1) tox_options getter pattern + builder helpers + non-data
-- @set_log_user_data@. The data class subsumes the getter side;
-- the configuration crosses JNI as a serialized proto @Options@
-- message inside @toxNew@, so the C builder (@tox_options_new@) has
-- no JVM surface at all — the @toxNew@ shim builds and frees the
-- @Tox_Options@ internally, mapping its allocation failure to
-- @ToxNewException(MALLOC)@.
isOptionsHidden :: Text -> Bool
isOptionsHidden n =
    "tox_options_get_" `Text.isPrefixOf` n
    || n == "tox_options_new"
    || n == "tox_options_copy"
    || n == "tox_options_default"
    || n == "tox_options_set_savedata_length"

-- | (2) Friend polling getter @tox_friend_get_<S>@ has a matching
-- callback @tox_friend_<S>_cb@. The JVM binding commits to event-
-- driven friend state — friend connection/name/status changes are
-- frequent and the polling getter would race the dispatcher.
--
-- Conference\/group getters (@tox_conference_get_title@,
-- @tox_conference_peer_get_name@, @tox_group_get_name@, …) and
-- @tox_friend_get_typing@ stay exposed — clients legitimately read
-- current state, callbacks only fire on transitions.
isFriendPollingGetter :: SemanticModel -> Text -> Bool
isFriendPollingGetter model methodN =
    case Text.stripPrefix "tox_friend_get_" methodN of
        Just stateName
            | stateName /= "typing"  -- predicate; the bool *is* the answer
            , let cbName = "tox_friend_" <> stateName <> "_cb"
            , cbName `elem` map Apigen.Semantic.cbCName (Apigen.Semantic.callbacks model)
            -> True
        _ -> False

-- | (3) @tox_<X>_size@ where @tox_<X>@ itself is a method in the
-- model. The data accessor's return type already encodes the length
-- (a Kotlin @ByteArray.size@ or @List.size@). Walks every method
-- on every resource to check the companion exists.
isPairedSizeGetter :: SemanticModel -> Text -> Bool
isPairedSizeGetter model methodN =
    case Text.stripSuffix "_size" methodN of
        Just base
            -> base `elem` allMethodNames
        Nothing
            -> False
  where
    allMethodNames =
        [ Apigen.Semantic.methodName m
        | r <- Apigen.Semantic.resources model
        , m <- Apigen.Semantic.methods r
        ]

-- | (1, continued) @tox_options_set_<X>@ where the X property is
-- 'SHandle' or 'SCallback' (currently just @log_user_data@). The
-- Kotlin Options data class has no field for these; the setter is
-- dead at the JVM surface.
isCallbackSetterOnOptionsHandle :: SemanticModel -> Text -> Bool
isCallbackSetterOnOptionsHandle model methodN =
    case Text.stripPrefix "tox_options_set_" methodN of
        Just propRaw ->
            let propName = propRaw
            in case [ Apigen.Semantic.propType p
                    | r <- Apigen.Semantic.resources model
                    , Apigen.Semantic.resourceName r == "Options"
                    , p <- Apigen.Semantic.properties r
                    , Apigen.Semantic.propName p == propName
                    ] of
                (Apigen.Semantic.SHandle _   : _) -> True
                (Apigen.Semantic.SCallback _ : _) -> True
                _                                  -> False
        Nothing -> False

