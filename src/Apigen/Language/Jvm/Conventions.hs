{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | jvm-toxcore-c conventions that the c-toxcore headers don't express
-- (yet). Kept in one place so they can later move into header annotations.
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

import           Apigen.Semantic (SCallbackTypeModel (..), SMethod (..),
                                  SParameter (..), SResource (..), SType (..),
                                  SemanticModel (..))
import qualified Apigen.Semantic
import qualified Data.List       as List
import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text

--------------------------------------------------------------------------------
-- Parameter wrapping
--------------------------------------------------------------------------------

-- | Numeric param names that c-toxcore passes as raw integers but Kotlin
-- wraps in a value class. @toxav_audio_data_cb@ names them @samples@ and
-- @sample_rate@, so those map to the same classes.
paramWrapperClass :: Text -> Maybe Text
paramWrapperClass = (`lookup` paramWrapperTable)

-- | A list, not a @case@, so 'paramNamesForWrapper' can map it backwards.
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

paramNamesForWrapper :: Text -> [Text]
paramNamesForWrapper cls = [n | (n, c) <- paramWrapperTable, c == cls]

-- | How a generated numeric value class backs its single @value@ field.
data ValueClassBacking
    = SignedWithBound  -- ^ @Int@ with a range check, as JNI has no
                       -- unsigned types (Width, Height).
    | UnsignedExact    -- ^ The unsigned type of the C width (Port).
    deriving (Show, Eq)

-- | A value class for a C integer with no typedef. Its bounds come from
-- the C param's width. BitRate, AudioChannels, SamplingRate and
-- AudioLength stay hand-written: their constants are in no header.
data ValueClassSpec = ValueClassSpec
    { vcClassName :: Text
    , vcPackage   :: Text             -- ^ e.g. @"av.data"@
    , vcBacking   :: ValueClassBacking
    , vcDoc       :: Maybe Text
    }

generatedValueClasses :: [ValueClassSpec]
generatedValueClasses =
    [ ValueClassSpec "Width"  "av.data"   SignedWithBound Nothing
    , ValueClassSpec "Height" "av.data"   SignedWithBound Nothing
    , ValueClassSpec "Port"   "core.data" UnsignedExact
        (Just "IP_Port stores an IP datastructure with a port.")
    ]

-- | The wrapper class for a param. An empty method name (callback params)
-- applies only 'paramWrapperClass'.
paramWrapperFor :: Text {- C method name -} -> SParameter -> Maybe Text
paramWrapperFor cMethod p
    | Text.null cMethod = paramWrapperClass pname
    -- Crypto buffers and @savedata@ stay plain @ByteArray@.
    | "tox_pass_"    `Text.isPrefixOf` cMethod = paramWrapperClass pname
    | "tox_options_" `Text.isPrefixOf` cMethod = paramWrapperClass pname
    | otherwise = case paramWrapperClass pname of
        Just cls -> Just cls
        -- Fixed-size byte typedefs are already wrapped by @renderType@.
        Nothing  -> case paramType p of
            SBytes -> deriveByteWrapper cMethod pname
            _      -> Nothing
  where
    pname = paramName p

paramWrapperImport :: Text -> Text
paramWrapperImport cls
    | cls `elem` avVocabularyClassNames = "im.tox.tox4j.av.data." <> cls
    | otherwise                          = "im.tox.tox4j.core.data." <> cls

-- | The wrapper classes in @av.data@. The rest, @Port@ included, are in
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

-- | The wrapper class name for a byte-array param (or return, with an
-- empty param name): @Tox@, the resources the method is on, and what it
-- acts on, or the param name.
--
-- @
-- tox_self_set_name(name)              → ToxName
-- tox_self_set_status_message(...)     → ToxStatusMessage
-- tox_friend_send_message(message)     → ToxFriendMessage
-- tox_friend_add(message)              → ToxFriendMessage
-- tox_friend_message_cb(message)       → ToxFriendMessage
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
        -- The method's words win over the param name when there's no
        -- param, it's generic (@data@), or it ends them (@status_message@
        -- for @message@, keeping it apart from @ToxFriendMessage@).
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

    -- Longest first.
    subsystemPrefixes = ["tox_self_", "tox_pass_", "toxav_", "tox_"]

    capitalize t = case Text.uncons (Text.toLower t) of
        Nothing      -> ""
        Just (c, cs) -> Text.cons (toUpperAscii c) cs

    toUpperAscii c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise            = c

genericParamNames :: [Text]
genericParamNames =
    [ "data", "value", "bytes", "buffer"
    , "plaintext", "ciphertext"
    ]

-- | Splits off the leading resource words. A @self@ after a resource is
-- dropped: @tox_group_self_set_name@ names a group peer's name too.
splitResourceChain :: [Text] -> ([Text], [Text])
splitResourceChain = go []
  where
    go acc [] = (reverse acc, [])
    go acc (t : rest)
        | t `elem` baseResources                = go (t : acc) rest
        | t `elem` subResources, not (null acc) = go (t : acc) rest
        | t == "self",           not (null acc) = go acc rest
        | otherwise                              = (reverse acc, t : rest)
    baseResources = ["friend", "conference", "group", "file"]
    subResources  = ["peer", "offline"]

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

-- | @tox_file_send(filename)@ gives @ToxFilename@, not @ToxFileFilename@.
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

returnWrapperClass :: Text -> SType -> Maybe Text
returnWrapperClass methodName returnType = case returnType of
    SBytes  -> deriveByteWrapper methodName ""
    -- By name suffix: @tox_self_get_udp_port@ returns a 'Port'.
    SUInt _ -> returnAvVocabMatch methodName
    SInt _  -> returnAvVocabMatch methodName
    SSizeT  -> returnAvVocabMatch methodName
    _       -> Nothing

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
    avVocabularyKeys =
        [ "audio_bit_rate", "video_bit_rate", "bit_rate"
        , "sample_count", "samples"
        , "channels", "sampling_rate", "sample_rate"
        , "width", "height", "port"
        ]

returnWrapperImport :: Text -> SType -> Maybe Text
returnWrapperImport name returnType = case returnWrapperClass name returnType of
    Just c  -> Just (paramWrapperImport c)
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Interface annotations & factories
--------------------------------------------------------------------------------

-- | Interface members that no single C function gives, emitted verbatim.
interfaceExtraMembers :: Text -> [Text]
interfaceExtraMembers = \case
    "ToxCore" ->
        [ "    override fun close(): Unit"
        ]
    -- @tox_hash@ isn't on a resource.
    "ToxCrypto" ->
        [ "    fun hash(data: ByteArray): ByteArray"
        ]
    _ -> []

-- | Interface type parameters: each @ToxCrypto@ impl picks its @PassKey@.
interfaceGenericParams :: Text -> [Text]
interfaceGenericParams = \case
    "ToxCrypto" -> ["PassKey"]
    _           -> []

-- | Impl class members with no C counterpart, emitted verbatim.
implExtraMembers :: Text -> [Text]
implExtraMembers = \case
    -- @toxFinalize@ throws on a live instance, and a finalizer's throw
    -- is swallowed, losing the slot. So kill (idempotent) first.
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
    "ToxCryptoImpl" ->
        [ "    override fun hash(data: ByteArray): ByteArray = ToxCryptoJni.toxHash(data)"
        ]
    _ -> []

--------------------------------------------------------------------------------
-- Constants extras
--------------------------------------------------------------------------------

-- | @*Constants@ members that aren't C @#define@s, emitted verbatim.
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
    -- libsodium sizes that @toxencryptsave.h@ doesn't expose.
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

-- | C fields that become one Kotlin field of a sealed type, e.g.
-- @proxy_type@, @proxy_host@ and @proxy_port@ become
-- @proxy: ProxyOptions.Type@.
data SealedGroup = SealedGroup
    { sealedGroupFieldName :: Text    -- ^ On @ToxOptions@.
    , sealedGroupTypeName  :: Text
    , sealedGroupDefault   :: Text
    , sealedGroupCFields   :: [Text]
    } deriving (Show, Eq)

-- | A group is the @Tox_Options@ fields with a common first word @X@,
-- when there are two or more and one is an enum @X_type@.
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

    -- Without the log callback and its user data.
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
    visiblePropType _                             = True

    isQualifiedGroup prefix ps =
        length ps >= 2
        && any (\p -> Apigen.Semantic.propName p == prefix <> "_type"
                   && isEnumType (Apigen.Semantic.propType p)) ps

    isEnumType (Apigen.Semantic.SEnum _) = True
    isEnumType _                         = False

    camelizeCompound p = case compoundPrefixSplit p of
        Just split -> camelCaseLocal split
        Nothing    -> p
    pascalizeCompound p = case compoundPrefixSplit p of
        Just split -> pascalCaseLocal split
        Nothing    -> pascalCaseLocal p

-- | C words that Kotlin splits: @savedata@ is @saveData@.
compoundPrefixSplit :: Text -> Maybe Text
compoundPrefixSplit "savedata" = Just "save_data"
compoundPrefixSplit _          = Nothing

camelCaseLocal :: Text -> Text
camelCaseLocal t = case Text.splitOn "_" t of
    []         -> ""
    (h : rest) -> Text.toLower h <> Text.concat (map capFirstLocal rest)

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

lookupSealedGroup :: SemanticModel -> Text -> Maybe SealedGroup
lookupSealedGroup model pname =
    case filter (\g -> pname `elem` sealedGroupCFields g) (sealedGroups model) of
        (g : _) -> Just g
        []      -> Nothing

-- | A grouped C field's Kotlin path from @options@.
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

-- | @static native@ declarations with no C function, for @*Jni.java@.
jniExtraJavaDecls :: Text -> [Text]
jniExtraJavaDecls = \case
    "ToxCoreJni" ->
        [ "    static native void toxFinalize(int instanceNumber);"
        ]
    "ToxAvJni" ->
        [ "    static native void toxavFinalize(int instanceNumber);"
        ]
    -- Static functions, which the JNI emitter skips.
    "ToxCryptoJni" ->
        [ "    static native byte[] toxGetSalt(byte[] ciphertext) throws ToxGetSaltException;"
        , "    static native boolean toxIsDataEncrypted(byte[] data);"
        , "    static native byte[] toxHash(byte[] data);"
        ]
    _ -> []

jniExtraJavaImports :: Text -> [Text]
jniExtraJavaImports = \case
    "ToxCryptoJni" ->
        [ "im.tox.tox4j.crypto.exceptions.ToxGetSaltException"
        ]
    _ -> []

-- | The @JNIEXPORT@ declarations for 'jniExtraJavaDecls'.
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

-- | KDoc lines for callbacks the C header doesn't document.
callbackKdocExtra :: Text -> [Text]
callbackKdocExtra = \case
    "toxav_audio_data_cb" ->
        [ "The AV conference audio callback. Its parameters have the same"
        , "types as `audioReceiveFrame`'s, under different C names."
        ]
    _ -> []

-- | @(jniMethodName, cFunctionName)@ pairs for @natives.h@ that no
-- resource gives: @tox_hash@ is on @Tox@ but bound in @ToxCryptoJni@.
cppExtraNativeRefs :: Text -> [(Text, Text)]
cppExtraNativeRefs = \case
    "ToxCryptoJni" ->
        [ ("toxHash", "tox_hash")
        ]
    _ -> []

--------------------------------------------------------------------------------
-- Exception class extras
--------------------------------------------------------------------------------

-- | The @<RESOURCE>_NOT_FOUND@ codes of 'syntheticExceptionEnums'.
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

-- | An exception for a public function that fills a byte buffer and
-- returns @bool@ with no @Tox_Err_*@, so failing throws it rather than
-- tripping a value class's size check.
syntheticExceptionFor :: SemanticModel -> Text -> Maybe Text
syntheticExceptionFor model mn =
    case [ enumNm | (m, enumNm) <- syntheticExceptionMethods model, m == mn ] of
        (e : _) -> Just e
        []      -> Nothing

-- | Error enums for 'syntheticExceptionFor', with members from
-- 'exceptionExtraCodes'.
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

-- | @(C method name, synthetic error enum name)@ pairs.
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
    isByteOutput _                                 = False

    deriveEnumName mn = case Text.stripPrefix "tox_" mn of
        Just rest -> "Tox_Err_" <> Text.intercalate "_" (map pascalCaseLocal (Text.splitOn "_" rest))
        Nothing   -> "Tox_Err_" <> pascalCaseLocal mn

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

-- | Whether a public method throws this error enum. Both the Kotlin
-- exceptions and the C++ @HANDLE@s use it: a @HANDLE@ for a missing
-- Kotlin class would fail its class lookup at runtime.
errorEnumIsReachable :: Apigen.Semantic.SemanticModel -> Text -> Bool
errorEnumIsReachable model name = name `elem`
    [ ref
    | r <- Apigen.Semantic.resources model
    , m <- Apigen.Semantic.methods r
    , not (isHiddenFromPublicApi model (Apigen.Semantic.methodName m))
    , Just ref <- [Apigen.Semantic.methodErrorType m]
    ]

-- | The @Tox_Options@ fields in the proto @Options@ message, for the
-- proto, the Kotlin builder and the C++ setter alike.
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

-- | A non-error enum no signature uses: C passes it as @uint32_t@ so
-- clients can add values (@Tox_File_Kind@), so Kotlin gets an @object@
-- of @const val@s instead of an @enum class@.
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
        Apigen.Semantic.SEnum n           -> n `elem` names
        Apigen.Semantic.SList t'          -> mentionsEnum t'
        Apigen.Semantic.SFixedList t' _ _ -> mentionsEnum t'
        _                                 -> False

-- | @(className, kdoc)@ of the wrapper classes for variable-size byte
-- arrays in the public API.
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

-- | Resources whose byte arrays get wrapper classes: not @Options@
-- (Kotlin has @ToxOptions@) or @Pass_Key@ (generic crypto buffers).
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

-- | Kotlin names that aren't the camel-cased C name.
kotlinMethodNameOverride :: Text -> Maybe Text
kotlinMethodNameOverride = \case
    -- These act on the PassKey itself.
    "tox_pass_key_encrypt"         -> Just "encrypt"
    "tox_pass_key_decrypt"         -> Just "decrypt"
    "tox_get_salt"                 -> Just "getSalt"
    "tox_is_data_encrypted"        -> Just "isDataEncrypted"
    "tox_hash"                     -> Just "hash"
    _                              -> Nothing

--------------------------------------------------------------------------------
-- Bool-return classification
--------------------------------------------------------------------------------

-- | Whether a @bool@ return is the answer (as in @_is_@), not success,
-- which becomes @void@ and a throw.
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

-- | Interfaces that expose static functions. Not
-- @interfaceSelfResource == Nothing@, which also holds for unknown names.
interfaceAllowsStatics :: Text -> Bool
interfaceAllowsStatics = \case
    "ToxCrypto" -> True
    _           -> False

-- | The resource an interface stands for, whose handle its methods
-- don't take.
interfaceSelfResource :: Text -> Maybe Text
interfaceSelfResource = \case
    "ToxCore"   -> Just "Tox"
    "ToxAv"     -> Just "AV"
    "ToxCrypto" -> Nothing
    _           -> Nothing

-- | Constructors that are the Impl classes' own, not interface methods.
-- The model can't tell them from ones like @tox_pass_key_derive@.
isLifecycleConstructor :: Text -> Bool
isLifecycleConstructor n =
    n `elem` [ "tox_new", "toxav_new" ]

-- | C functions the JVM API leaves out.
isHiddenFromPublicApi :: SemanticModel -> Text -> Bool
isHiddenFromPublicApi model n =
    n `elem` manualCarveOuts
    || isOptionsHidden n
    || isFriendPollingGetter model n
    || isPairedSizeGetter model n
    || isCallbackSetterOnOptionsHandle model n
  where
    manualCarveOuts =
        [ "tox_events_init"
        , "tox_pass_encrypt"
        , "tox_pass_decrypt"
        , "toxav_get_tox"
        , "tox_file_by_id"
        , "tox_group_by_id"
        , "tox_self_get_friend_list_size"
        -- Its callback gives the same value.
        , "tox_self_get_connection_status"
        -- Clients track joined groups from @group_join@ events.
        , "tox_group_get_group_list"
        ]

-- | Kotlin has @ToxOptions@, which @toxNew@ gets as a proto message.
isOptionsHidden :: Text -> Bool
isOptionsHidden n =
    "tox_options_get_" `Text.isPrefixOf` n
    || n == "tox_options_new"
    || n == "tox_options_copy"
    || n == "tox_options_default"
    || n == "tox_options_set_savedata_length"

-- | A @tox_friend_get_X@ with a @tox_friend_X_cb@: friend state comes
-- from events, and polling would race them.
isFriendPollingGetter :: SemanticModel -> Text -> Bool
isFriendPollingGetter model methodN =
    case Text.stripPrefix "tox_friend_get_" methodN of
        Just stateName
            | stateName /= "typing"
            , let cbName = "tox_friend_" <> stateName <> "_cb"
            , cbName `elem` map Apigen.Semantic.cbCName (Apigen.Semantic.callbacks model)
            -> True
        _ -> False

-- | A @tox_X_size@ with a @tox_X@, whose Kotlin result has a size.
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

-- | Setters for options @ToxOptions@ has no field for (@log_user_data@).
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
                _                                 -> False
        Nothing -> False

