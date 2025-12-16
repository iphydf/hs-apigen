{-# LANGUAGE OverloadedStrings #-}

-- | Kotlin slice of the JVM binding generator.
--
-- Produces files under @lib/src/main/kotlin/im/tox/tox4j/...@. First slice
-- covers constants; data classes, enums, exception classes, callback
-- interfaces, public-API interfaces, @*Impl.kt@ and @ToxCoreEventDispatch.kt@
-- will be added incrementally.
module Apigen.Language.Jvm.Kotlin (generate) where

import           Apigen.Language.Jvm.Conventions   (SealedGroup (..),
                                                    ValueClassBacking (..),
                                                    ValueClassSpec (..),
                                                    boolReturnsValue,
                                                    callbackKdocExtra,
                                                    constantsExtraMembers,
                                                    errorEnumIsReachable,
                                                    exceptionExtraCodes,
                                                    generatedValueClasses,
                                                    interfaceExtraMembers,
                                                    interfaceGenericParams,
                                                    interfaceSelfResource,
                                                    isHiddenFromPublicApi,
                                                    isOpenEnum,
                                                    lookupSealedGroup,
                                                    paramNamesForWrapper,
                                                    paramWrapperFor,
                                                    paramWrapperImport,
                                                    returnWrapperClass,
                                                    returnWrapperImport,
                                                    syntheticExceptionEnums,
                                                    varByteWrappers)
import           Apigen.Language.Jvm.Kdoc          (transformKdoc)
import           Apigen.Parser.Docs                (Docs (..))
import           Apigen.Language.Jvm.Kotlin.Common (camelCase,
                                                    isSkippableMethodFor,
                                                    kotlinMethodName,
                                                    kotlinPropertyName,
                                                    kotlinNameOf, pascalCase,
                                                    pathIdArgs, pathIdArgsFor,
                                                    renderMethodParamType,
                                                    renderParamType, renderType)
import           Apigen.Semantic                   (SCallbackTypeModel (..),
                                                    SConstantModel (..),
                                                    SEnumMember (..),
                                                    SEnumModel (..),
                                                    SEvent (..),
                                                    SIdTypeModel (..),
                                                    SMethod (..),
                                                    SMethodRole (..),
                                                    SParameter (..),
                                                    SProperty (..),
                                                    SResource (..),
                                                    SResourceType (..),
                                                    SType (..),
                                                    SemanticModel (..))
import qualified Data.Char                         as Char
import qualified Data.List                         as List
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Numeric                           (showHex)

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model =
    [ ("lib/src/main/kotlin/im/tox/tox4j/core/ToxCoreConstants.kt", coreConstants (funcDocs docs) model)
    , ("lib/src/main/kotlin/im/tox/tox4j/crypto/ToxCryptoConstants.kt", cryptoConstants (funcDocs docs) model)
    ]
        ++ map renderEnum (filter (not . isErrorOrSpecialEnum) (enums model))
        ++ map (renderOpenEnum model docs) (filter (isOpenEnum model) (enums model))
        ++ map (renderException docs model)
               (filter isReachableErrorEnum (enums model) ++ syntheticExceptionEnums model)
        ++ map renderIdNewtype (idTypes model)
        ++ map (renderValueClass model) generatedValueClasses
        ++ map renderFixedBytes (arrayTypes model)
        ++ map renderVarBytes (varByteWrappers model)
        ++ map (renderCallback (funcDocs docs) model) (callbacks model)
        ++ aggregateListeners (callbacks model)
        ++ publicApi (funcDocs docs) model
        ++ toxOptionsClass model
  where
    isErrorEnum e =
        "Tox_Err_" `Text.isPrefixOf` enumName e
            || "Toxav_Err_" `Text.isPrefixOf` enumName e
    -- @Tox_Err_*@ / @Toxav_Err_*@ enums fold into exception classes'
    -- @Code@ enums and are emitted by the exception generator instead.
    -- Open enumerations ('isOpenEnum' — today just @Tox_File_Kind@)
    -- are emitted by 'renderOpenEnum' as an extensible @object@ of
    -- @Int@ constants: a closed @enum class@ would make the
    -- client-defined values the C API explicitly allows
    -- unrepresentable.
    isErrorOrSpecialEnum e =
        isErrorEnum e
            || isOpenEnum model e

    -- Skip exception classes for enums that no exposed method actually
    -- uses. An enum like @Tox_Err_File_By_Id@ is dead if every method
    -- referencing it is in @isHiddenFromPublicApi@; emitting it
    -- ships an exception class with no throw site, which is just
    -- confusing API surface. Predicate lives in 'Conventions' so the
    -- C++ side's @HANDLE@ filter ('Apigen.Language.Jvm.Cpp.coreErrors'
    -- etc.) shares it — divergence here would leak either a Kotlin
    -- class without a throw site or a C++ HANDLE without a Kotlin
    -- class. (Hand-thrown exceptions like @ToxConferenceGetIdException@
    -- — where the C function has no error enum and the Kotlin class
    -- is synthesized via 'syntheticExceptionEnums' — are unaffected:
    -- their enums come in via the synthetic-enums append below, not
    -- via this filter.)
    isReachableErrorEnum e =
        isErrorEnum e && errorEnumIsReachable model (enumName e)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Constants from @tox.h@/@toxav.h@ go into @ToxCoreConstants@.
coreConstants :: Map Text Text -> SemanticModel -> Text
coreConstants docs model =
    renderConstantsObject model docs "core" "ToxCoreConstants" coreOnly
  where
    coreOnly = List.sortOn kotlinName (filter (not . isCryptoConstant . constantName) (constants model))

-- | Constants from @toxencryptsave.h@ go into @ToxCryptoConstants@.
-- Names also drop the @PASS_@ prefix so @TOX_PASS_KEY_LENGTH@ becomes
-- @ToxCryptoConstants.KEY_LENGTH@ (the @crypto@ package already
-- carries the @pass key@ context).
cryptoConstants :: Map Text Text -> SemanticModel -> Text
cryptoConstants docs model =
    renderConstantsObject model docs "crypto" "ToxCryptoConstants" cryptoOnly
  where
    cryptoOnly = List.sortOn kotlinName (map stripPassPrefix (filter (isCryptoConstant . constantName) (constants model)))
    stripPassPrefix c = case Text.stripPrefix "TOX_PASS_" (constantName c) of
        Just rest -> c {constantName = "TOX_" <> rest}
        Nothing -> c

-- | Render a Kotlin @object Foo { const val ... }@ block. Each entry
-- is emitted with its doxygen kdoc (looked up by the *original* C
-- macro name — we keep that on the 'SConstantModel' before any
-- prefix stripping). Successive entries are blank-line-separated
-- when they carry kdoc; a kdoc-less constant uses no separator so
-- the block stays compact when no docs are available.
renderConstantsObject :: SemanticModel -> Map Text Text -> Text -> Text -> [SConstantModel] -> Text
renderConstantsObject model docs pkg objName entries =
    Text.unlines $
        [ "package im.tox.tox4j." <> pkg
        , ""
        ]
            ++ imports
            ++ ["object " <> objName <> " {"]
            ++ drop 1 (concatMap renderEntry entries ++ extras)
            ++ ["}"]
  where
    imports = []
    extras = case constantsExtraMembers objName of
        []  -> []
        es  -> "" : es
    renderEntry c =
        let kdoc = case Map.lookup (constantName c) docs of
                Just raw -> renderMemberKdoc 4 (transformKdoc model raw)
                Nothing  -> []
            line =
                "    const val "
                    <> kotlinName c
                    <> " = "
                    <> Text.pack (show (constantValue c))
        in case kdoc of
            [] -> ["" , line]
            ks -> ["" ] ++ ks ++ [line]

-- | Strip the @TOX_@ / @TOXAV_@ subsystem prefix. The Kotlin convention
-- carries the package (@im.tox.tox4j.core@) as the namespace, so the
-- @TOX_@ on the C name would be redundant: @TOX_PUBLIC_KEY_SIZE@ becomes
-- @ToxCoreConstants.PUBLIC_KEY_SIZE@.
kotlinName :: SConstantModel -> Text
kotlinName c =
    case Text.stripPrefix "TOX_" (constantName c) of
        Just rest -> rest
        Nothing -> fromMaybe (constantName c) (Text.stripPrefix "TOXAV_" (constantName c))

-- | True when the constant originates in @toxencryptsave.h@. Identified
-- by the @TOX_PASS_@ / @TOX_HASH_@ C-name prefixes.
--
-- This will be replaced by a file-tracked categorisation once
-- @SConstantModel@ carries the originating header.
isCryptoConstant :: Text -> Bool
isCryptoConstant n =
    "TOX_PASS_" `Text.isPrefixOf` n || n == "TOX_HASH_LENGTH"

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

-- | Emit one Kotlin file per 'SEnumModel'. Core enums go under
-- @im.tox.tox4j.core.enums@; ToxAV enums under @im.tox.tox4j.av.enums@.
-- The Kotlin enum's member order matches the C order — and therefore the
-- ordinal mapping that the generated C++ @Enum::ordinal/valueOf@ switch
-- relies on.
renderEnum :: SEnumModel -> (FilePath, Text)
renderEnum e = (path, body)
  where
    -- The semantic model keeps the C typedef name verbatim — that's
    -- either uppercase (@TOXAV_CALL_CONTROL@) or PascalCase
    -- (@Toxav_Call_Control@) depending on how the header declares it.
    -- Either form should route to the @av@ package.
    isAv =
        "TOXAV_" `Text.isPrefixOf` enumName e
            || "Toxav_" `Text.isPrefixOf` enumName e
    pkg = if isAv then "av.enums" else "core.enums"
    className = kotlinEnumName (enumName e)
    path = "lib/src/main/kotlin/im/tox/tox4j/" <> Text.unpack (Text.replace "." "/" pkg) <> "/" <> Text.unpack className <> ".kt"
    body =
        Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "enum class " <> className <> " {"
            ]
                ++ memberLines
                ++ ["}"]
    memberLines = case enumMembers e of
        [] -> []
        members ->
            let names = map enumMemberName members
                go (n, isLast) = "    " <> n <> (if isLast then "," else ",")
            in -- Kotlin requires a comma after the last enumerator
               -- when followed by `;` or `}`; we just always end with `,`.
               map go (zip names (replicate (length names - 1) False ++ [True]))

-- | Open enumerations ('Conventions.isOpenEnum') become an @object@
-- of @const val@ Ints rather than a closed @enum class@: the C API
-- types these parameters @uint32_t@ precisely so clients can pass
-- values the header doesn't name. The real C values come from the
-- model ('enumMemberValue') — positional indices would silently
-- diverge if the header ever skipped a value.
renderOpenEnum :: SemanticModel -> Docs -> SEnumModel -> (FilePath, Text)
renderOpenEnum model docs e = (path, body)
  where
    isAvEnum =
        "TOXAV_" `Text.isPrefixOf` enumName e
            || "Toxav_" `Text.isPrefixOf` enumName e
    pkg = if isAvEnum then "av.enums" else "core.enums"
    className = kotlinEnumName (enumName e)
    path = "lib/src/main/kotlin/im/tox/tox4j/" <> Text.unpack (Text.replace "." "/" pkg) <> "/" <> Text.unpack className <> ".kt"
    body =
        Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "object " <> className <> " {"
            ]
                ++ List.intercalate [""] (map renderMember (enumMembers e))
                ++ ["}"]
    renderMember m =
        memberKdoc m
            ++ [ "    const val " <> enumMemberName m <> " = "
                    <> Text.pack (show (enumMemberValue m))
               ]
    memberKdoc m =
        case Map.lookup (enumName e, enumMemberCName m) (enumMemberDocs docs) of
            Nothing -> []
            Just raw ->
                let txt = Text.strip (transformKdoc model raw)
                in if Text.null txt
                    then []
                    else if "\n" `Text.isInfixOf` txt
                        then ["    /**"]
                            ++ map (\ln -> if Text.null ln then "     *" else "     * " <> ln)
                                   (Text.splitOn "\n" txt)
                            ++ ["     */"]
                        else ["    /** " <> txt <> " */"]

-- | Convert a C enum name like @Tox_Connection@ / @TOXAV_CALL_CONTROL@
-- into the Kotlin PascalCase class name (@ToxConnection@ /
-- @ToxavCallControl@). The existing tox4j convention is "ToxAV" lower-cased
-- to "Toxav" in the Kotlin class name.
kotlinEnumName :: Text -> Text
kotlinEnumName n
    | "TOXAV_" `Text.isPrefixOf` n =
        "Toxav" <> pascalCase (Text.drop (Text.length "TOXAV_") n)
    | otherwise = pascalCase n

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | Hand-curated crypto error enum names. These come from
-- @toxencryptsave.h@ but the semantic model doesn't track originating
-- header, so we list them explicitly to route the exception class into
-- the @crypto@ package.
cryptoErrorNames :: [Text]
cryptoErrorNames =
    [ "Tox_Err_Encryption"
    , "Tox_Err_Decryption"
    , "Tox_Err_Key_Derivation"
    , "Tox_Err_Get_Salt"
    ]

-- | Emit one Kotlin file per @Tox_Err_*@ / @Toxav_Err_*@ enum, as a
-- class with an inner @Code@ enum. The C ABI maps each non-OK error
-- value to a matching exception throw on the C++ side (using
-- @failure_case@ in @generated/errors.cpp@); Kotlin clients catch the
-- exception and read @code@ to discriminate. The C++ mapping is
-- name-based, so the order of @Code@ members is purely cosmetic — we
-- preserve the C source order to keep the file deterministic.
renderException :: Docs -> SemanticModel -> SEnumModel -> (FilePath, Text)
renderException docs model e = (path, body)
  where
    isAv = "Toxav_Err_" `Text.isPrefixOf` enumName e
    isCrypto = enumName e `elem` cryptoErrorNames
    pkg
        | isAv = "av.exceptions"
        | isCrypto = "crypto.exceptions"
        | otherwise = "core.exceptions"
    className = exceptionClassName e
    path =
        "lib/src/main/kotlin/im/tox/tox4j/"
            <> Text.unpack (Text.replace "." "/" pkg)
            <> "/"
            <> Text.unpack className
            <> ".kt"
    -- All members except the OK case, paired with the C name so we
    -- can look up the per-member doxygen comment. Synthetic codes
    -- (for exception enums fabricated by 'syntheticExceptionEnums')
    -- come from 'exceptionExtraCodes' — they have no C counterpart
    -- but the C++ shim throws them.
    codeMembers = [(Just (enumMemberCName m), enumMemberName m) | m <- enumMembers e, enumMemberName m /= "OK"]
        ++ [(Nothing, name) | (name, _) <- exceptionExtraCodes model (enumName e)]
    body =
        Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "import im.tox.tox4j.exceptions.ToxException"
            , ""
            , "class " <> className <> " : ToxException {"
            , "    enum class Code {"
            ]
                ++ List.intercalate [""] (map renderCodeMember codeMembers)
                ++ [ "    }"
                   , ""
                   , "    constructor(code: Code) : this(code, \"\")"
                   , ""
                   , "    constructor(code: Code, message: String) : super(code, message)"
                   , "}"
                   ]

    -- | One enum-member block. The C name is used to look up the
    -- doxygen comment from the parsed AST; entries with no C
    -- counterpart (extra JVM-only codes) pass @Nothing@ and pick
    -- up their kdoc from the 'exceptionExtraCodes' table instead.
    renderCodeMember :: (Maybe Text, Text) -> [Text]
    renderCodeMember (mCName, kname) =
        let raw = case mCName of
                Just cname -> Map.lookup (enumName e, cname) (enumMemberDocs docs)
                Nothing    -> lookup kname extras >>= id
            body' = case raw of
                Nothing  -> ""
                Just txt -> transformKdoc model txt
            stripped = Text.strip body'
        in if Text.null stripped
            then ["        " <> kname <> ","]
            else if "\n" `Text.isInfixOf` stripped
                then renderMultilineDoc body' ++ ["        " <> kname <> ","]
                else ["        /** " <> stripped <> " */", "        " <> kname <> ","]
    extras = exceptionExtraCodes model (enumName e)

    renderMultilineDoc raw =
        let stripped = Text.dropWhileEnd (== '\n') raw
            bodyLines = Text.splitOn "\n" stripped
        in ["        /**"]
            ++ map (\l -> if Text.null l
                            then "         *"
                            else "         * " <> l) bodyLines
            ++ ["         */"]

-- | @Tox_Err_Friend_Add@ -> @ToxFriendAddException@;
-- @Toxav_Err_Bit_Rate_Set@ -> @ToxavBitRateSetException@.
exceptionClassName :: SEnumModel -> Text
exceptionClassName e =
    case Text.stripPrefix "Tox_Err_" (enumName e) of
        Just rest -> "Tox" <> pascalCase rest <> "Exception"
        Nothing -> case Text.stripPrefix "Toxav_Err_" (enumName e) of
            Just rest -> "Toxav" <> pascalCase rest <> "Exception"
            Nothing -> enumName e -- fallback, shouldn't happen

--------------------------------------------------------------------------------
-- Data classes (value classes for typedef-newtypes)
--------------------------------------------------------------------------------

-- | Integer-backed typedef-newtype, e.g.
-- @typedef uint32_t Tox_Friend_Number;@ -> @@JvmInline value class ToxFriendNumber(val value: Int)@.
--
-- JNI doesn't natively support unsigned types, so every integer width
-- collapses to a Kotlin @Int@ — matching the existing hand-written
-- binding (and Rust, where rs-toxcore-c uses raw u32 only inside the
-- crate and exposes signed widths at the boundary where needed).
renderIdNewtype :: SIdTypeModel -> (FilePath, Text)
renderIdNewtype i =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    className = "Tox" <> pascalCase (idName i)
    body =
        Text.unlines
            [ "package im.tox.tox4j.core.data"
            , ""
            , "import kotlin.jvm.JvmInline"
            , ""
            , "@JvmInline"
            , "value class " <> className <> "("
            , "    val value: Int,"
            , ")"
            ]

-- | Numeric value class that c-toxcore exposes as a raw integer with no
-- typedef ('Apigen.Language.Jvm.Conventions.ValueClassSpec'). The C
-- scalar width comes from the model — the params named in
-- 'paramNamesForWrapper' — so the emitted bound tracks a header
-- re-typing instead of going stale. An empty or inconsistent set of
-- feeding params is a hard generator error: the spec has drifted away
-- from the API it describes.
renderValueClass :: SemanticModel -> ValueClassSpec -> (FilePath, Text)
renderValueClass model spec =
    ( "lib/src/main/kotlin/im/tox/tox4j/"
        <> Text.unpack (Text.replace "." "/" pkg) <> "/" <> Text.unpack cls <> ".kt"
    , body
    )
  where
    cls = vcClassName spec
    pkg = vcPackage spec
    bits = valueClassWidthBits model cls
    maxHex = "0x" <> Text.toUpper (Text.pack (showHex ((2 :: Integer) ^ bits - 1) ""))
    classDoc = maybe [] (\d -> ["/** " <> d <> " */"]) (vcDoc spec)
    body = Text.unlines $ case vcBacking spec of
        SignedWithBound ->
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "import kotlin.jvm.JvmInline"
            , ""
            ]
                ++ classDoc
                ++
            [ "@JvmInline"
            , "value class " <> cls <> "("
            , "    val value: Int,"
            , ") {"
            , "    init {"
            , "        // c-toxcore types this as uint" <> tInt bits <> "_t, so a value outside"
            , "        // [0, MAX] would silently truncate as it crosses JNI. Reject up front."
            , "        require(value in 0..MAX) { \"" <> cls <> " must fit in uint"
                <> tInt bits <> "_t (0..$MAX), got $value\" }"
            , "    }"
            , ""
            , "    companion object {"
            , "        const val MAX: Int = " <> maxHex
            , ""
            , "        /**"
            , "         * Coerce a raw int into a valid [" <> cls <> "], clamping to"
            , "         * `[0, MAX]`. Use this on the receive path (proto / IPC /"
            , "         * untrusted input) where an out-of-range value should silently"
            , "         * degrade rather than throw — direct construction via"
            , "         * `" <> cls <> "(value)` is the right choice on the send path where"
            , "         * bad input is a programming error."
            , "         */"
            , "        fun fromInt(value: Int): " <> cls <> " = " <> cls <> "(value.coerceIn(0, MAX))"
            , "    }"
            , "}"
            ]
        UnsignedExact ->
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "import kotlin.jvm.JvmInline"
            , ""
            ]
                ++ classDoc
                ++
            [ "@JvmInline"
            , "value class " <> cls <> "("
            , "    val value: " <> unsignedKotlinType bits <> ","
            , ")"
            ]

-- | The C scalar width (in bits) feeding a value class, read off the
-- model params that 'paramNamesForWrapper' associates with it. All
-- such params must agree; none, or a disagreement, is a generator
-- error (the spec no longer matches the API).
valueClassWidthBits :: SemanticModel -> Text -> Int
valueClassWidthBits model cls =
    case List.nub widths of
        [b] -> b
        []  -> error $ "renderValueClass: no model param feeds value class "
                    <> Text.unpack cls <> " (expected one of: "
                    <> show (paramNamesForWrapper cls) <> ")"
        bs  -> error $ "renderValueClass: inconsistent C widths " <> show bs
                    <> " for value class " <> Text.unpack cls
  where
    names  = paramNamesForWrapper cls
    widths = [ b | p <- allParams model, paramName p `elem` names
                 , b <- case paramType p of SUInt n -> [n]; SInt n -> [n]; _ -> [] ]

-- | Every 'SParameter' in the model: method inputs, high-level event
-- params, and raw callback typedef params.
allParams :: SemanticModel -> [SParameter]
allParams model =
       concatMap inputs (concatMap methods (resources model))
    ++ concatMap eventParams (concatMap events (resources model))
    ++ concatMap cbParams (callbacks model)

-- | The unsigned Kotlin type that exactly covers a C integer width.
unsignedKotlinType :: Int -> Text
unsignedKotlinType bits = case bits of
    8  -> "UByte"
    16 -> "UShort"
    32 -> "UInt"
    64 -> "ULong"
    n  -> error $ "renderValueClass: no unsigned Kotlin type for width " <> show n

tInt :: Int -> Text
tInt = Text.pack . show

-- | Fixed-byte typedef-newtype, e.g.
-- @typedef uint8_t Tox_Public_Key[TOX_PUBLIC_KEY_SIZE];@ ->
-- @class ToxPublicKey(val value: ByteArray)@ with content-based
-- @equals@ \/ @hashCode@ \/ @toString@.
--
-- Plain @class@ rather than @\@JvmInline value class@: a value
-- class over @ByteArray@ inherits @ByteArray@'s reference equality
-- (and can't override @equals@), so two instances with identical
-- bytes compare unequal. Public-key types are obvious Map keys,
-- and the silent miscompare is severe enough to outweigh the lost
-- inline allocation. The boundary cost is one JVM allocation per
-- wrapper; the correctness gain is that @Map<ToxPublicKey, …>@
-- behaves as written.
renderFixedBytes :: (Text, Text) -> (FilePath, Text)
renderFixedBytes (sizeConst, semanticName) =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    className = "Tox" <> pascalCase semanticName
    -- The C typedef pins a fixed byte length; emit a corresponding
    -- @require@ in the constructor so misuse fails at the boundary
    -- rather than further down inside JNI (where the assertion is a
    -- fatal abort).
    --
    -- Most size constants land in @ToxCoreConstants@; the crypto
    -- subsystem owns @HASH_LENGTH@ and @PASS_SALT_LENGTH@.
    (constsObj, constsImport) = case sizeConst of
        "TOX_HASH_LENGTH"      -> ("ToxCryptoConstants", "im.tox.tox4j.crypto.ToxCryptoConstants")
        "TOX_PASS_SALT_LENGTH" -> ("ToxCryptoConstants", "im.tox.tox4j.crypto.ToxCryptoConstants")
        _                      -> ("ToxCoreConstants",   "im.tox.tox4j.core.ToxCoreConstants")
    sizeRef = constsObj <> "." <> stripToxPrefix sizeConst
    stripToxPrefix n
        | "TOX_PASS_" `Text.isPrefixOf` n = Text.drop 9 n  -- drop "TOX_PASS_"
        | "TOX_" `Text.isPrefixOf` n      = Text.drop 4 n
        | otherwise                        = n
    body =
        Text.unlines $
            [ "package im.tox.tox4j.core.data"
            , ""
            , "import " <> constsImport
            , ""
            , "class " <> className <> "("
            , "    val value: ByteArray,"
            , ") {"
            , "    init {"
            , "        require(value.size == " <> sizeRef <> ") {"
            , "            \"" <> className <> " must be ${" <> sizeRef <> "} bytes, got ${value.size}\""
            , "        }"
            , "    }"
            , ""
            ] ++ equalsLines className ++
            [ ""
            , "    override fun hashCode(): Int = value.contentHashCode()"
            , ""
            , "    override fun toString(): String = \"" <> className <> "(<${value.size} bytes>)\""
            , "}"
            ]

-- | Variable-size @ByteArray@-wrapping typedef-newtype. Same shape
-- as 'renderFixedBytes' minus the @require(value.size == K)@ block
-- (no fixed length to enforce). Driven by 'varByteWrappers' rather
-- than 'arrayTypes' since these wrappers have no c-toxcore @typedef@
-- backing — they're hand-curated JVM-side conventions for the
-- @(const uint8_t *, size_t)@ parameters that cross the JNI
-- boundary.
-- | Adaptive content-equals line. Single-line when it fits the 140
-- column ktlint limit; wrapped onto two lines otherwise. ktlint's
-- @standard:function-signature@ rule fires both ways: emitting a
-- two-line body for a class that fits unwrapped is also a violation.
equalsLines :: Text -> [Text]
equalsLines className =
    let single = "    override fun equals(other: Any?): Boolean = this === other || (other is "
              <> className <> " && value.contentEquals(other.value))"
    in if Text.length single <= 140
         then [single]
         else [ "    override fun equals(other: Any?): Boolean ="
              , "        this === other || (other is " <> className <> " && value.contentEquals(other.value))"
              ]

renderVarBytes :: (Text, Text) -> (FilePath, Text)
renderVarBytes (className, kdoc) =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    body =
        Text.unlines $
            [ "package im.tox.tox4j.core.data"
            , ""
            , "/**"
            , " * " <> kdoc
            , " *"
            , " * Plain `class` rather than `@JvmInline value class`: a value class"
            , " * over `ByteArray` inherits reference equality (and can't override"
            , " * `equals`), so two instances with identical bytes compare unequal."
            , " * `toString` omits payload bytes — these can carry user-facing text"
            , " * or sensitive material that shouldn't land in logs."
            , " */"
            , "class " <> className <> "("
            , "    val value: ByteArray,"
            , ") {"
            ] ++ equalsLines className ++
            [ ""
            , "    override fun hashCode(): Int = value.contentHashCode()"
            , ""
            , "    override fun toString(): String = \"" <> className <> "(<${value.size} bytes>)\""
            , "}"
            ]

--------------------------------------------------------------------------------
-- Callback interfaces
--------------------------------------------------------------------------------


-- | One Kotlin interface per @SCallbackTypeModel@. AV callbacks (whose
-- C name starts with @toxav_@) go under @av.callbacks@; the rest under
-- @core.callbacks@. The trailing @_cb@ on the C name is stripped before
-- generating the Kotlin @<Name>Callback@ interface name.
renderCallback :: Map Text Text -> SemanticModel -> SCallbackTypeModel -> (FilePath, Text)
renderCallback funcs model cb = (path, body)
  where
    pkg = if isAv cb then "av.callbacks" else "core.callbacks"
    interfaceName = callbackInterfaceName cb
    path =
        "lib/src/main/kotlin/im/tox/tox4j/"
            <> Text.unpack (Text.replace "." "/" pkg)
            <> "/"
            <> Text.unpack interfaceName
            <> ".kt"

    -- Drop the receiver and user_data; everything else is a user-facing param.
    -- SHandle params are either the receiver (Tox / ToxAV / AV) or the
    -- void* user_data closure — both are wired by the dispatcher rather
    -- than passed through to the listener.
    userParams = map convertParam (filter isUserParam (cbParams cb))
    isUserParam p = case paramType p of
        SHandle _ -> False
        _ -> True
    -- Toxav callbacks use raw @uint32_t friend_number@ rather than the
    -- @Tox_Friend_Number@ typedef. Wrap them by name so the AV callback
    -- signatures match what core uses.
    convertParam p
        | paramName p == "friend_number" && isUInt (paramType p) =
            p {paramType = SResourceId "Friend_Number"}
        | otherwise = p
    isUInt (SUInt _) = True
    isUInt (SInt _) = True
    isUInt _ = False

    methodName = callbackMethodName cb

    -- Kdoc on the callback method. The C doxygen lives on the
    -- @typedef void tox_foo_cb(…)@ declaration; we attach it to the
    -- generated method so @param entries align with the rendered
    -- signature. Class-level summaries (the "This event is triggered
    -- when…" lines hand-written has) are not derived from C and are
    -- left out — unless 'callbackKdocExtra' has a hand-curated note
    -- (currently used for the audio_data naming-divergence).
    methodKdoc = case (Map.lookup (cbCName cb) funcs, callbackKdocExtra (cbCName cb)) of
        (Nothing, [])     -> []
        (Nothing, extras) -> renderMemberKdoc 4 (Text.intercalate "\n" extras)
        (Just raw, [])    -> renderMemberKdoc 4 (transformKdoc model raw)
        (Just raw, extras) ->
            renderMemberKdoc 4
                (transformKdoc model raw <> "\n\n" <> Text.intercalate "\n" extras)

    body =
        Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            ]
                ++ importsFor (cbCName cb) model userParams
                ++ [ "interface " <> interfaceName <> "<ToxCoreState> {"
                   ]
                ++ methodKdoc
                ++ [ "    fun " <> methodName <> "("
                   ]
                ++ [ "        " <> paramKotlinName cb p <> ": " <> renderMethodParamType model (cbCName cb) p <> ","
                   | p <- userParams
                   ]
                ++ [ "        state: ToxCoreState,"
                   , "    ): ToxCoreState = state"
                   , "}"
                   ]

-- | Render a transformed kdoc body as a member-level kdoc block,
-- indented by @col@ spaces. Used for interface-method, callback, and
-- enum-member docs.
renderMemberKdoc :: Int -> Text -> [Text]
renderMemberKdoc col body
    | Text.null stripped = []
    | "\n" `Text.isInfixOf` stripped =
        let bodyLines = Text.splitOn "\n" stripped
            indent = Text.replicate col " "
        in [indent <> "/**"]
            ++ map (\l -> if Text.null l
                            then indent <> " *"
                            else indent <> " * " <> l) bodyLines
            ++ [indent <> " */"]
    | otherwise =
        let indent = Text.replicate col " "
        in [indent <> "/** " <> stripped <> " */"]
  where
    stripped = Text.strip body

-- | Choose the Kotlin parameter name. A C param literally named @state@
-- would collide with the trailing @state: ToxCoreState@; the
-- hand-written binding renames it to @<callbackBase>State@ (e.g.
-- @callState@ inside @CallStateCallback@); we use that convention.
paramKotlinName :: SCallbackTypeModel -> SParameter -> Text
paramKotlinName cb p
    | paramName p == "state" = dedupTrailingState renamed
    | otherwise = camelCase (paramName p)
  where
    renamed = case Text.uncons (callbackBaseName cb) of
        Nothing -> "newState"
        Just (c, t) -> Text.cons (Char.toLower c) t <> "State"
    -- @CallStateCallback@ would otherwise yield @callStateState@.
    dedupTrailingState s = case Text.stripSuffix "StateState" s of
        Just rest -> rest <> "State"
        Nothing -> s

-- | Returns @True@ if this is a ToxAV callback. AV callbacks keep a
-- @toxav_@ prefix in the semantic model because @commonPrefix@ is
-- "Tox" (covering @Tox@ and @Tox_*@ but not @Toxav@).
isAv :: SCallbackTypeModel -> Bool
isAv cb = "toxav_" `Text.isPrefixOf` cbName cb

-- | @conference_message_cb@ -> @ConferenceMessage@;
-- @toxav_call_cb@ -> @Call@. The subsystem prefix (@tox_@ / @toxav_@)
-- and the @_cb@ suffix are both stripped; the package
-- qualifies the result.
callbackBaseName :: SCallbackTypeModel -> Text
callbackBaseName cb =
    pascalCase
        . stripSuffix "_cb"
        . stripPrefix "toxav_"
        . stripPrefix "tox_"
        $ cbName cb
  where
    stripPrefix p t = case Text.stripPrefix p t of
        Just rest -> rest
        Nothing -> t
    stripSuffix s t = case Text.stripSuffix s t of
        Just rest -> rest
        Nothing -> t

callbackInterfaceName :: SCallbackTypeModel -> Text
callbackInterfaceName cb = callbackBaseName cb <> "Callback"

callbackMethodName :: SCallbackTypeModel -> Text
callbackMethodName cb =
    case Text.uncons (callbackBaseName cb) of
        Nothing -> ""
        Just (c, t) -> Text.cons (Char.toLower c) t

-- | Emit @import@ lines for the types referenced by the callback's
-- user-facing parameters. Deduplicated and stable-sorted. Includes
-- 'paramWrapperImport' for any params that the conventions module
-- says should be wrapped (BitRate, SampleCount, ToxFilename, …).
importsFor :: Text {- C callback name -} -> SemanticModel -> [SParameter] -> [Text]
importsFor cbName_ model params =
    case List.sort . List.nub
            $ concatMap importsForType (map paramType params)
                ++ [ paramWrapperImport cls
                   | p <- params
                   , Just cls <- [paramWrapperFor cbName_ p]
                   ] of
        [] -> []
        imps -> map (\i -> "import " <> i) imps ++ [""]
  where
    importsForType :: SType -> [Text]
    importsForType (SResourceId n) =
        ["im.tox.tox4j.core.data.Tox" <> pascalCase n]
    importsForType (SFixedBytes sizeConst _) =
        case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing -> []
    importsForType (SEnum n) =
        ["im.tox.tox4j.core.enums.Tox" <> pascalCase n]
    importsForType _ = []

-- | Aggregate listener interfaces: @ToxCoreEventListener@ extending all
-- core per-callback interfaces; @ToxAvEventListener@ for AV; and
-- @ToxEventListener@ joining the two (at the top-level package).
aggregateListeners :: [SCallbackTypeModel] -> [(FilePath, Text)]
aggregateListeners cbs =
    [ aggregate "core.callbacks" "ToxCoreEventListener" coreCbNames
    , aggregate "av.callbacks" "ToxAvEventListener" avCbNames
    , combinedListener
    ]
  where
    coreCbNames = List.sort (map callbackInterfaceName (filter (not . isAv) cbs))
    avCbNames = List.sort (map callbackInterfaceName (filter isAv cbs))

    aggregate :: Text -> Text -> [Text] -> (FilePath, Text)
    aggregate pkg name members =
        ( "lib/src/main/kotlin/im/tox/tox4j/"
            <> Text.unpack (Text.replace "." "/" pkg)
            <> "/"
            <> Text.unpack name
            <> ".kt"
        , Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            , "interface " <> name <> "<ToxCoreState> :"
            ]
                ++ extendsLines members
        )

    extendsLines :: [Text] -> [Text]
    extendsLines [] = []
    extendsLines names =
        let formatted =
                [ "    " <> n <> "<ToxCoreState>" | n <- names
                ]
            withCommas = zipWith (<>) formatted (replicate (length formatted - 1) "," ++ [""])
        in withCommas

    combinedListener :: (FilePath, Text)
    combinedListener =
        ( "lib/src/main/kotlin/im/tox/tox4j/ToxEventListener.kt"
        , Text.unlines
            [ "package im.tox.tox4j"
            , ""
            , "import im.tox.tox4j.av.callbacks.ToxAvEventListener"
            , "import im.tox.tox4j.core.callbacks.ToxCoreEventListener"
            , ""
            , "interface ToxEventListener<ToxCoreState> :"
            , "    ToxCoreEventListener<ToxCoreState>,"
            , "    ToxAvEventListener<ToxCoreState>"
            ]
        )

--------------------------------------------------------------------------------
-- Public API interfaces (ToxCore.kt / ToxAv.kt / ToxCrypto.kt)
--------------------------------------------------------------------------------

-- | Emit the three top-level public Kotlin interfaces.
publicApi :: Map Text Text -> SemanticModel -> [(FilePath, Text)]
publicApi docs model =
    [ renderInterface docs model "core" "ToxCore" coreMethods
    , renderInterface docs model "av" "ToxAv" avMethods
    , renderInterface docs model "crypto" "ToxCrypto" cryptoMethods
    ]
  where
    coreMethods = methodsForApi model "ToxCore" coreResources
    avMethods = methodsForApi model "ToxAv" avResources
    cryptoMethods = methodsForApi model "ToxCrypto" cryptoResources

-- | Resource names whose methods land on ToxCore (the flattened
-- top-level interface in the JVM binding). All Tox sub-resources are
-- folded in: the JVM API is shaped as `tox.friendAdd(...)` rather than
-- `tox.friend().add(...)` like the Rust safe wrapper.
coreResources, avResources, cryptoResources :: [Text]
coreResources =
    [ "Tox"
    , "Friend"
    , "Conference"
    , "Conference_Peer"
    , "Conference_Offline_Peer"
    , "Group"
    , "Group_Peer"
    , "File"
    -- "Options" intentionally omitted: its set/get methods are
    -- plumbing for the Impl's constructor (which builds an Options
    -- handle, sets each field, calls toxNew, frees). Clients use the
    -- @ToxOptions@ data class instead.
    ]
avResources = ["AV"]
-- @tox_hash@ lives under the Tox resource in the parsed model but is
-- a free-standing crypto helper. Rather than pulling all of Tox into
-- ToxCrypto (and risk leaking unrelated statics), the helper is added
-- via 'interfaceExtraMembers'.
cryptoResources = ["Pass_Key"]

-- | Pull every "user-facing" method off the listed resources, skipping
-- constructors (handled by the Impl class), destructors (mapped to
-- close()), registrars (events flow through iterate()), and statics
-- (size constants + to_string helpers — not part of the public API).
--
-- Each method is paired with its owning resource so the public-API
-- emitter can compute the path-id arguments (e.g. ConferencePeer
-- methods get prefixed with conferenceNumber + peerNumber).
methodsForApi :: SemanticModel -> Text -> [Text] -> [(SResource, SMethod)]
methodsForApi model iface rs =
    [ (r, m)
    | r <- resources model
    , resourceName r `elem` rs
    , m <- methods r
    , not (isSkippableMethodFor model iface m)
    ]

renderInterface :: Map Text Text -> SemanticModel -> Text -> Text -> [(SResource, SMethod)] -> (FilePath, Text)
renderInterface docs model pkg name pairs =
    ( "lib/src/main/kotlin/im/tox/tox4j/"
        <> Text.unpack pkg
        <> "/"
        <> Text.unpack name
        <> ".kt"
    , body
    )
  where
    -- Each member chunk leads with @""@ as its inter-member separator.
    -- 'membersTrimmed' drops the very first blank so the first member
    -- opens on the line right after @{@ (ktlint's
    -- standard:no-empty-first-line-in-class-body rule).
    iterateMethods
        | pkg == "core" =
            [ ""
            , "    val iterationInterval: Int"
            , ""
            , "    fun <ToxCoreState> iterate("
            , "        handler: im.tox.tox4j.core.callbacks.ToxCoreEventListener<ToxCoreState>,"
            , "        state: ToxCoreState,"
            , "    ): ToxCoreState"
            ]
        | pkg == "av" =
            [ ""
            , "    val iterationInterval: Int"
            , ""
            , "    fun <ToxCoreState> iterate("
            , "        handler: im.tox.tox4j.av.callbacks.ToxAvEventListener<ToxCoreState>,"
            , "        state: ToxCoreState,"
            , "    ): ToxCoreState"
            ]
        | otherwise = []

    -- Walk every type referenced in the interface and emit imports for
    -- ones that live in another package (value classes in @core.data@,
    -- enums in @{core,av}.enums@). Path-id args (e.g.
    -- @ToxConferenceNumber@) are pulled from the parent chain too —
    -- they're already pre-formatted Kotlin class names by 'pathIdArgs'.
    interfaceImports =
        map (\i -> "import " <> i) . List.sort . List.nub $
            concat
                [ importsForSType ty
                | (_, m) <- pairs
                , ty <- output m : map paramType (inputs m)
                ]
            ++ [ "im.tox.tox4j.core.data." <> tname
               | (r, _) <- pairs
               , (_, tname) <- pathIdArgs model r
               ]
            -- Imports for convention-wrapped param types (BitRate,
            -- SampleCount, ToxNickname, ToxGroupName, …). Method-aware
            -- so per-method wrappers (ToxFriendMessage in send_message
            -- but ToxGroupMessage in group_send_message) get the right
            -- import.
            ++ [ paramWrapperImport cls
               | (_, m) <- pairs
               , p <- inputs m
               , Just cls <- [paramWrapperFor (methodName m) p]
               ]
            -- Imports for convention-wrapped return types (ToxName,
            -- Port, ToxGroupName, …).
            ++ [ imp
               | (_, m) <- pairs
               , Just imp <- [returnWrapperImport (methodName m) (output m)]
               ]
    importsForSType (SResourceId n) = ["im.tox.tox4j.core.data." <> kotlinNameOf n]
    importsForSType (SEnum n) = ["im.tox.tox4j." <> enumPkgOf n <> ".enums." <> kotlinNameOf n]
    importsForSType (SFixedBytes sizeConst _) =
        case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing -> []
    importsForSType _ = []
    enumPkgOf n
        | "Toxav_" `Text.isPrefixOf` n = "av"
        | otherwise = "core"

    -- Class-level annotations (e.g. @kotlin.ExperimentalStdlibApi) and
    -- Interfaces backed by a real lifecycle resource (Tox, AV) extend
    -- AutoCloseable so callers can @use { }@ them. ToxCrypto is a
    -- static-style helper collection with nothing to close.
    superclause = case interfaceSelfResource name of
        Just _  -> " : AutoCloseable"
        Nothing -> ""

    -- Generic type parameters (e.g. @ToxCrypto<PassKey>@). When the
    -- handle resource is an impl-chosen type, the parameter lets the
    -- impl decide its representation.
    generics = case interfaceGenericParams name of
        [] -> ""
        ps -> "<" <> Text.intercalate ", " ps <> ">"

    -- Hand-curated extras (e.g. ToxCore's @load@ factory and the
    -- @List<ToxFriendNumber>@-of-@IntArray@ derived properties). Each
    -- block is its own paragraph in the interface body.
    extras = case interfaceExtraMembers name of
        []  -> []
        ms  -> concatMap (\m -> ["", m]) ms

    members =
        iterateMethods
            ++ concatMap (renderMethodSig docs model name) pairs
            ++ extras
    -- ktlint's standard:no-empty-first-line-in-class-body forbids a
    -- blank line right after the @{@ of the interface body. Every
    -- member chunk leads with @""@ as its inter-member separator;
    -- drop that leading blank so the first member opens on the
    -- next line.
    membersTrimmed = dropWhile Text.null members

    body =
        Text.unlines $
            [ "package im.tox.tox4j." <> pkg
            , ""
            ]
                ++ List.sort (List.nub interfaceImports)
                ++ [ "" ]
                ++ [ "interface " <> name <> generics <> superclause <> " {"
                   ]
                ++ membersTrimmed
                ++ ["}"]

-- | First-cut method signature emitter. Prepends the path-id args
-- derived from the method's owning resource hierarchy, then the
-- method's own inputs. Will be refined for getters / setters /
-- proper exception naming.
--
-- Zero-arg methods (no path-id args, no user inputs, non-void return)
-- are emitted as Kotlin @val@ properties to match the hand-written
-- jvm-toxcore-c convention of exposing top-level accessors as
-- properties.
renderMethodSig :: Map Text Text -> SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderMethodSig docs model iface (r, m)
    | isPropertyShape =
        kdoc ++
        [ "    val " <> kotlinPropertyName m <> ret
        ]
    -- Wrap method signatures with 2+ args across multiple lines. ktlint's
    -- standard:function-signature rule normalises any signature to this
    -- shape; emitting it pre-wrapped means @apigen + ktlint = no diff@.
    | length allArgs >= 2 =
        kdoc ++
        [ "    " <> roleToKotlin (methodRole m) <> kotlinMethodName m <> "(" ]
            ++ [ "        " <> a <> "," | a <- allArgs ]
            ++ [ "    )" <> ret ]
    | otherwise =
        kdoc ++
        [ "    "
            <> roleToKotlin (methodRole m)
            <> kotlinMethodName m
            <> "("
            <> Text.intercalate ", " allArgs
            <> ")"
            <> ret
        ]
  where
    kdoc = renderKdoc model docs m
    allArgs = handleArg ++ pathArgs ++ ownArgs
    ret = renderReturn (output m)
    -- Property-shape applies only to plain getters: zero arguments
    -- and a non-void return. Constructors with no inputs (e.g.
    -- @tox_conference_new@) are still functions — calling a "new"
    -- accessor reads as a getter, not the side-effecting create.
    isPropertyShape =
        null handleArg
            && null pathArgs
            && null ownArgs
            && not (Text.null ret)
            && methodRole m /= Constructor

    roleToKotlin GetterRole = "val "
    roleToKotlin _ = "fun "

    -- Constructors create the resource, so the resource's own ID
    -- is the *output*, not an input. Other methods take the resource
    -- ID as the leading parameter (the JVM API flattens method
    -- dispatch onto a single top-level interface).
    includeOwnId = case methodRole m of
        Constructor -> False
        _           -> True
    pathArgs = [pname <> ": " <> tname | (pname, tname) <- pathIdArgsFor model r includeOwnId]

    -- If this method's owning resource is a handle resource that is
    -- *not* the interface's self receiver (e.g. ToxCrypto methods on
    -- @Pass_Key@), prepend the handle as an explicit parameter.
    -- Constructors create the handle (return it) and statics don't
    -- need it, so both skip this.
    handleArg = case interfaceSelfResource iface of
        Just selfName | selfName /= resourceName r -> renderHandleArg
        Nothing                                    -> renderHandleArg
        _                                          -> []
    renderHandleArg = case (resourceType r, methodRole m) of
        (ResHandle, Constructor) -> []
        (ResHandle, StaticRole)  -> []
        (ResHandle, _)           ->
            let nm = resourceName r
            in [camelCase nm <> ": " <> pascalCase nm]
        _                        -> []
    ownArgs = map renderInput (inputs m)

    renderInput p = camelCase (paramName p) <> ": " <> renderMethodParamType model (methodName m) p

    renderReturn SVoid = ""
    -- A @bool@ return with an error type is usually the "success/failure"
    -- pattern: the error is thrown as a ToxXxxException, the bool itself
    -- isn't meaningful to the caller. EXCEPTION: predicate-named methods
    -- (@is_*@, @has_*@, @*_exists@) — for those the bool *is* the answer
    -- and the error covers an out-of-band failure.
    renderReturn SBool
      | Just _ <- methodErrorType m
      , not (boolReturnsValue (methodName m)) = ""
    renderReturn ty = ": " <> case returnWrapperClass (methodName m) ty of
        Just cls -> cls
        Nothing  -> renderType model ty

--------------------------------------------------------------------------------
-- ToxOptions data class
--------------------------------------------------------------------------------

-- | Property-or-sealed-group iterator for 'renderToxOptions'.
data PropOrGroup
    = PlainProp SProperty
    | SealedGroupProp SealedGroup

-- | Emit @ToxOptions.kt@ as a Kotlin @data class@ that mirrors the
-- C @Tox_Options@ struct. Proxy- and savedata-related fields are
-- collapsed into sealed-interface hierarchies (@ProxyOptions.Type@
-- and @SaveDataOptions.Type@) so the type system carries the
-- "if proxy_type is HTTP, host and port are required" guarantee
-- that the flat C form can't.
--
-- The sealed support files ('ProxyOptions.kt', 'SaveDataOptions.kt')
-- are emitted as a hardcoded text block — they're a one-off pattern
-- that doesn't pay back a generic mechanism.
toxOptionsClass :: SemanticModel -> [(FilePath, Text)]
toxOptionsClass model =
    case List.find ((== "Options") . resourceName) (resources model) of
        Nothing -> []
        Just options ->
            [ ( "lib/src/main/kotlin/im/tox/tox4j/core/options/ToxOptions.kt"
              , renderToxOptions model (properties options)
              )
            ,
              ( "lib/src/main/kotlin/im/tox/tox4j/core/options/ProxyOptions.kt"
              , proxyOptionsKt
              )
            ,
              ( "lib/src/main/kotlin/im/tox/tox4j/core/options/SaveDataOptions.kt"
              , saveDataOptionsKt
              )
            ]

-- | Hardcoded ProxyOptions.kt content. The three variants
-- (@None@/@Http@/@Socks5@) correspond to @ToxProxyType@'s three
-- enumerators; the discriminator + (host, port) tuple is the C
-- model's flat shape, refactored into a sum type that the Kotlin
-- caller can pattern-match on.
proxyOptionsKt :: Text
proxyOptionsKt = Text.unlines
    [ "package im.tox.tox4j.core.options"
    , ""
    , "import im.tox.tox4j.core.ToxCore"
    , "import im.tox.tox4j.core.ToxCoreConstants"
    , "import im.tox.tox4j.core.enums.ToxProxyType"
    , ""
    , "/** Proxy options for [ToxCore]. */"
    , "object ProxyOptions {"
    , "    /** Base type for all proxy kinds. */"
    , "    sealed interface Type {"
    , "        /** Low level enumeration value to pass to [ToxCore]. */"
    , "        val proxyType: ToxProxyType"
    , ""
    , "        /**"
    , "         * The IP address or DNS name of the proxy to be used."
    , "         *"
    , "         * If used, this must be a valid DNS name. The name must not exceed"
    , "         * [ToxCoreConstants.MAX_HOSTNAME_LENGTH] characters. This member is ignored (it can be"
    , "         * anything) if [proxyType] is [ToxProxyType.NONE]."
    , "         */"
    , "        val proxyAddress: String"
    , ""
    , "        /**"
    , "         * The port to use to connect to the proxy server."
    , "         *"
    , "         * Ports must be in the range (1, 65535). The value is ignored if [proxyType] is"
    , "         * [ToxProxyType.NONE]."
    , "         */"
    , "        val proxyPort: UShort"
    , "    }"
    , ""
    , "    /** Don't use a proxy. Attempt to directly connect to other nodes. */"
    , "    object None : Type {"
    , "        override val proxyType: ToxProxyType = ToxProxyType.NONE"
    , "        override val proxyAddress: String = \"\""
    , "        override val proxyPort: UShort = 0.toUShort()"
    , "    }"
    , ""
    , "    /** Tunnel Tox TCP traffic over an HTTP proxy. The proxy must support CONNECT. */"
    , "    data class Http("
    , "        override val proxyAddress: String,"
    , "        override val proxyPort: UShort,"
    , "    ) : Type {"
    , "        override val proxyType: ToxProxyType = ToxProxyType.HTTP"
    , "    }"
    , ""
    , "    /**"
    , "     * Use a SOCKS5 proxy to make TCP connections. Although some SOCKS5 servers support UDP sockets,"
    , "     * the main use case (Tor) does not, and Tox will not use the proxy for UDP connections."
    , "     */"
    , "    data class Socks5("
    , "        override val proxyAddress: String,"
    , "        override val proxyPort: UShort,"
    , "    ) : Type {"
    , "        override val proxyType: ToxProxyType = ToxProxyType.SOCKS5"
    , "    }"
    , "}"
    ]

-- | Hardcoded SaveDataOptions.kt content. @SecretKey@'s field is
-- renamed and re-typed from the C @savedata@ (bytes) to a
-- @ToxSecretKey@ wrapper, matching the JVM convention of keeping
-- typed bytes around.
saveDataOptionsKt :: Text
saveDataOptionsKt = Text.unlines
    [ "package im.tox.tox4j.core.options"
    , ""
    , "import im.tox.tox4j.core.ToxCore"
    , "import im.tox.tox4j.core.data.ToxSecretKey"
    , "import im.tox.tox4j.core.enums.ToxSavedataType"
    , ""
    , "/** Base type for all save data kinds. */"
    , "@Suppress(\"ktlint:standard:no-consecutive-comments\")"
    , "object SaveDataOptions {"
    , "    sealed interface Type {"
    , "        /** The low level [ToxSavedataType] enum to pass to [ToxCore]. */"
    , "        val kind: ToxSavedataType"
    , ""
    , "        /** Serialised save data. The format depends on [kind]. */"
    , "        val data: ByteArray"
    , "    }"
    , ""
    , "    /** The various kinds of save data that can be loaded by [ToxCore]. */"
    , ""
    , "    /** No save data. */"
    , "    object None : Type {"
    , "        override val kind: ToxSavedataType = ToxSavedataType.NONE"
    , "        override val data: ByteArray = byteArrayOf()"
    , "    }"
    , ""
    , "    /**"
    , "     * Full save data containing friend list, last seen DHT nodes, name, and all other information"
    , "     * contained within a Tox instance."
    , "     */"
    , "    class ToxSave("
    , "        override val data: ByteArray,"
    , "    ) : Type {"
    , "        override val kind: ToxSavedataType = ToxSavedataType.TOX_SAVE"
    , ""
    , "        // `data class` over a `ByteArray` would use referential"
    , "        // equality; two equal save blobs would compare unequal."
    , "        // Hand-write content-based `equals` / `hashCode`."
    , "        // `toString` elides the payload — save data contains the"
    , "        // secret key and shouldn't land in logs."
    , "        override fun equals(other: Any?): Boolean = this === other || (other is ToxSave && data.contentEquals(other.data))"
    , ""
    , "        override fun hashCode(): Int = data.contentHashCode()"
    , ""
    , "        override fun toString(): String = \"ToxSave(data=<${data.size} bytes>)\""
    , "    }"
    , ""
    , "    /**"
    , "     * Minimal save data with just the secret key. The public key can be derived from it. Saving"
    , "     * this secret key, the friend list, name, and noSpam value is sufficient to restore the"
    , "     * observable behaviour of a Tox instance without the full save data in [ToxSave]."
    , "     */"
    , "    class SecretKey("
    , "        private val key: ToxSecretKey,"
    , "    ) : Type {"
    , "        override val kind: ToxSavedataType = ToxSavedataType.SECRET_KEY"
    , "        override val data: ByteArray = key.value"
    , ""
    , "        // Same ByteArray-equality footgun as `ToxSave`; compare"
    , "        // on the underlying secret-key bytes. `toString` omits"
    , "        // the key."
    , "        override fun equals(other: Any?): Boolean = this === other || (other is SecretKey && data.contentEquals(other.data))"
    , ""
    , "        override fun hashCode(): Int = data.contentHashCode()"
    , ""
    , "        override fun toString(): String = \"SecretKey(<${data.size} bytes>)\""
    , "    }"
    , "}"
    ]

-- | Render @ToxOptions.kt@ — a Kotlin @data class@ that mirrors the
-- C @Tox_Options@ struct fields. Proxy- and savedata-related fields
-- are collapsed into sealed-type fields so the generated class
-- looks like a thoughtfully-designed Kotlin API rather than a
-- transliteration. @log_callback@/@log_user_data@ skip — they're
-- wired through the listener path, not constructor args.
renderToxOptions :: SemanticModel -> [SProperty] -> Text
renderToxOptions model props =
    Text.unlines $
        [ "package im.tox.tox4j.core.options"
        , ""
        ]
            ++ optionImports
            ++ [ ""
               , "data class ToxOptions("
               ]
            ++ map renderField visibleProps
            ++ [")"]
  where
    -- Hide SHandle (log_callback) and SCallback (log_user_data)
    -- fields; also any C field that's part of a sealed group is
    -- collapsed into the group's single Kotlin field instead.
    visibleProps = nubGroups (filter visiblePred props)
    visiblePred p = case propType p of
        SHandle _   -> False
        SCallback _ -> False
        _           -> True
    -- Replace runs of "grouped" properties with a single sealed-group
    -- field, keeping the position of the first member.
    nubGroups [] = []
    nubGroups (p:rest) = case lookupSealedGroup model (propName p) of
        Just g  -> SealedGroupProp g : nubGroups (dropGroupMembers g rest)
        Nothing -> PlainProp p : nubGroups rest
    dropGroupMembers g = filter (\p -> propName p `notElem` sealedGroupCFields g)
    renderField (PlainProp p) =
        "    val "
            <> camelCase (propName p)
            <> ": "
            <> kotlinPropType p
            <> " = "
            <> defaultForName p
            <> ","
    renderField (SealedGroupProp g) =
        "    val "
            <> sealedGroupFieldName g
            <> ": "
            <> sealedGroupTypeName g
            <> " = "
            <> sealedGroupDefault g
            <> ","

    -- Per-field type, with port-typed @UShort@ for port fields
    -- (matches the constants' types so passing
    -- @ToxCoreConstants.DEFAULT_START_PORT@ Just Works).
    kotlinPropType p
        | isPortField (propName p) = "UShort"
        | otherwise                = renderType model (propType p)
    isPortField n =
        n == "start_port" || n == "end_port" || n == "tcp_port"

    -- Defaults mirror @tox_options_default()@: booleans true except
    -- experimental_*; strings empty; numbers 0; enums first member;
    -- ports defaulting via 'ToxCoreConstants'.
    defaultForName p = case propType p of
        _ | propName p == "start_port" -> "ToxCoreConstants.DEFAULT_START_PORT"
          | propName p == "end_port"   -> "ToxCoreConstants.DEFAULT_END_PORT"
          | propName p == "tcp_port"   -> "ToxCoreConstants.DEFAULT_TCP_PORT"
        SBool | "experimental_" `Text.isPrefixOf` propName p -> "false"
        SBool -> "true"
        SString -> "\"\""
        SInt _ -> "0"
        SUInt _ -> "0u"
        SSizeT -> "0L"
        SBytes -> "ByteArray(0)"
        SEnum n -> kotlinNameOf n <> ".entries.first()"
        _ -> "TODO()"

    -- Imports: enum types referenced as field types, the constants
    -- object (for port defaults), and the sealed-group containers.
    optionImports =
        case List.sort . List.nub $
            ["im.tox.tox4j.core.ToxCoreConstants"]
            ++ [ "im.tox.tox4j.core.enums." <> kotlinNameOf n
               | PlainProp p <- visibleProps
               , SEnum n <- [propType p]
               ] of
            [] -> []
            imps -> map (\i -> "import " <> i) imps

--------------------------------------------------------------------------------
-- Kdoc emission
--------------------------------------------------------------------------------

-- | Render a method's kdoc as a list of @[Text]@ lines. Looks up the
-- raw doxygen comment by C function name and applies the JVM-specific
-- transforms from 'transformKdoc'. Returns @[]@ (no leading blank
-- line) when no doc is present, so the caller's signature line still
-- gets its own blank separator.
renderKdoc :: SemanticModel -> Map Text Text -> SMethod -> [Text]
renderKdoc model docs m = case Map.lookup (methodName m) docs of
    Nothing  -> [""]
    Just raw ->
        let body = transformKdoc model raw
            -- Drop trailing blank lines from the body so the closing
            -- @*/@ sits right after the last content line.
            stripped = Text.dropWhileEnd (== '\n') body
            bodyLines = Text.splitOn "\n" stripped
        in [""]
            ++ ["    /**"]
            ++ map (\l -> if Text.null l
                            then "     *"
                            else "     * " <> l) bodyLines
            ++ ["     */"]

