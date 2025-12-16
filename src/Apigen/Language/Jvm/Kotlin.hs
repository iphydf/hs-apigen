{-# LANGUAGE OverloadedStrings #-}

-- | The Kotlin public API: constants, enums, exceptions, data classes,
-- callbacks and interfaces.
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
import           Apigen.Language.Jvm.Kotlin.Common (camelCase,
                                                    isSkippableMethodFor,
                                                    kotlinMethodName,
                                                    kotlinNameOf,
                                                    kotlinPropertyName,
                                                    pascalCase, pathIdArgs,
                                                    pathIdArgsFor,
                                                    renderMethodParamType,
                                                    renderParamType, renderType)
import           Apigen.Parser.Docs                (Docs (..))
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
    -- Error enums become exceptions' @Code@ enums, and open enums an
    -- @object@ of constants.
    isErrorOrSpecialEnum e =
        isErrorEnum e
            || isOpenEnum model e

    -- Only exceptions some public method can throw. The C++ @HANDLE@s
    -- use the same predicate.
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

-- | Constants from @toxencryptsave.h@ go into @ToxCryptoConstants@,
-- without @PASS_@: @ToxCryptoConstants.KEY_LENGTH@.
cryptoConstants :: Map Text Text -> SemanticModel -> Text
cryptoConstants docs model =
    renderConstantsObject model docs "crypto" "ToxCryptoConstants" cryptoOnly
  where
    cryptoOnly = List.sortOn kotlinName (map stripPassPrefix (filter (isCryptoConstant . constantName) (constants model)))
    stripPassPrefix c = case Text.stripPrefix "TOX_PASS_" (constantName c) of
        Just rest -> c {constantName = "TOX_" <> rest}
        Nothing   -> c

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
        [] -> []
        es -> "" : es
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

-- | @TOX_PUBLIC_KEY_SIZE@ becomes @ToxCoreConstants.PUBLIC_KEY_SIZE@.
kotlinName :: SConstantModel -> Text
kotlinName c =
    case Text.stripPrefix "TOX_" (constantName c) of
        Just rest -> rest
        Nothing -> fromMaybe (constantName c) (Text.stripPrefix "TOXAV_" (constantName c))

-- | Whether the constant is from @toxencryptsave.h@, which the model
-- doesn't record.
isCryptoConstant :: Text -> Bool
isCryptoConstant n =
    "TOX_PASS_" `Text.isPrefixOf` n || n == "TOX_HASH_LENGTH"

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

-- | Members are in C order: the C++ @Enum::ordinal/valueOf@ relies on it.
renderEnum :: SEnumModel -> (FilePath, Text)
renderEnum e = (path, body)
  where
    -- Headers spell it either way.
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
            in map go (zip names (replicate (length names - 1) False ++ [True]))

-- | An @object@ of C values, not an @enum class@: the C API takes
-- @uint32_t@ so clients can pass values the header doesn't name.
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

-- | @TOXAV_CALL_CONTROL@ becomes @ToxavCallControl@.
kotlinEnumName :: Text -> Text
kotlinEnumName n
    | "TOXAV_" `Text.isPrefixOf` n =
        "Toxav" <> pascalCase (Text.drop (Text.length "TOXAV_") n)
    | otherwise = pascalCase n

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | Error enums from @toxencryptsave.h@, which the model doesn't record.
cryptoErrorNames :: [Text]
cryptoErrorNames =
    [ "Tox_Err_Encryption"
    , "Tox_Err_Decryption"
    , "Tox_Err_Key_Derivation"
    , "Tox_Err_Get_Salt"
    ]

-- | An exception class with a @Code@ enum of the non-OK errors.
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
    -- Extra codes have no C name: only the C++ code throws them.
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

-- | @Tox_Err_Friend_Add@ becomes @ToxFriendAddException@, and
-- @Toxav_Err_Bit_Rate_Set@ @ToxavBitRateSetException@.
exceptionClassName :: SEnumModel -> Text
exceptionClassName e =
    case Text.stripPrefix "Tox_Err_" (enumName e) of
        Just rest -> "Tox" <> pascalCase rest <> "Exception"
        Nothing -> case Text.stripPrefix "Toxav_Err_" (enumName e) of
            Just rest -> "Toxav" <> pascalCase rest <> "Exception"
            Nothing   -> enumName e -- fallback, shouldn't happen

--------------------------------------------------------------------------------
-- Data classes (value classes for typedef-newtypes)
--------------------------------------------------------------------------------

-- | @typedef uint32_t Tox_Friend_Number;@ becomes a value class over
-- @Int@, as JNI has no unsigned types.
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

-- | A value class for an integer c-toxcore has no typedef for, bounded
-- by the C width of the params it wraps.
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
            , "        // Anything else would be truncated to uint" <> tInt bits <> "_t."
            , "        require(value in 0..MAX) { \"" <> cls <> " must fit in uint"
                <> tInt bits <> "_t (0..$MAX), got $value\" }"
            , "    }"
            , ""
            , "    companion object {"
            , "        const val MAX: Int = " <> maxHex
            , ""
            , "        /** Clamps to `[0, MAX]`, for values from outside the program. */"
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

-- | The C width of the params a value class wraps, which must agree.
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

allParams :: SemanticModel -> [SParameter]
allParams model =
       concatMap inputs (concatMap methods (resources model))
    ++ concatMap eventParams (concatMap events (resources model))
    ++ concatMap cbParams (callbacks model)

unsignedKotlinType :: Int -> Text
unsignedKotlinType bits = case bits of
    8  -> "UByte"
    16 -> "UShort"
    32 -> "UInt"
    64 -> "ULong"
    n  -> error $ "renderValueClass: no unsigned Kotlin type for width " <> show n

tInt :: Int -> Text
tInt = Text.pack . show

-- | A class for a fixed-size byte array typedef. Not a value class: those
-- can't override @equals@, and @ByteArray@ compares by reference.
renderFixedBytes :: (Text, Text) -> (FilePath, Text)
renderFixedBytes (sizeConst, semanticName) =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    className = "Tox" <> pascalCase semanticName
    (constsObj, constsImport) = case sizeConst of
        "TOX_HASH_LENGTH"      -> ("ToxCryptoConstants", "im.tox.tox4j.crypto.ToxCryptoConstants")
        "TOX_PASS_SALT_LENGTH" -> ("ToxCryptoConstants", "im.tox.tox4j.crypto.ToxCryptoConstants")
        _                      -> ("ToxCoreConstants",   "im.tox.tox4j.core.ToxCoreConstants")
    sizeRef = constsObj <> "." <> stripToxPrefix sizeConst
    stripToxPrefix n
        | "TOX_PASS_" `Text.isPrefixOf` n = Text.drop 9 n
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

-- | ktlint wants this on one line when it fits in 140 columns, and on
-- two otherwise.
equalsLines :: Text -> [Text]
equalsLines className =
    let single = "    override fun equals(other: Any?): Boolean = this === other || (other is "
              <> className <> " && value.contentEquals(other.value))"
    in if Text.length single <= 140
         then [single]
         else [ "    override fun equals(other: Any?): Boolean ="
              , "        this === other || (other is " <> className <> " && value.contentEquals(other.value))"
              ]

-- | A class for a variable-size byte parameter with no C typedef.
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
            , " * Not a value class: those can't override `equals` to compare the bytes."
            , " * `toString` leaves out the bytes, which can be private."
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

    -- Handles are the receiver and user_data, which the dispatcher fills in.
    userParams = map convertParam (filter isUserParam (cbParams cb))
    isUserParam p = case paramType p of
        SHandle _ -> False
        _         -> True
    -- toxav callbacks use @uint32_t@, not @Tox_Friend_Number@.
    convertParam p
        | paramName p == "friend_number" && isUInt (paramType p) =
            p {paramType = SResourceId "Friend_Number"}
        | otherwise = p
    isUInt (SUInt _) = True
    isUInt (SInt _)  = True
    isUInt _         = False

    methodName = callbackMethodName cb

    -- The C typedef's doxygen, so its @param entries match the method.
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

-- | A kdoc block indented by @col@ spaces.
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

-- | A C param named @state@ would clash with @state: ToxCoreState@, so it
-- becomes e.g. @callState@.
paramKotlinName :: SCallbackTypeModel -> SParameter -> Text
paramKotlinName cb p
    | paramName p == "state" = dedupTrailingState renamed
    | otherwise = camelCase (paramName p)
  where
    renamed = case Text.uncons (callbackBaseName cb) of
        Nothing     -> "newState"
        Just (c, t) -> Text.cons (Char.toLower c) t <> "State"
    -- @CallStateCallback@ would otherwise yield @callStateState@.
    dedupTrailingState s = case Text.stripSuffix "StateState" s of
        Just rest -> rest <> "State"
        Nothing   -> s

-- | AV callbacks keep their @toxav_@ prefix in the model.
isAv :: SCallbackTypeModel -> Bool
isAv cb = "toxav_" `Text.isPrefixOf` cbName cb

-- | @conference_message_cb@ becomes @ConferenceMessage@, and
-- @toxav_call_cb@ @Call@.
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
        Nothing   -> t
    stripSuffix s t = case Text.stripSuffix s t of
        Just rest -> rest
        Nothing   -> t

callbackInterfaceName :: SCallbackTypeModel -> Text
callbackInterfaceName cb = callbackBaseName cb <> "Callback"

callbackMethodName :: SCallbackTypeModel -> Text
callbackMethodName cb =
    case Text.uncons (callbackBaseName cb) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toLower c) t

importsFor :: Text {- C callback name -} -> SemanticModel -> [SParameter] -> [Text]
importsFor cbName_ model params =
    case List.sort . List.nub
            $ concatMap importsForType (map paramType params)
                ++ [ paramWrapperImport cls
                   | p <- params
                   , Just cls <- [paramWrapperFor cbName_ p]
                   ] of
        []   -> []
        imps -> map (\i -> "import " <> i) imps ++ [""]
  where
    importsForType :: SType -> [Text]
    importsForType (SResourceId n) =
        ["im.tox.tox4j.core.data.Tox" <> pascalCase n]
    importsForType (SFixedBytes sizeConst _) =
        case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing      -> []
    importsForType (SEnum n) =
        ["im.tox.tox4j.core.enums.Tox" <> pascalCase n]
    importsForType _ = []

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

-- | The resources each interface flattens: @tox.friendAdd(...)@.
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
    -- Not "Options": clients use the @ToxOptions@ class.
    ]
avResources = ["AV"]
-- @tox_hash@ is under Tox in the model, so 'interfaceExtraMembers' adds it.
cryptoResources = ["Pass_Key"]

-- | The public methods of the resources, with the resource for its
-- path-id args.
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
            ++ [ paramWrapperImport cls
               | (_, m) <- pairs
               , p <- inputs m
               , Just cls <- [paramWrapperFor (methodName m) p]
               ]
            ++ [ imp
               | (_, m) <- pairs
               , Just imp <- [returnWrapperImport (methodName m) (output m)]
               ]
    importsForSType (SResourceId n) = ["im.tox.tox4j.core.data." <> kotlinNameOf n]
    importsForSType (SEnum n) = ["im.tox.tox4j." <> enumPkgOf n <> ".enums." <> kotlinNameOf n]
    importsForSType (SFixedBytes sizeConst _) =
        case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing      -> []
    importsForSType _ = []
    enumPkgOf n
        | "Toxav_" `Text.isPrefixOf` n = "av"
        | otherwise = "core"

    superclause = case interfaceSelfResource name of
        Just _  -> " : AutoCloseable"
        Nothing -> ""

    generics = case interfaceGenericParams name of
        [] -> ""
        ps -> "<" <> Text.intercalate ", " ps <> ">"

    extras = case interfaceExtraMembers name of
        [] -> []
        ms -> concatMap (\m -> ["", m]) ms

    members =
        iterateMethods
            ++ concatMap (renderMethodSig docs model name) pairs
            ++ extras
    -- ktlint: no blank line after the @{@.
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

-- | A method signature, or a @val@ for a getter with no arguments.
renderMethodSig :: Map Text Text -> SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderMethodSig docs model iface (r, m)
    | isPropertyShape =
        kdoc ++
        [ "    val " <> kotlinPropertyName m <> ret
        ]
    -- ktlint's function-signature shape.
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
    -- @tox_conference_new@ takes no arguments but isn't a getter.
    isPropertyShape =
        null handleArg
            && null pathArgs
            && null ownArgs
            && not (Text.null ret)
            && methodRole m /= Constructor

    roleToKotlin GetterRole = "val "
    roleToKotlin _          = "fun "

    -- Constructors return the resource's own id.
    includeOwnId = case methodRole m of
        Constructor -> False
        _           -> True
    pathArgs = [pname <> ": " <> tname | (pname, tname) <- pathIdArgsFor model r includeOwnId]

    -- A handle that isn't the interface's receiver (e.g. @Pass_Key@ on
    -- ToxCrypto) is an explicit parameter.
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
    -- With an error, a @bool@ is usually just success, which the
    -- exception already says, except for predicates like @is_*@.
    renderReturn SBool
      | Just _ <- methodErrorType m
      , not (boolReturnsValue (methodName m)) = ""
    renderReturn ty = ": " <> case returnWrapperClass (methodName m) ty of
        Just cls -> cls
        Nothing  -> renderType model ty

--------------------------------------------------------------------------------
-- ToxOptions data class
--------------------------------------------------------------------------------

data PropOrGroup
    = PlainProp SProperty
    | SealedGroupProp SealedGroup

-- | @ToxOptions@, with the proxy and savedata fields as sealed types so
-- e.g. an HTTP proxy must have a host and port.
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
    , "        // A data class would compare the arrays by reference. `toString`"
    , "        // leaves out the data, which holds the secret key."
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
    , "        override fun equals(other: Any?): Boolean = this === other || (other is SecretKey && data.contentEquals(other.data))"
    , ""
    , "        override fun hashCode(): Int = data.contentHashCode()"
    , ""
    , "        override fun toString(): String = \"SecretKey(<${data.size} bytes>)\""
    , "    }"
    , "}"
    ]

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
    -- The log callback and its user data are set elsewhere.
    visibleProps = nubGroups (filter visiblePred props)
    visiblePred p = case propType p of
        SHandle _   -> False
        SCallback _ -> False
        _           -> True
    -- A sealed group goes where its first field was.
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

    -- @UShort@, the type of @ToxCoreConstants.DEFAULT_START_PORT@.
    kotlinPropType p
        | isPortField (propName p) = "UShort"
        | otherwise                = renderType model (propType p)
    isPortField n =
        n == "start_port" || n == "end_port" || n == "tcp_port"

    -- As @tox_options_default()@.
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

    optionImports =
        case List.sort . List.nub $
            ["im.tox.tox4j.core.ToxCoreConstants"]
            ++ [ "im.tox.tox4j.core.enums." <> kotlinNameOf n
               | PlainProp p <- visibleProps
               , SEnum n <- [propType p]
               ] of
            []   -> []
            imps -> map (\i -> "import " <> i) imps

--------------------------------------------------------------------------------
-- Kdoc emission
--------------------------------------------------------------------------------

-- | A method's kdoc, after a blank line.
renderKdoc :: SemanticModel -> Map Text Text -> SMethod -> [Text]
renderKdoc model docs m = case Map.lookup (methodName m) docs of
    Nothing  -> [""]
    Just raw ->
        let body = transformKdoc model raw
            stripped = Text.dropWhileEnd (== '\n') body
            bodyLines = Text.splitOn "\n" stripped
        in [""]
            ++ ["    /**"]
            ++ map (\l -> if Text.null l
                            then "     *"
                            else "     * " <> l) bodyLines
            ++ ["     */"]

