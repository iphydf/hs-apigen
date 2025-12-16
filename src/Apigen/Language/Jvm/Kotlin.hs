{-# LANGUAGE OverloadedStrings #-}

-- | Kotlin public API: constants, enums, exceptions, data classes, callbacks
-- and interfaces.
module Apigen.Language.Jvm.Kotlin (generate) where

import qualified Apigen.Knowledge                  as K
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
                                                    isCryptoConstant,
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
import           Apigen.Language.Jvm.Model         (avResourceNames,
                                                    classPrefix,
                                                    coreResourceNames,
                                                    cryptoResourceNames,
                                                    exceptionClassFor,
                                                    inOtherLibrary,
                                                    inStandalone,
                                                    isOtherLibraryName,
                                                    standalonePrefix)
import           Apigen.Model                      (callbackIn, enumIn)
import           Apigen.Namespace                  (stripNamespace)
import           Apigen.Parser.Docs                (Docs (..))
import           Apigen.Semantic                   (SCallbackTypeModel (..),
                                                    SConstantModel (..),
                                                    SEnumKind (..),
                                                    SEnumMember (..),
                                                    SEnumModel (..),
                                                    SEvent (..),
                                                    SIdTypeModel (..),
                                                    SMethod (..),
                                                    SMethodRole (..),
                                                    SParameter (..),
                                                    SProperty (..),
                                                    SResource (..),
                                                    SResourceKind (SettingsHandle),
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
        ++ map (renderEnum model) (filter (not . isErrorOrSpecialEnum) (enums model))
        ++ map (renderOpenEnum model docs) (filter (isOpenEnum model) (enums model))
        ++ map (renderException docs model)
               (filter isReachableErrorEnum (enums model) ++ syntheticExceptionEnums model)
        ++ map (renderIdNewtype model) (idTypes model)
        ++ map (renderValueClass model) generatedValueClasses
        ++ map (renderFixedBytes model) (arrayTypes model)
        ++ map renderVarBytes (varByteWrappers model)
        ++ map (renderCallback (funcDocs docs) model) (callbacks model)
        ++ aggregateListeners model (callbacks model)
        ++ publicApi (funcDocs docs) model
        ++ toxOptionsClass model
  where
    isErrorEnum e = enumKind e == ErrorEnum
    isErrorOrSpecialEnum e =
        isErrorEnum e
            || isOpenEnum model e

    isReachableErrorEnum e =
        isErrorEnum e && errorEnumIsReachable model (enumName e)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

coreConstants :: Map Text Text -> SemanticModel -> Text
coreConstants docs model =
    renderConstantsObject model docs "core" "ToxCoreConstants" coreOnly
  where
    coreOnly = List.sortOn (kotlinName model) (filter (not . isCryptoConstant model . constantName) (constants model))

-- | Without @PASS_@: @ToxCryptoConstants.KEY_LENGTH@.
cryptoConstants :: Map Text Text -> SemanticModel -> Text
cryptoConstants docs model =
    renderConstantsObject model docs "crypto" "ToxCryptoConstants" cryptoOnly
  where
    cryptoOnly = List.sortOn (kotlinName model) (map stripPassPrefix (filter (isCryptoConstant model . constantName) (constants model)))
    -- @TOX_PASS_KEY_LENGTH@ is @TOX_KEY_LENGTH@.
    stripPassPrefix c = case Text.stripPrefix (Text.toUpper (standalonePrefix model)) (constantName c) of
        Just rest -> c {constantName = Text.toUpper (K.rootLibrary model) <> "_" <> rest}
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
                    <> kotlinName model c
                    <> " = "
                    <> Text.pack (show (constantValue c))
        in case kdoc of
            [] -> ["" , line]
            ks -> ["" ] ++ ks ++ [line]

-- | @TOX_PUBLIC_KEY_SIZE@ becomes @ToxCoreConstants.PUBLIC_KEY_SIZE@.
kotlinName :: SemanticModel -> SConstantModel -> Text
kotlinName model c = stripNamespace (K.libraryWords model) (constantName c)

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

-- | Members are in C order: the C++ @Enum::ordinal/valueOf@ relies on it.
renderEnum :: SemanticModel -> SEnumModel -> (FilePath, Text)
renderEnum model e = (path, body)
  where
    pkg = if inOtherLibrary model (enumFile e) then "av.enums" else "core.enums"
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

-- | An @object@ of C values (the C API takes @uint32_t@ so clients can pass
-- values the header doesn't name).
renderOpenEnum :: SemanticModel -> Docs -> SEnumModel -> (FilePath, Text)
renderOpenEnum model docs e = (path, body)
  where
    pkg = if inOtherLibrary model (enumFile e) then "av.enums" else "core.enums"
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
kotlinEnumName n = pascalCase n

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

renderException :: Docs -> SemanticModel -> SEnumModel -> (FilePath, Text)
renderException docs model e = (path, body)
  where
    isAvError = inOtherLibrary model (enumFile e)
    isCrypto = inStandalone model (enumFile e)
    pkg
        | isAvError = "av.exceptions"
        | isCrypto = "crypto.exceptions"
        | otherwise = "core.exceptions"
    className = exceptionClassName model e
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

-- | @Tox_Err_Friend_Add@ becomes @ToxFriendAddException@.
exceptionClassName :: SemanticModel -> SEnumModel -> Text
exceptionClassName model e = exceptionClassFor model pascalCase (enumName e)

--------------------------------------------------------------------------------
-- Data classes (value classes for typedef-newtypes)
--------------------------------------------------------------------------------

-- | @typedef uint32_t Tox_Friend_Number;@ becomes a value class over @Int@,
-- as JNI has no unsigned types.
renderIdNewtype :: SemanticModel -> SIdTypeModel -> (FilePath, Text)
renderIdNewtype model i =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    className = classPrefix model <> pascalCase (idName i)
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

-- | C width of the params a value class wraps, which must agree.
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

-- | Not a value class, as those can't override @equals@, and @ByteArray@
-- compares by reference.
renderFixedBytes :: SemanticModel -> (Text, Text) -> (FilePath, Text)
renderFixedBytes model (sizeConst, semanticName) =
    ( "lib/src/main/kotlin/im/tox/tox4j/core/data/" <> Text.unpack className <> ".kt"
    , body
    )
  where
    className = classPrefix model <> pascalCase semanticName
    (constsObj, constsImport)
        | isCryptoConstant model sizeConst = ("ToxCryptoConstants", "im.tox.tox4j.crypto.ToxCryptoConstants")
        | otherwise                  = ("ToxCoreConstants",   "im.tox.tox4j.core.ToxCoreConstants")
    sizeRef = constsObj <> "." <> stripToxPrefix sizeConst
    stripToxPrefix n
        | Just rest <- Text.stripPrefix (Text.toUpper (standalonePrefix model)) n = rest
        | Just rest <- Text.stripPrefix (Text.toUpper (K.rootLibrary model) <> "_") n = rest
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

-- | ktlint wants this on one line if it fits in 140 columns.
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
    pkg = if isAv model cb then "av.callbacks" else "core.callbacks"
    interfaceName = callbackInterfaceName model cb
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

    cbMethod = callbackMethodName model cb

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
                ++ [ "    fun " <> cbMethod <> "("
                   ]
                ++ [ "        " <> paramKotlinName model cb p <> ": " <> renderMethodParamType model (cbCName cb) p <> ","
                   | p <- userParams
                   ]
                ++ [ "        state: ToxCoreState,"
                   , "    ): ToxCoreState = state"
                   , "}"
                   ]

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

-- | A C param named @state@ would clash with @state: ToxCoreState@, so it's
-- e.g. @callState@.
paramKotlinName :: SemanticModel -> SCallbackTypeModel -> SParameter -> Text
paramKotlinName model cb p
    | paramName p == "state" = dedupTrailingState renamed
    | otherwise = camelCase (paramName p)
  where
    renamed = case Text.uncons (callbackBaseName model cb) of
        Nothing     -> "newState"
        Just (c, t) -> Text.cons (Char.toLower c) t <> "State"
    -- @CallStateCallback@ would otherwise yield @callStateState@.
    dedupTrailingState s = case Text.stripSuffix "StateState" s of
        Just rest -> rest <> "State"
        Nothing   -> s

-- | Another library's callback, like toxav's.
isAv :: SemanticModel -> SCallbackTypeModel -> Bool
isAv model cb = inOtherLibrary model (cbFile cb)

-- | @conference_message_cb@ becomes @ConferenceMessage@, @toxav_call_cb@
-- @Call@.
callbackBaseName :: SemanticModel -> SCallbackTypeModel -> Text
callbackBaseName model cb =
    pascalCase
        . stripSuffix "_cb"
        . stripNamespace (K.libraryWords model)
        $ cbName cb
  where
    stripPrefix p t = case Text.stripPrefix p t of
        Just rest -> rest
        Nothing   -> t
    stripSuffix s t = case Text.stripSuffix s t of
        Just rest -> rest
        Nothing   -> t

callbackInterfaceName :: SemanticModel -> SCallbackTypeModel -> Text
callbackInterfaceName model cb = callbackBaseName model cb <> "Callback"

callbackMethodName :: SemanticModel -> SCallbackTypeModel -> Text
callbackMethodName model cb =
    case Text.uncons (callbackBaseName model cb) of
        Nothing     -> ""
        Just (c, t) -> Text.cons (Char.toLower c) t

importsFor :: Text {- C callback name -} -> SemanticModel -> [SParameter] -> [Text]
importsFor cbName_ model params =
    case List.sort . List.nub
            $ concatMap importsForType (map paramType params)
                ++ [ paramWrapperImport cls
                   | p <- params
                   , Just cls <- [paramWrapperFor model cbName_ p]
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

aggregateListeners :: SemanticModel -> [SCallbackTypeModel] -> [(FilePath, Text)]
aggregateListeners model cbs =
    [ aggregate "core.callbacks" "ToxCoreEventListener" coreCbNames
    , aggregate "av.callbacks" "ToxAvEventListener" avCbNames
    , combinedListener
    ]
  where
    coreCbNames = List.sort (map (callbackInterfaceName model) (filter (not . isAv model) cbs))
    avCbNames = List.sort (map (callbackInterfaceName model) (filter (isAv model) cbs))

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
    coreMethods = methodsForApi model "ToxCore" (coreResourceNames model)
    avMethods = methodsForApi model "ToxAv" (avResourceNames model)
    cryptoMethods = methodsForApi model "ToxCrypto" (cryptoResourceNames model)

-- | Resources each interface flattens: @tox.friendAdd(...)@.

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
               , Just cls <- [paramWrapperFor model (methodName m) p]
               ]
            ++ [ imp
               | (_, m) <- pairs
               , Just imp <- [returnWrapperImport model (methodName m) (output m)]
               ]
    importsForSType (SResourceId n) = ["im.tox.tox4j.core.data." <> kotlinNameOf model n]
    importsForSType (SEnum n) = ["im.tox.tox4j." <> enumPkgOf n <> ".enums." <> kotlinNameOf model n]
    importsForSType (SFixedBytes sizeConst _) =
        case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing      -> []
    importsForSType _ = []
    enumPkgOf n
        | isOtherLibraryName model n = "av"
        | otherwise = "core"

    superclause = case interfaceSelfResource model name of
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

renderMethodSig :: Map Text Text -> SemanticModel -> Text -> (SResource, SMethod) -> [Text]
renderMethodSig docs model iface (r, m)
    | isPropertyShape =
        kdoc ++
        [ "    val " <> kotlinPropertyName model m <> ret
        ]
    -- ktlint's function-signature shape.
    | length allArgs >= 2 =
        kdoc ++
        [ "    " <> roleToKotlin (methodRole m) <> kotlinMethodName model m <> "(" ]
            ++ [ "        " <> a <> "," | a <- allArgs ]
            ++ [ "    )" <> ret ]
    | otherwise =
        kdoc ++
        [ "    "
            <> roleToKotlin (methodRole m)
            <> kotlinMethodName model m
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

    includeOwnId = case methodRole m of
        Constructor -> False
        _           -> True
    pathArgs = [pname <> ": " <> tname | (pname, tname) <- pathIdArgsFor model r includeOwnId]

    -- A handle that isn't the receiver (@Pass_Key@ on ToxCrypto) is an explicit
    -- parameter.
    handleArg = case interfaceSelfResource model iface of
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
    renderReturn SBool
      | Just _ <- methodErrorType m
      , not (boolReturnsValue (methodName m)) = ""
    renderReturn ty = ": " <> case returnWrapperClass model (methodName m) ty of
        Just cls -> cls
        Nothing  -> renderType model ty

--------------------------------------------------------------------------------
-- ToxOptions data class
--------------------------------------------------------------------------------

data PropOrGroup
    = PlainProp SProperty
    | SealedGroupProp SealedGroup

-- | Proxy and savedata are sealed types, so an HTTP proxy must have a host
-- and port.
toxOptionsClass :: SemanticModel -> [(FilePath, Text)]
toxOptionsClass model =
    case K.handlesOfKind model SettingsHandle of
        [] -> []
        options : _ ->
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
        SEnum n -> kotlinNameOf model n <> ".entries.first()"
        _ -> "TODO()"

    optionImports =
        case List.sort . List.nub $
            ["im.tox.tox4j.core.ToxCoreConstants"]
            ++ [ "im.tox.tox4j.core.enums." <> kotlinNameOf model n
               | PlainProp p <- visibleProps
               , SEnum n <- [propType p]
               ] of
            []   -> []
            imps -> map (\i -> "import " <> i) imps

--------------------------------------------------------------------------------
-- Kdoc emission
--------------------------------------------------------------------------------

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

