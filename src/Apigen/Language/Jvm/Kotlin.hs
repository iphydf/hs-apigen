{-# LANGUAGE OverloadedStrings #-}

-- | Kotlin public API: constants, enums, exceptions, data classes, callbacks
-- and interfaces.
module Apigen.Language.Jvm.Kotlin (generate) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.Jvm.Conventions   (SealedGroup (..),
                                                    callbackKdocExtra,
                                                    constantsExtraMembers,
                                                    errorEnumIsReachable,
                                                    exceptionExtraCodes,
                                                    interfaceExtraMembers,
                                                    interfaceGenericParams,
                                                    interfaceSelfResource,
                                                    isCryptoConstant,
                                                    lookupSealedGroup,
                                                    syntheticExceptionEnums)
import           Apigen.Language.Jvm.Kdoc          (transformKdoc)
import           Apigen.Language.Jvm.Kotlin.Common (cRange, camelCase,
                                                    isSkippableMethodFor,
                                                    kotlinMethodName,
                                                    kotlinNameOf,
                                                    kotlinPropertyName,
                                                    kotlinSource, pascalCase,
                                                    pathIdArgs, pathIdArgsFor,
                                                    renderType, typeImports)
import           Apigen.Language.Jvm.Model         (Subsystem (..), classPrefix,
                                                    exceptionClassFor,
                                                    fileSubsystem, jvmName,
                                                    optionsClass, rootSubsystem,
                                                    standalonePrefix, stateType,
                                                    subsystems)
import           Apigen.Model                      (ignoresReturn, isOpenEnum)
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
                                                    SResourceKind (..),
                                                    SResourceType (..),
                                                    SScalarTypeModel (..),
                                                    SType (..),
                                                    SemanticModel (..))
import qualified Data.Char                         as Char
import qualified Data.List                         as List
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           System.FilePath                   (takeDirectory, takeFileName)

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model =
    [ (kotlinSource model (subWord s) (constantsClass s), constantsObject (funcDocs docs) model s)
    | s <- subsystems model, subKind s `elem` [RootHandle, StandaloneHandle] ]
        ++ map (renderEnum model) (filter (not . isErrorOrSpecialEnum) (enums model))
        ++ map (renderOpenEnum model docs) (filter isOpenEnum (enums model))
        ++ map (renderException docs model)
               (filter isReachableErrorEnum (enums model) ++ syntheticExceptionEnums model)
        ++ map (renderIdNewtype model) (idTypes model)
        ++ map (renderUnit docs model) (scalarTypes model)
        ++ map (renderFixedBytes model) (arrayTypes model)
        ++ map (renderCallback (funcDocs docs) model) (callbacks model)
        ++ aggregateListeners model (callbacks model)
        ++ publicApi (funcDocs docs) model
        ++ toxOptionsClass model
  where
    isErrorEnum e = enumKind e == ErrorEnum
    isErrorOrSpecialEnum e =
        isErrorEnum e
            || isOpenEnum e

    isReachableErrorEnum e =
        isErrorEnum e && errorEnumIsReachable model (enumName e)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The root's constants, and a standalone handle's without its word:
-- @ToxCryptoConstants.KEY_LENGTH@.
constantsObject :: Map Text Text -> SemanticModel -> Subsystem -> Text
constantsObject docs model s =
    renderConstantsObject model docs (subWord s) (constantsClass s) own
  where
    own = List.sortOn (kotlinName model) $ case subKind s of
        StandaloneHandle -> map stripPassPrefix (filter (isCryptoConstant model . constantName) (constants model))
        _                -> filter (not . isCryptoConstant model . constantName) (constants model)
    -- @TOX_PASS_KEY_LENGTH@ is @TOX_KEY_LENGTH@.
    stripPassPrefix c = case Text.stripPrefix (Text.toUpper (standalonePrefix model)) (constantName c) of
        Just rest -> c {constantName = Text.toUpper (K.rootLibrary model) <> "_" <> rest}
        Nothing   -> c

-- | A subsystem's constants object: @ToxCoreConstants@.
constantsClass :: Subsystem -> Text
constantsClass s = subClass s <> "Constants"

-- | The subsystem whose object has a constant.
constantsOwner :: SemanticModel -> Text -> Subsystem
constantsOwner model c = case [ s | s <- subsystems model, subKind s == StandaloneHandle ] of
    s : _ | isCryptoConstant model c -> s
    _                                -> rootSubsystem model

renderConstantsObject :: SemanticModel -> Map Text Text -> Text -> Text -> [SConstantModel] -> Text
renderConstantsObject model docs pkg objName entries =
    Text.unlines $
        [ "package " <> jvmName model pkg
        , ""
        ]
            ++ ["object " <> objName <> " {"]
            ++ drop 1 (concatMap renderEntry entries ++ extras)
            ++ ["}"]
  where
    extras = case constantsExtraMembers model objName of
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
    pkg = subWord (fileSubsystem model (enumFile e)) <> ".enums"
    className = kotlinNameOf model (enumSemanticName e)
    path = kotlinSource model pkg className
    body =
        Text.unlines $
            [ "package " <> jvmName model pkg
            , ""
            , "enum class " <> className <> " {"
            ]
                ++ memberLines
                ++ ["}"]
    memberLines = case enumMembers e of
        [] -> []
        members ->
            map (\m -> "    " <> enumMemberName m <> ",") members

-- | An @object@ of C values (the C API takes @uint32_t@ so clients can pass
-- values the header doesn't name).
renderOpenEnum :: SemanticModel -> Docs -> SEnumModel -> (FilePath, Text)
renderOpenEnum model docs e = (path, body)
  where
    pkg = subWord (fileSubsystem model (enumFile e)) <> ".enums"
    className = kotlinNameOf model (enumSemanticName e)
    path = kotlinSource model pkg className
    body =
        Text.unlines $
            [ "package " <> jvmName model pkg
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
    memberKdoc m = maybe [] (renderMemberKdoc 4 . transformKdoc model)
        (Map.lookup (enumName e, enumMemberCName m) (enumMemberDocs docs))

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

renderException :: Docs -> SemanticModel -> SEnumModel -> (FilePath, Text)
renderException docs model e = (path, body)
  where
    pkg = subWord (fileSubsystem model (enumFile e)) <> ".exceptions"
    className = exceptionClassName model e
    path =
        kotlinSource model pkg className
    -- Extra codes have no C name: only the C++ code throws them.
    codeMembers = [(Just (enumMemberCName m), enumMemberName m) | m <- enumMembers e, enumMemberName m /= "OK"]
        ++ [(Nothing, name) | (name, _) <- exceptionExtraCodes model (enumName e)]
    body =
        Text.unlines $
            [ "package " <> jvmName model pkg
            , ""
            , "import " <> jvmName model ("exceptions." <> classPrefix model <> "Exception")
            , ""
            , "class " <> className <> " : " <> classPrefix model <> "Exception {"
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
        in maybe [] (renderMemberKdoc 8 . transformKdoc model) raw ++ ["        " <> kname <> ","]
    extras = exceptionExtraCodes model (enumName e)

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
    ( kotlinSource model "core.data" className
    , body
    )
  where
    className = classPrefix model <> pascalCase (idName i)
    body =
        Text.unlines
            [ "package " <> jvmName model "core.data"
            , ""
            , "import kotlin.jvm.JvmInline"
            , ""
            , "@JvmInline"
            , "value class " <> className <> "("
            , "    val value: Int,"
            , ")"
            ]

-- | A unit, as a value class of the number JNI passes: @ToxAvBitRate@.
renderUnit :: Docs -> SemanticModel -> SScalarTypeModel -> (FilePath, Text)
renderUnit docs model u = (kotlinSource model pkg cls, body)
  where
    cls = kotlinNameOf model (scalarName u)
    pkg = subWord (fileSubsystem model (scalarFile u)) <> ".data"
    body = Text.unlines $
        [ "package " <> jvmName model pkg
        , ""
        , "import kotlin.jvm.JvmInline"
        , ""
        ]
        ++ maybe [] (renderMemberKdoc 0 . transformKdoc model) (Map.lookup (scalarCName u) (funcDocs docs))
        ++
        [ "@JvmInline"
        , "value class " <> cls <> "("
        , "    val value: " <> renderType model (scalarType u) <> ","
        ]
        ++ case cRange (scalarType u) of
            Just (cType, lo, hi) ->
                [ ") {"
                , "    init {"
                , "        require(value in " <> lo <> ".." <> hi <> ") { \"" <> cls <> " must fit in " <> cType <> ", got $value\" }"
                , "    }"
                , "}"
                ]
            Nothing -> [")"]

-- | Not a value class, as those can't override @equals@, and @ByteArray@
-- compares by reference.
renderFixedBytes :: SemanticModel -> (Text, Text) -> (FilePath, Text)
renderFixedBytes model (sizeConst, semanticName) =
    ( kotlinSource model "core.data" className
    , body
    )
  where
    className = classPrefix model <> pascalCase semanticName
    owner = constantsOwner model sizeConst
    constsObj = constantsClass owner
    constsImport = jvmName model (subWord owner <> "." <> constsObj)
    sizeRef = constsObj <> "." <> stripToxPrefix sizeConst
    stripToxPrefix n
        | Just rest <- Text.stripPrefix (Text.toUpper (standalonePrefix model)) n = rest
        | Just rest <- Text.stripPrefix (Text.toUpper (K.rootLibrary model) <> "_") n = rest
        | otherwise                        = n
    body =
        Text.unlines $
            [ "package " <> jvmName model "core.data"
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

--------------------------------------------------------------------------------
-- Callback interfaces
--------------------------------------------------------------------------------

renderCallback :: Map Text Text -> SemanticModel -> SCallbackTypeModel -> (FilePath, Text)
renderCallback funcs model cb = (path, body)
  where
    pkg = subWord (fileSubsystem model (cbFile cb)) <> ".callbacks"
    interfaceName = callbackInterfaceName model cb
    path =
        kotlinSource model pkg interfaceName

    -- Handles are the receiver and user_data, which the dispatcher fills in.
    userParams = filter isUserParam (cbParams cb)
    isUserParam p = case paramType p of
        SHandle _ -> False
        _         -> True

    cbMethod = callbackMethodName model cb

    -- The C typedef's doxygen, so its @param entries match the method.
    methodKdoc = case (Map.lookup (cbCName cb) funcs, callbackKdocExtra model (cbCName cb)) of
        (Nothing, [])     -> []
        (Nothing, extras) -> renderMemberKdoc 4 (Text.intercalate "\n" extras)
        (Just raw, [])    -> renderMemberKdoc 4 (transformKdoc model raw)
        (Just raw, extras) ->
            renderMemberKdoc 4
                (transformKdoc model raw <> "\n\n" <> Text.intercalate "\n" extras)

    body =
        Text.unlines $
            [ "package " <> jvmName model pkg
            , ""
            ]
                ++ importsFor model userParams
                ++ [ "interface " <> interfaceName <> "<" <> stateType model <> "> {"
                   ]
                ++ methodKdoc
                ++ [ "    fun " <> cbMethod <> "("
                   ]
                ++ [ "        " <> paramKotlinName model cb p <> ": " <> renderType model (paramType p) <> ","
                   | p <- userParams
                   ]
                ++ [ "        state: " <> stateType model <> ","
                   , "    ): " <> stateType model <> " = state"
                   , "}"
                   ]

-- | One line if it fits there.
renderMemberKdoc :: Int -> Text -> [Text]
renderMemberKdoc col body = case Text.lines (Text.strip body) of
    []     -> []
    [line] -> [Text.replicate col " " <> "/** " <> line <> " */"]
    ls     -> kdocBlock col ls

kdocBlock :: Int -> [Text] -> [Text]
kdocBlock col ls =
    [indent <> "/**"]
        ++ [ if Text.null l then indent <> " *" else indent <> " * " <> l | l <- ls ]
        ++ [indent <> " */"]
  where
    indent = Text.replicate col " "

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

-- | @conference_message_cb@ becomes @ConferenceMessage@, @toxav_call_cb@
-- @Call@.
callbackBaseName :: SemanticModel -> SCallbackTypeModel -> Text
callbackBaseName model cb =
    pascalCase
        . stripSuffix "_cb"
        . stripNamespace (K.libraryWords model)
        $ cbName cb
  where
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

importsFor :: SemanticModel -> [SParameter] -> [Text]
importsFor model params =
    case List.sort . List.nub $ concatMap (typeImports model . paramType) params of
        []   -> []
        imps -> map (\i -> "import " <> i) imps ++ [""]

aggregateListeners :: SemanticModel -> [SCallbackTypeModel] -> [(FilePath, Text)]
aggregateListeners model cbs =
    [ aggregate (subWord s <> ".callbacks") (listenerOf s)
        (List.sort (map (callbackInterfaceName model) (filter (inDir s . cbFile) cbs)))
    | s <- eventSubs ]
    ++ [combinedListener]
  where
    eventSubs = [ s | s <- subsystems model, subKind s `elem` [RootHandle, LibraryHandle] ]
    listenerOf s = subClass s <> "EventListener"
    state = stateType model
    inDir s file = dirOf file == dirOf (resourceFile (subHandle s))
    dirOf = takeFileName . takeDirectory . Text.unpack

    aggregate :: Text -> Text -> [Text] -> (FilePath, Text)
    aggregate pkg name members =
        ( kotlinSource model pkg name
        , Text.unlines $
            [ "package " <> jvmName model pkg
            , ""
            , "interface " <> name <> "<" <> state <> "> :"
            ]
                ++ extendsLines members
        )

    extendsLines :: [Text] -> [Text]
    extendsLines [] = []
    extendsLines names =
        let formatted = [ "    " <> n <> "<" <> state <> ">" | n <- names ]
        in zipWith (<>) formatted (replicate (length formatted - 1) "," ++ [""])

    -- Every subsystem's listener.
    combinedListener :: (FilePath, Text)
    combinedListener =
        let name = classPrefix model <> "EventListener"
        in ( kotlinSource model "" name
           , Text.unlines $
               [ "package " <> jvmName model ""
               , ""
               ]
               ++ List.sort [ "import " <> jvmName model (subWord s <> ".callbacks." <> listenerOf s) | s <- eventSubs ]
               ++ [ ""
                  , "interface " <> name <> "<" <> state <> "> :"
                  ]
               ++ extendsLines (map listenerOf eventSubs)
           )

--------------------------------------------------------------------------------
-- Public API interfaces (ToxCore.kt / ToxAv.kt / ToxCrypto.kt)
--------------------------------------------------------------------------------

publicApi :: Map Text Text -> SemanticModel -> [(FilePath, Text)]
publicApi docs model =
    [ renderInterface docs model s (methodsForApi model (subClass s) (subResources s))
    | s <- subsystems model ]

methodsForApi :: SemanticModel -> Text -> [Text] -> [(SResource, SMethod)]
methodsForApi model iface rs =
    [ (r, m)
    | r <- resources model
    , resourceName r `elem` rs
    , m <- methods r
    , not (isSkippableMethodFor model iface m)
    ]

renderInterface :: Map Text Text -> SemanticModel -> Subsystem -> [(SResource, SMethod)] -> (FilePath, Text)
renderInterface docs model sub pairs =
    ( kotlinSource model pkg name
    , body
    )
  where
    pkg = subWord sub
    name = subClass sub
    state = stateType model
    iterateMethods
        | subKind sub `elem` [RootHandle, LibraryHandle] =
            [ ""
            , "    val iterationInterval: Int"
            , ""
            , "    fun <" <> state <> "> iterate("
            , "        handler: " <> jvmName model (pkg <> ".callbacks." <> name <> "EventListener") <> "<" <> state <> ">,"
            , "        state: " <> state <> ","
            , "    ): " <> state
            ]
        | otherwise = []

    interfaceImports =
        map (\i -> "import " <> i) . List.sort . List.nub $
            concat
                [ typeImports model ty
                | (_, m) <- pairs
                , ty <- output m : map paramType (inputs m)
                ]
            ++ [ jvmName model ("core.data." <> tname)
               | (r, _) <- pairs
               , (_, tname) <- pathIdArgs model r
               ]

    superclause = case interfaceSelfResource model name of
        Just _  -> " : AutoCloseable"
        Nothing -> ""

    generics = case interfaceGenericParams model name of
        [] -> ""
        ps -> "<" <> Text.intercalate ", " ps <> ">"

    extras = case interfaceExtraMembers model name of
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
            [ "package " <> jvmName model pkg
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

    renderInput p = camelCase (paramName p) <> ": " <> renderType model (paramType p)

    renderReturn SVoid = ""
    renderReturn SBool
      | Just _ <- methodErrorType m
      , ignoresReturn m = ""
    renderReturn ty = ": " <> renderType model ty

--------------------------------------------------------------------------------
-- ToxOptions data class
--------------------------------------------------------------------------------

data PropOrGroup
    = PlainProp SProperty
    | SealedGroupProp SealedGroup

-- | Proxy and savedata are sealed types, so an HTTP proxy must have a host
-- and port. Their classes are hand-written.
toxOptionsClass :: SemanticModel -> [(FilePath, Text)]
toxOptionsClass model =
    case K.handlesOfKind model SettingsHandle of
        [] -> []
        options : _ ->
            [ ( kotlinSource model (subWord (rootSubsystem model) <> ".options") (optionsClass model)
              , renderToxOptions model (properties options)
              )
            ]

renderToxOptions :: SemanticModel -> [SProperty] -> Text
renderToxOptions model props =
    Text.unlines $
        [ "package " <> jvmName model (rootWord <> ".options")
        , ""
        ]
            ++ optionImports
            ++ [ ""
               , "data class " <> optionsClass model <> "("
               ]
            ++ map renderField visibleProps
            ++ [")"]
  where
    rootWord = subWord (rootSubsystem model)
    rootConstants = constantsClass (rootSubsystem model)
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

    kotlinPropType p = renderType model (propType p)

    -- As @tox_options_default()@.
    defaultForName p = case propType p of
        -- C's are in the constants: @DEFAULT_START_PORT@.
        SScalar n _ -> kotlinNameOf model n <> "(" <> rootConstants <> ".DEFAULT_" <> Text.toUpper (propName p) <> ")"
        SBool | "experimental_" `Text.isPrefixOf` propName p -> "false"
        SBool -> "true"
        SString -> "\"\""
        SInt _ -> "0"
        SUInt _ -> "0u"
        SSizeT -> "0L"
        SBytes -> "ByteArray(0)"
        SEnum n -> kotlinNameOf model n <> ".entries.first()"
        t -> error (Text.unpack (optionsClass model) <> ": no default for " <> show (propName p) <> ": " <> show t)

    optionImports =
        case List.sort . List.nub $
            [jvmName model (rootWord <> "." <> rootConstants)]
            ++ [ imp | PlainProp p <- visibleProps, imp <- typeImports model (propType p) ] of
            []   -> []
            imps -> map (\i -> "import " <> i) imps

--------------------------------------------------------------------------------
-- Kdoc emission
--------------------------------------------------------------------------------

renderKdoc :: SemanticModel -> Map Text Text -> SMethod -> [Text]
renderKdoc model docs m = "" : case Map.lookup (methodName m) docs of
    Nothing  -> []
    Just raw -> kdocBlock 4 (Text.splitOn "\n" (Text.dropWhileEnd (== '\n') (transformKdoc model raw)))

