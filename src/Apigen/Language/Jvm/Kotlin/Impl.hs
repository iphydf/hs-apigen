{-# LANGUAGE OverloadedStrings #-}

-- | Generates the Kotlin @Tox{Core,Av,Crypto}Impl.kt@ classes that
-- bridge the public-API interfaces to the @*Jni.java@ native
-- declarations.
--
-- Per-method shape:
--
-- @
-- override fun nameInKotlin(arg1: ValueClass, arg2: EnumX): ReturnT =
--     ToxCoreJni.toxNameInC(instanceNumber, arg1.value, arg2.ordinal)
-- @
--
-- Value classes unwrap via @.value@, enums via @.ordinal@, primitives
-- pass through. Return values are wrapped back into the public type
-- (value class constructor, @EnumX.values()[result]@). Methods that
-- raise an exception via the @Tox_Err_*@ pattern declare a @throws@.
--
-- Constructor, destructor, and iteration are emitted as special cases
-- — see the @specialBlock*@ helpers.
module Apigen.Language.Jvm.Kotlin.Impl (generate) where

import           Apigen.Language.Jvm.Conventions   (implExtraMembers,
                                                    interfaceGenericParams,
                                                    optionsWireFields,
                                                    paramWrapperFor,
                                                    paramWrapperImport,
                                                    returnWrapperClass,
                                                    sealedGroupAccess)
import           Apigen.Language.Jvm.Kotlin.Common (camelCase,
                                                    isSkippableMethodFor,
                                                    kotlinPropertyName,
                                                    kotlinMethodName,
                                                    kotlinNameOf, pascalCase,
                                                    pathIdArgsFor,
                                                    renderMethodParamType,
                                                    renderType)
import           Apigen.Semantic                   (SMethod (..),
                                                    SMethodRole (..),
                                                    SParameter (..),
                                                    SProperty (..),
                                                    SResource (..),
                                                    SResourceType (..),
                                                    SType (..),
                                                    SemanticModel (..))
import qualified Data.List                         as List
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreImpl.kt"
      , renderImpl model coreSpec
      )
    , ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxAvImpl.kt"
      , renderImpl model avSpec
      )
    , ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxCryptoImpl.kt"
      , renderImpl model cryptoSpec
      )
    ]

--------------------------------------------------------------------------------
-- Subsystem specs
--------------------------------------------------------------------------------

data ImplSpec = ImplSpec
    { subsystemPkg      :: Text -- ^ Sub-package: @core@, @av@, @crypto@.
    , implClassName     :: Text -- ^ @ToxCoreImpl@, @ToxAvImpl@, @ToxCryptoImpl@.
    , interfaceName     :: Text -- ^ @ToxCore@, @ToxAv@, @ToxCrypto@.
    , jniClass          :: Text -- ^ @ToxCoreJni@, @ToxAvJni@, @ToxCryptoJni@.
    , dispatcher        :: Maybe Text -- ^ @ToxCoreEventDispatch@ if iterate() applies.
    , listenerType      :: Maybe Text -- ^ @ToxCoreEventListener@ if iterate() applies.
    , primaryConstructor :: PrimaryCtor
    , resourceNames     :: [Text] -- ^ Resources whose methods land on this Impl.
    }

-- | How the Impl is constructed and how the underlying @instanceNumber@
-- is obtained.
data PrimaryCtor
    = -- | @class ToxCoreImpl(val options: ToxOptions) : ToxCore@
      OptionsCtor
    | -- | @class ToxAvImpl(private val tox: ToxCoreImpl) : ToxAv@
      ToxRefCtor
    | -- | @object ToxCryptoImpl : ToxCrypto<PassKey>@ — no managed
      -- instance of its own. The interface's generic resource
      -- (Pass_Key) is reified as a value class the caller holds:
      -- Constructor-role methods return a freshly wrapped handle,
      -- instance methods take it as their leading parameter, and the
      -- destructor lives on the value class itself (@PassKey.close@),
      -- not on the impl. Multiple keys can be alive concurrently —
      -- a per-impl @instanceNumber@ could not express that.
      StatelessObject

coreSpec, avSpec, cryptoSpec :: ImplSpec
coreSpec = ImplSpec
    { subsystemPkg = "core"
    , implClassName = "ToxCoreImpl"
    , interfaceName = "ToxCore"
    , jniClass = "ToxCoreJni"
    , dispatcher = Just "ToxCoreEventDispatch"
    , listenerType = Just "ToxCoreEventListener"
    , primaryConstructor = OptionsCtor
    , resourceNames =
        [ "Tox", "Friend", "Conference", "Conference_Peer", "Conference_Offline_Peer"
        , "Group", "Group_Peer", "File"
        -- "Options" omitted: see Apigen.Language.Jvm.Kotlin for why.
        ]
    }

avSpec = ImplSpec
    { subsystemPkg = "av"
    , implClassName = "ToxAvImpl"
    , interfaceName = "ToxAv"
    , jniClass = "ToxAvJni"
    , dispatcher = Just "ToxAvEventDispatch"
    , listenerType = Just "ToxAvEventListener"
    , primaryConstructor = ToxRefCtor
    , resourceNames = ["AV"]
    }

cryptoSpec = ImplSpec
    { subsystemPkg = "crypto"
    , implClassName = "ToxCryptoImpl"
    , interfaceName = "ToxCrypto"
    , jniClass = "ToxCryptoJni"
    , dispatcher = Nothing
    , listenerType = Nothing
    , primaryConstructor = StatelessObject
    , resourceNames = ["Pass_Key"]
    }

--------------------------------------------------------------------------------
-- File body
--------------------------------------------------------------------------------

renderImpl :: SemanticModel -> ImplSpec -> Text
renderImpl model spec =
    Text.unlines $
        [ "package im.tox.tox4j.impl.jni"
        , ""
        ]
            ++ importLines model spec methodList
            ++ [""]
            ++ topLevelHelpers
            ++ classHeader spec
            ++ classBody spec methodList
  where
    methodList =
        [ (r, m)
        | r <- resources model
        , resourceName r `elem` resourceNames spec
        , m <- methods r
        , not (isSkippableMethodFor model (interfaceName spec) m)
        ]

    -- File-private extensions that the generated accessors call.
    -- @atOrFirst@ shortens the enum-fallback pattern so long-named
    -- enums don't blow past ktlint's 140-char ceiling on the inline
    -- @ToxX.values().getOrElse(jni()) { ToxX.values()[0] }@ form.
    topLevelHelpers
        | any (any returnsEnum . inputs') methodList =
            [ "private fun <T> Array<T>.atOrFirst(index: Int): T = getOrNull(index) ?: this[0]"
            , ""
            ]
        | otherwise = []
    inputs' (_, m) = [output m]
    returnsEnum ty = case ty of
        SEnum _ -> True
        _ -> False

    classBody _ ms =
        constructorBlock model spec
            ++ closeBlock spec
            ++ (case dispatcher spec of
                  Just _ -> iterateBlock spec
                  Nothing -> [])
            ++ concatMap (renderMethod model spec) ms
            ++ implExtras
            ++ ["}"]

    -- Per-Impl-class hand-curated extras (e.g. ToxCoreImpl's
    -- @load(options) = ToxCoreImpl(options)@ factory that
    -- implements the matching @ToxCore.load@ entry from
    -- 'interfaceExtraMembers'). The trailing @""@ that 'renderMethod'
    -- emits supplies the blank-line separator before this block, so
    -- entries are interspersed with blanks (no leading blank).
    implExtras = List.intercalate [""] $ map (:[]) $ implExtraMembers (implClassName spec)

importLines :: SemanticModel -> ImplSpec -> [(SResource, SMethod)] -> [Text]
importLines model spec ms =
    map (\i -> "import " <> i) $ List.sort $ List.nub $ baseImports ++ typeImports
  where
    baseImports = ctorImports ++ ifaceImport ++ listenerImport
    ctorImports = case primaryConstructor spec of
        OptionsCtor ->
            [ "com.google.protobuf.ByteString"
            , "im.tox.tox4j.core.options.SaveDataOptions"
            , "im.tox.tox4j.core.options.ToxOptions"
            , "im.tox.tox4j.core.proto.Options"
            ]
        _ -> []
    ifaceImport =
        ["im.tox.tox4j." <> subsystemPkg spec <> "." <> interfaceName spec]
    listenerImport = case listenerType spec of
        Just listener -> ["im.tox.tox4j." <> subsystemPkg spec <> ".callbacks." <> listener]
        Nothing -> []
    typeImports =
        [ imp
        | (_, m) <- ms
        , p <- inputs m
        , imp <- importsForType (paramType p)
        ]
            ++ [ imp
               | (_, m) <- ms
               , imp <- importsForType (output m)
               ]
            ++ [ "im.tox.tox4j.core.data." <> tname
               | (r, _) <- ms
               , (_, tname) <- pathIdArgsFor model r True
               ]
            -- Imports for convention-wrapped param and return types
            -- (ToxConferenceMessage, ToxNickname, …). Same routing as
            -- the public-interface generator's 'paramWrapperImport'.
            ++ [ paramWrapperImport cls
               | (_, m) <- ms
               , p <- inputs m
               , Just cls <- [paramWrapperFor (methodName m) p]
               ]
            ++ [ paramWrapperImport cls
               | (_, m) <- ms
               , Just cls <- [returnWrapperClass (methodName m) (output m)]
               ]

    importsForType ty = case ty of
        SResourceId n -> ["im.tox.tox4j.core.data." <> kotlinNameOf n]
        SEnum n -> ["im.tox.tox4j." <> enumPkg n <> ".enums." <> kotlinNameOf n]
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data.Tox" <> pascalCase typedef]
            Nothing -> []
        _ -> []
    enumPkg n
        | "Toxav_" `Text.isPrefixOf` n = "av"
        | otherwise = "core"

    exceptionImport _ ety =
        "im.tox.tox4j." <> exnPkg ety <> ".exceptions." <> exceptionClass ety
    exnPkg ety
        | "Toxav_" `Text.isPrefixOf` ety = "av"
        | ety `elem` ["Tox_Err_Encryption", "Tox_Err_Decryption", "Tox_Err_Key_Derivation", "Tox_Err_Get_Salt"] =
            "crypto"
        | otherwise = "core"

    exceptionClass ety = case Text.stripPrefix "Tox_Err_" ety of
        Just rest -> "Tox" <> pascalCase rest <> "Exception"
        Nothing -> case Text.stripPrefix "Toxav_Err_" ety of
            Just rest -> "Toxav" <> pascalCase rest <> "Exception"
            Nothing -> ety

-- | ktlint wraps class headers with a single constructor parameter to
-- a three-line form. Pre-emit that shape so regen is a no-op.
classHeader :: ImplSpec -> [Text]
classHeader spec = case primaryConstructor spec of
    OptionsCtor ->
        -- The savedata blob is consumed during init and the C side
        -- keeps its own copy; the Kotlin-side reference would
        -- otherwise hold the (potentially multi-MB) bytes for the
        -- lifetime of the instance. Strip on assignment via @copy@,
        -- keep everything else.
        [ "class " <> implClassName spec <> "("
        , "    initOptions: ToxOptions,"
        , ") : " <> interfaceName spec <> " {"
        , "    val options: ToxOptions = initOptions.copy(saveData = SaveDataOptions.None)"
        , ""
        ]
    ToxRefCtor ->
        [ "class " <> implClassName spec <> "("
        , "    private val tox: ToxCoreImpl,"
        , ") : " <> interfaceName spec <> " {"
        ]
    StatelessObject ->
        let generics = case interfaceGenericParams (interfaceName spec) of
                [] -> ""
                ps -> "<" <> Text.intercalate ", " ps <> ">"
        in [ "object " <> implClassName spec <> " : " <> interfaceName spec <> generics <> " {" ]

--------------------------------------------------------------------------------
-- Constructor / destructor / iterate
--------------------------------------------------------------------------------

constructorBlock :: SemanticModel -> ImplSpec -> [Text]
constructorBlock model spec = case primaryConstructor spec of
    -- ktlint wraps multi-line @run { … }@ initialisers off the @=@
    -- onto their own indented line; emit that shape directly so
    -- regen is a no-op.
    OptionsCtor ->
        [ "    internal val instanceNumber ="
        , "        run {"
        , "            val opts ="
        , "                Options"
        , "                    .newBuilder()"
        ]
            ++ map ("                    " <>) (optionsBuilderCalls model)
            ++ [ "                    .build()"
               , "            " <> jniClass spec <> ".toxNew(opts.toByteArray())"
               , "        }"
               , ""
               ]
    ToxRefCtor ->
        [ "    internal val instanceNumber = " <> jniClass spec <> ".toxavNew(tox.instanceNumber)"
        , ""
        ]
    StatelessObject -> []

-- | Emit one proto-builder @.set<Field>(initOptions.<field>)@ call
-- per wire field ('optionsWireFields'), mirroring the @ToxOptions@
-- data class field-for-field. The same field list drives the proto
-- schema and the generated C++ decode side.
--
-- Reads from @initOptions@, the unmodified constructor argument,
-- rather than the public @options@ field — the latter is stripped of
-- @saveData@ to avoid pinning a multi-MB blob for the lifetime of
-- the instance.
optionsBuilderCalls :: SemanticModel -> [Text]
optionsBuilderCalls model =
    [ ".set" <> pascalCase (propName p) <> "(" <> value p <> ")"
    | p <- optionsWireFields model
    ]
  where
    value p = wrap p ("initOptions." <> access p <> unwrap p)

    -- Sealed-group properties (proxy_host, savedata, …) reach the
    -- Kotlin value through their group field; everything else is a
    -- direct camelCase property on @options@.
    access p = case sealedGroupAccess (propName p) of
        Just path -> path
        Nothing   -> camelCase (propName p)

    -- Enums ride as their ordinal (the JNI Enum::valueOf convention);
    -- ports are UShort in Kotlin (for ToxCoreConstants.DEFAULT_*_PORT
    -- compatibility) but the proto field is uint32, so widen.
    unwrap p = case propType p of
        SEnum _                      -> ".ordinal"
        _ | isPortField (propName p) -> ".toInt()"
        _                            -> ""

    -- Proto bytes fields take a ByteString, not a ByteArray.
    wrap p val = case propType p of
        SBytes -> "ByteString.copyFrom(" <> val <> ")"
        _      -> val

    isPortField n =
        n == "start_port" || n == "end_port" || n == "tcp_port" || n == "proxy_port"

closeBlock :: ImplSpec -> [Text]
closeBlock spec = case primaryConstructor spec of
    -- The stateless impl owns no instance; the handle value class
    -- (PassKey) is the AutoCloseable.
    StatelessObject -> []
    OptionsCtor -> close "toxKill"
    ToxRefCtor  -> close "toxavKill"
  where
    close destructor =
        [ "    override fun close(): Unit = " <> jniClass spec <> "." <> destructor <> "(instanceNumber)"
        , ""
        ]

iterateBlock :: ImplSpec -> [Text]
iterateBlock spec = case (dispatcher spec, listenerType spec) of
    (Just disp, Just listener) ->
        let body =
                "    ): ToxCoreState = " <> disp <> ".dispatch(handler, "
                    <> jniClass spec <> "." <> iterateMethod spec
                    <> "(instanceNumber), state)"
        in [ "    override val iterationInterval: Int"
           , "        get() = " <> jniClass spec <> "." <> intervalMethod spec <> "(instanceNumber)"
           , ""
           , "    override fun <ToxCoreState> iterate("
           , "        handler: " <> listener <> "<ToxCoreState>,"
           , "        state: ToxCoreState,"
           ]
           ++ if Text.length body <= 140
                  then [body, ""]
                  else
                      [ "    ): ToxCoreState ="
                      , "        " <> disp <> ".dispatch(handler, "
                            <> jniClass spec <> "." <> iterateMethod spec
                            <> "(instanceNumber), state)"
                      , ""
                      ]
    _ -> []
  where
    iterateMethod s = case primaryConstructor s of
        ToxRefCtor -> "toxavIterate"
        _ -> "toxIterate"
    intervalMethod s = case primaryConstructor s of
        ToxRefCtor -> "toxavIterationInterval"
        _ -> "toxIterationInterval"

--------------------------------------------------------------------------------
-- Method dispatch
--------------------------------------------------------------------------------

renderMethod :: SemanticModel -> ImplSpec -> (SResource, SMethod) -> [Text]
renderMethod model spec (r, m)
    -- A zero-arg getter on the interface is declared as @val
    -- foo: T@. The Impl matches that shape with a @val
    -- ... get() = ...@ accessor; emitting @override fun foo(): T@
    -- here wouldn't satisfy the property override. The property name
    -- drops a leading @get@ (see 'kotlinPropertyName') so callers see
    -- @tox.savedata@, not @tox.getSavedata@.
    | isPropertyShape =
        let singleLine =
                "    override val " <> kotlinPropertyName m <> returnSig
                    <> " get() = " <> wrappedCall
            sigLine = "    override val " <> kotlinPropertyName m <> returnSig <> " get() ="
        in if Text.length singleLine <= 140
               then [singleLine, ""]
               else [sigLine, "        " <> wrappedCall, ""]
    -- 2+ args: ktlint wraps the signature across multiple lines.
    -- Tier 1: body on the same line as the closing @)@ when it fits.
    -- Tier 2: body on its own indented line when that fits.
    -- Tier 3: peel any outer wrapper (e.g. @ToxGroupMessageId(...)@)
    -- onto its own line; if the inner call alone still doesn't fit,
    -- split the call's argument list across lines.
    | length sigArgs >= 2 =
        let singleLine = "    )" <> returnSig <> " = " <> wrappedCall
            bodyLine = "        " <> wrappedCall
            sigClose = "    )" <> returnSig <> " ="
        in [ "    override fun " <> kotlinMethodName m <> "(" ]
            ++ [ "        " <> a <> "," | a <- sigArgs ]
            ++ if Text.length singleLine <= 140
                   then [singleLine, ""]
                   else if Text.length bodyLine <= 140
                       then [sigClose, bodyLine, ""]
                       else if wrappedCall == jniCall
                           -- No outer wrap: hoist the JNI head onto
                           -- the @)@-close line, args one per line.
                           then [ "    )" <> returnSig <> " = " <> jniHead <> "(" ]
                               ++ [ "        " <> arg <> "," | arg <- jniCallArgs ]
                               ++ [ "    )", "" ]
                           -- Outer wrap (e.g. @ToxGroupMessageId(...)@):
                           -- indent the wrapper on its own line.
                           else sigClose : wrapBody 8 wrappedCall ++ [""]
    -- 0/1 arg: keep on a single line; ktlint splits across lines if
    -- the result exceeds 140 chars.
    | otherwise =
        let singleLine =
                "    override fun " <> kotlinMethodName m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " = " <> wrappedCall
            sigLine =
                "    override fun " <> kotlinMethodName m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " ="
        in if Text.length singleLine <= 140
               then [singleLine, ""]
               else [sigLine, "        " <> wrappedCall, ""]
  where
    stateless = case primaryConstructor spec of
        StatelessObject -> True
        _               -> False

    -- On a stateless impl the owning resource's handle is reified as
    -- a value class: Constructors *return* it (wrap the raw JNI int),
    -- statics never see it, and instance methods take it as their
    -- leading parameter — mirroring the interface emitter's
    -- @handleArg@ (Apigen.Language.Jvm.Kotlin).
    handleSigArgs
        | stateless = case (resourceType r, methodRole m) of
            (ResHandle, Constructor) -> []
            (ResHandle, StaticRole)  -> []
            (ResHandle, _)           ->
                [camelCase (resourceName r) <> ": " <> pascalCase (resourceName r)]
            _                        -> []
        | otherwise = []

    instanceJniArgs
        | stateless = case methodRole m of
            Constructor -> []
            StaticRole  -> []
            _           -> [camelCase (resourceName r) <> ".instanceNumber"]
        | otherwise = ["instanceNumber"]

    wrappedCall
        | stateless, SHandle h <- output m =
            pascalCase h <> "(" <> wrappedCall0 <> ")"
        | otherwise = wrappedCall0
    wrappedCall0 = wrapReturnFor (methodName m) model (output m) (effectiveErrorType m) jniCall
    isPropertyShape =
        null handleSigArgs
            && null pathArgsList
            && null (inputs m)
            && not (Text.null returnSig)
            && methodRole m /= Constructor
    -- Predicate methods get the bool as their return; their errorType
    -- still applies (the exception fires on failure) but it doesn't
    -- swallow the return value.
    effectiveErrorType meth = case (output meth, methodErrorType meth) of
        (SBool, Just _) | isPredicate (methodName meth) -> Nothing
        (_, e) -> e
    pathArgsList = pathIdArgsFor model r (methodRole m /= Constructor)
    sigArgs =
        handleSigArgs
            ++ [pname <> ": " <> tname | (pname, tname) <- pathArgsList]
            ++ map renderInputSig (inputs m)
    renderInputSig p =
        camelCase (paramName p) <> ": " <> renderMethodParamType model (methodName m) p

    -- 'returnWrapperClass' overrides the C-derived type when set
    -- (e.g. tox_self_get_name returns ToxNickname, not raw
    -- ByteArray). Falls through to renderType otherwise.
    returnSig = case (output m, methodErrorType m) of
        (SVoid, _)                                        -> ""
        (SBool, Just _) | not (isPredicate (methodName m)) -> ""
        (ty, _) -> case returnWrapperClass (methodName m) ty of
            Just cls -> ": " <> cls
            Nothing  -> ": " <> renderType model ty

    isPredicate n =
        Text.isInfixOf "_is_" n
            || Text.isInfixOf "_has_" n
            || Text.isSuffixOf "_exists" n

    jniCallArgs =
        instanceJniArgs
            ++ [pname <> ".value" | (pname, _) <- pathArgsList]
            ++ map (unwrapInput model (methodName m)) (inputs m)
    jniHead = jniClass spec <> "." <> jniMethodName (methodName m)
    jniCall = jniHead <> "(" <> Text.intercalate ", " jniCallArgs <> ")"

    -- | Lay out a too-long expression body across multiple lines at
    -- the given indent. If the body is the JNI call itself, split the
    -- call's argument list. If it's an outer wrapper like
    -- @ToxGroupMessageId(jniCall)@, peel the wrapper onto its own
    -- line, then recurse on the inner call.
    wrapBody :: Int -> Text -> [Text]
    wrapBody indent body
        | body == jniCall =
            [ indentS <> jniHead <> "(" ]
                ++ [ indentS <> "    " <> a <> "," | a <- jniCallArgs ]
                ++ [ indentS <> ")" ]
        | Just inner <- peelOuterWrap body
        , wrapperOpen <- Text.takeWhile (/= '(') body <> "("
        , Text.length (indentS <> inner <> ",") <= 140 =
            -- Inner fits on its own line under the wrapper.
            [ indentS <> wrapperOpen
            , indentS <> "    " <> inner <> ","
            , indentS <> ")"
            ]
        | Just _ <- peelOuterWrap body
        , wrapperOpen <- Text.takeWhile (/= '(') body <> "(" =
            -- Inner is still too long: recurse on it.
            [indentS <> wrapperOpen]
                ++ wrapBody (indent + 4) (fromMaybe body (peelOuterWrap body))
                ++ [indentS <> ")"]
        | otherwise = [indentS <> body]
      where
        indentS = Text.replicate indent " "

    -- | If @expr@ is of the form @<Wrapper>(<inner>)@ for a single
    -- balanced outer paren pair, return @<inner>@. Returns 'Nothing'
    -- for non-wrapped expressions (raw JNI calls, @.map { ... }@,
    -- etc.).
    peelOuterWrap :: Text -> Maybe Text
    peelOuterWrap expr = do
        rest <- Text.stripSuffix ")" expr
        let (prefix, afterOpen) = Text.breakOn "(" rest
        inner <- Text.stripPrefix "(" afterOpen
        -- Must be an identifier prefix and balanced inner.
        if Text.null prefix
            || Text.any (`elem` (" ()" :: String)) prefix
            || not (balanced inner)
        then Nothing
        else Just inner
      where
        balanced =
            (== 0)
                . Text.foldl
                    (\d c -> case c of
                        '(' -> d + 1
                        ')' -> d - 1
                        _ -> d)
                    (0 :: Int)

-- | Unwrap a Kotlin arg to its JNI primitive. Value classes (typedef
-- newtypes, named array typedefs, and convention-wrapped param
-- classes like @ToxConferenceMessage@) use @.value@; enums use
-- @.ordinal@; everything else passes through. @Port@ widens its
-- @UShort@ inner value to @Int@ for the JNI surface; @SampleCount@
-- (Kotlin @Int@) widens to @Long@ for @size_t@, and @AudioChannels@
-- (Kotlin @Int@) narrows to @Byte@ for @uint8_t@.
unwrapInput :: SemanticModel -> Text -> SParameter -> Text
unwrapInput model cMethod p = case paramWrapperFor cMethod p of
    Just "Port" -> name <> ".value.toInt()"
    Just _      -> applyJniConv (name <> ".value")
    Nothing -> case paramType p of
        SEnum _ -> applyJniConv (name <> ".ordinal")
        SResourceId _ -> name <> ".value"
        SFixedBytes sizeConst _ | hasTypedef model sizeConst -> name <> ".value"
        _ -> applyJniConv name
  where
    name = camelCase (paramName p)
    -- Widen/narrow the Kotlin @Int@-backed wrapper to match the JNI
    -- signature when the underlying C type isn't a plain 32-bit int.
    -- These are the cases where 'Apigen.Language.Jvm.Java.jniTypeFor'
    -- emits a non-@int@ primitive (@long@ for @size_t@, @byte@ for
    -- @uint8_t@).
    applyJniConv val = case paramType p of
        SSizeT  -> val <> ".toLong()"
        SUInt 8 -> val <> ".toByte()"
        _       -> val

-- | Wrap a JNI primitive return into the Kotlin public type.
--
--   * @SBool@ with @errorType@ — success/fail via exception, no value.
--   * @SResourceId X@ — wrap with the @ToxX@ value-class constructor.
--   * @SEnum X@ — pick the enum entry from its ordinal.
--   * @SFixedBytes@ — wrap with the matching typedef's value class
--     when one is registered in 'arrayTypes'; otherwise pass through.
wrapReturn :: SemanticModel -> SType -> Maybe Text -> Text -> Text
wrapReturn = wrapReturnFor ""

-- | Same as 'wrapReturn' but consults 'returnWrapperClass' for an
-- override keyed by C method name (e.g. @tox_self_get_name@ →
-- @ToxNickname(call)@). Falls through to the structural rules when
-- no override applies.
wrapReturnFor :: Text {- C method name -} -> SemanticModel -> SType -> Maybe Text -> Text -> Text
wrapReturnFor cMethod model ty err call =
    case returnWrapperClass cMethod ty of
        Just "Port" -> "Port(" <> call <> ".toUShort())"
        Just cls    -> cls <> "(" <> call <> ")"
        Nothing     -> structuralWrap model ty err call

structuralWrap :: SemanticModel -> SType -> Maybe Text -> Text -> Text
structuralWrap _ SVoid _ call = call
structuralWrap _ SBool (Just _) call = call
structuralWrap _ (SResourceId name) _ call = kotlinNameOf name <> "(" <> call <> ")"
structuralWrap model (SFixedBytes sizeConst _) _ call =
    case lookup sizeConst (arrayTypes model) of
        Just typedef -> "Tox" <> pascalCase typedef <> "(" <> call <> ")"
        Nothing -> call
-- Defensive lookup: a future c-toxcore enum extension would
-- otherwise IndexOutOfBounds the accessor. @atOrFirst@ (an Array
-- extension declared at the top of the Impl file by
-- 'topLevelHelpers') falls back to the zero ordinal (always
-- present) when the C side returns a value the Kotlin enum doesn't
-- know about. Matches the same pattern the event dispatcher uses.
structuralWrap _ (SEnum name) _ call =
    let cls = kotlinNameOf name
    in cls <> ".values().atOrFirst(" <> call <> ")"
-- @int[]@ → @List<ToxX>@ where the element is a value-class newtype.
structuralWrap _ (SList (SResourceId name)) _ call =
    call <> ".map { " <> kotlinNameOf name <> "(it) }"
structuralWrap _ _ _ call = call

hasTypedef :: SemanticModel -> Text -> Bool
hasTypedef model sizeConst = case lookup sizeConst (arrayTypes model) of
    Just _ -> True
    Nothing -> False

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Mirrors @Apigen.Language.Jvm.Java.jniMethodName@.
jniMethodName :: Text -> Text
jniMethodName n =
    case Text.stripPrefix "tox_pass_" n of
        Just rest -> "toxPass" <> ph rest
        Nothing -> case Text.stripPrefix "toxav_" n of
            Just rest -> "toxav" <> ph rest
            Nothing -> case Text.stripPrefix "tox_" n of
                Just rest -> "tox" <> ph rest
                Nothing -> camelCase n
  where
    ph t = pascalCase t

-- Unused right now but pre-staged for callbacks dedup if needed later.
_unused :: [SResource] -> [SResource]
_unused = List.sortOn resourceName
