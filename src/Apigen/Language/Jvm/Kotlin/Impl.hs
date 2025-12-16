{-# LANGUAGE OverloadedStrings #-}

-- | @Tox{Core,Av,Crypto}Impl.kt@, implementing the interfaces with the
-- @*Jni.java@ natives.
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
                                                    kotlinMethodName,
                                                    kotlinNameOf,
                                                    kotlinPropertyName,
                                                    pascalCase, pathIdArgsFor,
                                                    renderMethodParamType,
                                                    renderType)
import           Apigen.Language.Jvm.Model         (avResourceNames,
                                                    classPrefix,
                                                    coreResourceNames,
                                                    cryptoResourceNames,
                                                    isOtherLibraryName,
                                                    libraryMethodName)
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
      , renderImpl model (coreSpec model)
      )
    , ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxAvImpl.kt"
      , renderImpl model (avSpec model)
      )
    , ( "lib/src/main/java/im/tox/tox4j/impl/jni/ToxCryptoImpl.kt"
      , renderImpl model (cryptoSpec model)
      )
    ]

--------------------------------------------------------------------------------
-- Subsystem specs
--------------------------------------------------------------------------------

data ImplSpec = ImplSpec
    { subsystemPkg       :: Text -- ^ Sub-package: @core@, @av@, @crypto@.
    , implClassName      :: Text -- ^ @ToxCoreImpl@, @ToxAvImpl@, @ToxCryptoImpl@.
    , interfaceName      :: Text -- ^ @ToxCore@, @ToxAv@, @ToxCrypto@.
    , jniClass           :: Text -- ^ @ToxCoreJni@, @ToxAvJni@, @ToxCryptoJni@.
    , dispatcher         :: Maybe Text -- ^ @ToxCoreEventDispatch@ if iterate() applies.
    , listenerType       :: Maybe Text -- ^ @ToxCoreEventListener@ if iterate() applies.
    , primaryConstructor :: PrimaryCtor
    , resourceNames      :: [Text] -- ^ Resources whose methods land on this Impl.
    }

data PrimaryCtor
    = -- | @class ToxCoreImpl(val options: ToxOptions) : ToxCore@
      OptionsCtor
    | -- | @class ToxAvImpl(private val tox: ToxCoreImpl) : ToxAv@
      ToxRefCtor
    | -- | @object ToxCryptoImpl : ToxCrypto<PassKey>@: callers hold the
      -- handles (several can be alive), so the impl has no instance.
      StatelessObject

coreSpec, avSpec, cryptoSpec :: SemanticModel -> ImplSpec
coreSpec model = ImplSpec
    { subsystemPkg = "core"
    , implClassName = "ToxCoreImpl"
    , interfaceName = "ToxCore"
    , jniClass = "ToxCoreJni"
    , dispatcher = Just "ToxCoreEventDispatch"
    , listenerType = Just "ToxCoreEventListener"
    , primaryConstructor = OptionsCtor
    , resourceNames = coreResourceNames model
    }

avSpec model = ImplSpec
    { subsystemPkg = "av"
    , implClassName = "ToxAvImpl"
    , interfaceName = "ToxAv"
    , jniClass = "ToxAvJni"
    , dispatcher = Just "ToxAvEventDispatch"
    , listenerType = Just "ToxAvEventListener"
    , primaryConstructor = ToxRefCtor
    , resourceNames = avResourceNames model
    }

cryptoSpec model = ImplSpec
    { subsystemPkg = "crypto"
    , implClassName = "ToxCryptoImpl"
    , interfaceName = "ToxCrypto"
    , jniClass = "ToxCryptoJni"
    , dispatcher = Nothing
    , listenerType = Nothing
    , primaryConstructor = StatelessObject
    , resourceNames = cryptoResourceNames model
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

    -- Keeps enum lookups under ktlint's 140-char limit.
    topLevelHelpers
        | any (any returnsEnum . inputs') methodList =
            [ "private fun <T> Array<T>.atOrFirst(index: Int): T = getOrNull(index) ?: this[0]"
            , ""
            ]
        | otherwise = []
    inputs' (_, m) = [output m]
    returnsEnum ty = case ty of
        SEnum _ -> True
        _       -> False

    classBody _ ms =
        constructorBlock model spec
            ++ closeBlock spec
            ++ (case dispatcher spec of
                  Just _  -> iterateBlock spec
                  Nothing -> [])
            ++ concatMap (renderMethod model spec) ms
            ++ implExtras
            ++ ["}"]

    -- No leading blank: 'renderMethod' ends with one.
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
            ++ [ paramWrapperImport cls
               | (_, m) <- ms
               , p <- inputs m
               , Just cls <- [paramWrapperFor model (methodName m) p]
               ]
            ++ [ paramWrapperImport cls
               | (_, m) <- ms
               , Just cls <- [returnWrapperClass model (methodName m) (output m)]
               ]

    importsForType ty = case ty of
        SResourceId n -> ["im.tox.tox4j.core.data." <> kotlinNameOf model n]
        SEnum n -> ["im.tox.tox4j." <> enumPkg n <> ".enums." <> kotlinNameOf model n]
        SFixedBytes sizeConst _ -> case lookup sizeConst (arrayTypes model) of
            Just typedef -> ["im.tox.tox4j.core.data." <> classPrefix model <> pascalCase typedef]
            Nothing      -> []
        _ -> []
    enumPkg n
        | isOtherLibraryName model n = "av"
        | otherwise = "core"


-- | ktlint's 3-line form for a one-parameter class header.
classHeader :: ImplSpec -> [Text]
classHeader spec = case primaryConstructor spec of
    OptionsCtor ->
        -- Drops the savedata, which can be megabytes, once C has it.
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
    -- In ktlint's layout for a multi-line @run@ initialiser.
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

-- | Proto builder calls for 'optionsWireFields'. They read @initOptions@
-- (@options@ has no savedata).
optionsBuilderCalls :: SemanticModel -> [Text]
optionsBuilderCalls model =
    [ ".set" <> pascalCase (propName p) <> "(" <> value p <> ")"
    | p <- optionsWireFields model
    ]
  where
    value p = wrap p ("initOptions." <> access p <> unwrap p)

    access p = case sealedGroupAccess (propName p) of
        Just path -> path
        Nothing   -> camelCase (propName p)

    -- Ports are @UShort@ in Kotlin but @uint32@ in the proto.
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
    -- The handle value class (PassKey) is the AutoCloseable.
    StatelessObject -> []
    OptionsCtor     -> close "toxKill"
    ToxRefCtor      -> close "toxavKill"
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
        _          -> "toxIterate"
    intervalMethod s = case primaryConstructor s of
        ToxRefCtor -> "toxavIterationInterval"
        _          -> "toxIterationInterval"

--------------------------------------------------------------------------------
-- Method dispatch
--------------------------------------------------------------------------------

renderMethod :: SemanticModel -> ImplSpec -> (SResource, SMethod) -> [Text]
renderMethod model spec (r, m)
    | isPropertyShape =
        let singleLine =
                "    override val " <> kotlinPropertyName model m <> returnSig
                    <> " get() = " <> wrappedCall
            sigLine = "    override val " <> kotlinPropertyName model m <> returnSig <> " get() ="
        in if Text.length singleLine <= 140
               then [singleLine, ""]
               else [sigLine, "        " <> wrappedCall, ""]
    -- ktlint puts two or more parameters on their own lines.
    | length sigArgs >= 2 =
        let singleLine = "    )" <> returnSig <> " = " <> wrappedCall
            bodyLine = "        " <> wrappedCall
            sigClose = "    )" <> returnSig <> " ="
        in [ "    override fun " <> kotlinMethodName model m <> "(" ]
            ++ [ "        " <> a <> "," | a <- sigArgs ]
            ++ if Text.length singleLine <= 140
                   then [singleLine, ""]
                   else if Text.length bodyLine <= 140
                       then [sigClose, bodyLine, ""]
                       else if wrappedCall == jniCall
                           then [ "    )" <> returnSig <> " = " <> jniHead <> "(" ]
                               ++ [ "        " <> arg <> "," | arg <- jniCallArgs ]
                               ++ [ "    )", "" ]
                           else sigClose : wrapBody 8 wrappedCall ++ [""]
    | otherwise =
        let singleLine =
                "    override fun " <> kotlinMethodName model m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " = " <> wrappedCall
            sigLine =
                "    override fun " <> kotlinMethodName model m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " ="
        in if Text.length singleLine <= 140
               then [singleLine, ""]
               else [sigLine, "        " <> wrappedCall, ""]
  where
    stateless = case primaryConstructor spec of
        StatelessObject -> True
        _               -> False

    -- On a stateless impl, instance methods take the handle first.
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
    -- A predicate returns its bool, though it still throws on error.
    effectiveErrorType meth = case (output meth, methodErrorType meth) of
        (SBool, Just _) | isPredicate (methodName meth) -> Nothing
        (_, e)                                          -> e
    pathArgsList = pathIdArgsFor model r (methodRole m /= Constructor)
    sigArgs =
        handleSigArgs
            ++ [pname <> ": " <> tname | (pname, tname) <- pathArgsList]
            ++ map renderInputSig (inputs m)
    renderInputSig p =
        camelCase (paramName p) <> ": " <> renderMethodParamType model (methodName m) p

    returnSig = case (output m, methodErrorType m) of
        (SVoid, _)                                        -> ""
        (SBool, Just _) | not (isPredicate (methodName m)) -> ""
        (ty, _) -> case returnWrapperClass model (methodName m) ty of
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
    jniHead = jniClass spec <> "." <> jniMethodName model (methodName m)
    jniCall = jniHead <> "(" <> Text.intercalate ", " jniCallArgs <> ")"

    wrapBody :: Int -> Text -> [Text]
    wrapBody indent body
        | body == jniCall =
            [ indentS <> jniHead <> "(" ]
                ++ [ indentS <> "    " <> a <> "," | a <- jniCallArgs ]
                ++ [ indentS <> ")" ]
        | Just inner <- peelOuterWrap body
        , wrapperOpen <- Text.takeWhile (/= '(') body <> "("
        , Text.length (indentS <> inner <> ",") <= 140 =
            [ indentS <> wrapperOpen
            , indentS <> "    " <> inner <> ","
            , indentS <> ")"
            ]
        | Just _ <- peelOuterWrap body
        , wrapperOpen <- Text.takeWhile (/= '(') body <> "(" =
            [indentS <> wrapperOpen]
                ++ wrapBody (indent + 4) (fromMaybe body (peelOuterWrap body))
                ++ [indentS <> ")"]
        | otherwise = [indentS <> body]
      where
        indentS = Text.replicate indent " "

    peelOuterWrap :: Text -> Maybe Text
    peelOuterWrap expr = do
        rest <- Text.stripSuffix ")" expr
        let (prefix, afterOpen) = Text.breakOn "(" rest
        inner <- Text.stripPrefix "(" afterOpen
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
                        _   -> d)
                    (0 :: Int)

unwrapInput :: SemanticModel -> Text -> SParameter -> Text
unwrapInput model cMethod p = case paramWrapperFor model cMethod p of
    Just "Port" -> name <> ".value.toInt()"
    Just _      -> applyJniConv (name <> ".value")
    Nothing -> case paramType p of
        SEnum _ -> applyJniConv (name <> ".ordinal")
        SResourceId _ -> name <> ".value"
        SFixedBytes sizeConst _ | hasTypedef model sizeConst -> name <> ".value"
        _ -> applyJniConv name
  where
    name = camelCase (paramName p)
    -- Where 'Apigen.Language.Jvm.Java.jniTypeFor' isn't @int@.
    applyJniConv val = case paramType p of
        SSizeT  -> val <> ".toLong()"
        SUInt 8 -> val <> ".toByte()"
        _       -> val

wrapReturnFor :: Text {- C method name -} -> SemanticModel -> SType -> Maybe Text -> Text -> Text
wrapReturnFor cMethod model ty err call =
    case returnWrapperClass model cMethod ty of
        Just "Port" -> "Port(" <> call <> ".toUShort())"
        Just cls    -> cls <> "(" <> call <> ")"
        Nothing     -> structuralWrap model ty err call

structuralWrap :: SemanticModel -> SType -> Maybe Text -> Text -> Text
structuralWrap _ SVoid _ call = call
structuralWrap _ SBool (Just _) call = call
structuralWrap model (SResourceId name) _ call = kotlinNameOf model name <> "(" <> call <> ")"
structuralWrap model (SFixedBytes sizeConst _) _ call =
    case lookup sizeConst (arrayTypes model) of
        Just typedef -> classPrefix model <> pascalCase typedef <> "(" <> call <> ")"
        Nothing      -> call
-- Use the first entry rather than throw (a newer c-toxcore can return values we
-- don't know).
structuralWrap model (SEnum name) _ call =
    let cls = kotlinNameOf model name
    in cls <> ".values().atOrFirst(" <> call <> ")"
structuralWrap model (SList (SResourceId name)) _ call =
    call <> ".map { " <> kotlinNameOf model name <> "(it) }"
structuralWrap _ _ _ call = call

hasTypedef :: SemanticModel -> Text -> Bool
hasTypedef model sizeConst = case lookup sizeConst (arrayTypes model) of
    Just _  -> True
    Nothing -> False

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

jniMethodName :: SemanticModel -> Text -> Text
jniMethodName model = libraryMethodName model pascalCase camelCase

_unused :: [SResource] -> [SResource]
_unused = List.sortOn resourceName
