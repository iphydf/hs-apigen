{-# LANGUAGE OverloadedStrings #-}

-- | @Tox{Core,Av,Crypto}Impl.kt@, implementing the interfaces with the
-- @*Jni.java@ natives.
module Apigen.Language.Jvm.Kotlin.Impl (generate) where

import qualified Apigen.Knowledge                  as K
import           Apigen.Language.Jvm.Conventions   (SealedGroup (..),
                                                    implExtraMembers,
                                                    interfaceGenericParams,
                                                    optionsWireFields,
                                                    sealedGroupAccess,
                                                    sealedGroups)
import           Apigen.Language.Jvm.Kotlin.Common (camelCase,
                                                    isSkippableMethodFor,
                                                    kotlinMethodName,
                                                    kotlinNameOf,
                                                    kotlinPropertyName,
                                                    pascalCase, pathIdArgsFor,
                                                    renderType, typeImports)
import           Apigen.Language.Jvm.Model         (Subsystem (..), classPrefix,
                                                    intervalNative,
                                                    jniMethodName, jvmName,
                                                    jvmSource, native,
                                                    optionsClass, rootSubsystem,
                                                    rootVar, stateType,
                                                    subsystems)
import           Apigen.Model                      (arrayTypeOf, ignoresReturn)
import           Apigen.Semantic                   (SMethod (..),
                                                    SMethodRole (..),
                                                    SParameter (..),
                                                    SProperty (..),
                                                    SResource (..),
                                                    SResourceKind (..),
                                                    SResourceType (..),
                                                    SType (..),
                                                    SemanticModel (..))
import           Apigen.SizeExpr                   (SizeExpr)
import qualified Data.List                         as List
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: SemanticModel -> [(FilePath, Text)]
generate model =
    [ (jvmSource model "java" "impl.jni" (implClassName spec <> ".kt"), renderImpl model spec)
    | s <- subsystems model, let spec = specOf model s ]

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
    , nativeNew          :: Text   -- ^ @toxNew@
    , nativeKill         :: Text
    , nativeIterate      :: Text
    , nativeInterval     :: Text
    }

data PrimaryCtor
    = -- | @class ToxCoreImpl(val options: ToxOptions) : ToxCore@
      OptionsCtor
    | -- | @class ToxAvImpl(private val tox: ToxCoreImpl) : ToxAv@
      ToxRefCtor
    | -- | @object ToxCryptoImpl : ToxCrypto<PassKey>@: callers hold the
      -- handles (several can be alive), so the impl has no instance.
      StatelessObject

specOf :: SemanticModel -> Subsystem -> ImplSpec
specOf model s = ImplSpec
    { subsystemPkg = subWord s
    , implClassName = subClass s <> "Impl"
    , interfaceName = subClass s
    , jniClass = subClass s <> "Jni"
    , dispatcher = (subClass s <> "EventDispatch") <$ hasEvents
    , listenerType = (subClass s <> "EventListener") <$ hasEvents
    , primaryConstructor = case subKind s of
        RootHandle    -> OptionsCtor
        LibraryHandle -> ToxRefCtor
        _             -> StatelessObject
    , resourceNames = subResources s
    , nativeNew = native model s Constructor
    , nativeKill = native model s Destructor
    , nativeIterate = native model s IterateRole
    , nativeInterval = intervalNative model s
    }
  where
    hasEvents = if subKind s `elem` [RootHandle, LibraryHandle] then Just () else Nothing

--------------------------------------------------------------------------------
-- File body
--------------------------------------------------------------------------------

renderImpl :: SemanticModel -> ImplSpec -> Text
renderImpl model spec =
    Text.unlines $
        [ "package " <> jvmName model "impl.jni"
        , ""
        ]
            ++ importLines model spec methodList
            ++ [""]
            ++ topLevelHelpers
            ++ classHeader model spec
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
                  Just _  -> iterateBlock model spec
                  Nothing -> [])
            ++ concatMap (renderMethod model spec) ms
            ++ implExtras
            ++ ["}"]

    -- No leading blank: 'renderMethod' ends with one.
    implExtras = List.intercalate [""] $ map (:[]) $ implExtraMembers model (implClassName spec)

importLines :: SemanticModel -> ImplSpec -> [(SResource, SMethod)] -> [Text]
importLines model spec ms =
    map (\i -> "import " <> i) $ List.sort $ List.nub $ baseImports ++ usedImports
  where
    baseImports = ctorImports ++ ifaceImport ++ listenerImport
    ctorImports = case primaryConstructor spec of
        OptionsCtor ->
            [ "com.google.protobuf.ByteString"
            , jvmName model (subsystemPkg spec <> ".options." <> optionsClass model)
            , jvmName model (subsystemPkg spec <> ".proto.Options")
            ]
            ++ [ jvmName model (subsystemPkg spec <> ".options." <> sealedGroupClass g) | g <- bytesGroups model ]
        _ -> []
    ifaceImport =
        [jvmName model (subsystemPkg spec <> "." <> interfaceName spec)]
    listenerImport = case listenerType spec of
        Just listener -> [jvmName model (subsystemPkg spec <> ".callbacks." <> listener)]
        Nothing -> []
    usedImports =
        [ imp
        | (_, m) <- ms
        , p <- inputs m
        , imp <- typeImports model (paramType p)
        ]
            ++ [ imp
               | (_, m) <- ms
               , imp <- typeImports model (output m)
               ]
            ++ [ jvmName model ("core.data." <> tname)
               | (r, _) <- ms
               , (_, tname) <- pathIdArgsFor model r True
               ]

-- | ktlint's 3-line form for a one-parameter class header.
classHeader :: SemanticModel -> ImplSpec -> [Text]
classHeader model spec = case primaryConstructor spec of
    OptionsCtor ->
        -- Drops the bytes, which can be megabytes, once C has them.
        [ "class " <> implClassName spec <> "("
        , "    initOptions: " <> optionsClass model <> ","
        , ") : " <> interfaceName spec <> " {"
        , "    val options: " <> optionsClass model <> " = initOptions" <> dropped
        , ""
        ]
    ToxRefCtor ->
        [ "class " <> implClassName spec <> "("
        , "    private val " <> rootVar model <> ": " <> subClass (rootSubsystem model) <> "Impl,"
        , ") : " <> interfaceName spec <> " {"
        ]
    StatelessObject ->
        let generics = case interfaceGenericParams model (interfaceName spec) of
                [] -> ""
                ps -> "<" <> Text.intercalate ", " ps <> ">"
        in [ "object " <> implClassName spec <> " : " <> interfaceName spec <> generics <> " {" ]
  where
    dropped = case [ sealedGroupFieldName g <> " = " <> sealedGroupDefault g | g <- bytesGroups model ] of
        []     -> ""
        resets -> ".copy(" <> Text.intercalate ", " resets <> ")"

-- | The options' groups holding bytes, which an instance doesn't keep.
bytesGroups :: SemanticModel -> [SealedGroup]
bytesGroups model = [ g | g <- sealedGroups model, any holdsBytes (sealedGroupCFields g) ]
  where
    holdsBytes f = any (\p -> propName p == f && propType p == SBytes) (concatMap properties (K.handlesOfKind model SettingsHandle))

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
               , "            " <> jniClass spec <> "." <> nativeNew spec <> "(opts.toByteArray())"
               , "        }"
               , ""
               ]
    ToxRefCtor ->
        [ "    internal val instanceNumber = " <> jniClass spec <> "." <> nativeNew spec <> "(" <> rootVar model <> ".instanceNumber)"
        , ""
        ]
    StatelessObject -> []

-- | Proto builder calls for 'optionsWireFields'. They read @initOptions@
-- (@options@ has no bytes).
optionsBuilderCalls :: SemanticModel -> [Text]
optionsBuilderCalls model =
    [ ".set" <> pascalCase (propName p) <> "(" <> value p <> ")"
    | p <- optionsWireFields model
    ]
  where
    value p = wrap p ("initOptions." <> access p <> unwrap p)

    access p = case sealedGroupAccess model (propName p) of
        Just path -> path
        Nothing   -> camelCase (propName p)

    -- The proto holds an enum's ordinal and a unit's number.
    unwrap p = case propType p of
        SEnum _     -> ".ordinal"
        SScalar _ _ -> ".value"
        _           -> ""

    -- Proto bytes fields take a ByteString, not a ByteArray.
    wrap p val = case propType p of
        SBytes -> "ByteString.copyFrom(" <> val <> ")"
        _      -> val

closeBlock :: ImplSpec -> [Text]
closeBlock spec = case primaryConstructor spec of
    -- The handle value class (PassKey) is the AutoCloseable.
    StatelessObject -> []
    OptionsCtor     -> close (nativeKill spec)
    ToxRefCtor      -> close (nativeKill spec)
  where
    close destructor =
        [ "    override fun close(): Unit = " <> jniClass spec <> "." <> destructor <> "(instanceNumber)"
        , ""
        ]

iterateBlock :: SemanticModel -> ImplSpec -> [Text]
iterateBlock model spec = case (dispatcher spec, listenerType spec) of
    (Just disp, Just listener) ->
        let body =
                "    ): " <> state <> " = " <> disp <> ".dispatch(handler, "
                    <> jniClass spec <> "." <> iterateMethod spec
                    <> "(instanceNumber), state)"
        in [ "    override val iterationInterval: Int"
           , "        get() = " <> jniClass spec <> "." <> intervalMethod spec <> "(instanceNumber)"
           , ""
           , "    override fun <" <> state <> "> iterate("
           , "        handler: " <> listener <> "<" <> state <> ">,"
           , "        state: " <> state <> ","
           ]
           ++ oneLineOr body
                  [ "    ): " <> state <> " ="
                  , "        " <> disp <> ".dispatch(handler, "
                        <> jniClass spec <> "." <> iterateMethod spec
                        <> "(instanceNumber), state)"
                  ]
           ++ [""]
    _ -> []
  where
    state = stateType model
    iterateMethod = nativeIterate
    intervalMethod = nativeInterval

--------------------------------------------------------------------------------
-- Method dispatch
--------------------------------------------------------------------------------

renderMethod :: SemanticModel -> ImplSpec -> (SResource, SMethod) -> [Text]
renderMethod model spec (r, m)
    | isPropertyShape =
        let singleLine =
                "    override val " <> kotlinPropertyName model m <> returnSig
                    <> " get() = " <> flat wrappedCall
            sigLine = "    override val " <> kotlinPropertyName model m <> returnSig <> " get() ="
        in oneLineOr singleLine [sigLine, "        " <> flat wrappedCall] ++ [""]
    -- ktlint puts two or more parameters on their own lines.
    | length sigArgs >= 2 =
        [ "    override fun " <> kotlinMethodName model m <> "(" ]
            ++ [ "        " <> a <> "," | a <- sigArgs ]
            ++ oneLineOr ("    )" <> returnSig <> " = " <> flat wrappedCall) longBody
            ++ [""]
    | otherwise =
        let singleLine =
                "    override fun " <> kotlinMethodName model m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " = " <> flat wrappedCall
            sigLine =
                "    override fun " <> kotlinMethodName model m <> "("
                    <> Text.intercalate ", " sigArgs <> ")"
                    <> returnSig <> " ="
        in oneLineOr singleLine [sigLine, "        " <> flat wrappedCall] ++ [""]
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
            Call (pascalCase h) [wrappedCall0]
        | otherwise = wrappedCall0
    wrappedCall0 = wrapReturn model (output m) jniCall
    isPropertyShape =
        null handleSigArgs
            && null pathArgsList
            && null (inputs m)
            && not (Text.null returnSig)
            && methodRole m /= Constructor
    pathArgsList = pathIdArgsFor model r (methodRole m /= Constructor)
    sigArgs =
        handleSigArgs
            ++ [pname <> ": " <> tname | (pname, tname) <- pathArgsList]
            ++ map renderInputSig (inputs m)
    renderInputSig p =
        camelCase (paramName p) <> ": " <> renderType model (paramType p)

    returnSig = case (output m, methodErrorType m) of
        (SVoid, _)                        -> ""
        (SBool, Just _) | ignoresReturn m -> ""
        (ty, _)                           -> ": " <> renderType model ty

    jniCallArgs =
        instanceJniArgs
            ++ [pname <> ".value" | (pname, _) <- pathArgsList]
            ++ map (unwrapInput model) (inputs m)
    jniHead = jniClass spec <> "." <> jniMethodName model (methodName m)
    jniCall = Call jniHead (map Atom jniCallArgs)

    -- The body on its own line, else laid out. A plain native call opens on
    -- the signature's last line.
    longBody
        | fits ("        " <> flat wrappedCall) = [sigClose, "        " <> flat wrappedCall]
        | wrappedCall == jniCall =
            ["    )" <> returnSig <> " = " <> jniHead <> "("]
                ++ [ "        " <> arg <> "," | arg <- jniCallArgs ]
                ++ ["    )"]
        | otherwise = sigClose : layout 8 wrappedCall
    sigClose = "    )" <> returnSig <> " ="

-- | ktlint's line length.
fits :: Text -> Bool
fits = (<= 140) . Text.length

oneLineOr :: Text -> [Text] -> [Text]
oneLineOr line alt = if fits line then [line] else alt

-- | A Kotlin expression, kept apart enough to lay it out.
data Expr
    = Atom Text         -- ^ As it is.
    | Call Text [Expr]  -- ^ @f(a, b)@
    | Method Expr Text  -- ^ @e.toUShort()@
    deriving (Eq)

flat :: Expr -> Text
flat (Atom t)        = t
flat (Call f args)   = f <> "(" <> Text.intercalate ", " (map flat args) <> ")"
flat (Method e rest) = flat e <> "." <> rest

-- | A call too long for its line, as ktlint lays it out: its arguments one
-- to a line, but one call argument laid out the same way.
layout :: Int -> Expr -> [Text]
layout indent e = case e of
    Call f [arg]
        | fits (pad <> flat arg <> ",") ->
            [pad <> f <> "(", pad <> "    " <> flat arg <> ",", pad <> ")"]
        | Call{} <- arg -> [pad <> f <> "("] ++ layout (indent + 4) arg ++ [pad <> ")"]
    Call f args -> [pad <> f <> "("] ++ [ pad <> "    " <> flat a <> "," | a <- args ] ++ [pad <> ")"]
    _ -> [pad <> flat e]
  where
    pad = Text.replicate indent " "

-- | A parameter as JNI takes it, whose Kotlin types are JNI's.
unwrapInput :: SemanticModel -> SParameter -> Text
unwrapInput model p = case paramType p of
    SScalar _ _                                          -> name <> ".value"
    SEnum _                                              -> name <> ".ordinal"
    SResourceId _                                        -> name <> ".value"
    SFixedBytes sizeConst _ | hasTypedef model sizeConst -> name <> ".value"
    _                                                    -> name
  where
    name = camelCase (paramName p)

-- | What JNI returns as its Kotlin type.
wrapReturn :: SemanticModel -> SType -> Expr -> Expr
wrapReturn model (SScalar name _) call = Call (kotlinNameOf model name) [call]
wrapReturn model (SResourceId name) call = Call (kotlinNameOf model name) [call]
wrapReturn model (SFixedBytes sizeConst _) call =
    case arrayTypeOf model sizeConst of
        Just typedef -> Call (classPrefix model <> pascalCase typedef) [call]
        Nothing      -> call
-- Use the first entry rather than throw (a newer c-toxcore can return values we
-- don't know).
wrapReturn model (SEnum name) call = Call (kotlinNameOf model name <> ".values().atOrFirst") [call]
wrapReturn model (SList (SResourceId name)) call =
    Method call ("map { " <> kotlinNameOf model name <> "(it) }")
wrapReturn _ _ call = call

hasTypedef :: SemanticModel -> SizeExpr -> Bool
hasTypedef model = isJust . arrayTypeOf model
