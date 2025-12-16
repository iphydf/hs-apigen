{-# LANGUAGE OverloadedStrings #-}
-- | Commands: a struct per call on a Tox or ToxAV, how the engine runs it,
-- and the proxy method sending it.
module Apigen.Language.Cpp.Commands
    ( Command (..)
    , Field (..)
    , commands
    , cErrorEnum
    , commandsHeader
    , executorSource
    ) where

import           Apigen.Inference          (resolvePathTarget)
import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Namespace          (namespaces, resourceNamespace,
                                            rootNamespace, secondaryHandles,
                                            shortNamespace)
import           Apigen.Semantic
import           Data.List                 (find, sortOn)
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

data Field = Field
    { fieldName :: Text
    , fieldType :: CppType
    } deriving (Show, Eq)

data Command = Command
    { cmdName   :: Text      -- ^ @GetFriendName@
    , cmdCName  :: Text
    , cmdWords  :: [Text]    -- ^ @get friend name@
    , cmdFields :: [Field]
    , cmdValue  :: CppType   -- ^ What it results in, 'TVoid' for nothing
    , cmdError  :: Text      -- ^ Its error type: an enum, @NoError@ or @GenericError@
    , cmdBody   :: [Text]    -- ^ How the engine runs it, ending in a @push@
    } deriving (Show, Eq)

-- | Resources whose functions are commands: the root's, and the other
-- libraries' handles.
commandResources :: SemanticModel -> [Text]
commandResources model = map resourceName (secondaryHandles model) ++ map resourceName (rootMembers model)

commands :: SemanticModel -> [Command]
commands model =
    [ c | (r, m) <- allMethods model
        , resourceName r `elem` commandResources model
        , methodRole m `notElem` [StaticRole, Destructor, RegistrarRole, IterateRole]
        , let mapping = mappingOf model r m
        , not (K.isLoopFunction model (cFunctionName mapping))
        , not (isSizeCompanion model (cFunctionName mapping))
        , Just c <- [command model r m mapping] ]

cErrorEnum :: SemanticModel -> Text -> Maybe (Text, Text)
cErrorEnum model e = do
    enum <- findEnum model e
    ok <- lookup "OK" [ (enumMemberName mem, enumMemberCName mem) | mem <- enumMembers enum ]
    return (enumName enum, ok)

data Arg = Arg
    { argFields :: [Field]
    , argExprs  :: [Text]   -- ^ The C arguments, one per 'CArgSource' it covers
    }

command :: SemanticModel -> SResource -> SMethod -> CFunctionMapping -> Maybe Command
command model res m mapping = do
    let fn = cFunctionName mapping
        names = cArgNames mapping
        handle = handleVar model res
        params = cSemParams mapping
    args <- mapM (argOf model res fn handle params) (group (K.frames model) (zip names (argMapping mapping)))
    out <- outputOf model m mapping
    let fields = concatMap argFields args
        cArgs = concatMap argExprs args
        ws = commandWords (namespaces model) fn
        name = pascal ws
    body <- bodyOf model m mapping name cArgs out
    return Command
        { cmdName   = name
        , cmdCName  = fn
        , cmdWords  = ws
        , cmdFields = fields
        , cmdValue  = outType out
        , cmdError  = errorName model m
        , cmdBody   = body
        }

-- | The member holding a resource's library handle: @tox_@, or @av_@ for
-- toxav's.
handleVar :: SemanticModel -> SResource -> Text
handleVar model res = shortNamespace nss ns <> "_"
  where
    nss = namespaces model
    ns = fromMaybe (rootNamespace model) (resourceNamespace nss res)

errorName :: SemanticModel -> SMethod -> Text
errorName model m
    | isJust (K.failureReturn model (methodName m)) = "GenericError"
    | otherwise = maybe "NoError" (enumCppName model) (methodErrorType m)

-- | C arguments in the groups one C++ field holds: a frame's, bytes with
-- their length, or one.
group :: [(Text, [Text])] -> [(Text, CArgSource)] -> [[(Text, CArgSource)]]
group frames' args = case args of
    []                                                -> []
    _ | Just (taken, rest) <- compound                -> taken : group frames' rest
    (a@(_, SemanticArg _) : b@(_, BufferSize) : rest) -> [a, b] : group frames' rest
    (a : rest)                                        -> [a] : group frames' rest
  where
    compound = case [ splitAt (length ps) args | (_, ps) <- frames', map fst (take (length ps) args) == ps ] of
        x : _ -> Just x
        []    -> Nothing

-- | A frame's C arguments: its arrays by accessors named for them, its
-- numbers from its metadata: @payload.frame.pcm().data()@,
-- @payload.frame.metadata().channels.value@.
frameArgs :: [SParameter] -> [Text] -> [Text]
frameArgs params run = [ arg n (paramType <$> find ((== n) . paramName) params) | n <- run ]
  where
    arg n ty = case ty of
        Just SFixedList{}  -> "payload.frame." <> n <> "().data()"
        Just SFixedBytes{} -> "payload.frame." <> n <> "().data()"
        Just SScalar{}     -> "payload.frame.metadata()." <> n <> ".value"
        _                  -> "payload.frame.metadata()." <> n

argOf :: SemanticModel -> SResource -> Text -> Text -> [SParameter] -> [(Text, CArgSource)] -> Maybe Arg
argOf model res fn handle params grouped = case grouped of
    _ | Just (name, run) <- find ((== map fst grouped) . snd) (K.frames model) ->
        Just (Arg [Field "frame" (TFrame (frameTypeName name))] (frameArgs params run))
    [(_, ThisObject _)] -> Just (Arg [] [handle])
    [(_, PathObject _ _)] -> Just (Arg [] [handle])
    [(n, PathId i)] -> do
        idTy <- idModel model (resolvePathTarget (resources model) res i)
        Just (Arg [Field n (TId (idTypeName (idName idTy)))] ["payload." <> n <> ".value"])
    [(n, SemanticArg i)] -> case drop i params of
        p : _ -> semanticArg model fn n p
        []    -> Nothing
    [(n, SemanticArg _), (_, BufferSize)] ->
        Just (Arg [Field n TBytes] ["payload." <> n <> ".data()", "payload." <> n <> ".size()"])
    [(_, ErrorPtr)] -> Just (Arg [] ["&err"])
    [(n, BufferPtr _)] -> Just (Arg [] [n <> "Ptr"])  -- 'bodyOf' fills it in
    [(_, Constant v)] -> Just (Arg [] [Text.pack (show v)])
    _ -> Nothing

semanticArg :: SemanticModel -> Text -> Text -> SParameter -> Maybe Arg
semanticArg model fn n p
    | Just e <- K.enumParam model (fn, n) =
        Just (Arg [Field n (TEnum (enumCppName model e))] ["static_cast<uint32_t>(payload." <> n <> ")"])
    | otherwise = case paramType p of
        SBool -> plain TBool
        SInt k -> plain (TInt k)
        SUInt k -> plain (TUInt k)
        SSizeT -> plain TSize
        SString -> Just (Arg [Field n TString] [v <> ".c_str()"])
        SEnum e -> do
            enum <- findEnum model e
            Just (Arg [Field n (TEnum (enumCppName model e))] ["static_cast<" <> enumName enum <> ">(" <> v <> ")"])
        SResourceId _ -> do
            ty <- typeOf model (paramType p)
            Just (Arg [Field n ty] [v <> ".value"])
        SScalar _ _ -> do
            ty <- typeOf model (paramType p)
            Just (Arg [Field n ty] [v <> ".value"])
        SFixedBytes size _ | Just a <- arrayTypeOf model size ->
            if paramNullable p
                then Just (Arg [Field n (TOptional (TArray (arrayTypeName a)))]
                               [v <> " ? " <> v <> "->data.data() : nullptr"])
                else Just (Arg [Field n (TArray (arrayTypeName a))] [v <> ".data.data()"])
        _ -> Nothing
  where
    v = "payload." <> n
    plain t = Just (Arg [Field n t] [v])

data Output
    = OutValue CppType           -- ^ Its return value
    | OutNone                    -- ^ Nothing, or a bool saying it worked
    | OutArray CppType Text Bool -- ^ A fixed-size buffer, by name; whether C's bool says it's there
    | OutSized CppType Text Text -- ^ A buffer of the size another function gives, by name
    | OutFailure                 -- ^ An integer that's 'K.failureReturn' on failure

outType :: Output -> CppType
outType o = case o of
    OutValue t        -> t
    OutNone           -> TVoid
    OutArray t _ True -> TOptional t
    OutArray t _ _    -> t
    OutSized t _ _    -> t
    OutFailure        -> TVoid

outputOf :: SemanticModel -> SMethod -> CFunctionMapping -> Maybe Output
outputOf model m mapping
    | isJust (K.failureReturn model (cFunctionName mapping)) = Just OutFailure
    | [n] <- [ n | (n, BufferPtr _) <- zip (cArgNames mapping) (argMapping mapping) ] =
        case (output m, cSizeFunctionName mapping) of
            (SFixedBytes size _, _) | Just a <- arrayTypeOf model size ->
                Just (OutArray (TArray (arrayTypeName a)) n
                               (cReturnType mapping == SBool && methodErrorType m == Nothing))
            (t, Just sizeFn) -> (\ct -> OutSized ct n sizeFn) <$> typeOf model t
            _ -> Nothing
    | methodResultStrategy m == IgnoreReturn = Just OutNone
    | output m == SVoid = Just OutNone
    | otherwise = OutValue <$> typeOf model (output m)

bodyOf :: SemanticModel -> SMethod -> CFunctionMapping -> Text -> [Text] -> Output -> Maybe [Text]
bodyOf model m mapping name cArgs out = do
    errDecl <- case methodErrorType m of
        Nothing -> Just Nothing
        Just e  -> Just <$> cErrorEnum model e
    let fn = cFunctionName mapping
        call extra = fn <> "(" <> Text.intercalate ", " (map (fill extra) cArgs) <> ")"
        fill extra a = fromMaybe a (lookup a extra)
        declErr = [ cTy <> " err = " <> ok <> ";" | Just (cTy, ok) <- [errDecl] ]
        result value = case errDecl of
            Just (_, ok) -> "result<command::" <> name <> ">(err, " <> ok <> value <> ")"
            Nothing      -> "command::" <> name <> "::Result::ok(" <> Text.drop 2 value <> ")"
        push r = "push<command::" <> name <> ">(id, " <> r <> ");"
    case out of
        OutNone -> Just $ declErr ++ [call [] <> ";", push (result "")]
        OutFailure -> Just
            [ "const int32_t value = " <> call [] <> ";"
            , push ("value == " <> failure <> " ? command::" <> name <> "::Result::err(GenericError::Failure) : command::" <> name <> "::Result::ok()")
            ]
          where failure = maybe "-1" (Text.pack . show) (K.failureReturn model fn)
        OutValue t -> Just $ declErr ++
            [ "const auto value = " <> call [] <> ";"
            , push (result (", " <> fromC t "value"))
            ]
        OutArray t n present -> Just $ declErr ++
            if present
                then [ render (unOpt t) <> " " <> local <> "{};"
                     , "const bool present = " <> call [(n <> "Ptr", local <> ".data.data()")] <> ";"
                     , push (result (", present ? std::optional{" <> local <> "} : std::nullopt"))
                     ]
                else [ render t <> " " <> local <> "{};"
                     , call [(n <> "Ptr", local <> ".data.data()")] <> ";"
                     , push (result (", " <> local))
                     ]
          where
            -- By its type: a C name like @id@ is the command's.
            local = unwords' (words' (Text.concatMap snake (render (unOpt t))))
            snake ch = if ch >= 'A' && ch <= 'Z' then Text.pack ['_', ch] else Text.singleton ch
        OutSized t n sizeFn -> do
            (sizeRes, sizeMethod) <- find' sizeFn
            let sizeMapping = mappingOf model sizeRes sizeMethod
                sizeArgs = [ a | (a, src) <- zip cArgs (argMapping mapping), keep src ]
                keep src = case src of
                    BufferPtr _ -> False
                    ErrorPtr    -> ErrorPtr `elem` argMapping sizeMapping
                    _           -> True
                sizeCall = sizeFn <> "(" <> Text.intercalate ", " sizeArgs <> ")"
                (raw, value) = case t of
                    TVector (TId idTy) -> ("std::vector<uint32_t>", "ids<" <> idTy <> ">(" <> n <> ")")
                    _                  -> (render t, "std::move(" <> n <> ")")
                fetch = call [(n <> "Ptr", n <> ".data()")] <> ";"
            Just $ declErr
                ++ [raw <> " " <> n <> "(" <> sizeCall <> ");"]
                ++ (case errDecl of
                        Just (_, ok) | ErrorPtr `elem` argMapping sizeMapping ->
                            ["if (err == " <> ok <> ") {", "    " <> fetch, "}"]
                        _ -> [fetch])
                ++ [push (result (", " <> value))]
  where
    unOpt (TOptional t) = t
    unOpt t             = t
    find' fn = case [ (r, sm) | (r, sm) <- allMethods model, cFunctionName (mappingOf model r sm) == fn ] of
        x : _ -> Just x
        []    -> Nothing

-- | @tox/generated/commands.hpp@: the command structs.
commandsHeader :: Lib -> (Text -> Maybe Text) -> [Command] -> Text
commandsHeader l doc cmds = header l
    [ own l "errors.hpp", own l "media.hpp", own l "result.hpp", own l "types.hpp"
    , "<cstdint>", "<optional>", "<string>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> "::command {", ""]
    ++ concat [ struct c ++ [""] | c <- sorted ]
    ++ ["} // namespace " <> libNamespace l <> "::command", ""]
    ++ ["// clang-format off", "#define FOR_EACH_COMMAND_TYPE(macro) \\"]
    ++ macroList (map cmdName sorted)
    ++ ["// clang-format on"]
  where
    sorted = sortOn cmdName cmds
    struct c = docBlock (("@brief " <>) . summary <$> doc (cmdCName c))
        ++ declaration ("struct " <> cmdName c)
            ( ("using Result = " <> libNamespace l <> "::Result<" <> renderIn (libNamespace l) (map cmdName cmds) (cmdValue c) <> ", " <> cmdError c <> ">;")
            : [ renderIn (libNamespace l) (map cmdName cmds) (fieldType f) <> " " <> fieldName f <> ";" | f <- cmdFields c ] )

-- | @src/generated/commands.cpp@: how the engine thread runs commands.
executorSource :: Lib -> [Command] -> Text
executorSource l cmds = source l
    [ "session_impl.hpp", own l "commands.hpp", own l "events.hpp"
    , "<cstdint>", "<memory>", "<optional>", rootInclude l, avInclude l, "<utility>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> " {", "", "namespace {", ""]
    ++ helpers
    ++ declaration "class CommandExecutor final : public CommandVisitor"
        ( ["public:"]
        ++  ( [ "CommandExecutor(" <> rootType l <> "* _Nonnull " <> rootVar l <> ", " <> avType l <> "* _Nonnull " <> avVar l <> ", Session::Impl& impl)"
              , "    : " <> rootVar l <> "_(" <> rootVar l <> ")"
              , "    , " <> avVar l <> "_(" <> avVar l <> ")"
              , "    , impl_(impl)"
              , "{"
              , "}"
              , ""
              ]
            ++ concat [ on c ++ [""] | c <- sortOn cmdName cmds ] )
        ++ ["private:"]
        ++  [ "template <typename Command>"
            , "void push(CommandId id, typename Command::Result res)"
            , "{"
            , "    impl_.push_event(event::Result<Command>{.id = id, .value = std::move(res)});"
            , "}"
            , ""
            , rootType l <> "* _Nonnull " <> rootVar l <> "_;"
            , avType l <> "* _Nonnull " <> avVar l <> "_;"
            , "Session::Impl& impl_;"
            ] )
    ++ [ "", "} // namespace", "" ]
    ++ braced ("void execute_command(" <> rootType l <> "* _Nonnull " <> rootVar l <> ", " <> avType l <> "* _Nonnull " <> avVar l <> ", Session::Impl& impl, AnyCommand& command)")
        [ "CommandExecutor executor(" <> rootVar l <> ", " <> avVar l <> ", impl);"
        , "command.accept(executor);"
        ]
    ++ ["", "} // namespace " <> libNamespace l]
  where
    on c = braced ("void on(CommandId id, command::" <> cmdName c <> "& " <> payload c <> ") override") (cmdBody c)
    payload c = if any ("payload." `Text.isInfixOf`) (cmdBody c) then "payload" else "/*payload*/"
    helpers =
        [ "/**"
        , " * @brief A command's result, from the C error that is `ok` on success."
        , " */"
        , "template <typename Command, typename CError, typename... Value>"
        , "typename Command::Result result(CError err, CError ok, Value&&... value)"
        , "{"
        , "    using Result = typename Command::Result;"
        , "    if (err != ok) {"
        , "        return Result::err(static_cast<typename Result::error_type>(err));"
        , "    }"
        , "    return Result::ok(std::forward<Value>(value)...);"
        , "}"
        , ""
        , "/**"
        , " * @brief The IDs of the integers C gives for them."
        , " */"
        , "template <typename Id>"
        , "std::vector<Id> ids(const std::vector<typename Id::underlying_type>& values)"
        , "{"
        , "    std::vector<Id> result;"
        , "    result.reserve(values.size());"
        , "    for (const auto value : values) {"
        , "        result.push_back(Id{value});"
        , "    }"
        , "    return result;"
        , "}"
        , ""
        ]
