{-# LANGUAGE OverloadedStrings #-}
-- | The root handle as a class: a method per function of its library, which
-- calls C on the calling thread and returns what C gives.
module Apigen.Language.Cpp.Methods
    ( Method (..)
    , rootMethods
    , cErrorEnum
    , rootHeader
    , rootSource
    ) where

import           Apigen.Inference          (resolvePathTarget)
import qualified Apigen.Knowledge          as K
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Namespace          (namespaces, rootNamespace)
import           Apigen.Semantic
import           Apigen.Types              (Constness (..))
import           Data.List                 (find, sortOn)
import           Data.Maybe                (fromMaybe, isNothing)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

data Param = Param
    { paramVar :: Text
    , paramCpp :: CppType
    } deriving (Show, Eq)

data Method = Method
    { methodVar   :: Text          -- ^ @friend_get_name@
    , methodCName :: Text
    , methodArgs  :: [Param]
    , methodValue :: CppType       -- ^ What it gives, 'TVoid' for nothing
    , methodError :: Maybe Text    -- ^ Its error enum, if C has one
    , methodConst :: Bool          -- ^ Whether C takes a const handle
    , methodBody  :: [Text]        -- ^ After taking the handle
    } deriving (Show, Eq)

-- | The root's functions and its members', of its own library: another
-- library's functions on the root, like toxav's, are its own class's.
rootMethods :: SemanticModel -> [Method]
rootMethods model =
    [ c | (r, m) <- allMethods model
        , resourceName r `elem` map resourceName (rootMembers model)
        , methodRole m `notElem` [StaticRole, Destructor, RegistrarRole, IterateRole]
        , let mapping = mappingOf model r m
              fn = cFunctionName mapping
        , take 1 (words' fn) == [rootNamespace model]
        , not (K.isLoopFunction model fn)
        , not (isSizeCompanion model fn)
        , isNothing (K.failureReturn model fn)
        , Just c <- [method model r m mapping] ]

cErrorEnum :: SemanticModel -> Text -> Maybe (Text, Text)
cErrorEnum model e = do
    enum <- findEnum model e
    ok <- lookup "OK" [ (enumMemberName mem, enumMemberCName mem) | mem <- enumMembers enum ]
    return (enumName enum, ok)

data Arg = Arg
    { argParams :: [Param]
    , argExprs  :: [Text]   -- ^ The C arguments, one per 'CArgSource' it covers
    }

method :: SemanticModel -> SResource -> SMethod -> CFunctionMapping -> Maybe Method
method model res m mapping = do
    let fn = cFunctionName mapping
        names = cArgNames mapping
        params = cSemParams mapping
        handle = "use." <> rootVar (lib model) <> "()"
    args <- mapM (argOf model res fn handle params) (group (zip names (argMapping mapping)))
    out <- outputOf model m mapping
    let cArgs = concatMap argExprs args
        value = outType out
        err = enumCppName model <$> methodErrorType m
    body <- bodyOf model m mapping (returnType value err) cArgs out
    return Method
        { methodVar   = unwords' (functionWords (namespaces model) (shownName model res m))
        , methodCName = fn
        , methodArgs  = concatMap argParams args
        , methodValue = value
        , methodError = err
        , methodConst = isConst (argMapping mapping)
        , methodBody  = body
        }
  where
    isConst srcs = case [ c | src <- srcs, Just c <- [handleConstness src] ] of
        c : _ -> c == ConstThis
        []    -> False
    handleConstness src = case src of
        ThisObject c   -> Just c
        PathObject _ c -> Just c
        _              -> Nothing

-- | @Result<T, E>@, or the value alone when C has no error.
returnType :: CppType -> Maybe Text -> Text
returnType value err = case err of
    Just e  -> "Result<" <> render value <> ", " <> e <> ">"
    Nothing -> render value

-- | C arguments in the groups one C++ parameter holds: bytes with their
-- length, or one.
group :: [(Text, CArgSource)] -> [[(Text, CArgSource)]]
group args = case args of
    []                                                -> []
    (a@(_, SemanticArg _) : b@(_, BufferSize _) : rest) -> [a, b] : group rest
    (a : rest)                                        -> [a] : group rest

argOf :: SemanticModel -> SResource -> Text -> Text -> [SParameter] -> [(Text, CArgSource)] -> Maybe Arg
argOf model res fn handle params grouped = case grouped of
    [(_, ThisObject _)] -> Just (Arg [] [handle])
    [(_, PathObject _ _)] -> Just (Arg [] [handle])
    [(n, PathId i)] -> do
        idTy <- idModel model (resolvePathTarget (resources model) res i)
        Just (Arg [Param n (TId (idTypeName (idName idTy)))] [n <> ".value"])
    [(n, SemanticArg i)] -> case drop i params of
        p : _ -> semanticArg model fn n p
        []    -> Nothing
    [(n, SemanticArg _), (_, BufferSize _)] ->
        Just (Arg [Param n TBytes] [n <> ".data()", n <> ".size()"])
    [(_, ErrorPtr)] -> Just (Arg [] ["&err"])
    [(n, BufferPtr _)] -> Just (Arg [] [n <> "Ptr"])  -- 'bodyOf' fills it in
    [(_, Constant v)] -> Just (Arg [] [Text.pack (show v)])
    _ -> Nothing

semanticArg :: SemanticModel -> Text -> Text -> SParameter -> Maybe Arg
semanticArg model fn n p
    | Just e <- K.enumParam model (fn, n) =
        Just (Arg [Param n (TEnum (enumCppName model e))] ["static_cast<uint32_t>(" <> n <> ")"])
    | otherwise = case paramType p of
        SBool -> plain TBool
        SInt k -> plain (TInt k)
        SUInt k -> plain (TUInt k)
        SSizeT -> plain TSize
        SString -> Just (Arg [Param n TString] [n <> ".c_str()"])
        SEnum e -> do
            enum <- findEnum model e
            Just (Arg [Param n (TEnum (enumCppName model e))] ["static_cast<" <> enumName enum <> ">(" <> n <> ")"])
        SResourceId _ -> do
            ty <- typeOf model (paramType p)
            Just (Arg [Param n ty] [n <> ".value"])
        SScalar _ _ -> do
            ty <- typeOf model (paramType p)
            Just (Arg [Param n ty] [n <> ".value"])
        SFixedBytes size _ | Just a <- arrayTypeOf model size ->
            if paramNullable p
                then Just (Arg [Param n (TOptional (TArray (arrayTypeName a)))]
                               [n <> " ? " <> n <> "->data.data() : nullptr"])
                else Just (Arg [Param n (TArray (arrayTypeName a))] [n <> ".data.data()"])
        _ -> Nothing
  where
    plain t = Just (Arg [Param n t] [n])

data Output
    = OutValue CppType           -- ^ Its return value
    | OutNone                    -- ^ Nothing, or a bool saying it worked
    | OutArray CppType Text Bool -- ^ A fixed-size buffer, by name; whether C's bool says it's there
    | OutSized CppType Text Text -- ^ A buffer of the size another function gives, by name

outType :: Output -> CppType
outType o = case o of
    OutValue t        -> t
    OutNone           -> TVoid
    OutArray t _ True -> TOptional t
    OutArray t _ _    -> t
    OutSized t _ _    -> t

outputOf :: SemanticModel -> SMethod -> CFunctionMapping -> Maybe Output
outputOf model m mapping
    | [n] <- [ n | (n, BufferPtr _) <- zip (cArgNames mapping) (argMapping mapping) ] =
        case (output m, cSizeFunctionName mapping) of
            (SFixedBytes size _, _) | Just a <- arrayTypeOf model size ->
                Just (OutArray (TArray (arrayTypeName a)) n
                               (cReturnType mapping == SBool && isNothing (methodErrorType m)))
            (SBytes, Just sizeFn) | K.isSecretBytes (cFunctionName mapping) -> Just (OutSized TSecretBytes n sizeFn)
            (t, Just sizeFn) -> (\ct -> OutSized ct n sizeFn) <$> typeOf model t
            _ -> Nothing
    | methodResultStrategy m == IgnoreReturn = Just OutNone
    | output m == SVoid = Just OutNone
    | otherwise = OutValue <$> typeOf model (output m)

bodyOf :: SemanticModel -> SMethod -> CFunctionMapping -> Text -> [Text] -> Output -> Maybe [Text]
bodyOf model m mapping resultTy cArgs out = do
    errDecl <- case methodErrorType m of
        Nothing -> Just Nothing
        Just e  -> Just <$> cErrorEnum model e
    let fn = cFunctionName mapping
        call extra = fn <> "(" <> Text.intercalate ", " (map (fill extra) cArgs) <> ")"
        fill extra a = fromMaybe a (lookup a extra)
        declErr = [ cTy <> " err = " <> ok <> ";" | Just (cTy, ok) <- [errDecl] ]
        -- The value, or C's error.
        ret value = case errDecl of
            Just (_, ok) -> ["return result<" <> resultTy <> ">(err, " <> ok <> maybe "" (", " <>) value <> ");"]
            Nothing      -> [ "return " <> v <> ";" | Just v <- [value] ]
    case out of
        OutNone -> Just $ declErr ++ [call [] <> ";"] ++ ret Nothing
        OutValue t -> Just $ declErr ++
            [ "const auto value = " <> call [] <> ";" ]
            ++ ret (Just (fromC t "value"))
        OutArray t n present -> Just $ declErr ++
            if present
                then [ render (unOpt t) <> " " <> local <> "{};"
                     , "const bool present = " <> call [(n <> "Ptr", local <> ".data.data()")] <> ";"
                     ] ++ ret (Just ("present ? std::optional{" <> local <> "} : std::nullopt"))
                else [ render t <> " " <> local <> "{};"
                     , call [(n <> "Ptr", local <> ".data.data()")] <> ";"
                     ] ++ ret (Just local)
          where
            -- By its type: a C name like @id@ is the method's.
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
                    -- A local is moved on return anyway; into a Result it isn't.
                    _ | isNothing errDecl -> (render t, n)
                      | otherwise         -> (render t, "std::move(" <> n <> ")")
                fetch = call [(n <> "Ptr", n <> ".data()")] <> ";"
            Just $ declErr
                ++ (case errDecl of
                        -- C's size is SIZE_MAX on an error: nothing is allocated then.
                        -- An empty vector's data is NULL, which C may take as an error,
                        -- so nothing is fetched for an empty value.
                        Just (_, ok) | ErrorPtr `elem` argMapping sizeMapping ->
                            [ "const size_t " <> n <> "_size = " <> sizeCall <> ";"
                            , raw <> " " <> n <> "(err == " <> ok <> " ? " <> n <> "_size : 0);"
                            , "if (err == " <> ok <> " && " <> n <> "_size != 0) {", "    " <> fetch, "}" ]
                        _ -> [raw <> " " <> n <> "(" <> sizeCall <> ");", fetch])
                ++ ret (Just value)
  where
    unOpt (TOptional t) = t
    unOpt t             = t
    find' fn = case [ (r, sm) | (r, sm) <- allMethods model, cFunctionName (mappingOf model r sm) == fn ] of
        x : _ -> Just x
        []    -> Nothing

-- | A parameter as the method takes it: bytes and arrays by view or
-- reference, numbers and IDs by value.
paramDecl :: Param -> Text
paramDecl (Param n t) = case t of
    TBytes      -> "std::span<const uint8_t> " <> n
    TString     -> "const std::string& " <> n
    TArray a    -> "const " <> a <> "& " <> n
    TOptional _ -> "const " <> render t <> "& " <> n
    _           -> render t <> " " <> n

signature :: Text -> Method -> Text
signature scope mt =
    returnType (methodValue mt) (methodError mt) <> " " <> scope <> methodVar mt
    <> "(" <> Text.intercalate ", " (map paramDecl (methodArgs mt)) <> ")"
    <> (if methodConst mt then " const" else "")

-- | @tox/generated/tox.hpp@: the root handle's class.
rootHeader :: Lib -> (Text -> Maybe Text) -> [Method] -> Text
rootHeader l doc ms = header l
    [ own l "errors.hpp", own l "generated/events.hpp", own l "generated/options.hpp"
    , own l "result.hpp", own l "types.hpp"
    , "<chrono>", "<cstdint>", "<memory>", "<optional>", "<span>", "<string>", "<vector>" ]
    $ namespaced l
    $ [ "namespace detail {", "struct Core;", "} // namespace detail", "" ]
    ++ docBlock (Just (Text.intercalate "\n"
        [ "@brief A " <> cls <> " instance."
        , ""
        , "Each call does its work on the calling thread and returns the result."
        , "What happens meanwhile comes back from iterate(), which is to be"
        , "called again after iteration_interval()."
        , ""
        , "A " <> cls <> " may move to another thread, but two threads can't use it"
        , "at once: that stops the program with a message."
        ]))
    ++ declaration ("class " <> cls)
        ( ["public:"]
        ++ docBlock (Just (Text.intercalate "\n"
            [ "@brief Starts a " <> cls <> " instance. With a " <> word <> " level, " <> cls <> "'s"
            , word <> " messages of that level and above are events."
            ]))
        ++ [ "static Result<" <> cls <> ", " <> newErrorType l <> "> make(const Options& options, std::optional<"
             <> logLevelType l <> "> " <> unwords' (words' (Text.concatMap snake (logLevelType l))) <> " = std::nullopt);"
           , ""
           , "~" <> cls <> "();"
           , cls <> "(" <> cls <> "&& other) noexcept;"
           , cls <> "& operator=(" <> cls <> "&& other) noexcept;"
           , cls <> "(const " <> cls <> "&) = delete;"
           , cls <> "& operator=(const " <> cls <> "&) = delete;"
           , "" ]
        ++ docBlock (Just "@brief How long to wait before calling iterate() again.")
        ++ [ "[[nodiscard]] std::chrono::milliseconds iteration_interval() const;", "" ]
        ++ docBlock (Just ("@brief Does " <> cls <> "'s work, and returns what happened since the last call."))
        ++ [ "[[nodiscard]] std::vector<Event> iterate();", "" ]
        ++ concat [ docBlock (("@brief " <>) . summary <$> doc (methodCName mt))
                    ++ [nodiscard mt <> signature "" mt <> ";", ""]
                  | mt <- sorted ]
        ++ ["private:"]
        ++ [ "friend class " <> avClass l <> ";" | not (Text.null (avClass l)) ]
        ++ [ ""
           , "explicit " <> cls <> "(std::shared_ptr<detail::Core> core) noexcept;"
           , ""
           , "std::shared_ptr<detail::Core> core_;"
           ] )
    ++ [""]
  where
    cls = rootType l
    word = callbackWord l
    sorted = sortOn methodVar ms
    snake ch = if ch >= 'A' && ch <= 'Z' then Text.pack ['_', ch] else Text.singleton ch
    -- A value without an error to look at is the call's whole point.
    nodiscard mt
        | isNothing (methodError mt) && methodValue mt /= TVoid = "[[nodiscard]] "
        | otherwise = ""

-- | @src/generated/tox.cpp@: the methods, each a C call under the handle's
-- guard.
rootSource :: Lib -> [Method] -> Text
rootSource l ms = source l
    [ own l ("generated/" <> rootVar l <> ".hpp"), "core.hpp"
    , "<cstdint>", "<optional>", "<span>", "<string>", rootInclude l, "<utility>", "<vector>" ]
    $ namespaced l
    $ ["namespace {", ""]
    ++ helpers
    ++ ["} // namespace", ""]
    ++ concat
        [ braced (signature (rootType l <> "::") mt)
            ("const detail::Use use(core_.get());" : methodBody mt)
          ++ [""]
        | mt <- sortOn methodVar ms ]
  where
    helpers =
        [ "/**"
        , " * @brief A call's result, from the C error that is `ok` on success."
        , " */"
        , "template <typename R, typename CError, typename... Value>"
        , "R result(CError err, CError ok, Value&&... value)"
        , "{"
        , "    if (err != ok) {"
        , "        return R::err(static_cast<typename R::error_type>(err));"
        , "    }"
        , "    return R::ok(std::forward<Value>(value)...);"
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
