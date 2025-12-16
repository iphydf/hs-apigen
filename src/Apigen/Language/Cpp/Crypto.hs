{-# LANGUAGE OverloadedStrings #-}
-- | toxencryptsave: encrypting data with a passphrase, or a key derived
-- from one.
module Apigen.Language.Cpp.Crypto
    ( cryptoFiles
    ) where

import qualified Apigen.Knowledge             as K
import           Apigen.Language.Cpp.Commands (cErrorEnum)
import           Apigen.Language.Cpp.Names    (arrayTypeName, enumeratorName)
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Semantic
import           Apigen.SizeExpr              (parseSizeExpr)
import           Data.Maybe                   (fromMaybe, mapMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

data Kind = Static | Derive | OnKey
    deriving (Eq)

data Function = Function
    { fnKind   :: Kind
    , fnCName  :: Text
    , fnName   :: Text
    , fnParams :: [Text]   -- ^ C++ parameter declarations
    , fnResult :: Text     -- ^ C++ result type
    , fnBody   :: [Text]
    }

cryptoFiles :: (Text -> Maybe Text) -> SemanticModel -> [(FilePath, Text)]
cryptoFiles doc model = case K.handlesOfKind model StandaloneHandle of
    [] -> []
    res : _ ->
        let fns = mapMaybe (function model res) (methods res)
        in [ ("include/tox/generated/crypto.hpp", cryptoHeader doc fns)
           , ("src/generated/crypto.cpp", cryptoSource fns) ]

-- | A toxencryptsave function as a C++ one, if it's one a client calls.
function :: SemanticModel -> SResource -> SMethod -> Maybe Function
function model res m = do
    kind <- case methodRole m of
        StaticRole | not (null (argMapping mapping)), not (isEnumNamer m) -> Just Static
        Constructor -> Just Derive
        ActionRole  -> Just OnKey
        _           -> Nothing
    args <- mapM arg (groups (zip (cArgNames mapping) (argMapping mapping)))
    let params = concat [ p | (p, _, _) <- args ]
        cArgs = concat [ a | (_, a, _) <- args ]
        checks = concat [ c | (_, _, c) <- args ]
        sizeVar v = fromMaybe v (lookup v [ (len, n <> ".size()") | ((n, SemanticArg _), (len, BufferSize)) <- pairs ])
        pairs = zip (zip (cArgNames mapping) (argMapping mapping)) (drop 1 (zip (cArgNames mapping) (argMapping mapping)))
        call extra = fn <> "(" <> Text.intercalate ", " (map (\a -> fromMaybe a (lookup a extra)) cArgs) <> ")"
    errInfo <- case methodErrorType m of
        Nothing -> Just Nothing
        Just e  -> Just <$> cErrorEnum model e
    let errType = maybe "NoError" (enumCppName model) (methodErrorType m)
        declErr = [ cTy <> " err = " <> ok <> ";" | Just (cTy, ok) <- [errInfo] ]
        ok' = maybe "" snd errInfo
        resultOf t = "Result<" <> t <> ", " <> errType <> ">"
    (resultTy, body) <- case (kind, output m) of
        (Derive, _) -> Just
            ( resultOf "PassKey"
            , declErr
              ++ [ "Tox_Pass_Key* key = " <> call [] <> ";" ]
              ++ braced "if (key == nullptr)" [ "return " <> resultOf "PassKey" <> "::err(static_cast<" <> errType <> ">(err));" ]
              ++ [ "return " <> resultOf "PassKey" <> "::ok(PassKey(key));" ] )
        (_, SBool) -> Just ("bool", checks ++ ["return " <> call [] <> ";"])
        (_, SFixedBytes size _) -> do
            (decl, ptr, value, ty) <- case lookup size (arrayTypes model) of
                Just a -> Just ( arrayTypeName a <> " out{};", "out.data.data()", "out", arrayTypeName a )
                Nothing -> do
                    e <- parseSizeExpr size
                    Just ("std::vector<uint8_t> out(" <> sizeExpr sizeVar e <> ");", "out.data()", "std::move(out)", "std::vector<uint8_t>")
            out <- case [ n | (n, BufferPtr _) <- zip (cArgNames mapping) (argMapping mapping) ] of
                [n] -> Just n
                _   -> Nothing
            Just
                ( resultOf ty
                , checks ++ declErr ++ [decl, call [(out <> "Ptr", ptr)] <> ";"]
                  ++ braced ("if (err != " <> ok' <> ")") [ "return " <> resultOf ty <> "::err(static_cast<" <> errType <> ">(err));" ]
                  ++ ["return " <> resultOf ty <> "::ok(" <> value <> ");"] )
        _ -> Nothing
    return Function
        { fnKind   = kind
        , fnCName  = fn
        , fnName   = name kind
        , fnParams = params
        , fnResult = resultTy
        , fnBody   = body
        }
  where
    mapping = mappingOf model res m
    fn = cFunctionName mapping
    name kind = case kind of
        -- A static's prefix stops before the object's last word:
        -- @tox_pass_encrypt@ is @encrypt@ and @tox_get_salt@ @get_salt@.
        Static -> fromMaybe fn (firstJust [ Text.stripPrefix p fn | p <- drop 1 (reverse (wordPrefixes (cPrefix res))) ])
        _      -> fromMaybe fn (Text.stripPrefix (cPrefix res) fn)
    firstJust xs = case [ x | Just x <- xs ] of
        x : _ -> Just x
        []    -> Nothing
    -- @a_@, @a_b_@ for @a_b_@.
    wordPrefixes p = [ Text.intercalate "_" ws <> "_" | ws <- drop 1 (inits' (filter (not . Text.null) (Text.splitOn "_" p))) ]
    inits' xs = [ take n xs | n <- [0 .. length xs] ]
    params' = cSemParams mapping

    groups as = case as of
        (a@(_, SemanticArg _) : b@(_, BufferSize) : rest) -> [a, b] : groups rest
        (a : rest) -> [a] : groups rest
        [] -> []

    arg grouped = case grouped of
        [(_, ThisObject _)] -> Just ([], ["key_.get()"], [])
        [(n, SemanticArg _), (_, BufferSize)] -> Just (["std::span<const uint8_t> " <> n], [n <> ".data()", n <> ".size()"], [])
        [(n, SemanticArg i)] -> case paramType <$> take 1 (drop i params') of
            [SFixedBytes size _]
                | Just a <- lookup size (arrayTypes model) -> Just (["const " <> arrayTypeName a <> "& " <> n], [n <> ".data.data()"], [])
                | Just failure <- K.minimumSize model (fn, n) ->
                    Just ( ["std::span<const uint8_t> " <> n], [n <> ".data()"]
                         , braced ("if (" <> n <> ".size() < " <> size <> ")") ["return " <> failed failure <> ";"] )
            _ -> Nothing
        [(_, ErrorPtr)] -> Just ([], ["&err"], [])
        [(n, BufferPtr _)] -> Just ([], [n <> "Ptr"], [])
        _ -> Nothing

    failed failure
        | failure == "false" = "false"
        | otherwise = "Result<" <> outName <> ", " <> errName <> ">::err(" <> errName <> "::" <> member failure <> ")"
      where
        errName = maybe "NoError" (enumCppName model) (methodErrorType m)
        outName = case output m of
            SFixedBytes size _ | Just a <- lookup size (arrayTypes model) -> arrayTypeName a
            _ -> "std::vector<uint8_t>"
        member c = case [ enumMemberName mem | Just e <- [methodErrorType m >>= findEnum model], mem <- enumMembers e, enumMemberCName mem == c ] of
            mem : _ -> enumeratorName mem
            []      -> c

cryptoHeader :: (Text -> Maybe Text) -> [Function] -> Text
cryptoHeader doc fns = header
    [ "tox/errors.hpp", "tox/result.hpp", "tox/types.hpp", "<cstdint>", "<memory>", "<span>", "<vector>" ]
    $  ["struct Tox_Pass_Key;", "", "namespace tox {", ""]
    ++ docBlock (Just "@brief Encrypting data, such as save data, with a passphrase.")
    ++ declaration "class Crypto"
        ( ["public:"]
        ++  ( concat [ decl "static " f | f <- fns, fnKind f == Static ]
            ++ docBlock (Just "@brief A key derived from a passphrase, to encrypt with it faster.")
            ++ declaration "class PassKey"
                ( ["public:"]
                ++  ( concat [ decl "static " f | f <- fns, fnKind f == Derive ]
                    ++ concat [ decl "" f | f <- fns, fnKind f == OnKey ] )
                ++ ["private:"]
                ++  [ "explicit PassKey(Tox_Pass_Key* _Nonnull key) noexcept;"
                    , ""
                    , "std::unique_ptr<Tox_Pass_Key, void (*)(Tox_Pass_Key* _Nullable)> key_;"
                    ] ) ) )
    ++ ["", "} // namespace tox"]
  where
    decl prefix f =
        docBlock (("@brief " <>) . summary <$> doc (fnCName f))
        ++ [ prefix <> fnResult f <> " " <> fnName f <> "(" <> Text.intercalate ", " (fnParams f) <> ")" <> constness f <> " noexcept;", "" ]
    constness f = if fnKind f == OnKey then " const" else ""

cryptoSource :: [Function] -> Text
cryptoSource fns = source
    [ "tox/generated/crypto.hpp", "<cstdint>", "<memory>", "<span>", "<tox/toxencryptsave.h>", "<utility>", "<vector>" ]
    $  ["namespace tox {", ""]
    ++ braced "Crypto::PassKey::PassKey(Tox_Pass_Key* _Nonnull key) noexcept\n    : key_(key, tox_pass_key_free)" []
    ++ [""]
    ++ concat [ braced (qualified (fnResult f) <> " " <> scope f <> fnName f <> "(" <> Text.intercalate ", " (fnParams f) <> ")" <> constness f <> " noexcept") (fnBody f) ++ [""] | f <- fns ]
    ++ ["} // namespace tox"]
  where
    scope f = if fnKind f == Static then "Crypto::" else "Crypto::PassKey::"
    -- Outside the class, its key type needs its name.
    qualified = Text.replace "<PassKey," "<Crypto::PassKey,"
    constness f = if fnKind f == OnKey then " const" else ""
