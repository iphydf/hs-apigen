{-# LANGUAGE OverloadedStrings #-}
-- | toxencryptsave: encrypting data with a passphrase, or a key derived
-- from one.
module Apigen.Language.Cpp.Crypto
    ( cryptoFiles
    ) where

import qualified Apigen.Knowledge            as K
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Methods (cErrorEnum)
import           Apigen.Language.Cpp.Names   (arrayTypeName, enumeratorName)
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Model
import           Apigen.Semantic
import           Apigen.SizeExpr             (renderSizeExpr)
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

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
cryptoFiles doc model = let l = lib model in case K.handlesOfKind model StandaloneHandle of
    [] -> []
    res : _ ->
        let fns = mapMaybe (function model res) (methods res)
        in [ (generatedHeader l "crypto.hpp", cryptoHeader l doc fns)
           , ("src/generated/crypto.cpp", cryptoSource l fns) ]

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
        sizeVar v = fromMaybe v (lookup v [ (len, n <> ".size()") | ((n, SemanticArg _), (len, BufferSize _)) <- pairs ])
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
              ++ [ passKeyType l <> "* key = " <> call [] <> ";" ]
              ++ braced "if (key == nullptr)" [ "return " <> resultOf "PassKey" <> "::err(static_cast<" <> errType <> ">(err));" ]
              ++ [ "return " <> resultOf "PassKey" <> "::ok(PassKey(key));" ] )
        (_, SBool) -> Just ("bool", checks ++ ["return " <> call [] <> ";"])
        (_, SFixedBytes size _) -> do
            (decl, ptr, value, ty) <- case arrayTypeOf model size of
                Just a -> Just ( arrayTypeName a <> " out{};", "out.data.data()", "out", arrayTypeName a )
                -- What's decrypted is erased when it goes.
                Nothing | K.isSecretBytes fn ->
                    Just ("SecretBytes out(" <> sizeExpr sizeVar size <> ");", "out.data()", "std::move(out)", "SecretBytes")
                Nothing ->
                    Just ("std::vector<uint8_t> out(" <> sizeExpr sizeVar size <> ");", "out.data()", "std::move(out)", "std::vector<uint8_t>")
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
    l = lib model
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
        (a@(_, SemanticArg _) : b@(_, BufferSize _) : rest) -> [a, b] : groups rest
        (a : rest) -> [a] : groups rest
        [] -> []

    arg grouped = case grouped of
        [(_, ThisObject _)] -> Just ([], ["key_.get()"], [])
        [(n, SemanticArg _), (_, BufferSize _)] -> Just (["std::span<const uint8_t> " <> n], [n <> ".data()", n <> ".size()"], [])
        [(n, SemanticArg i)] -> case paramType <$> take 1 (drop i params') of
            [SFixedBytes size _]
                | Just a <- arrayTypeOf model size -> Just (["const " <> arrayTypeName a <> "& " <> n], [n <> ".data.data()"], [])
                | Just failure <- K.minimumSize model (fn, n) ->
                    Just ( ["std::span<const uint8_t> " <> n], [n <> ".data()"]
                         , braced ("if (" <> n <> ".size() < " <> renderSizeExpr size <> ")") ["return " <> failed failure <> ";"] )
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
            SFixedBytes size _ | Just a <- arrayTypeOf model size -> arrayTypeName a
            _ | K.isSecretBytes fn -> "SecretBytes"
            _ -> "std::vector<uint8_t>"
        member c = case [ enumMemberName mem | Just e <- [methodErrorType m >>= findEnum model], mem <- enumMembers e, enumMemberCName mem == c ] of
            mem : _ -> enumeratorName mem
            []      -> c

cryptoHeader :: Lib -> (Text -> Maybe Text) -> [Function] -> Text
cryptoHeader l doc fns = header l
    [ own l "errors.hpp", own l "result.hpp", own l "types.hpp", "<cstdint>", "<memory>", "<span>", "<vector>" ]
    $  ["struct " <> passKeyType l <> ";", "", "namespace " <> libNamespace l <> " {", ""]
    ++ docBlock (Just ("@brief The functions of " <> Text.dropAround (`elem` ("<>" :: String)) (cryptoInclude l) <> "."))
    ++ declaration "class Crypto"
        ( ["public:"]
        ++  ( concat [ decl "static " f | f <- fns, fnKind f == Static ]
            ++ docBlock (("@brief " <>) . summary <$> doc (passKeyType l))
            ++ declaration "class PassKey"
                ( ["public:"]
                ++  ( concat [ decl "static " f | f <- fns, fnKind f == Derive ]
                    ++ concat [ decl "" f | f <- fns, fnKind f == OnKey ] )
                ++ ["private:"]
                ++  [ "explicit PassKey(" <> passKeyType l <> "* _Nonnull key) noexcept;"
                    , ""
                    , "std::unique_ptr<" <> passKeyType l <> ", void (*)(" <> passKeyType l <> "* _Nullable)> key_;"
                    ] ) ) )
    ++ ["", "} // namespace " <> libNamespace l]
  where
    decl prefix f =
        docBlock (("@brief " <>) . summary <$> doc (fnCName f))
        ++ [ prefix <> fnResult f <> " " <> fnName f <> "(" <> Text.intercalate ", " (fnParams f) <> ")" <> constness f <> " noexcept;", "" ]
    constness f = if fnKind f == OnKey then " const" else ""

cryptoSource :: Lib -> [Function] -> Text
cryptoSource l fns = source l
    [ own l "generated/crypto.hpp", "<cstdint>", "<memory>", "<span>", cryptoInclude l, "<utility>", "<vector>" ]
    $  ["namespace " <> libNamespace l <> " {", ""]
    ++ braced ("Crypto::PassKey::PassKey(" <> passKeyType l <> "* _Nonnull key) noexcept\n    : key_(key, " <> passKeyFree l <> ")") []
    ++ [""]
    ++ concat [ braced (qualified (fnResult f) <> " " <> scope f <> fnName f <> "(" <> Text.intercalate ", " (fnParams f) <> ")" <> constness f <> " noexcept") (fnBody f) ++ [""] | f <- fns ]
    ++ ["} // namespace " <> libNamespace l]
  where
    scope f = if fnKind f == Static then "Crypto::" else "Crypto::PassKey::"
    -- Outside the class, its key type needs its name.
    qualified = Text.replace "<PassKey," "<Crypto::PassKey,"
    constness f = if fnKind f == OnKey then " const" else ""
