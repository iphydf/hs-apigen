{-# LANGUAGE OverloadedStrings #-}
-- | Proxies for the local Tox, a friend, a group or a conference, with a
-- method sending each command.
module Apigen.Language.Cpp.Proxies
    ( proxyFiles
    ) where

import qualified Apigen.Knowledge             as K
import           Apigen.Language.Cpp.Commands
import           Apigen.Language.Cpp.Lib
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Semantic              (SResource (..),
                                               SResourceType (..), SType (..),
                                               SemanticModel (..))
import           Control.Applicative          ((<|>))
import           Data.List                    (find, isPrefixOf, sortOn,
                                               stripPrefix)
import           Data.Maybe                   (fromMaybe, isNothing)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

data Proxy = Proxy
    { proxyClass :: Text
    , proxyWord  :: Text                  -- ^ Its object's first word in C names
    , proxyId    :: Maybe (Text, Text)   -- ^ Its ID's field and type
    , proxyDoc   :: Text
    }

selfProxy :: SemanticModel -> Proxy
selfProxy model = Proxy "Self" "self" Nothing ("A proxy representing the local " <> root <> " instance.")
  where
    root = maybe "" resourceName (find isRoot (resources model))

-- | The local instance, and what an id under it names on its own: a
-- friend, but not a file, which also needs its friend.
proxies :: SemanticModel -> [Proxy]
proxies model = selfProxy model :
    [ Proxy (pascal ws) (unwords' ws) (Just (unwords' (words' i), idTypeName i))
            ("A proxy representing a " <> Text.unwords ws <> ".")
    | r <- resources model, ResId (SResourceId i) <- [resourceType r]
    , maybe False (\p -> any (\h -> isRoot h && resourceName h == p) (resources model)) (parent r)
    , isNothing (K.idScope model (resourceName r))
    , let ws = words' (resourceName r) ]

-- | Proxy a command is a method of: the one its name starts with, if it
-- takes that one's id, else its first id's, else the local instance.
proxyOf :: SemanticModel -> Command -> Proxy
proxyOf model c = fromMaybe (selfProxy model) $
    find (\p -> has p && named p) (proxies model)
    <|> find has [ p | f <- cmdFields c, p <- proxies model, fmap fst (proxyId p) == Just (fieldName f) ]
  where
    has p = maybe False (\(n, _) -> any ((== n) . fieldName) (cmdFields c)) (proxyId p)
    named p = words' (proxyWord p) `isPrefixOf` cmdWords c

-- | A command's method name on its proxy, the rest of its name:
-- @get_name@ on a friend, @self_get_name@ on a group.
methodName :: Proxy -> Command -> Text
methodName p c
    -- @Friend::delete@ can't be, so it's @friend_delete@.
    | [w] <- short, w `elem` keywords = unwords' (cmdWords c)
    | otherwise = unwords' short
  where
    pw = words' (proxyWord p)
    short = fromMaybe (cmdWords c) (stripPrefix pw (cmdWords c))
    keywords = ["delete", "new"]

proxyFiles :: SemanticModel -> (Text -> Maybe Text) -> [Command] -> [(FilePath, Text)]
proxyFiles model doc cmds =
    [ (generatedHeader l $ Text.unpack (Text.toLower (proxyClass p)) <> ".hpp", proxyHeader l doc p (methodsOf p))
    | p <- proxies model ]
    ++ [("src/generated/proxies.cpp", proxySource l [ (p, methodsOf p) | p <- proxies model ])]
  where
    l = lib model
    methodsOf p = sortOn (methodName p) [ c | c <- cmds, proxyClass (proxyOf model c) == proxyClass p ]

params :: Proxy -> Command -> [Field]
params p c = [ f | f <- cmdFields c, Just (fieldName f) /= fmap fst (proxyId p) ]

paramDecl :: Field -> Text
paramDecl (Field n t) = case t of
    TBytes               -> "std::span<const uint8_t> " <> n
    TString              -> "std::string " <> n
    TArray a             -> "const " <> a <> "& " <> n
    TOptional (TArray a) -> "const std::optional<" <> a <> ">& " <> n
    TFrame f             -> f <> "&& " <> n
    _                    -> render t <> " " <> n

paramValue :: Field -> Text
paramValue (Field n t) = case t of
    TBytes   -> "{" <> n <> ".begin(), " <> n <> ".end()}"
    TString  -> "std::move(" <> n <> ")"
    TFrame _ -> "std::move(" <> n <> ")"
    _        -> n

-- | A proxy only sends commands to its session, so no call changes it.
signature :: Proxy -> Command -> Text
signature p c = methodName p c <> "(" <> Text.intercalate ", " (map paramDecl (params p c)) <> ") const noexcept"

proxyHeader :: Lib -> (Text -> Maybe Text) -> Proxy -> [Command] -> Text
proxyHeader l doc p cmds = header l
    [ own l "commands.hpp", own l "media.hpp", own l "types.hpp", "<cstdint>", "<optional>", "<span>", "<string>" ]
    $  ["namespace " <> libNamespace l <> " {", "", "class SessionInterface;", ""]
    ++ docBlock (Just ("@brief " <> proxyDoc p <> "\n\nEach method sends a command, and returns its ID."))
    ++ declaration ("class " <> proxyClass p)
        ( ["public:"]
        ++  ( constructor
            ++ concat [ docBlock (("@brief " <>) . summary <$> doc (cmdCName c)) ++ ["CommandId " <> signature p c <> ";", ""] | c <- cmds ] )
        ++ ["private:"]
        ++ [ ty <> " id_;" | Just (_, ty) <- [proxyId p] ] ++ ["SessionInterface& session_;"] )
    ++ ["", "} // namespace " <> libNamespace l]
  where
    constructor = case proxyId p of
        Nothing -> ["explicit " <> proxyClass p <> "(SessionInterface& session) noexcept;", ""]
        Just (_, ty) ->
            [ proxyClass p <> "(" <> ty <> " id, SessionInterface& session) noexcept;"
            , ""
            ] ++ braced (ty <> " id() const noexcept") ["return id_;"] ++ [""]

proxySource :: Lib -> [(Proxy, [Command])] -> Text
proxySource l ps = source l
    ( [ own l ("generated/" <> Text.toLower (proxyClass p) <> ".hpp") | (p, _) <- ps ]
    ++ [ own l "session_interface.hpp", "<utility>" ] )
    $ ["namespace " <> libNamespace l <> " {", ""] ++ concatMap proxy ps ++ ["} // namespace " <> libNamespace l]
  where
    proxy (p, cmds) = constructor p ++ [""] ++ concat [ method p c ++ [""] | c <- cmds ]
    constructor p = case proxyId p of
        Nothing ->
            [ proxyClass p <> "::" <> proxyClass p <> "(SessionInterface& session) noexcept"
            , "    : session_(session)"
            , "{"
            , "}"
            ]
        Just (_, ty) ->
            [ proxyClass p <> "::" <> proxyClass p <> "(" <> ty <> " id, SessionInterface& session) noexcept"
            , "    : id_(id)"
            , "    , session_(session)"
            , "{"
            , "}"
            ]
    method p c = braced ("CommandId " <> proxyClass p <> "::" <> signature p c) $
        [ "return session_.send(command::" <> cmdName c <> "{" ]
        ++ indent [ "." <> fieldName f <> " = " <> value p f <> "," | f <- cmdFields c ]
        ++ ["});"]
    value p f
        | Just (fieldName f) == fmap fst (proxyId p) = "id_"
        | otherwise = paramValue f
