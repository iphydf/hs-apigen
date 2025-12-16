{-# LANGUAGE OverloadedStrings #-}
-- | Proxies for the local Tox, a friend, a group or a conference, with a
-- method sending each command.
module Apigen.Language.Cpp.Proxies
    ( proxyFiles
    ) where

import qualified Apigen.Knowledge             as K
import qualified Apigen.Language.Cpp.Binding  as Binding
import           Apigen.Language.Cpp.Commands
import           Apigen.Language.Cpp.Names
import           Apigen.Language.Cpp.Text
import           Apigen.Language.Cpp.Types
import           Apigen.Semantic              (SResource (..),
                                               SResourceType (..), SType (..),
                                               SemanticModel (..))
import           Data.List                    (delete, find, sortOn)
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
            (Binding.proxyDoc (resourceName r) (Text.unwords ws))
    | r <- resources model, ResId (SResourceId i) <- [resourceType r]
    , maybe False (\p -> any (\h -> isRoot h && resourceName h == p) (resources model)) (parent r)
    , isNothing (K.idScope model (resourceName r))
    , let ws = words' (resourceName r) ]

-- | Proxy a command is a method of: what it acts on, else its first ID.
proxyOf :: SemanticModel -> Command -> Proxy
proxyOf model c = fromMaybe (selfProxy model) $ case objectOf (K.libraryWords model) (cmdCName c) of
    Just (o : _) | Just p <- find ((== o) . proxyWord) (proxies model), has p -> Just p
    _ -> find has [ p | f <- cmdFields c, p <- proxies model, fmap fst (proxyId p) == Just (fieldName f) ]
  where
    has p = maybe False (\(n, _) -> any ((== n) . fieldName) (cmdFields c)) (proxyId p)

-- | A command's method name on its proxy: @get_name@ on a friend.
methodName :: Proxy -> Command -> Text
methodName p c
    -- @Friend::delete@ can't be, so it's @delete_friend@.
    | [w] <- short, w `elem` keywords = unwords' (cmdWords c)
    | otherwise = unwords' short
  where
    short = delete (proxyWord p) (cmdWords c)
    keywords = ["delete", "new"]

proxyFiles :: SemanticModel -> (Text -> Maybe Text) -> [Command] -> [(FilePath, Text)]
proxyFiles model doc cmds =
    [ ("include/tox/generated/" <> Text.unpack (Text.toLower (proxyClass p)) <> ".hpp", proxyHeader doc p (methodsOf p))
    | p <- proxies model ]
    ++ [("src/generated/proxies.cpp", proxySource [ (p, methodsOf p) | p <- proxies model ])]
  where
    methodsOf p = sortOn (methodName p) [ c | c <- cmds, proxyClass (proxyOf model c) == proxyClass p ]

params :: Proxy -> Command -> [Field]
params p c = [ f | f <- cmdFields c, Just (fieldName f) /= fmap fst (proxyId p) ]

paramDecl :: Field -> Text
paramDecl (Field n t) = case t of
    TBytes               -> "std::span<const uint8_t> " <> n
    TString              -> "std::string " <> n
    TArray a             -> "const " <> a <> "& " <> n
    TOptional (TArray a) -> "const std::optional<" <> a <> ">& " <> n
    TNamed "AudioFrame"  -> "AudioFrame&& " <> n
    TNamed "VideoFrame"  -> "VideoFrame&& " <> n
    _                    -> render t <> " " <> n

paramValue :: Field -> Text
paramValue (Field n t) = case t of
    TBytes              -> "{" <> n <> ".begin(), " <> n <> ".end()}"
    TString             -> "std::move(" <> n <> ")"
    TNamed "AudioFrame" -> "std::move(" <> n <> ")"
    TNamed "VideoFrame" -> "std::move(" <> n <> ")"
    _                   -> n

qualifier :: Command -> Text
qualifier c = if take 1 (cmdWords c) == ["get"] then " const noexcept" else " noexcept"

signature :: Proxy -> Command -> Text
signature p c = methodName p c <> "(" <> Text.intercalate ", " (map paramDecl (params p c)) <> ")" <> qualifier c

proxyHeader :: (Text -> Maybe Text) -> Proxy -> [Command] -> Text
proxyHeader doc p cmds = header
    [ "tox/commands.hpp", "tox/media.hpp", "tox/types.hpp", "<cstdint>", "<optional>", "<span>", "<string>" ]
    $  ["namespace tox {", "", "class SessionInterface;", ""]
    ++ docBlock (Just ("@brief " <> proxyDoc p <> "\n\nEach method sends a command, and returns its ID."))
    ++ declaration ("class " <> proxyClass p)
        ( ["public:"]
        ++  ( constructor
            ++ concat [ docBlock (("@brief " <>) . summary <$> doc (cmdCName c)) ++ ["CommandId " <> signature p c <> ";", ""] | c <- cmds ] )
        ++ ["private:"]
        ++ [ ty <> " id_;" | Just (_, ty) <- [proxyId p] ] ++ ["SessionInterface& session_;"] )
    ++ ["", "} // namespace tox"]
  where
    constructor = case proxyId p of
        Nothing -> ["explicit " <> proxyClass p <> "(SessionInterface& session) noexcept;", ""]
        Just (_, ty) ->
            [ proxyClass p <> "(" <> ty <> " id, SessionInterface& session) noexcept;"
            , ""
            ] ++ braced (ty <> " id() const noexcept") ["return id_;"] ++ [""]

proxySource :: [(Proxy, [Command])] -> Text
proxySource ps = source
    [ "tox/generated/conference.hpp", "tox/generated/friend.hpp", "tox/generated/group.hpp"
    , "tox/generated/self.hpp", "tox/session_interface.hpp", "<utility>" ]
    $ ["namespace tox {", ""] ++ concatMap proxy ps ++ ["} // namespace tox"]
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
