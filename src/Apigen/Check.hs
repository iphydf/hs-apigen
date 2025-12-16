{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Conventions of the headers that the bindings rely on, checked so a header
-- can't drift from them unnoticed.
module Apigen.Check (check) where

import qualified Apigen.Knowledge          as K
import           Apigen.Model              (mappingOf, shownName,
                                            sizeCompanions)
import           Apigen.Namespace          (rootNamespace)
import qualified Apigen.Parser.AST         as T
import           Apigen.Parser.Semantic    (flattenName, getLoc)
import           Apigen.Parser.SymbolTable (Name)
import qualified Apigen.Semantic           as S
import           Apigen.Types              (BitSize (..), BuiltinType (..))
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Ord                  (Down (..))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Language.Cimple           (Lexeme (..))
import           System.FilePath           (dropExtension, takeFileName)

type Decl = T.Decl (Lexeme Name)

-- | What the rules need to know of the other declarations.
data Env = Env
    { envFunctions :: Set Text
    , envHandles   :: Set Text
    , envIds       :: Set Text
    , envEnums     :: Map.Map Text [Text]
    , envParams    :: Map.Map Text [Decl]
    , envTakers    :: Map.Map Text Int
    , envArrays    :: Set Text
    , envUnlisted  :: Set Text
    , envAnnotated :: Set FilePath
    }

check :: S.SemanticModel -> T.Model (Lexeme Name) -> [S.SDiagnostic]
check sem model = List.nub $
    [ diagnostic (Just (getLoc fp d)) (nameOf d) problem
    | (d, fp) <- decls, problem <- problems env d
    ]
    ++ [ diagnostic (locationOf n) n problem | (n, problem) <- eventProblems (envFunctions env) sem ]
    ++ [ diagnostic (Just (getLoc fp d)) (nameOf d) problem | (d, fp) <- decls, problem <- integerProblems d ]
    ++ [ diagnostic (Just (getLoc fp d)) (nameOf d) problem | (d, fp) <- decls, problem <- sizeT fp d ++ writtenSizes fp d ++ prefixes fp d ++ nullability env fp d ]
  where
    decls =
        [ (d, T.moduleFile m)
        | m <- T.modelMods model, d <- flatten (T.moduleDecls m)
        , nameOf d `Set.notMember` oldGroupCalls
        ]
    integerProblems = integerTypes decls
    env = Env
        { envFunctions = Set.fromList [ name n | (T.Function _ n _ _ _, _) <- decls ]
        , envHandles   = Set.fromList [ name n | (T.TypeDecl n, _) <- decls ]
        , envIds       = Set.fromList (map S.idCName (S.idTypes sem))
        , envEnums     = Map.fromList [ (name n, [ name v | T.EnumMember v _ <- vs ]) | (T.Enumeration _ n vs, _) <- decls ]
        , envParams    = Map.fromList [ (name n, ps) | (T.Function _ n ps _ _, _) <- decls ]
        , envArrays    = Set.fromList [ name n | (T.ArrayTypeDecl n _, _) <- decls ]
        , envUnlisted  = Set.fromList [ S.enumName e | e <- S.enums sem, S.enumKind e `elem` [S.OpenEnum, S.FlagsEnum] ]
        , envAnnotated = Set.fromList [ fp | (d, fp) <- decls, not (null (nullables d)) ]
        , envTakers    = Map.fromListWith (+)
            [ (cb, 1) | (T.Function _ _ ps _ _, _) <- decls, T.Var ty _ <- ps, Just cb <- [callbackName ty] ]
        }

    flatten = concatMap $ \d -> d : case d of
        T.Namespace _ ms -> flatten ms
        T.ClassDecl _ ms -> flatten ms
        _                -> []

    nameOf d = maybe "" name (declName d)
    locationOf n = listToMaybe [ getLoc fp d | (d, fp) <- decls, nameOf d == n ]
    diagnostic loc n problem = S.SDiagnostic S.Error loc (n <> ": " <> problem)

declName :: Decl -> Maybe (Lexeme Name)
declName = \case
    T.Function _ n _ _ _     -> Just n
    T.CallbackTypeDecl _ n _ _ -> Just n
    T.Enumeration _ n _      -> Just n
    T.Define n _             -> Just n
    _                        -> Nothing

name :: Lexeme Name -> Text
name (L _ _ n) = flattenName n

problems :: Env -> Decl -> [Text]
problems env = \case
    T.Enumeration _ n members ->
        [ p | isErrorEnum (name n), p <- errorEnum members ]
        ++ valueNames (name n) members
        ++ toString env (name n)
    T.Function ret n params _ _ ->
        errorParam params
        ++ toStringParam (envEnums env) (name n) params
        ++ handleFirst (envHandles env) params
        ++ accessor (envHandles env) (name n) params
        ++ registration (name n) params
        ++ lengths params
        ++ outputs env (name n) params
        ++ idParams (envIds env) (name n) params
        ++ notFound env (name n) params
        ++ reason env (name n) ret params
        ++ sizeGetter (envFunctions env) (name n) ret
        ++ constructor (envFunctions env) (name n) ret
    T.CallbackTypeDecl _ n params _ ->
        callbackType params
        ++ lengths params
        ++ idParams (envIds env) (name n) params
        ++ takers (Map.findWithDefault 0 (name n) (envTakers env))
    T.Define n _ -> constant (envFunctions env) (name n)
    _ -> []

-- | The old ToxAV group calls, to be removed (see toxav.h). The rules don't
-- look at them.
oldGroupCalls :: Set Text
oldGroupCalls = Set.fromList
    [ "toxav_add_av_groupchat"
    , "toxav_audio_data_cb"
    , "toxav_group_send_audio"
    , "toxav_groupchat_av_enabled"
    , "toxav_groupchat_disable_av"
    , "toxav_groupchat_enable_av"
    , "toxav_join_av_groupchat"
    ]

isErrorEnum :: Text -> Bool
isErrorEnum = ("_Err_" `Text.isInfixOf`)

-- | An error enum's first value is @..._OK@, which is 0.
errorEnum :: [Decl] -> [Text]
errorEnum = \case
    T.EnumMember first value : _
        | "_OK" `Text.isSuffixOf` name first && maybe True isZero value -> []
    _ -> ["an error enum's first value must be ..._OK, which is 0"]
  where
    isZero (T.IntVal v) = name v == "0"
    isZero _            = False

-- | An enum's values start with its whole name, so each says which type it
-- belongs to: @TOX_MESSAGE_TYPE_NORMAL@ in @Tox_Message_Type@. tokstyle lets
-- internal enums drop a @_Type@, but the public ones keep it.
valueNames :: Text -> [Decl] -> [Text]
valueNames n members =
    [ "its value " <> name v <> " must start with " <> prefix
    | T.EnumMember v _ <- members
    , not (prefix `Text.isPrefixOf` name v)
    ]
  where
    prefix = Text.toUpper n <> "_"

-- | Each enum has @..._to_string@, so every binding can show its values the same
-- way. Not one whose values aren't all in its list, which it would call
-- invalid: clients make up their own @Tox_File_Kind@s, and flags combine.
toString :: Env -> Text -> [Text]
toString env n =
    [ "an enum needs its " <> f
    | f `Set.notMember` envFunctions env
    , n `Set.notMember` envUnlisted env
    ]
  where
    f = Text.toLower n <> "_to_string"

-- | @tox_connection_to_string@ takes only a @Tox_Connection value@.
toStringParam :: Map.Map Text [Text] -> Text -> [Decl] -> [Text]
toStringParam enums n params =
    [ "it must take only " <> e <> " value"
    | Just stem <- [Text.stripSuffix "_to_string" n]
    , e <- filter ((== stem) . Text.toLower) (Map.keys enums)
    , not (takesValue e)
    ]
  where
    takesValue e = case params of
        [T.Var ty p] | T.Typename t <- stripNull ty -> name t == e && name p == "value"
        _                                           -> False

-- | The error is the last parameter, and is called @error@.
errorParam :: [Decl] -> [Text]
errorParam params =
    [ "the error must be the last parameter, called error"
    | (i, T.Var ty n) <- zip [1 :: Int ..] params
    , Just t <- [pointee ty], isErrorEnum t
    , i /= length params || name n /= "error"
    ]

-- | A function taking a handle takes it first: @tox_self_get_name(tox, ...)@.
handleFirst :: Set Text -> [Decl] -> [Text]
handleFirst handles params = case params of
    first : rest | not (isHandle first), any isHandle rest -> ["a handle must be the first parameter"]
    _ -> []
  where
    isHandle (T.Var ty _) = maybe False (`Set.member` handles) (pointee ty)
    isHandle _            = False

-- | A getter reads its handle, so takes it @const@. A setter doesn't.
accessor :: Set Text -> Text -> [Decl] -> [Text]
accessor handles n = \case
    T.Var ty _ : _ | Just h <- pointee ty, h `Set.member` handles ->
        [ "a getter must take a const " <> h | "_get_" `Text.isInfixOf` n, not (isConst ty) ]
        ++ [ "a setter must take a non-const " <> h | "_set_" `Text.isInfixOf` n, isConst ty ]
    _ -> []
  where
    isConst ty = case stripNull ty of
        T.ConstPointerType _ -> True
        _                    -> False

-- | @tox_callback_friend_message@ takes a @tox_friend_message_cb *callback@.
registration :: Text -> [Decl] -> [Text]
registration n params = case Text.breakOn "_callback_" n of
    (prefix, rest) | not (Text.null prefix), Just event <- Text.stripPrefix "_callback_" rest ->
        let cb = prefix <> "_" <> event <> "_cb"
        in [ "a registration must take a " <> cb <> " *callback" | not (any (takes cb) params) ]
    _ -> []
  where
    takes cb (T.Var ty p) = callbackName ty == Just cb && name p == "callback"
    takes _ _             = False

-- | The callback type a parameter takes: @tox_friend_message_cb@ for
-- @tox_friend_message_cb *callback@.
callbackName :: Decl -> Maybe Text
callbackName ty = case stripNull ty of
    T.CallbackType t                                 -> Just (name t)
    T.PointerType t | "_cb" `Text.isSuffixOf` name t -> Just (name t)
    _                                                -> Nothing

-- | One function takes each callback type: the one setting it.
takers :: Int -> [Text]
takers n = [ "one function must take a callback type, not " <> Text.pack (show n) | n /= 1 ]

-- | An array of no fixed size that the function reads is followed by its
-- length: @const uint8_t message[], size_t length@ (or @message_len@).
lengths :: [Decl] -> [Text]
lengths params =
    [ "the array " <> name a <> " must be followed by its length"
    | (T.Var ty a, next) <- zip params (map Just (drop 1 params) ++ [Nothing])
    , isInputArray ty
    , not (maybe False isLength next)
    ]
  where
    isInputArray ty = case stripNull ty of
        T.ConstArrayType _ -> True
        _                  -> False

-- | An array of no fixed size that the function writes has its size known
-- before the call: from a length after it, or from @tox_self_get_name_size@
-- for @tox_self_get_name@ (@tox_events_bytes_size@ for @tox_events_get_bytes@).
--
-- The size function takes what the function takes but the array, its size and
-- the error, so a binding can call one and then the other with the same
-- arguments.
outputs :: Env -> Text -> [Decl] -> [Text]
outputs env n params = concat
    [ case filter (`Map.member` envParams env) sizeFunctions of
        [] -> [ "the array " <> name a <> " needs its size: a length after it, or " <> n <> "_size"
              | not sized ]
        s : _ ->
            [ "its parameters but " <> name a <> " must be " <> s <> "'s"
            | Just ps <- [Map.lookup s (envParams env)]
            , arguments ps /= arguments [ p | p <- params, not (isArray a p), not (sized && Just p == next) ]
            ]
    | (T.Var ty a, next) <- zip params (map Just (drop 1 params) ++ [Nothing])
    , isOutputArray ty
    , let sized = maybe False isLength next
    ]
  where
    isArray a = \case
        T.Var _ p -> name p == name a
        _         -> False
    arguments ps = [ (fmap name (stripNull ty), name p) | T.Var ty p <- ps, not (isErrorVar ty) ]
    isErrorVar = maybe False isErrorEnum . pointee
    sizeFunctions = (n <> "_size") : [ Text.replace "_get_" "_" n <> "_size" | "_get_" `Text.isInfixOf` n ]

-- | An array of no fixed size that a function writes.
isOutputArray :: Decl -> Bool
isOutputArray ty = case stripNull ty of
    T.ArrayType _ -> True
    _             -> False

-- | The headers from before the rules, which keep what the rules don't allow in
-- new ones until their ABI can change.
oldHeaders :: [FilePath]
oldHeaders = ["tox.h", "tox_options.h", "toxav.h", "toxencryptsave.h"]

-- | In a new header, an array a function writes is followed by its size, so the
-- function can't write past its end. A size from @..._size@ can be stale by the
-- time the function runs, e.g. when the client doesn't lock around Tox.
writtenSizes :: FilePath -> Decl -> [Text]
writtenSizes file = \case
    T.Function _ _ params _ _ | takeFileName file `notElem` oldHeaders ->
        [ "the array " <> name a <> " it writes must be followed by its size"
        | (T.Var ty a, next) <- zip params (map Just (drop 1 params) ++ [Nothing])
        , isOutputArray ty
        , not (maybe False isLength next)
        ]
    _ -> []

-- | The length of the array before it: @message[], size_t length@ (or
-- @message_len@).
isLength :: Decl -> Bool
isLength = \case
    T.Var ty n -> isInteger ty && any (`Text.isSuffixOf` name n) ["length", "size", "_len"]
    _          -> False
  where
    isInteger ty = case stripNull ty of
        T.BuiltinType (UInt _) -> True
        T.BuiltinType (SInt _) -> True
        T.BuiltinType SizeT    -> True
        _                      -> False

-- | @X_get_Y_size@ gives the size of what @X_get_Y@ writes.
sizeGetter :: Set Text -> Text -> Decl -> [Text]
sizeGetter functions n ret = case Text.stripSuffix "_size" n of
    Just getter | "_get_" `Text.isInfixOf` getter, isSizeT ret, getter `Set.notMember` functions ->
        ["a size getter needs its getter, " <> getter]
    _ -> []
  where
    isSizeT t = case stripNull t of
        T.BuiltinType SizeT -> True
        _                   -> False

-- | A handle made by @X_new@ is freed by @X_kill@ or @X_free@.
constructor :: Set Text -> Text -> Decl -> [Text]
constructor functions n ret = case (Text.stripSuffix "_new" n, pointee ret) of
    (Just prefix, Just _) | not (any (`Set.member` functions) [prefix <> "_kill", prefix <> "_free"]) ->
        ["a constructor needs its destructor, " <> prefix <> "_kill or " <> prefix <> "_free"]
    _ -> []

-- | A callback's last parameter is the user data. (Its name ends in @_cb@, or
-- cimple wouldn't parse it as a function type.)
callbackType :: [Decl] -> [Text]
callbackType params =
    [ "a callback's last parameter must be its user data, void *user_data" | not (lastIsUserData params) ]
  where
    lastIsUserData ps = case reverse ps of
        T.Var ty n : _ | T.BuiltinType VoidPtr <- stripNull ty -> name n == "user_data"
        _                                                      -> False

-- | An id parameter is named after its type, perhaps with a word in front:
-- @Tox_Friend_Number friend_number@, @Tox_Group_Peer_Number source_peer_number@.
idParams :: Set Text -> Text -> [Decl] -> [Text]
idParams ids fn params =
    [ "a " <> t <> " parameter's name must end in " <> want
    | T.Var ty n <- params
    , T.Typename tn <- [stripNull ty]
    , let t = name tn
    , t `Set.member` ids
    , let want = Text.toLower (Text.intercalate "_" (lastTwo (Text.splitOn "_" t)))
    , not (want `Text.isSuffixOf` name n)
    , (fn, name n) `notElem` exceptions
    ]
  where
    lastTwo ws = drop (length ws - 2) ws

    -- Named like their events' fields, whose getters are in the API. They
    -- have a TODO in tox.h.
    exceptions =
        [ (cb, "peer_id")
        | cb <- [ "tox_group_peer_name_cb", "tox_group_peer_status_cb", "tox_group_topic_cb"
                , "tox_group_message_cb", "tox_group_private_message_cb", "tox_group_custom_packet_cb"
                , "tox_group_custom_private_packet_cb", "tox_group_peer_join_cb", "tox_group_peer_exit_cb" ]
        ]
        ++ [ ("tox_group_moderation_cb", "source_peer_id"), ("tox_group_moderation_cb", "target_peer_id") ]

-- | A parameter name has one integer type in all the headers: a peer limit
-- that is 16 bits in one function is 16 bits in all of them. A @size_t@ counts
-- as the @uint32_t@ it becomes in new headers.
integerTypes :: [(Decl, FilePath)] -> Decl -> [Text]
integerTypes decls = \d ->
    [ "its " <> p <> " is " <> have <> ", but " <> want <> " in " <> example
    | (p, have) <- integerParams d
    , Just uses <- [Map.lookup p usesOf]
    , let (want, example) = mostCommon uses
    , sameAs have /= sameAs want
    ]
  where
    -- For each name, its types, each with the functions using it.
    usesOf = Map.fromListWith (Map.unionWith (flip (++)))
        [ (p, Map.singleton (sameAs t) [(t, maybe "" name (declName decl))])
        | (decl, _) <- decls, (p, t) <- integerParams decl
        ]
    mostCommon uses = case List.sortOn (Down . length . snd) (Map.toList uses) of
        (_, (t, f) : _) : _ -> (t, f)
        _                   -> ("", "")
    sameAs t = if t == "size_t" then "uint32_t" else t

-- | The parameters of integer type, and their C type, but for the ones that
-- are wrong and can't change yet.
integerParams :: Decl -> [(Text, Text)]
integerParams d =
    [ (name p, t)
    | T.Var ty p <- params
    , Just t <- [integer ty]
    , (fn, name p) `notElem` exceptions
    ]
  where
    fn = maybe "" name (declName d)
    params = case d of
        T.Function _ _ ps _ _     -> ps
        T.CallbackTypeDecl _ _ ps _ -> ps
        _                         -> []

    integer ty = case stripNull ty of
        T.BuiltinType (UInt b) -> Just ("uint" <> bits b <> "_t")
        T.BuiltinType (SInt b) -> Just ("int" <> bits b <> "_t")
        T.BuiltinType SizeT    -> Just "size_t"
        _                      -> Nothing
    bits = \case
        B8  -> "8"
        B16 -> "16"
        B32 -> "32"
        B64 -> "64"

    -- In the ABI until 0.3.0. Each has a TODO in its header.
    exceptions =
        [ ("tox_group_peer_limit_cb", "peer_limit")
        ]

-- | A function taking an id can say it doesn't exist: ids go stale when a
-- friend is deleted or a peer leaves. Its error has @..._FRIEND_NOT_FOUND@ for a
-- friend number, or @..._NOT_FOUND@ if the error is about files and the id is a
-- file number.
notFound :: Env -> Text -> [Decl] -> [Text]
notFound env n params
    | "_exists" `Text.isSuffixOf` n = []
    | otherwise =
        [ problem
        | T.Var ty p <- params
        , T.Typename t <- [stripNull ty]
        , name t `Set.member` envIds env
        , (n, name p) `notElem` exceptions
        , problem <- case errorOf of
            Nothing -> [ "it takes " <> name p <> ", so needs an error to say it doesn't exist" ]
            Just e  -> [ e <> " must say " <> name p <> " doesn't exist" | not (canSay e (name t)) ]
        ]
  where
    errorOf = listToMaybe [ e | T.Var ty p <- params, name p == "error", Just e <- [pointee ty], isErrorEnum e ]

    canSay e t = any (says e t) (Map.findWithDefault [] e (envEnums env))
    says e t member = case Text.stripPrefix (Text.toUpper e <> "_") member of
        Just "NOT_FOUND" -> any (`elem` Text.splitOn "_" e) (idWords t)
        Just v           -> any (\w -> (Text.toUpper w <> "_NOT_FOUND") `Text.isSuffixOf` v) (take 1 (reverse (idWords t)))
        Nothing          -> False
    -- @Tox_Conference_Peer_Number@: Conference, Peer.
    idWords = drop 1 . reverse . drop 1 . reverse . Text.splitOn "_"

    -- The old conference API, in the ABI until 0.3.0. Each has a TODO in its
    -- header.
    exceptions =
        [ ("tox_conference_get_id", "conference_number")
        , ("tox_conference_invite", "friend_number")
        ]

-- | A function writing a buffer says why it failed: a bool alone can't. One
-- that can't fail returns nothing.
reason :: Env -> Text -> Decl -> [Decl] -> [Text]
reason env n ret params =
    [ "it returns bool and writes " <> name a <> ", so needs an error to say why it failed"
    | isBool ret
    , not (any isError params)
    , T.Var _ a : _ <- [filter writes params]
    , n `notElem` exceptions
    ]
  where
    isBool t = case stripNull t of
        T.BuiltinType Bool -> True
        _                  -> False
    isError = \case
        T.Var ty _ -> maybe False isErrorEnum (pointee ty)
        _          -> False
    writes = \case
        T.Var ty _ -> case dropNullable ty of
            T.ArrayType _                       -> True
            T.SizedArrayType (T.ConstType _) _ -> False
            T.SizedArrayType _ _                -> True
            T.Typename t                        -> name t `Set.member` envArrays env
            _                                   -> False
        _ -> False
    dropNullable = \case
        T.NullableType t -> dropNullable t
        t                -> t

    -- In the ABI until 0.3.0. Each has a TODO in its header.
    exceptions =
        [ "tox_conference_get_id"
        , "tox_hash"
        ]

-- | A header's names start with its own, so the name says where to look:
-- @tox_options_new@ and @TOX_OPTIONS_*@ in @tox_options.h@, and its errors
-- @Tox_Err_Options_*@. A @_private.h@ header adds to its root's names.
-- Types aren't checked: a header may declare one from another header again, and
-- the functions taking it are checked.
prefixes :: FilePath -> Decl -> [Text]
prefixes file d =
    [ what <> " must start with " <> own <> ", like the rest of " <> Text.pack (takeFileName file)
    | header `notElem` ["toxencryptsave"]
    , (what, n) <- names
    , not (any (`starts` Text.toLower n) allowed)
    ]
  where
    header = Text.pack (dropExtension (takeFileName file))
    own = fromMaybe header (Text.stripSuffix "_private" header)
    allowed = own : errors ++ Map.findWithDefault [] header exceptions
    errors = case Text.breakOn "_" own of
        (root, rest) | not (Text.null rest) -> [root <> "_err" <> rest]
        _                                   -> []
    starts p n = n == p || (p <> "_") `Text.isPrefixOf` n

    names = case d of
        T.Function _ n _ _ _     -> [("its name", name n)]
        T.CallbackTypeDecl _ n _ _ -> [("its name", name n)]
        T.Enumeration _ n vs     -> ("its name", name n) : [ ("its value " <> name v, name v) | T.EnumMember v _ <- vs ]
        T.Define n _             -> [("its name", name n)]
        _                        -> []

    -- In the ABI until 0.3.0. Each has a TODO in its header. toxencryptsave.h
    -- has no prefix of its own (tox_pass_*, tox_get_salt), so isn't checked.
    exceptions = Map.fromList
        [ ("tox_options", ["tox_proxy_type", "tox_savedata_type", "tox_log_cb"])
        ]

-- | A callback's buffer followed by its length is @_Nullable@: the events
-- pass NULL for an empty one. Only headers with annotations are checked: the
-- parser can't tell @_Nonnull@ from none.
nullability :: Env -> FilePath -> Decl -> [Text]
nullability env file = \case
    T.CallbackTypeDecl _ n params nulls | file `Set.member` envAnnotated env ->
        [ "its " <> name p <> " is _Nonnull, but the events pass NULL when its length is 0"
        | (T.Var ty p, next) <- zip params (drop 1 params)
        , isBuffer ty, isLength next
        , name p `notElem` map name nulls
        , (name n, name p) `notElem` neverEmpty
        ]
    _ -> []
  where
    isBuffer ty = case stripNull ty of
        T.ArrayType _        -> True
        T.ConstArrayType _   -> True
        T.PointerType _      -> True
        T.ConstPointerType _ -> True
        _                    -> False

    -- Buffers whose length is never 0, so the events never pass NULL.
    neverEmpty =
        [ ("tox_conference_invite_cb", "cookie")  -- the invite packet has a fixed size
        ]

-- | The parameters marked @_Nullable@.
nullables :: Decl -> [Lexeme Name]
nullables = \case
    T.Function _ _ _ _ ns       -> ns
    T.CallbackTypeDecl _ _ _ ns -> ns
    _                           -> []

-- | @size_t@ is as big as the system makes it, so new headers use @uint32_t@.
sizeT :: FilePath -> Decl -> [Text]
sizeT file d =
    [ "size_t depends on the system: use uint32_t"
    | takeFileName file `notElem` oldHeaders
    , any isSizeT types
    ]
  where
    types = case d of
        T.Function ret _ ps _ _     -> ret : [ ty | T.Var ty _ <- ps ]
        T.CallbackTypeDecl ret _ ps _ -> ret : [ ty | T.Var ty _ <- ps ]
        _                           -> []
    isSizeT ty = case stripNull ty of
        T.BuiltinType SizeT -> True
        _                   -> False

-- | Bindings that can't read macros call @tox_public_key_size()@ for
-- @TOX_PUBLIC_KEY_SIZE@.
constant :: Set Text -> Text -> [Text]
constant functions n =
    [ "a constant needs its function, " <> Text.toLower n
    | Text.toLower n `Set.notMember` functions
    ]

-- | Each event has its callback, whose parameters are the event's fields: the
-- bindings build one from the other. A set of registrations, like
-- @tox_callback_*@ or @tox_dispatch_callback_*@, has one for every event whose
-- callback is in the headers checked.
eventProblems :: Set Text -> S.SemanticModel -> [(Text, Text)]
eventProblems functions sem = maybe [] (\v -> concatMap member (S.variantMembers v) ++ eventless) (K.eventsVariant sem)
  where
    -- @tox_callback_friend_message@ needs the event @friend_message@, or the
    -- events API doesn't have it.
    eventless =
        [ (f, "a callback needs its event, " <> e)
        | f <- Set.toList functions
        , Just e <- [Text.stripPrefix (rootNamespace sem <> "_callback_") f]
        , e `notElem` events
        ]

    events = maybe [] (map (Text.toLower . S.memberName) . S.variantMembers) (K.eventsVariant sem)
    registrations = List.nub
        [ prefix
        | f <- Set.toList functions, e <- events
        , Just prefix <- [Text.stripSuffix ("_callback_" <> e) f]
        ]

    member m = registered ++ case K.rootEvent sem event of
        Just e
            | fields /= map S.paramName (S.eventParams e) ->
                [ (S.cCallback e, "its parameters must be the event's fields, " <> Text.intercalate ", " fields) ]
            | otherwise -> []
        Nothing
            | cb `notElem` callbacks -> [ (S.memberGetter m, "an event needs its callback, " <> cb) ]
            | otherwise -> []
      where
        event = Text.toLower (S.memberName m)
        cb = rootNamespace sem <> "_" <> event <> "_cb"
        registered =
            [ (S.memberGetter m, "an event needs its registration, " <> r)
            | cb `elem` map S.cbCName (S.callbacks sem)
            , prefix <- registrations
            , let r = prefix <> "_callback_" <> event
            , r `Set.notMember` functions
            ]
        fields = maybe [] eventFields (List.find ((== S.memberType m) . S.resourceName) (S.resources sem))

    callbacks = map S.cbCName (S.callbacks sem ++ S.contextCallbacks (S.context sem))

    eventFields r =
        [ f
        | meth <- S.methods r
        , let c = S.cFunctionName (mappingOf sem r meth)
        , c `notElem` sizeCompanions sem
        , Just f <- [Text.stripPrefix "get_" =<< Text.stripPrefix (S.cPrefix r) (shownName sem r meth)]
        ]

-- | The type a pointer points to: @Tox_Err_Bootstrap@ for @Tox_Err_Bootstrap *@.
pointee :: Decl -> Maybe Text
pointee ty = case stripNull ty of
    T.PointerType t      -> Just (name t)
    T.ConstPointerType t -> Just (name t)
    _                    -> Nothing

stripNull :: Decl -> Decl
stripNull = \case
    T.NullableType t -> stripNull t
    T.ConstType t    -> stripNull t
    t                -> t
