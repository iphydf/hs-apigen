{-# LANGUAGE OverloadedStrings #-}
module Apigen.CheckSpec (spec) where

import           Test.Hspec                      (Spec, describe, it, shouldBe)

import           Apigen.Check                    (check)
import           Apigen.Parser                   (parseModel)
import qualified Apigen.Parser.Semantic          as Semantic
import qualified Apigen.Semantic                 as S
import qualified Data.ByteString                 as BS
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Language.Cimple.IO              (parseText)
import           Language.Cimple.TranslationUnit (TranslationUnit)

-- From data = ["//c-toxcore:public_headers"]: the headers the bindings read,
-- and tox_dispatch.h.
headers :: [FilePath]
headers =
    [ "c-toxcore/tox/tox.h"
    , "c-toxcore/tox/tox_attributes.h"
    , "c-toxcore/tox/tox_dispatch.h"
    , "c-toxcore/tox/tox_event.h"
    , "c-toxcore/tox/tox_events.h"
    , "c-toxcore/tox/tox_log_level.h"
    , "c-toxcore/tox/tox_options.h"
    , "c-toxcore/tox/toxav.h"
    , "c-toxcore/tox/toxencryptsave.h"
    ]

-- | Whatever the locale: the headers are UTF-8.
readUtf8 :: FilePath -> IO Text
readUtf8 = fmap Text.decodeUtf8 . BS.readFile

mustParse :: FilePath -> Text -> IO (TranslationUnit Text)
mustParse file code = either fail (\ok -> return (file, ok)) (parseText code)

-- | The problems in some headers, looking names up in the context headers
-- too, as @--context@ does.
problemsIn :: [(FilePath, Text)] -> [(FilePath, Text)] -> IO [Text]
problemsIn files context = do
    units <- mapM (uncurry mustParse) files
    contextUnits <- mapM (uncurry mustParse) context
    let model = parseModel units
        sem = Semantic.toSemantic False model
        withContext
            | null context = id
            | otherwise = Semantic.withContext (Semantic.toSemantic False (parseModel (units ++ contextUnits)))
    return $ map S.message (check (withContext sem) model)

problems :: [(FilePath, Text)] -> IO [Text]
problems files = problemsIn files []

-- | The public headers, and @tox_private.h@ for the callbacks of private events.
publicHeaders :: IO ([(FilePath, Text)], [(FilePath, Text)])
publicHeaders = do
    files <- mapM (\f -> (,) f <$> readUtf8 f) headers
    private <- readUtf8 "c-toxcore/tox/tox_private.h"
    return (files, [("c-toxcore/tox/tox_private.h", private)])

-- | A header without its nullability annotations, which the rules don't look
-- at, so the snippets below are the same on every c-toxcore branch.
withoutNullability :: Text -> Text
withoutNullability t = List.foldl' (\acc a -> Text.replace a "" acc) t ["_Nonnull ", "_Nullable ", "_Nonnull", "_Nullable"]

-- | The problems in @tox.h@ with one of its snippets replaced.
problemsWith :: Text -> Text -> IO [Text]
problemsWith old new = do
    tox <- withoutNullability <$> readUtf8 "c-toxcore/tox/tox.h"
    if old `Text.isInfixOf` tox
        then problems [("tox.h", Text.replace old new tox)]
        else fail ("tox.h no longer has: " <> Text.unpack old)

-- | The problems in @tox.h@ as it is, annotations and all, with one of its
-- snippets replaced.
problemsWithAnnotated :: Text -> Text -> IO [Text]
problemsWithAnnotated old new = do
    tox <- readUtf8 "c-toxcore/tox/tox.h"
    if old `Text.isInfixOf` tox
        then problems [("tox.h", Text.replace old new tox)]
        else fail ("tox.h no longer has: " <> Text.unpack old)

-- | The problems in all the public headers, with a snippet of one replaced.
problemsWithAll :: FilePath -> Text -> Text -> IO [Text]
problemsWithAll file old new = do
    (files, context) <- publicHeaders
    let plain = [ (f, withoutNullability t) | (f, t) <- files ]
        replaced = [ (f, if f == "c-toxcore/tox/" <> file then Text.replace old new t else t) | (f, t) <- plain ]
    if replaced == plain
        then fail (file <> " no longer has: " <> Text.unpack old)
        else problemsIn replaced context

-- | @tox_bootstrap@'s declaration, with its error parameter.
bootstrap :: Text -> Text
bootstrap err =
    "bool tox_bootstrap(Tox *tox, const char *host, Tox_Port port, const Tox_Dht_Id dht_id, "
    <> err <> ");"

-- | @tox_self_set_name@'s declaration, with its first parameters.
setName :: Text -> Text
setName params = "bool tox_self_set_name(" <> params <> ", Tox_Err_Set_Info *error);"

spec :: Spec
spec = describe "check" $ do
    it "finds no problems in the public headers" $ do
        (files, context) <- publicHeaders
        problemsIn files context >>= (`shouldBe` [])

    it "finds no problems in tox.h on its own" $ do
        tox <- readUtf8 "c-toxcore/tox/tox.h"
        problems [("tox.h", tox)] >>= (`shouldBe` [])

    it "finds an error enum not starting with OK" $
        problemsWith "typedef enum Tox_Err_Bootstrap {\n"
            "typedef enum Tox_Err_Bootstrap {\n    TOX_ERR_BOOTSTRAP_FIRST,\n"
            >>= (`shouldBe` ["Tox_Err_Bootstrap: an error enum's first value must be ..._OK, which is 0"])

    it "finds an enum value not starting with its enum's name" $
        problemsWith "    TOX_CONNECTION_UDP,\n" "    TOX_UDP,\n"
            >>= (`shouldBe` ["Tox_Connection: its value TOX_UDP must start with TOX_CONNECTION_"])

    it "finds an enum without its to_string" $
        problemsWith "const char *tox_connection_to_string(Tox_Connection value);" ""
            >>= (`shouldBe` ["Tox_Connection: an enum needs its tox_connection_to_string"])

    it "finds a to_string not taking a value" $
        problemsWith "const char *tox_connection_to_string(Tox_Connection value);"
            "const char *tox_connection_to_string(Tox_Connection connection);"
            >>= (`shouldBe` ["tox_connection_to_string: it must take only Tox_Connection value"])

    it "finds a name not starting with its header's" $
        problemsWithAll "tox_options.h" "bool tox_options_get_ipv6_enabled(" "bool tox_get_ipv6_enabled("
            >>= (`shouldBe` ["tox_get_ipv6_enabled: its name must start with tox_options, like the rest of tox_options.h"])

    it "finds an enum value not starting with its header's" $
        problemsWithAll "tox_events.h" "    TOX_ERR_EVENTS_GET_BYTES_TOO_SMALL,\n" "    TOX_ERR_EVENTS_GET_BYTES_TOO_SMALL,\n    TOX_ERR_GET_BYTES_TOO_LONG,\n"
            >>= (`shouldBe`
                [ "Tox_Err_Events_Get_Bytes: its value TOX_ERR_GET_BYTES_TOO_LONG must start with TOX_ERR_EVENTS_GET_BYTES_"
                , "Tox_Err_Events_Get_Bytes: its value TOX_ERR_GET_BYTES_TOO_LONG must start with tox_events, like the rest of tox_events.h"
                ])

    it "finds a callback buffer with a length that isn't _Nullable" $
        problemsWithAnnotated "Tox_Friend_Number friend_number,\n    const uint8_t name[_Nullable], size_t length, void *_Nullable user_data);"
            "Tox_Friend_Number friend_number,\n    const uint8_t name[_Nonnull], size_t length, void *_Nullable user_data);"
            >>= (`shouldBe` ["tox_friend_name_cb: its name is _Nonnull, but the events pass NULL when its length is 0"])

    it "finds an error parameter not called error" $
        problemsWith (bootstrap "Tox_Err_Bootstrap *error")
            (bootstrap "Tox_Err_Bootstrap *err")
            >>= (`shouldBe` ["tox_bootstrap: the error must be the last parameter, called error"])

    it "finds an error parameter that isn't last" $
        problemsWith (bootstrap "Tox_Err_Bootstrap *error")
            "bool tox_bootstrap(Tox *tox, Tox_Err_Bootstrap *error, const char *host, Tox_Port port, const Tox_Dht_Id dht_id);"
            >>= (`shouldBe` ["tox_bootstrap: the error must be the last parameter, called error"])

    it "finds a callback without its user data" $
        problemsWith "const uint8_t message[], size_t length, void *user_data);\n\n/**\n * @brief Set the callback for the `friend_message` event."
            "const uint8_t message[], size_t length);\n\n/**\n * @brief Set the callback for the `friend_message` event."
            >>= (`shouldBe` ["tox_friend_message_cb: a callback's last parameter must be its user data, void *user_data"])

    it "finds user data not called user_data" $
        problemsWith "const uint8_t message[], size_t length, void *user_data);\n\n/**\n * @brief Set the callback for the `friend_message` event."
            "const uint8_t message[], size_t length, void *userdata);\n\n/**\n * @brief Set the callback for the `friend_message` event."
            >>= (`shouldBe` ["tox_friend_message_cb: a callback's last parameter must be its user data, void *user_data"])

    it "finds an id parameter not named after its type" $
        problemsWith "bool tox_friend_delete(Tox *tox, Tox_Friend_Number friend_number, Tox_Err_Friend_Delete *error);"
            "bool tox_friend_delete(Tox *tox, Tox_Friend_Number friend_id, Tox_Err_Friend_Delete *error);"
            >>= (`shouldBe` ["tox_friend_delete: a Tox_Friend_Number parameter's name must end in friend_number"])

    it "finds a size constant without its function" $
        problemsWith "uint32_t tox_public_key_size(void);" "uint32_t tox_public_key_length(void);"
            >>= (`shouldBe` ["TOX_PUBLIC_KEY_SIZE: a constant needs its function, tox_public_key_size"])

    it "finds any other constant without its function" $
        problemsWith "uint32_t tox_version_major(void);" "uint32_t tox_major_version(void);"
            >>= (`shouldBe` ["TOX_VERSION_MAJOR: a constant needs its function, tox_version_major"])

    it "finds a handle that isn't the first parameter" $
        problemsWith "void tox_self_get_name(const Tox *tox, uint8_t name[]);"
            "void tox_self_get_name(uint8_t name[], const Tox *tox);"
            >>= (`shouldBe` ["tox_self_get_name: a handle must be the first parameter"])

    it "finds a getter taking a handle it could change" $
        problemsWith "uint32_t tox_self_get_nospam(const Tox *tox);" "uint32_t tox_self_get_nospam(Tox *tox);"
            >>= (`shouldBe` ["tox_self_get_nospam: a getter must take a const Tox"])

    it "finds a setter taking a const handle" $
        problemsWith (setName "Tox *tox, const uint8_t name[], size_t length")
            (setName "const Tox *tox, const uint8_t name[], size_t length")
            >>= (`shouldBe` ["tox_self_set_name: a setter must take a non-const Tox"])

    it "finds an array without its length" $
        problemsWith (setName "Tox *tox, const uint8_t name[], size_t length")
            (setName "Tox *tox, size_t length, const uint8_t name[]")
            >>= (`shouldBe` ["tox_self_set_name: the array name must be followed by its length"])

    it "finds a parameter whose integer type differs from the others of its name" $
        problemsWith (setName "Tox *tox, const uint8_t name[], size_t length")
            (setName "Tox *tox, const uint8_t name[], uint16_t length")
            >>= (`shouldBe` ["tox_self_set_name: its length is uint16_t, but size_t in tox_self_set_status_message"])

    it "finds an id its function can't say doesn't exist" $
        problemsWith "TOX_ERR_FRIEND_DELETE_FRIEND_NOT_FOUND," "TOX_ERR_FRIEND_DELETE_FRIEND_GONE,"
            >>= (`shouldBe` ["tox_friend_delete: Tox_Err_Friend_Delete must say friend_number doesn't exist"])

    it "finds an id taken without an error" $
        problemsWith "bool tox_friend_exists(" "bool tox_friend_is_there("
            >>= (`shouldBe` ["tox_friend_is_there: it takes friend_number, so needs an error to say it doesn't exist"])

    it "finds a callback without its event" $
        problemsWithAll "tox.h" "void tox_callback_friend_message("
            ("typedef void tox_friend_poke_cb(Tox *tox, Tox_Friend_Number friend_number, void *user_data);\n"
                <> "void tox_callback_friend_poke(Tox *tox, tox_friend_poke_cb *callback);\n"
                <> "void tox_callback_friend_message(")
            >>= (`shouldBe` ["tox_callback_friend_poke: a callback needs its event, friend_poke"])

    it "finds a callback type taken by two functions" $
        problemsWith "void tox_callback_friend_message(Tox *tox, tox_friend_message_cb *callback);"
            ("void tox_callback_friend_message(Tox *tox, tox_friend_message_cb *callback);\n"
                <> "void tox_set_friend_message_handler(Tox *tox, tox_friend_message_cb *handler);")
            >>= (`shouldBe` ["tox_friend_message_cb: one function must take a callback type, not 2"])

    it "finds a size function not taking what its getter takes" $
        problemsWith "size_t tox_self_get_name_size(const Tox *tox);" "size_t tox_self_get_name_size(const Tox *self);"
            >>= (`shouldBe` ["tox_self_get_name: its parameters but name must be tox_self_get_name_size's"])

    it "finds a bool function writing a buffer without an error" $
        problemsWithAll "tox_events.h" "uint32_t bytes_size,\n                          Tox_Err_Events_Get_Bytes *error);" "uint32_t bytes_size);"
            >>= (`shouldBe` ["tox_events_get_bytes: it returns bool and writes bytes, so needs an error to say why it failed"])

    it "finds an array a new header's function writes without its size" $
        problemsWithAll "tox_events.h" "uint8_t bytes[], uint32_t bytes_size," "uint8_t bytes[],"
            >>= (`shouldBe` ["tox_events_get_bytes: the array bytes it writes must be followed by its size"])

    it "finds size_t in a new header" $
        problemsWithAll "tox_events.h" "uint32_t tox_events_bytes_size(" "size_t tox_events_bytes_size("
            >>= (`shouldBe` ["tox_events_bytes_size: size_t depends on the system: use uint32_t"])

    it "finds a registration not taking its callback type" $
        problemsWith "void tox_callback_friend_message(Tox *tox, tox_friend_message_cb *callback);"
            "void tox_callback_friend_message(Tox *tox, tox_friend_message_cb *handler);"
            >>= (`shouldBe` ["tox_callback_friend_message: a registration must take a tox_friend_message_cb *callback"])

    it "finds a callback whose parameters aren't its event's fields" $
        problemsWithAll "tox.h" "Tox *tox, Tox_Friend_Number friend_number, Tox_Message_Type type,\n    const uint8_t message[]"
            "Tox *tox, Tox_Friend_Number friend_number, Tox_Message_Type kind,\n    const uint8_t message[]"
            >>= (`shouldBe` ["tox_friend_message_cb: its parameters must be the event's fields, friend_number, type, message"])

    it "finds an event without its callback" $ do
        (files, _) <- publicHeaders
        problemsIn files []
            >>= (`shouldBe` ["tox_event_get_dht_nodes_response: an event needs its callback, tox_dht_nodes_response_cb"])

    it "finds an event missing from a set of registrations" $
        problemsWithAll "tox_dispatch.h" "void tox_dispatch_callback_friend_message(" "void tox_dispatch_set_friend_message("
            >>= (`shouldBe` ["tox_event_get_friend_message: an event needs its registration, tox_dispatch_callback_friend_message"])

    it "finds an array it writes without its size" $
        problemsWith "size_t tox_self_get_name_size(const Tox *tox);" "size_t tox_self_name_length(const Tox *tox);"
            >>= (`shouldBe` ["tox_self_get_name: the array name needs its size: a length after it, or tox_self_get_name_size"])

    it "finds a size getter without its getter" $
        problemsWith "void tox_self_get_name(const Tox *tox, uint8_t name[]);"
            "void tox_self_get_nick(const Tox *tox, uint8_t name[]);"
            >>= (`shouldBe`
                [ "tox_self_get_name_size: a size getter needs its getter, tox_self_get_name"
                , "tox_self_get_nick: the array name needs its size: a length after it, or tox_self_get_nick_size"
                ])

    it "finds a constructor without its destructor" $
        problemsWith "void tox_kill(Tox *tox);" "void tox_end(Tox *tox);"
            >>= (`shouldBe` ["tox_new: a constructor needs its destructor, tox_kill or tox_free"])
