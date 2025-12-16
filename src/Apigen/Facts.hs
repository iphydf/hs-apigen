{-# LANGUAGE OverloadedStrings #-}
-- | Facts about the toxcore API that its headers don't say in a form apigen
-- can read yet. Remove an entry once the header says it.
module Apigen.Facts
    ( minimumSizes
    , secretArrays
    , arrayPrefixes
    , enumValuedParams
    , failureReturns
    , idDestructors
    , alwaysOnOptions
    , lockingOptions
    , secretBytes
    ) where

import           Data.Text (Text)

-- | Arrays whose header size is a minimum: C reads at least that many bytes.
-- With what the function gives for fewer: an error enumerator, or @false@.
minimumSizes :: [((Text, Text), Text)]
minimumSizes =
    [ (("tox_get_salt", "ciphertext"), "TOX_ERR_GET_SALT_BAD_FORMAT")
    , (("tox_is_data_encrypted", "data"), "false")
    ]

-- | Byte arrays holding secrets, which memory shouldn't keep.
secretArrays :: [Text]
secretArrays = ["Tox_Secret_Key"]

-- | Byte array types that start with another: a Tox address is the public
-- key, then the nospam and a checksum.
arrayPrefixes :: [(Text, Text)]
arrayPrefixes = [("Tox_Address", "Tox_Public_Key")]

-- | Plain integers holding an enum's values, so clients can use values of
-- their own.
enumValuedParams :: [((Text, Text), Text)]
enumValuedParams =
    [ (("tox_file_send", "kind"), "Tox_File_Kind")
    , (("tox_file_recv_cb", "kind"), "Tox_File_Kind")
    , (("tox_event_file_recv", "kind"), "Tox_File_Kind")
    , (("toxav_call_state_cb", "state"), "Toxav_Friend_Call_State")
    ]

-- | Functions returning this plain integer on failure, otherwise their
-- result (or 0).
failureReturns :: [(Text, Integer)]
failureReturns =
    [ ("toxav_add_av_groupchat", -1)
    , ("toxav_join_av_groupchat", -1)
    , ("toxav_group_send_audio", -1)
    , ("toxav_groupchat_enable_av", -1)
    , ("toxav_groupchat_disable_av", -1)
    ]

-- | Functions after which their id names nothing.
idDestructors :: [Text]
idDestructors = ["tox_friend_delete", "tox_conference_delete", "tox_group_leave"]

-- | Options every binding turns on: with owned data, C copies the strings
-- and bytes it's given, so they needn't outlive the call. Gone once that is
-- C's only behaviour. (C++ turns it off: its options outlive the call, so C
-- keeps no copy of a secret it doesn't erase.)
alwaysOnOptions :: [Text]
alwaysOnOptions = ["experimental_owned_data"]

-- | Options that make C lock each call. Every binding turns them on, and
-- doesn't offer them, only because toxav's own threads call into Tox behind
-- the binding's back. Nothing else relies on them: a binding still keeps each
-- handle to one thread at a time. Gone once toxav no longer calls into Tox.
lockingOptions :: [Text]
lockingOptions = ["experimental_thread_safety"]

-- | Functions whose bytes hold a secret, which memory shouldn't keep: the
-- save data has the secret key, and is what clients decrypt.
secretBytes :: [Text]
secretBytes =
    [ "tox_get_savedata"
    , "tox_options_set_savedata_data"
    , "tox_pass_decrypt"
    , "tox_pass_key_decrypt"
    ]
