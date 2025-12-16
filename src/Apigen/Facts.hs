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
-- C's only behaviour.
alwaysOnOptions :: [Text]
alwaysOnOptions = ["experimental_owned_data"]
