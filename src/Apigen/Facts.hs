{-# LANGUAGE OverloadedStrings #-}
-- | Facts about the toxcore API that its headers don't say in a form apigen
-- can read yet. Remove an entry once the header says it.
module Apigen.Facts
    ( nullableParams
    , nullableReturns
    , bufferSizes
    , minimumSizes
    , secretArrays
    , arrayPrefixes
    , enumValuedParams
    , failureReturns
    , eventFieldTypes
    , privateEnums
    , idDestructors
    , scopedIds
    ) where

import           Apigen.Semantic (SType (..))
import           Data.Text       (Text)

-- | Pointer params that may be NULL to mean "none". A callback setter's
-- callback always may (@tox.h@: "Passing a NULL callback ...").
nullableParams :: [(Text, Text)]
nullableParams =
    [ ("tox_new", "options")        -- the default options
    , ("tox_file_send", "file_id")  -- a random file id
    ]

-- | Functions returning NULL for an unset option.
nullableReturns :: [Text]
nullableReturns =
    [ "tox_options_get_proxy_host"
    , "tox_options_get_savedata_data"
    ]

-- | Element counts of unsized array params, as a header's @[/*! ... */]@
-- would give them.
bufferSizes :: [((Text, Text), Text)]
bufferSizes =
    [ (("toxav_group_send_audio", "pcm"), "samples * channels")
    , (("toxav_group_audio_cb", "pcm"), "samples * channels")
    , (("toxav_audio_data_cb", "pcm"), "samples * channels")
    ]

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

-- | Event fields whose callback is in a header apigen doesn't read
-- (@tox_private.h@), so no typed parameter says what they are.
eventFieldTypes :: [((Text, Text), SType)]
eventFieldTypes =
    [ (("dht_nodes_response", "public_key"), SFixedBytes "TOX_PUBLIC_KEY_SIZE" False)
    ]

-- | Enums in a header apigen doesn't read (@tox_private.h@) that a public
-- function returns, with their members.
privateEnums :: [(Text, [Text])]
privateEnums =
    [ ( "Tox_Err_Iterate_Options_New"
      , [ "TOX_ERR_ITERATE_OPTIONS_NEW_OK"
        , "TOX_ERR_ITERATE_OPTIONS_NEW_MALLOC"
        ]
      )
    ]

-- | Functions after which their id names nothing.
idDestructors :: [Text]
idDestructors = ["tox_friend_delete", "tox_conference_delete", "tox_group_leave"]

-- | Ids that only name something with another: file numbers are per friend.
-- @(resource, (field, id type))@.
scopedIds :: [(Text, (Text, Text))]
scopedIds = [("File", ("friend", "FriendNumber"))]
