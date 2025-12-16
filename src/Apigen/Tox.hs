{-# LANGUAGE OverloadedStrings #-}
-- | The Tox bindings' repos: how each is laid out, which the C API can't
-- say. The generators get it in the model's 'targets'.
module Apigen.Tox (targets) where

import           Apigen.Semantic (SJvmExtras (..), SJvmNative (..),
                                  STargets (..), noJvmExtras)

targets :: STargets
targets = STargets
    { haskellModules    = "Network.Tox.C"
    , haskellRawModules = "FFI.Tox"
    , haskellModuleDocs =
        [ ("Tox", "A Tox instance: its own address, name and status, its connections, and its save data.")
        , ("Friend", "Friends: adding and removing them, what they say about themselves, and messages and packets to them.")
        , ("File", "File transfers with friends.")
        , ("Conference", "Conferences, the older group chats, and their peers.")
        , ("Group", "Group chats and their peers.")
        , ("Av", "Audio and video calls with friends.")
        , ("EncryptSave", "Encrypting and decrypting data, such as save data, with a passphrase.")
        , ("Options", "The options a Tox instance starts with.")
        , ("Events", "What happened since the last iteration, as Tox records it.")
        ]
    , headerModuleNames = [("toxencryptsave.h", "EncryptSave")]
    , includeDir        = "tox"
    , pythonPackage     = "pytox"
    , cppNamespace      = "tox"
    , dartPackage       = "toxcore"
    , dartLibrary       = ("ffi/tox_library.dart", "ToxLibrary")
    , dartFfi           = ("ffi/generated/toxcore.ffi.dart", "ToxFfi")
    , dartKeyClasses    =
        [ ("Address", "ToxAddress")
        , ("Conference_Id", "ConferenceId")
        , ("Dht_Id", "PublicKey")
        , ("File_Id", "FileId")
        , ("Group_Chat_Id", "GroupChatId")
        , ("Public_Key", "PublicKey")
        , ("Secret_Key", "SecretKey")
        ]
    -- Their sample count is the length of pcm over the channels.
    , rustHandWritten   = (["toxav_group_send_audio"], ["toxav_audio_send_frame"])
    -- The hand-written iterate options' constructor returns it.
    , rustPrivateEnums  = ["Tox_Err_Iterate_Options_New"]
    -- The conference's AV functions take a handler.
    , rustWrappers      =
        [ "toxav_groupchat_enable_av"
        , "toxav_groupchat_av_enabled"
        , "toxav_groupchat_disable_av"
        , "toxav_group_send_audio"
        ]
    , rustCorePaths     = [("Tox", "inner.core"), ("ToxAV", "inner"), ("Tox_Options", "inner")]
    , rustWrapperNotes  =
        [ ("Conference",
            [ "// The handler-based AV group-chat methods (`enable_av`, `av_enabled`,"
            , "// `send_audio`) and the `peer_list` helper cannot be derived from the"
            , "// model; they stay hand-written in `tox/conference.rs`."
            ])
        ]
    , jvmPackage        = "im.tox.tox4j"
    , jvmNativeLibrary  = "tox4j-c"
    , jvmSubsystems     = [("toxencryptsave", "crypto")]
    , jvmHidden         =
        -- PassKey replaces the passphrase versions.
        [ "tox_pass_encrypt"
        , "tox_pass_decrypt"
        , "tox_file_by_id"
        , "tox_group_by_id"
        -- Its callback gives the same value.
        , "tox_self_get_connection_status"
        ]
    -- Friend state comes from events, and polling would race them.
    , jvmEventOnly      = [("Friend", ["typing"])]
    , jvmStandaloneConstants = ["TOX_HASH_LENGTH"]
    , jvmExtras         =
        [ ("core", noJvmExtras
            { jxInterfaceMembers = ["    override fun close(): Unit"]
            , jxConstants =
                [ "    /** Default port for HTTP proxies. */"
                , "    const val DEFAULT_PROXY_PORT: Int = 8080"
                , ""
                , "    /** Default start port for Tox UDP sockets. */"
                , "    const val DEFAULT_START_PORT: Int = 33445"
                , ""
                , "    /** Default end port for Tox UDP sockets. */"
                , "    const val DEFAULT_END_PORT: Int = DEFAULT_START_PORT + 100"
                , ""
                , "    /** Default port for Tox TCP relays. A value of 0 means disabled. */"
                , "    const val DEFAULT_TCP_PORT: Int = 0"
                ]
            })
        -- tox_hash is the root's, but bound here, and the statics are
        -- written by hand.
        , ("crypto", noJvmExtras
            { jxInterfaceGenerics = ["PassKey"]
            , jxInterfaceMembers = ["    fun hash(data: ByteArray): ByteArray"]
            , jxImplMembers = ["    override fun hash(data: ByteArray): ByteArray = ToxCryptoJni.toxHash(data)"]
            -- libsodium sizes that toxencryptsave.h doesn't expose.
            , jxConstants =
                [ "    const val PUBLIC_KEY_LENGTH = 32"
                , "    const val SECRET_KEY_LENGTH = 32"
                , "    const val SHARED_KEY_LENGTH = 32"
                , "    const val NONCE_LENGTH = 24"
                , ""
                , "    const val ZERO_BYTES = 32"
                , "    const val BOX_ZERO_BYTES = 16"
                ]
            , jxNatives =
                [ SJvmNative "toxGetSalt" "byte[]" [("byte[]", "ciphertext")] (Just "ToxGetSaltException")
                , SJvmNative "toxIsDataEncrypted" "boolean" [("byte[]", "data")] Nothing
                , SJvmNative "toxHash" "byte[]" [("byte[]", "data")] Nothing
                ]
            , jxJavaImports = ["im.tox.tox4j.crypto.exceptions.ToxGetSaltException"]
            , jxNativeRefs = [("toxHash", "tox_hash")]
            })
        ]
    }
