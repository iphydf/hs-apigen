{-# LANGUAGE OverloadedStrings #-}
-- | rs-toxcore-c's own choices: what it writes by hand, and what it names
-- its own way. The generator leaves these to it.
module Apigen.Language.Rust.Crate
    ( handWrittenCore
    , handWrittenSafe
    , handWrittenWrappers
    , methodRenames
    , typeNames
    , extraNewtypes
    , moduleAliases
    , HandleGlue (..)
    , handleGlue
    , wrapperCore
    , wrapperGetters
    , wrapperNotes
    , errorType
    , enumMacro
    , safeModule
    ) where

import           Apigen.Language.Rust.AST (RsType (..))
import           Data.Text                (Text)
import qualified Data.Text                as T

-- | Hand-written in core/av_groupchat.rs, so not generated.
handWrittenCore :: [Text]
handWrittenCore =
    -- They take handlers.
    [ "toxav_add_av_groupchat"
    , "toxav_join_av_groupchat"
    , "toxav_group_send_audio"
    , "toxav_groupchat_enable_av"
    -- These must register the Rust dispatch callbacks first.
    , "toxav_iterate"
    , "toxav_audio_iterate"
    , "toxav_video_iterate"
    -- Returns a non-owning `Tox*`. An owning `Tox` would `tox_kill` it again.
    , "toxav_get_tox"
    ]

-- | Hand-written in `tox/mod.rs` and `toxav/mod.rs`, so not in the safe layer.
handWrittenSafe :: [Text]
handWrittenSafe =
    [ "tox_iterate"                    -- takes a &mut handler
    , "tox_events_init"                -- folded into the bespoke `events()`
    , "tox_events_iterate"             -- ditto
    , "toxav_audio_send_frame"         -- derives sample_count from pcm.len()
    , "tox_options_set_proxy_host"     -- safe API discards the core `bool`
    , "tox_options_set_savedata_data"  -- ditto
    , "toxav_iterate"                  -- &mut self: runs &mut handler callbacks
    , "toxav_audio_iterate"            -- ditto
    , "toxav_video_iterate"            -- ditto
    ]

-- | Hand-written: `tox_group_invite_friend` and `tox_conference_invite` take
-- a `&Friend`, `tox_group_leave` and `tox_group_set_password` an
-- `Option<&[u8]>`, and the conference's AV functions a handler (in
-- `tox/conference.rs`). The crate doesn't offer `disable_av`.
handWrittenWrappers :: [Text]
handWrittenWrappers =
    [ "tox_group_invite_friend"
    , "tox_group_leave"
    , "tox_group_set_password"
    , "tox_conference_invite"
    , "toxav_groupchat_enable_av"
    , "toxav_groupchat_av_enabled"
    , "toxav_groupchat_disable_av"
    , "toxav_group_send_audio"
    ]

-- | Safe-layer method names.
methodRenames :: [(Text, Text)]
methodRenames = [("friend_list_size", "friend_list_len")]

-- | Rust names, by model name. @Events@ is @ToxEvents@ so it doesn't clash
-- with the @core::Events@ struct.
typeNames :: [(Text, Text)]
typeNames =
    [ ("Events", "ToxEvents")
    , ("AV", "ToxAV")
    , ("ToxAV", "ToxAV")
    , ("Tox_Message_Type", "MessageType")
    ]

-- | Public newtypes with no C id type in the parsed headers.
extraNewtypes :: [Text]
extraNewtypes = ["MessageId"]

-- | Modules also known by another name.
moduleAliases :: [(Text, Text)]
moduleAliases = [("av", "toxav")]

-- | How a handle's safe layer reaches its core struct. @hgField@ is the
-- path from `self` to `core`.
data HandleGlue = HandleGlue
    { hgFile     :: FilePath
    , hgImports  :: [Text]
    , hgImplType :: RsType
    , hgGenerics :: [Text]
    , hgField    :: Text
    }

-- | The `Tox`, `ToxAV` and `Options` structs are hand-written. `Pass_Key`
-- isn't here: its safe layer is generated whole.
handleGlue :: [(Text, HandleGlue)]
handleGlue =
    [ ("Tox", HandleGlue
        { hgFile     = "safe_tox.rs"
        , hgImports  = ["use crate::tox::Tox;", "use crate::types::*;"]
        , hgImplType = TyPath "Tox"
        , hgGenerics = []
        , hgField    = "inner.core"
        })
    , ("ToxAV", HandleGlue
        { hgFile     = "safe_toxav.rs"
        , hgImports  = [ "use crate::toxav::ToxAV;"
                       , "use crate::toxav::ToxAVHandler;"
                       , "use crate::types::*;"
                       ]
        , hgImplType = TyGeneric "ToxAV" [TyPath "'a", TyPath "H"]
        , hgGenerics = ["'a", "H: ToxAVHandler"]
        , hgField    = "inner"
        })
    , ("Options", HandleGlue
        { hgFile     = "safe_options.rs"
        , hgImports  = ["use crate::tox::Options;", "use crate::types::*;"]
        , hgImplType = TyPath "Options"
        , hgGenerics = []
        , hgField    = "inner"
        })
    ]

-- | The path from a resource wrapper to the core `Tox`.
wrapperCore :: [Text]
wrapperCore = ["tox", "inner", "core"]

-- | A wrapper's id getters, @(name, type, field)@, where they aren't
-- @get_number@.
wrapperGetters :: Text -> Text -> [(Text, Text, Text)]
wrapperGetters wrapper idType = case wrapper of
    "File"       -> [ ("number", "FileNumber", "number")
                    , ("friend_number", "FriendNumber", "friend")
                    ]
    "Conference" -> [ ("number", idType, "number") ]
    _            -> [ ("get_number", idType, "number") ]

-- | What a wrapper's file says it leaves to the hand-written code.
wrapperNotes :: [(Text, Text)]
wrapperNotes =
    [ ("Conference", T.unlines
        [ "// The handler-based AV group-chat methods (`enable_av`, `av_enabled`,"
        , "// `send_audio`) and the `peer_list` helper cannot be derived from the"
        , "// model; they stay hand-written in `tox/conference.rs`."
        ])
    ]

-- | The crate's error type, with a variant per C error enum.
errorType :: Text
errorType = "ToxError"

-- | The crate's macro implementing a C enum's Rust enum.
enumMacro :: Text
enumMacro = "impl_tox_enum"

-- | The module with the crate's own structs for the handles.
safeModule :: Text
safeModule = "crate::tox"
