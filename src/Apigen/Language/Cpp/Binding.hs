{-# LANGUAGE OverloadedStrings #-}
-- | cpp-toxcore-c's own choices: its wording, and what it always sets.
module Apigen.Language.Cpp.Binding
    ( proxyDoc
    , alwaysOn
    ) where

import           Data.Text (Text)

-- | A proxy's doc, by its resource and that resource's words.
proxyDoc :: Text -> Text -> Text
proxyDoc "Group" _      = "A proxy representing a group chat."
proxyDoc "Conference" _ = "A proxy representing a conference, the older group chats."
proxyDoc _ ws           = "A proxy representing a " <> ws <> "."

-- | Options the binding always sets: with owned data, toxcore copies strings
-- and bytes.
alwaysOn :: [Text]
alwaysOn = ["experimental_owned_data"]
