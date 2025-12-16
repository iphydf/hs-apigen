{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module Apigen.Types
    ( BitSize (..)
    , BuiltinType (..)
    , Constness (..)
    , Generated (..)
    ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)

data BitSize
    = B8
    | B16
    | B32
    | B64
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''BitSize)

data BuiltinType
    = Void
    | VoidPtr
    | Bool
    | Char
    | SInt BitSize
    | UInt BitSize
    | SizeT
    | String
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''BuiltinType)

data Constness
    = ConstThis
    | MutableThis
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''Constness)

data Generated
    = GeneratedToString
    | GeneratedFromInt
    deriving (Show, Eq)
$(deriveJSON defaultOptions ''Generated)
