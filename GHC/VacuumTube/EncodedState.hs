{-# LANGUAGE DeriveGeneric #-}

module GHC.VacuumTube.EncodedState
  ( EncodedState(..)
  ) where

import Data.Binary
import Data.Map.Lazy (Map)
import Foreign.Ptr (Ptr)

import GHC.Generics

import GHC.VacuumTube.Types
import GHC.VacuumTube.VacuumNode

data EncodedState = EncodedState
  { encodedRoot  :: Maybe (Ptr Closure, VacuumNode)
  , encodedStack :: [(Ptr Closure, VacuumNode)]
  , encodedNodes :: Map (Ptr Closure) VacuumNode
  } deriving (Show, Generic)

instance Binary EncodedState
