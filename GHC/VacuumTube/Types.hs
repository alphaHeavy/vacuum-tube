{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.VacuumTube.Types
  ( Closure
  , InfoTable
  , Payload(..)
  , ClosureMap
  , Tag
  , Ptrs
  , NPtrs
  , PointerTag
  ) where

import Control.Applicative
import Data.Binary
import Data.Map.Lazy (Map)
import Foreign.Ptr (Ptr)

import GHC.Generics

import GHC.VacuumTube.VacuumPtr

data Closure

instance Binary (Ptr Closure) where
  put = put . VacuumPtr
  get = unVacuumPtr <$> get

data InfoTable

instance Binary (Ptr InfoTable) where
  put = put . VacuumPtr
  get = unVacuumPtr <$> get

data Payload
  = PtrPayload (Ptr Closure)
  | NPtrPayload Word
    deriving (Eq, Ord, Generic, Show)

instance Binary Payload

type ClosureMap a = Map (Ptr Closure) a

type Tag = Word
type Ptrs = Word
type NPtrs = Word
type PointerTag = Word
