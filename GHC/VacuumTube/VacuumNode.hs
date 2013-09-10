{-# LANGUAGE DeriveGeneric #-}

module GHC.VacuumTube.VacuumNode
  ( VacuumNode(..)
  , VacuumMap
  ) where

import Data.Array.Base
import Data.Binary
import Data.Map.Lazy (Map)
import Foreign.Ptr (Ptr)

import GHC.Generics

import GHC.VacuumTube.Types

data VacuumNode
  = VacuumClosure {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload}
  | VacuumThunk   {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload}
  | VacuumSelector{vacuumInfoTable :: Ptr InfoTable, vacuumSelectee   :: Ptr Closure}
  | VacuumPAP     {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumArity :: Word, vacuumNArgs :: Word, vacuumFun :: Ptr Closure}
  | VacuumAP      {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumArity :: Word, vacuumNArgs :: Word, vacuumFun :: Ptr Closure}
  | VacuumAP_STACK{vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumFun :: Ptr Closure}
  | VacuumInd     {vacuumInfoTable :: Ptr InfoTable, vacuumIndirectee :: Ptr Closure}
  | VacuumArray   {vacuumInfoTable :: Ptr InfoTable, vacuumArray      :: UArray Word Word8}
  | VacuumStatic
  | VacuumUndefined
    deriving (Eq, Ord, Generic, Show)

instance Binary VacuumNode

type VacuumMap = ClosureMap VacuumNode
