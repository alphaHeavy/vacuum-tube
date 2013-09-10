{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.VacuumTube.VacuumPtr where

import Data.Binary

import GHC.Prim
import GHC.Ptr
import GHC.Types

newtype VacuumPtr a = VacuumPtr{unVacuumPtr :: a}

instance Binary (VacuumPtr (Ptr a)) where
  put (VacuumPtr (Ptr val#)) = put (W# (unsafeCoerce# val# :: Word#))
  get = do
    W# val# <- get
    return . VacuumPtr $ Ptr (unsafeCoerce# val# :: Addr#)
