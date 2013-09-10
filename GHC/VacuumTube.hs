module GHC.VacuumTube where

import Control.Applicative
import Data.Binary

import GHC.VacuumTube.Limbo
import GHC.VacuumTube.Morgue

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

instance Binary (VacuumTube a) where
  put = vacuumPut . unVacuumTube
  get = VacuumTube <$> vacuumGet
