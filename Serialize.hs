{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import GHC.Prim
import GHC.Int
import GHC.Word
import GHC.Ptr

popTag :: Addr# -> Addr#
popTag x = x

pushTag :: Word# -> Addr# -> Addr#
pushTag _tag x = x

yieldPtr :: Any -> Addr# -> Addr#
yieldPtr _ptr x = x

yieldNPtr :: Word# -> Addr# -> Addr#
yieldNPtr _val x = x
