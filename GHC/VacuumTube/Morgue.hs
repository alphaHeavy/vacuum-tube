{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.VacuumTube.Morgue
  ( vacuumPut

  -- * Callbacks from Cmm need to be exported
  , popState
  , pushArrWords
  , pushClosure
  , pushIndirection
  , pushSelector
  , pushStatic
  , pushThunk
  , yieldNPtr
  , yieldPtr
  , unsupportedTag
  ) where

import Data.Array.Base
import Data.Binary
import Data.Map.Lazy as Map

import GHC.Prim
import GHC.Ptr
import GHC.ST
import GHC.Types

import GHC.VacuumTube.EncodedState
import GHC.VacuumTube.Types
import GHC.VacuumTube.VacuumNode

import Debug.Trace

foreign import prim "VacuumWalker_encodeObject" unsafeEncodeObject :: Any -> Any -> State# s -> (# State# s, Any #)

vacuumPut :: a -> Put
vacuumPut val =
  let st = EncodedState Nothing [] Map.empty
  in put $ runST $ encodeObject val st

encodeObject :: a -> EncodedState -> ST s EncodedState
encodeObject val enc = ST $ \ st ->
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# enc :: Any) st of
    (# st', res #) -> (# st', unsafeCoerce# res :: EncodedState #)

popState :: EncodedState -> EncodedState
popState (EncodedState _ (x@(ptr, closure):xs) graph) =
  traceShow ("pop", closure) (EncodedState (Just x) xs (Map.insert ptr closure graph))

pushClosure :: Word# -> Addr# -> Word# -> Addr# -> EncodedState -> EncodedState
pushClosure type# infoTable# tag# closure# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      node = VacuumClosure{vacuumInfoTable = Ptr infoTable#, vacuumTag = W# tag#, vacuumPayload = Map.empty}
      encst = st{encodedStack = (ptr, node):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("closure", W# type#, Ptr infoTable#, ptr) encst

pushThunk :: Word# -> Addr# -> Word# -> Addr# -> EncodedState -> EncodedState
pushThunk type# infoTable# tag# closure# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      node = VacuumThunk{vacuumInfoTable = Ptr infoTable#, vacuumTag = W# tag#, vacuumPayload = Map.empty}
      encst = st{encodedStack = (ptr, node):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("thunk", W# type#, Ptr infoTable#, ptr) encst

pushArrWords :: Addr# -> ByteArray# -> EncodedState -> EncodedState
pushArrWords infoTable# arr# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr    = Ptr (unsafeCoerce# arr# :: Addr#)
      bytes  = fromIntegral $ I# (sizeofByteArray# arr#)
      uarray = UArray 1 bytes (fromIntegral bytes) arr#
      node   = VacuumArray{vacuumInfoTable = Ptr infoTable#, vacuumArray = uarray}
      encst  = st{encodedStack = (ptr, node):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("arr", Ptr infoTable#, ptr) encst

pushSelector :: Addr# -> Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
pushSelector infoTable# tag# closure# selectee# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      node = VacuumSelector{vacuumInfoTable = Ptr infoTable#, vacuumTag = W# tag#, vacuumSelectee = Ptr selectee#}
      encst = st{encodedStack = (ptr, node):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("selector", Ptr infoTable#, ptr) encst

pushIndirection :: Addr# -> Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
pushIndirection infoTable# tag# closure# indirectee# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      node = VacuumInd{vacuumInfoTable = Ptr infoTable#, vacuumTag = W# tag#, vacuumIndirectee = Ptr indirectee#}
      encst = st{encodedStack = (ptr, node):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("indirection", Ptr infoTable#, ptr) encst

pushStatic :: Addr# -> EncodedState -> EncodedState
pushStatic closure# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      encst = st{encodedStack = (ptr, VacuumStatic):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("static", ptr) encst

unsupportedTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
unsupportedTag _tag# _infoTable# _closure# EncodedState{} =
  error "omg"

yieldPtr :: Word# -> Any -> Addr# -> EncodedState -> EncodedState
yieldPtr slot# val ptr# st@EncodedState{encodedStack = (k, !v):xs, encodedNodes} = do
  let ptr = Ptr ptr#
      v'  = v{vacuumPayload = Map.insert (W# slot#) (PtrPayload ptr) (vacuumPayload v)}
      st' = st{encodedStack = (k, v'):xs}

  if Map.member ptr encodedNodes
    then st'
    else runST $ encodeObject val st'

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot# val# st@EncodedState{encodedStack = (k, !v):xs} =
  let v' = v{vacuumPayload = Map.insert (W# slot#) (NPtrPayload (W# val#)) (vacuumPayload v)}
  in st{encodedStack = (k, v'):xs}
