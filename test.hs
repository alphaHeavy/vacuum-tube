{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim
import GHC.Int
import GHC.Word
import GHC.Ptr
import Unsafe.Coerce

foreign import prim "Serializze_encodeObject" blah :: Any -> Any -> (# Any #)

encodeObject :: forall a st . a -> st -> st
encodeObject val st =
  case blah (unsafeCoerce val :: Any) (unsafeCoerce st :: Any) of
    (# res #) -> unsafeCoerce res :: st

main :: IO ()
main = do
  print (encodeObject (10 :: Int) (20 :: Int))
