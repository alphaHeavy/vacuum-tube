{-# LANGUAGE BangPatterns #-}

import Serialize
import Data.Set as Set
import Data.Map.Lazy as Map
import qualified Data.ByteString.Char8 as ByteString
-- import qualified Data.Text as Text
import System.Environment
import Data.Array.Unboxed
import Data.Word

-- data Test = Test {-# UNPACK #-} !Int Int
-- data Test = Test {-# UNPACK #-} !Int {-# UNPACK #-} !Int
data Test = Test Integer Integer

x :: Int -> Int -> Int
x = (*)

main :: IO ()
main = do
  [arg] <- getArgs
  let st = EncodedState [] Set.empty Map.empty
  print (encodeObject (10 :: Int) st)
  -- print (encodeObject (Test 11 17) st)
  -- print (encodeObject (Test 11 17000) st)
  -- print (encodeObject (ByteString.pack arg) st)
  -- let x :: UArray Word Word8
      -- !x = listArray (1, 10) [1..10]
  -- print (encodeObject x st)
  -- print (encodeObject (x ll) st)
