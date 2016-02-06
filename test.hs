{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.VacuumTube
import Control.Monad.ST (runST)
import Data.Set as Set
import Data.Map.Lazy as Map
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
-- import qualified Data.Text as Text
import System.Environment
import Data.Array.Unboxed
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
-- import Text.Groom (groom)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Debug.Trace (traceShow)

-- data Test = Test {-# UNPACK #-} !Int Int deriving Show
data Test = Test Int {-# UNPACK #-} !Int deriving Show
-- data Test = Test {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data Test = Test Integer Integer
--
data Noop = Noop deriving (Eq, Show)

roundTrip :: forall a b . (Show b, Eq b) => (a -> b) -> a -> Bool
roundTrip f !val =
  let bs = runPut . put $ VacuumTube val
      val' :: a
      val' = unVacuumTube $ runGet get bs
  in traceShow ("rt", f val, f val') (f val == f val')

-- prop1 :: forall a proxy . Arbitrary a => proxy a -> Property
-- prop1 _ = do
prop1 :: Gen Property
prop1 = do
  x <- arbitrary
  return . property $ roundTrip id (x :: Integer)

{-
tests :: TestTree
tests = testGroup "tests"
-}

tests :: TestTree
tests = testGroup "tests"
  [ testCase "Int" (roundTrip id (1 :: Int) @? "Int")
  , testCase "Small Integer" (roundTrip id (1 :: Integer) @? "Small Integer")
  , testCase "Large Integer" (roundTrip id (13581358135813518351385013580135801353 :: Integer) @? "Large Integer")
  , QC.testProperty "blah" prop1
  ]
  -- [ testCase "Constructor" (roundTrip id (1 :: Int) @?= True) ]

main :: IO ()
main = defaultMain tests
{-
  print $ roundTrip id (1 :: Int)
  print $ roundTrip id (1 :: Integer)
  print $ roundTrip id (13581358135813518351385013580135801353 :: Integer)

  [arg1, arg2] <- getArgs
  -- print $ runST (dup Noop)
  -- let st = EncodedState (error f[] Set.empty Map.empty
  -- print (encodeObject (10 :: Int) st)
  -- print (encodeObject (Test 11 17) st)
  -- print (encodeObject (Test 11 17000) st)
  -- putStrLn . encode $ encodeObject (ByteString.pack arg) st
  -- let bs = runPut . put . VacuumTube $ ByteString.pack arg
  -- let bs = runPut . put . VacuumTube $ Test 11 83
      -- val :: Test
      -- val = unVacuumTube $ runGet get bs
  -- let bs = runPut . put $ VacuumTube Noop
      -- val :: Noop
      -- val = unVacuumTube $ runGet get bs
  let bs = encode . VacuumTube $ \ x -> (x :: Int) * arg1'
      arg1' :: Int
      !arg1' = read arg1
      f :: Int -> Int
      f  = unVacuumTube $ decode bs
  print bs
  print $ f (read arg2)
  -- print bs
  -- print val
  -- print $ Noop == val
  -- let x :: UArray Word Word8
      -- !x = listArray (1, 10) [1..10]
  -- print (encodeObject x st)
  -- print (encodeObject (x ll) st)
  -- -}
