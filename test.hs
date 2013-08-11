import Serialize
import Data.Map.Lazy as Map

data Test = Test Int Int

main :: IO ()
main = do
  -- print (encodeObject (10 :: Int) (20 :: Int))
  let st = EncodedState Map.empty
  print (encodeObject (Test 11 17) st)
