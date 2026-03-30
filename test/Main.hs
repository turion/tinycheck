{- HLINT ignore "Avoid reverse" -}
module Main (main) where

-- base
import GHC.Generics (Generically(..), Generic)

-- tinycheck
import Data.TestCases

data LargeRecord = LargeRecord
  { field1 :: Bool
  , field2 :: String
  , field3 :: Int
  , field4 :: Maybe LargeRecord
  , field5 :: [Bool]
  , field6 :: [Int]
  , field7 :: Maybe LargeRecord
  , field8 :: Either String LargeRecord
  }
  deriving stock (Show, Generic)
  deriving (Arbitrary) via Generically LargeRecord


main :: IO ()
main = do
  test "reverse . reverse == id" $ \(as :: [Int]) -> ((reverse . reverse) as == as, "")
  test "LargeRecord show ends with }" $ \(largeRecord :: LargeRecord) -> (last (show largeRecord) == '}', show largeRecord)
