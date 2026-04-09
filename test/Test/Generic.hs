{- HLINT ignore "Avoid reverse" -}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Redundant not" -}
module Test.Generic (tests) where

-- base
import Data.List (isPrefixOf, tails)
import GHC.Generics (Generic, Generically (..))

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases

{- | A deeply-nested record type used to exercise the generic 'Arbitrary'
instance and to verify that tinycheck eventually reaches large inputs.
-}
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

-- | Count non-overlapping occurrences of a substring.
countOccurrences :: String -> String -> Int
countOccurrences needle haystack =
  length $ filter (isPrefixOf needle) (tails haystack)

tests :: TestTree
tests =
  testGroup
    "Generic"
    [ testGroup
        "LargeRecord (Generic)"
        [ testProperty "show ends with }" $
            \(r :: LargeRecord) -> last (show r) == '}'
        ]
    , testGroup
        "Preconditions"
        [ testProperty "n > 0 ==> n * 2 > 0" $
            \(n :: Int) -> (n > 0) ==> property (n * 2 > 0)
        ]
    , testGroup
        "Expected failures"
        [ expectFailureWith
            "falsified"
            "False is always falsified"
            False
        , expectFailureWith "falsified" "const False always falsified" $
            \(_ :: Int) -> False
        , expectFailureWith "0" "reverse [0] /= [0] is falsified with 0" $
            \(x :: Int) -> reverse [x] /= [x]
        , expectFailureWith "True" "even True falsified: input shown" $
            \(b :: Bool) -> not b
        , expectFailureWithN 1_000_000_0 "LargeRecord" "LargeRecord appears at most 6 times in show" $
            \(r :: LargeRecord) -> countOccurrences "LargeRecord" (show r) <= 6
        ]
    ]
