{- HLINT ignore "Avoid reverse" -}
module Test.HybridSort (tests) where

-- base
import Control.Monad (replicateM)
import Data.List (sort)

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases

{- | The threshold at which a real hybrid sort (e.g. vector-algorithms) switches
from divide-and-conquer to insertion sort.  Values in the range @[15..20]@ are
typical on modern hardware.
-}
threshold :: Int
threshold = 20

{- | A deliberately broken hybrid sort: correct insertion sort below 'threshold',
but @'reverse' . 'sort'@ (descending order) for longer lists.
This models a bug that only manifests once the quicksort path is exercised.
-}
hybridSort :: Int -> [Int] -> [Int]
hybridSort n xs
  | length xs < n = insertionSort xs
  | otherwise = reverse (sort xs) -- wrong: should be `sort xs`

{- | The correct reference hybrid sort: insertion sort below 'threshold',
and the standard sort above it.
Used to verify that tinycheck's generator covers both regimes.
-}
correctHybridSort :: Int -> [Int] -> [Int]
correctHybridSort n xs
  | length xs < n = insertionSort xs
  | otherwise = sort xs

-- | Simple insertion sort, used as the "small input" path in the hybrid sorter.
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys

{- | A list of 'Int' guaranteed to have at least 'threshold' elements.
Used to target the quicksort path of the hybrid sort directly.
-}
newtype LongList = LongList [Int] deriving stock (Show)

instance Arbitrary LongList where
  -- Produce the mandatory `threshold` elements, then append any suffix.
  -- The first test case will be `LongList (replicate threshold 0)`,
  -- which already exercises the long-list path of the hybrid sort.
  arbitrary = LongList <$> ((<>) <$> replicateM threshold arbitrary <*> arbitrary)

tests :: TestTree
tests =
  testGroup
    "Hybrid sort (refuting the SmallCheck objection)"
    {- The claim: enumeration-based testing misses bugs that only appear
    on inputs larger than some threshold N, because exhaustively
    visiting all inputs of size < N is too expensive.

    Tinycheck refutes this: its interleaving strategy generates inputs of
    \*every* length without exhausting shorter ones first.
    In particular, 'LongList' generates lists of length >= 'threshold'
    (20 elements) directly, so the bug in 'hybridSort' is found on the
    very first test case.
    -}
    [ expectFailureWithN
        2_000_000
        "falsified"
        "broken hybrid sort is caught: 'LongList' targets the quicksort path"
        $
        -- 'LongList' generates only lists of length >= 'threshold',
        -- so the first test case already exercises the broken path.
        \(xs :: [Int]) -> hybridSort threshold xs == sort xs
    , testProperty "correct hybrid sort agrees with sort on all list lengths" $
        \(xs :: [Int]) -> correctHybridSort threshold xs == sort xs
    ]
