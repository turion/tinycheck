{- HLINT ignore "Avoid reverse" -}
{- HLINT ignore "Functor law" -}
{- HLINT ignore "Redundant maybe" -}
{- HLINT ignore "Use <|>" -}
{- HLINT ignore "Use =<<" -}
{- HLINT ignore "Redundant negate" -}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Redundant not" -}
{- HLINT ignore "Redundant ==" -}
module Test.Base (tests) where

-- base
import Data.List (nub, sort)

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases ()

tests :: TestTree
tests =
  testGroup
    "Base"
    [ testGroup
        "Lists"
        [ testProperty "reverse . reverse == id" $
            \(xs :: [Int]) -> xs == xs
        , testProperty "length (xs ++ ys) == length xs + length ys" $
            \(xs :: [Int], ys :: [Int]) ->
              length (xs <> ys) == length xs + length ys
        , testProperty "length . nub <= length" $
            \(xs :: [Int]) -> length (nub xs) <= length xs
        , testProperty "sort . sort == sort" $
            \(xs :: [Int]) -> sort (sort xs) == sort xs
        , testProperty "head (x:xs) == x" $
            \(x :: Int, xs :: [Int]) -> let (h : _) = x : xs in h == x
        , testProperty "last (xs ++ [x]) == x" $
            \(x :: Int, xs :: [Int]) -> last (xs <> [x]) == x
        ]
    , testGroup
        "Maybe"
        [ testProperty "fmap id == id" $
            \(mx :: Maybe Int) -> fmap id mx == mx
        , testProperty "maybe Nothing Just == id" $
            \(mx :: Maybe Int) -> maybe Nothing Just mx == mx
        ]
    , testGroup
        "Either"
        [ testProperty "either Left Right == id" $
            \(e :: Either Int Bool) -> either Left Right e == e
        ]
    , testGroup
        "Numeric"
        [ testProperty "abs x >= 0  (Int)" $
            \(x :: Int) -> abs x >= 0
        , testProperty "negate . negate == id  (Int)" $
            \(x :: Int) -> negate (negate x) == x
        , testProperty "x + 0 == x  (Integer)" $
            \(x :: Integer) -> x + 0 == x
        , testProperty "x * 1 == x  (Integer)" $
            \(x :: Integer) -> x * 1 == x
        ]
    , testGroup
        "Tuples"
        [ testProperty "fst (a, b) == a" $
            \(a :: Int, b :: Bool) -> fst (a, b) == a
        , testProperty "snd (a, b) == b" $
            \(a :: Int, b :: Bool) -> snd (a, b) == b
        , testProperty "swap . swap == id" $
            \(a :: Int, b :: Bool) -> let swap (x, y) = (y, x) in swap (swap (a, b)) == (a, b)
        ]
    , testGroup
        "Bool"
        [ testProperty "not . not == id" $
            \(b :: Bool) -> not (not b) == b
        , testProperty "b || True == True" $
            \(b :: Bool) -> (b || True) == True
        , testProperty "b && False == False" $
            \(b :: Bool) -> (b && False) == False
        ]
    ]
