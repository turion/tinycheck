{- HLINT ignore "Redundant id" -}
module Test.Combinators (tests) where

-- base
import Data.List (sort)

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases

tests :: TestTree
tests =
  testGroup
    "Combinators"
    [ testGroup
        "interleaveN"
        [ testProperty "3-way interleave matches documented output" $
            getTestCases (interleaveN [TestCases [1, 2, 3], TestCases [10, 20, 30], TestCases [100, 200, 300 :: Int]])
              == [1, 10, 100, 2, 20, 200, 3, 30, 300]
        , testProperty "unequal lengths: shorter sources exhaust cleanly" $
            getTestCases (interleaveN [TestCases [1, 2], TestCases [10, 20, 30], TestCases [100 :: Int]])
              == [1, 10, 100, 2, 20, 30]
        , testProperty "interleaveN [] == mempty" $
            getTestCases (interleaveN ([] :: [TestCases Int])) == ([] :: [Int])
        , testProperty "interleaveN [a, b] == a <> b" $
            let a = TestCases [1, 2, 3 :: Int]; b = TestCases [10, 20, 30]
             in getTestCases (interleaveN [a, b]) == getTestCases (a <> b)
        , testProperty "does not get stuck with finite-many infinite sources" $
            -- Take 100 elements from 5 infinite lists; must terminate.
            let sources = replicate 5 (TestCases [0 :: Int ..])
             in length (take 100 (getTestCases (interleaveN sources))) == 100
        , testProperty "fair: each of n infinite sources contributes within first n*k elements" $
            -- With n infinite sources, every source must appear in every window
            -- of n consecutive elements (one full round).
            let n = 5
                sources = fmap (\i -> (,) i <$> TestCases [0 :: Int ..]) [0 .. n - 1]
                -- sources !! i produces (i, 0), (i, 1), (i, 2), ...
                -- so the first component uniquely identifies which source contributed
                elems = take (n * 10) (getTestCases (interleaveN sources))
                -- in each window of n, all source indices 0..n-1 must appear
                windows = do
                  k <- [0, n .. n * 9]
                  pure $ take n (drop k elems)
             in all (\w -> sort (fmap fst w) == [0 .. n - 1]) windows
        ]
    , testGroup
        "Applicative interleaving"
        [ testProperty "(,) <$> \"abc\" <*> [1..6] matches documented output" $
            getTestCases ((,) <$> TestCases "abc" <*> TestCases [1, 2, 3, 4, 5, 6 :: Int])
              == [ ('a', 1)
                 , ('b', 1)
                 , ('a', 2)
                 , ('c', 1)
                 , ('a', 3)
                 , ('b', 2)
                 , ('a', 4)
                 , ('c', 2)
                 , ('a', 5)
                 , ('b', 3)
                 , ('a', 6)
                 , ('c', 3)
                 , ('b', 4)
                 , ('c', 4)
                 , ('b', 5)
                 , ('c', 5)
                 , ('b', 6)
                 , ('c', 6)
                 ]
        , testProperty "TestCases interleaves, plain list does not" $
            getTestCases ((,) <$> TestCases "abc" <*> TestCases [1, 2, 3, 4, 5, 6 :: Int])
              /= ((,) <$> "abc" <*> [1, 2, 3, 4, 5, 6])
        ]
    , testGroup
        "CoArbitrary newtype wrappers (nontrivial functions)"
        [ testProperty "IntegralCoArbitrary: some f distinguishes 0 from 1" $
            -- A constant function satisfies f 0 == f 1 for all inputs, but the
            -- generated functions split on sign×parity, so some must differ.
            any
              (\f -> f (0 :: Int) /= f (1 :: Int))
              (take 100 $ getTestCases (coArbitrary :: TestCases (Int -> Bool)))
        , testProperty "OrdCoArbitrary: some f distinguishes LT from GT" $
            any
              (\f -> f LT /= f (GT :: Ordering))
              (take 100 $ getTestCases (coArbitrary :: TestCases (Ordering -> Bool)))
        , testProperty "EnumCoArbitrary: some f distinguishes 'a' from 'b'" $
            any
              (\f -> f 'a' /= f 'b')
              (take 100 $ getTestCases (coArbitrary :: TestCases (Char -> Bool)))
        , testProperty "RealFracCoArbitrary: some f distinguishes 0.0 from 2.0" $
            any
              (\f -> f (0.0 :: Double) /= f 2.0)
              (take 100 $ getTestCases (coArbitrary :: TestCases (Double -> Bool)))
        ]
    ]
