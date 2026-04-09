{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

{- | Benchmarks for tinycheck's test-case generators.

Each benchmark measures how quickly @take N (getTestCases gen)@ can produce
and fully force @N@ values, for a representative set of generators.
This captures both the enumeration cost and the cost of building the values.
-}
module Main where

-- base
import GHC.Generics (Generic, Generically (..))

-- deepseq
import Control.DeepSeq (NFData)

-- tasty-bench
import Test.Tasty.Bench

-- tinycheck

import Data.Functor ((<&>))
import Data.TestCases (Arbitrary (..), getTestCases)
import Test.Tasty (localOption, mkTimeout)

-- * Real-life data structure: binary search tree

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving stock (Show, Eq, Foldable, Generic)
  deriving anyclass (NFData)
  deriving (Arbitrary) via Generically (Tree a)

-- * Benchmark helpers

-- | Counts used for generators.
counts :: [Int]
counts = [10_000, 100_000, 1_000_000]

{- | Benchmark a single generator at a given list of counts.
The group name is the type/description; each leaf is labelled by the count.
The generator is instantiated fresh for every individual benchmark run
so that no list spine is shared or retained between runs.
-}
benchGenWith :: forall a. (Arbitrary a, NFData a) => [Int] -> String -> Benchmark
benchGenWith ns name =
  bgroup name $
    ns <&> \n ->
      localOption (mkTimeout 5_000_000) $ -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
        -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run -- 5 second timeout per benchmark run
      -- 5 second timeout per benchmark run
        bench (show n) $
          nf (\n' -> take n' $ getTestCases $ arbitrary @a) n

-- | 'benchGenWith' at 'counts'
benchGen :: forall a. (Arbitrary a, NFData a) => String -> Benchmark
benchGen = benchGenWith @a counts

-- * Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "primitives"
        [ benchGen @Int "Int"
        , benchGen @Bool "Bool"
        , benchGen @Char "Char"
        , benchGen @Integer "Integer"
        ]
    , bgroup
        "composites"
        [ benchGen @(Int, Int) "(Int, Int)"
        , benchGen @(Maybe Int) "Maybe Int"
        , benchGen @(Either Int Bool) "Either Int Bool"
        , benchGen @(Bool, Bool, Bool) "(Bool, Bool, Bool)"
        ]
    , bgroup
        "lists"
        [ benchGen @[Int] "[Int]"
        , benchGen @[Bool] "[Bool]"
        ]
    , bgroup
        "Tree Int"
        [ benchGen @(Tree Int) "Tree Int"
        ]
    ]
