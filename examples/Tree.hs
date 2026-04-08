{-# LANGUAGE DeriveGeneric #-}

{- | Standalone example: a binary tree and properties comparing its
'Foldable' instance against list operations.
-}
module Main where

-- base
import Data.Foldable (toList)
import Data.List (nub, sort)
import GHC.Generics (Generic, Generically (..))

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases (Arbitrary (..), getTestCases)

-- * Tree type

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving stock (Show, Eq, Foldable, Generic)
  deriving (Arbitrary) via Generically (Tree a)

-- * Operations

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = Node l y r -- duplicate: ignore

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

-- * Main

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tree"
      [ testGroup
          "fromList"
          [ testProperty "sort . toList . fromList == sort . nub" $
              \(xs :: [Int]) ->
                sort (toList (fromList xs)) == sort (nub xs)
          , testProperty "elem x (fromList xs) == elem x xs" $
              \(x :: Int, xs :: [Int]) ->
                (x `elem` fromList xs) == elem x xs
          ]
      , testGroup
          "Foldable"
          [ testProperty "sum t == sum (toList t)" $
              \(t :: Tree Int) ->
                sum t == sum t
          , testProperty "product t == product (toList t)" $
              \(t :: Tree Int) ->
                product t == product t
          , testProperty "length t == length (toList t)" $
              \(t :: Tree Int) ->
                length t == length t
          , testProperty "null t == null (toList t)" $
              \(t :: Tree Int) ->
                null t == null t
          ]
      , testGroup
          "insert"
          [ testProperty "inserted element is found" $
              \(x :: Int, xs :: [Int]) ->
                x `elem` insert x (fromList xs)
          , testProperty "insert doesn't remove elements" $
              \(x :: Int, xs :: [Int]) ->
                all (`elem` insert x (fromList xs)) xs
          , testProperty "insert is idempotent up to sorted toList" $
              \(x :: Int, xs :: [Int]) ->
                let t = fromList xs
                 in sort (toList (insert x (insert x t))) == sort (toList (insert x t))
          ]
      , testProperty "first cases match documentation in main module" $
          take 4 (getTestCases arbitrary :: [Tree Int]) == [Leaf, Node Leaf 0 Leaf, Node (Node Leaf 0 Leaf) 0 Leaf, Node Leaf (-1) Leaf]
      ]
