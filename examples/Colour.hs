{-# LANGUAGE DeriveGeneric #-}

-- | Example from the README: deriving 'Arbitrary' via 'Generically'.
module Main where

import Data.TestCases (Arbitrary, TestCases, arbitrary, getTestCases)
import GHC.Generics (Generic, Generically (..))

data Colour = Red | Green | Blue
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via Generically Colour

{- | Print all generated 'Colour' test cases, demonstrating that deriving
'Arbitrary' via 'Generically' enumerates all constructors.
-}
main :: IO ()
main = mapM_ print (getTestCases (arbitrary :: TestCases Colour))
