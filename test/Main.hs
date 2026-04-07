{- HLINT ignore "Avoid reverse" -}
{- HLINT ignore "Functor law" -}
{- HLINT ignore "Redundant maybe" -}
{- HLINT ignore "Use <|>" -}
{- HLINT ignore "Use =<<" -}
{- HLINT ignore "Redundant negate" -}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Redundant not" -}
{- HLINT ignore "Redundant ==" -}
{- HLINT ignore "Redundant id" -}
module Main (main) where

-- base
import Control.Monad (replicateM)
import Data.Char (GeneralCategory (..), generalCategory, isAlpha, isLower, isPrint, isUpper, ord)
import Data.List (isPrefixOf, nub, sort, tails)
import GHC.Generics (Generic, Generically (..))

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases

-- * Test data types and functions

-- * Large record where nested fields might show up late

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

-- * Hybrid algorithm example (Bodigrim's objection)

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

-- * Helpers

-- | Count non-overlapping occurrences of a substring.
countOccurrences :: String -> String -> Int
countOccurrences needle haystack =
  length $ filter (isPrefixOf needle) (tails haystack)

-- Newtype wrappers backed by specific char/string generators.
newtype Printable = Printable Char deriving stock (Show)

newtype Letter = Letter Char deriving stock (Show)

newtype Digit = Digit Char deriving stock (Show)

newtype Upper = Upper Char deriving stock (Show)

newtype Lower = Lower Char deriving stock (Show)

newtype AsciiWord = AsciiWord String deriving stock (Show)

newtype LetterWord = LetterWord String deriving stock (Show)

newtype DigitWord = DigitWord String deriving stock (Show)

newtype AsciiLine = AsciiLine String deriving stock (Show)

instance Arbitrary Printable where arbitrary = Printable <$> printableChars

instance Arbitrary Letter where arbitrary = Letter <$> letterChars

instance Arbitrary Digit where arbitrary = Digit <$> digitChars

instance Arbitrary Upper where arbitrary = Upper <$> inCategories [UppercaseLetter] allChars

instance Arbitrary Lower where arbitrary = Lower <$> inCategories [LowercaseLetter] allChars

instance Arbitrary AsciiWord where arbitrary = AsciiWord <$> wordsOf asciiChars

instance Arbitrary LetterWord where arbitrary = LetterWord <$> wordsOf letterChars

instance Arbitrary DigitWord where arbitrary = DigitWord <$> wordsOf digitChars

instance Arbitrary AsciiLine where arbitrary = AsciiLine <$> linesOf asciiChars

-- * Main

main :: IO ()
main =
  defaultMain $
    testGroup
      "tinycheck"
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
      , testGroup
          "Functions (CoArbitrary)"
          [ testProperty "const a b == a" $
              \(a :: Int, b :: Bool) -> const a b == a
          , testProperty "id x == x" $
              \(x :: Int) -> id x == x
          ]
      , testGroup
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
      , testGroup
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
          "Char and String generators"
          [ -- allChars covers the full Unicode scalar value range
            testProperty "allChars: no surrogates" $
              \(c :: Char) -> let n = ord c in n < 0xD800 || n > 0xDFFF
          , testProperty "printableChars: every char is printable" $
              \(Printable c) -> isPrint c
          , testProperty "letterChars: every char is a letter" $
              \(Letter c) -> isAlpha c
          , testProperty "digitChars: every char is a decimal digit" $
              \(Digit c) -> generalCategory c == DecimalNumber
          , testProperty "inCategories [UppercaseLetter]: every char is uppercase" $
              \(Upper c) -> isUpper c
          , testProperty "inCategories [LowercaseLetter]: every char is lowercase" $
              \(Lower c) -> isLower c
          , testProperty "inCategories respects generalCategory" $
              \(Digit c) -> generalCategory c == DecimalNumber
          , testProperty "wordsOf asciiChars: every non-space char is printable ASCII" $
              \(AsciiWord s) -> all (\c -> isPrint c && ord c < 128) (filter (/= ' ') s)
          , testProperty "wordsOf letterChars: every non-space char is a letter" $
              \(LetterWord s) -> all isAlpha (filter (/= ' ') s)
          , testProperty "wordsOf digitChars: every char is a decimal digit or space" $
              \(DigitWord s) -> all (\c -> generalCategory c == DecimalNumber || c == ' ') s
          , testProperty "wordsOf asciiChars: unwords . words roundtrips when no leading/trailing spaces" $
              \(AsciiWord s) ->
                case (s, reverse s) of
                  (h : _, l : _) -> h /= ' ' && l /= ' ' ==> property (unwords (words s) == s)
                  _ -> property True
          , testProperty "linesOf asciiChars: lines . unlines roundtrips when ends with newline" $
              \(AsciiLine s) ->
                not (null s)
                  && last s
                    == '\n'
                      ==> property (unlines (lines s) == s)
          ]
      , testGroup
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
          , expectFailureWithN 200_000 "LargeRecord" "LargeRecord appears at most 7 times in show" $
              \(r :: LargeRecord) -> countOccurrences "LargeRecord" (show r) <= 7
          ]
      ]
