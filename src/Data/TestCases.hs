{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A lightweight enumeration-based property testing library.

Instead of random generation, 'TestCases' is a deterministic, ordered list
of test inputs.  The 'Semigroup' instance interleaves two lists so that
both finite and infinite generators compose fairly (neither starves the other).
-}
module Data.TestCases (
  -- * TestCases
  TestCases (..),
  testCase,
  interleaveN,

  -- * Running tests (plain IO)
  test,
  testWithMsg,

  -- * Arbitrary
  Arbitrary (..),

  -- ** Newtype wrappers for deriving
  SignedArbitrary (..),
  BoundedArbitrary (..),
  RealFracArbitrary (..),

  -- ** String generators
  string,
  atLeast,
  wordsOf,
  linesOf,

  -- ** Char generators
  allChars,
  commonASCIIChars,
  printableChars,
  letterChars,
  digitChars,
  inCategories,

  -- ** Newtype wrappers for common char/string generators
  Printable (..),
  Letter (..),
  Digit (..),
  Upper (..),
  Lower (..),
  AsciiWord (..),
  LetterWord (..),
  DigitWord (..),
  AsciiLine (..),

  -- * CoArbitrary
  CoArbitrary (..),

  -- ** Newtype wrappers for deriving
  OrdCoArbitrary (..),
  IntegralCoArbitrary (..),
  EnumCoArbitrary (..),
  RealFracCoArbitrary (..),
)
where

-- base
import Control.Monad (ap, forM_, replicateM, unless)
import Data.Char (GeneralCategory (..), chr, generalCategory)
import Data.Coerce (coerce)
import Data.Complex (Complex (..))
import Data.Fixed (Fixed (..), HasResolution)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as FP (Product (..))
import Data.Functor.Sum qualified as FS (Sum (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (All (..), Alt (..), Any (..), Ap (..), Dual (..), Endo (..), First (..), Last (..))
import Data.Monoid qualified as Monoid (Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, (%))
import Data.Semigroup qualified as Semigroup (Arg (..), First (..), Last (..), Max (..), Min (..), WrappedMonoid (..))
import Data.Version (Version (..))
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))

-- * TestCases

{- | An ordered collection of test inputs.

'TestCases' is a newtype over a list, but its 'Semigroup', 'Applicative',
and 'Monad' instances differ from those of ordinary lists: instead of
concatenation and cartesian product, they use /fair interleaving/.

=== Why interleaving?

Property tests often combine several generators.  With plain list
concatenation, @xs <> ys@ visits every element of @xs@ before touching
@ys@: if @xs@ is infinite, @ys@ is never reached.  Interleaving
alternates between the two sources, so both infinite generators are
explored fairly.

=== How interleaving works

@'TestCases' [1,2,3] '<>' 'TestCases' [10,20,30]@
produces @[1,10,2,20,3,30]@.
If one side is longer the remainder is appended:

> TestCases [1,2,3] <> TestCases [10,20] == TestCases [1,10,2,20,3]
> TestCases [1,2]   <> TestCases [10,20,30] == TestCases [1,10,2,20,30]

With two infinite sides, both are explored fairly:

> (Left <$> TestCases [1..]) <> (Right <$> TestCases [1..])
>   == TestCases [Left 1, Right 1, Left 2, Right 2, Left 3, Right 3, ...]

In practice, this means that all different /shapes/ of data can be explored early,
before all values of one shape are exhausted.

=== Applicative and Monad

'Applicative' is derived from 'Monad' via @('<*>') = 'ap'@, and 'Monad'
bind is @'foldMap'@, which accumulates results with '<>'.  This means
@('<*>')@ interleaves the results of applying each function to each
argument, rather than producing a full cartesian product in lexicographic
order.  Compare:

> -- Plain list: exhausts the first argument before moving to the next
> (,) <$> "abc" <*> [1,2,3,4,5,6]
>   == [('a',1),('a',2),('a',3),('a',4),('a',5),('a',6),
>       ('b',1),('b',2),('b',3),('b',4),('b',5),('b',6),
>       ('c',1),('c',2),('c',3),('c',4),('c',5),('c',6)]
>
> -- TestCases: interleaves, so all three characters appear immediately
> (,) <$> TestCases "abc" <*> TestCases [1,2,3,4,5,6]
>   == TestCases
>        [('a',1),('b',1),('a',2),('c',1),('a',3),('b',2),
>         ('a',4),('c',2),('a',5),('b',3),('a',6),('c',3),
>         ('b',4),('c',4),('b',5),('c',5),('b',6),('c',6)]

=== Laws and why they don\'t hold — and why that\'s fine

The 'Semigroup' associativity law (@(a '<>' b) '<>' c == a '<>' (b '<>' c)@)
does /not/ hold for 'TestCases': interleaving is not associative in general.
For example:

> (TestCases [1] <> TestCases [2]) <> TestCases [3]
>   == TestCases [1,2] <> TestCases [3]
>   == TestCases [1,3,2]
>
> TestCases [1] <> (TestCases [2] <> TestCases [3])
>   == TestCases [1] <> TestCases [2,3]
>   == TestCases [1,2,3]

Consequently the 'Applicative' and 'Monad' laws (which depend on associativity of bind) also fail.

In practice this does not matter much: a property test only cares whether /all/
generated cases pass, not about the order in which they are visited.
Any permutation of the input list yields the same test outcome.  The interleaving
strategy is chosen purely to ensure that each case is visited early and no generator is starved,
not to produce any canonical ordering.

You can use this fact to your advantage:
if you want to visit some cases earlier,
merge them into your generator last.
For example, in @(a '<>' b) '<>' c@, cases from @c@ are visited at double the speed compared to each @a@ and @b@.
-}
newtype TestCases a = TestCases {getTestCases :: [a]}
  deriving newtype (Show, Foldable, Functor)

-- | Lift a single value into 'TestCases'.
testCase :: a -> TestCases a
testCase = TestCases . pure

{- | Fairly interleave any number of 'TestCases'.

Each round takes one element from each non-empty source in order,
then repeats with the remaining tails.  Empty sources are skipped.
This generalises '<>' from two sources to @n@ sources:

> interleaveN [TestCases [1,2,3], TestCases [10,20,30], TestCases [100,200,300]]
>   == TestCases [1,10,100, 2,20,200, 3,30,300]

Works correctly when sources have different lengths or are infinite:

> interleaveN [TestCases [1,2], TestCases [10,20,30], TestCases [100]]
>   == TestCases [1,10,100, 2,20, 30]

@'interleaveN' [a, b] == a '<>' b@.
-}
interleaveN :: [TestCases a] -> TestCases a
interleaveN = TestCases . go . fmap getTestCases
  where
    go [] = []
    go xss =
      let (heads, tails') = foldr collect ([], []) xss
       in (heads <> go tails')
    collect [] (hs, ts) = (hs, ts)
    collect (x : xs) (hs, ts) = (x : hs, xs : ts)

{- | Interleave two 'TestCases' so that neither side starves the other.
This is the key operation: it lets us combine two (potentially infinite) generators and still visit cases from both.

The left-hand element is always emitted first, then the sources alternate:

> TestCases [a1,a2,...] <> TestCases [b1,b2,...] = TestCases [a1,b1,a2,b2,...]

See the documentation of 'TestCases' for a discussion of the law violations
this entails and why they are harmless.
-}
instance Semigroup (TestCases a) where
  TestCases as1 <> TestCases as2 = TestCases $ mingle as1 as2
    where
      mingle [] bs = bs
      mingle (a : as') bs = a : mingle bs as'

instance Monoid (TestCases a) where
  mempty = TestCases []

instance Applicative TestCases where
  pure = testCase
  (<*>) = ap

{- | Bind via 'foldMap', which accumulates results with the interleaving '<>'.

This means @xs '>>=' f@ visits the outputs of @f@ applied to each element of @xs@ in an interleaved fashion,
rather than fully exhausting @f x1@ before starting @f x2@.
As a result infinite generators compose without starvation.
-}
instance Monad TestCases where
  as >>= f = foldMap f as

-- | Generate lists of at least @n@ elements.
atLeast :: (Arbitrary a) => Int -> TestCases [a]
atLeast n = (<>) <$> replicateM n arbitrary <*> arbitrary

-- * Running tests (plain IO)

{- | Run a named test over (up to) 10 million generated cases.

Throws an error on the first failure, printing the failing input and a debug string.
For proper integration into a testing framework, see "Test.Tasty.TinyCheck".
-}
testWithMsg :: (Show a, Arbitrary a) => String -> (a -> (Bool, String)) -> IO ()
testWithMsg msg f = do
  forM_ (take 10_000_000 $ getTestCases arbitrary) $ \a ->
    let (passed, dbg) = f a
     in unless passed $
          error $
            unlines
              [ "Test failure:"
              , msg
              , show a
              , dbg
              ]
  putStrLn $ "Test passed: " <> msg

-- | Like 'testWithMsg', but accepts a pure predicate with no debug string.
test :: (Show a, Arbitrary a) => String -> (a -> Bool) -> IO ()
test msg f = testWithMsg msg (\a -> (f a, ""))

-- * Arbitrary

{- | Class of types that have a canonical enumeration of test cases.

For any type with a 'GHC.Generics.Generic' instance, you can derive 'Arbitrary' for free via 'Generically':

@
data Colour = Red | Green | Blue deriving ('GHC.Generics.Generic')

deriving via 'Generically' Colour instance 'Arbitrary' Colour
-- generates: TestCases [Red, Green, Blue]
@

For types with fields, the fields\' generators are interleaved fairly:

@
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving ('GHC.Generics.Generic')

deriving via 'Generically' (Tree a) instance ('Arbitrary' a) => 'Arbitrary' (Tree a)
-- generates: Leaf, Node Leaf 0 Leaf, Node (Node Leaf 0 Leaf) 0 Leaf, Node Leaf (-1) Leaf, ...
@
-}
class Arbitrary a where
  arbitrary :: TestCases a

-- ** Arbitrary newtype wrappers for deriving

{- | Derive 'Arbitrary' for any @'Num' a@ that is also @'Enum'@ by
interleaving non-negatives @[0..]@ with negatives @[1..]@ negated.
Hits small values first, covering both sides of zero fairly.
-}
newtype SignedArbitrary a = SignedArbitrary a

instance (Num a, Enum a) => Arbitrary (SignedArbitrary a) where
  arbitrary = coerce (TestCases [0 ..] <> (negate <$> TestCases [1 ..]) :: TestCases a)

-- | Derive 'Arbitrary' for any @'Bounded'@ @'Enum'@ type by enumerating all values from 'minBound' upward.
newtype BoundedArbitrary a = BoundedArbitrary a

instance (Bounded a, Enum a) => Arbitrary (BoundedArbitrary a) where
  arbitrary = coerce (TestCases [minBound ..] :: TestCases a)

{- | Derive 'Arbitrary' for any 'Fractional' type by delegating to 'Arbitrary' @('Ratio' 'Integer')@
and converting each rational via 'fromRational'.
This enumerates all rationals via a Cantor diagonal, so every rational value reachable by the type is eventually tested.
-}
newtype RealFracArbitrary a = RealFracArbitrary a

instance (Fractional a) => Arbitrary (RealFracArbitrary a) where
  arbitrary = RealFracArbitrary . fromRational <$> arbitrary

-- Numeric instances
-- Signed integer types: interleave non-negatives and negatives.
deriving via SignedArbitrary Int instance Arbitrary Int

deriving via SignedArbitrary Integer instance Arbitrary Integer

deriving via SignedArbitrary Int8 instance Arbitrary Int8

deriving via SignedArbitrary Int16 instance Arbitrary Int16

deriving via SignedArbitrary Int32 instance Arbitrary Int32

deriving via SignedArbitrary Int64 instance Arbitrary Int64

-- Unsigned types enumerate from 'minBound'.
deriving via BoundedArbitrary Word instance Arbitrary Word

deriving via BoundedArbitrary Word8 instance Arbitrary Word8

deriving via BoundedArbitrary Word16 instance Arbitrary Word16

deriving via BoundedArbitrary Word32 instance Arbitrary Word32

deriving via BoundedArbitrary Word64 instance Arbitrary Word64

-- 'Natural' has no 'Bounded', so enumerate directly from 0.
instance Arbitrary Natural where
  arbitrary = TestCases [0 ..]

-- Floating-point types and rationals: use the 'RealFracArbitrary' wrapper.
deriving via RealFracArbitrary Float instance Arbitrary Float

deriving via RealFracArbitrary Double instance Arbitrary Double

{- | Enumerate all ratios via a Cantor diagonal over @(p, q)@ pairs with @q > 0@,
interleaved with their negatives and zero.
Every ratio @p '%' q@ with @p, q@ reachable by 'Arbitrary' @a@ appears in finite time.
-}
instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
  arbitrary =
    testCase 0 <> do
      n <- TestCases [1 ..]
      q <- fromInteger <$> TestCases [1 .. n]
      let p = fromInteger n - q + 1
      testCase (p % q) <> testCase (negate (p % q))

-- Char and String

{- | All Unicode scalar values in order: U+0000 .. U+D7FF, U+E000 .. U+10FFFF.
This is the most general 'Char' generator and is used for the 'Arbitrary' instance.
-}
allChars :: TestCases Char
allChars = TestCases $ fmap chr ([0 .. 0xD7FF] <> [0xE000 .. 0x10FFFF])

-- | Filter a 'Char' generator to only those characters whose Unicode 'GeneralCategory' is in the supplied list.
inCategories :: [GeneralCategory] -> TestCases Char -> TestCases Char
inCategories cats (TestCases cs) = TestCases $ filter ((`elem` cats) . generalCategory) cs

{- | ASCII letters and digits plus common punctuation and whitespace.
Useful for testing with printable ASCII.
-}
commonASCIIChars :: TestCases Char
commonASCIIChars = TestCases $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

{- | All printable Unicode characters (letters, marks, numbers, punctuation,
symbols, and space separators).
-}
printableChars :: TestCases Char
printableChars =
  inCategories
    [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    , ModifierLetter
    , OtherLetter
    , NonSpacingMark
    , SpacingCombiningMark
    , EnclosingMark
    , DecimalNumber
    , LetterNumber
    , OtherNumber
    , ConnectorPunctuation
    , DashPunctuation
    , OpenPunctuation
    , ClosePunctuation
    , InitialQuote
    , FinalQuote
    , OtherPunctuation
    , MathSymbol
    , CurrencySymbol
    , ModifierSymbol
    , OtherSymbol
    , Space
    ]
    allChars

-- | Only Unicode letters ('UppercaseLetter', 'LowercaseLetter', etc.).
letterChars :: TestCases Char
letterChars =
  inCategories
    [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter]
    allChars

-- | Only Unicode decimal digits.
digitChars :: TestCases Char
digitChars = inCategories [DecimalNumber] allChars

-- ** Newtype wrappers for common char\/string generators

{- | A printable Unicode character.
Useful when a test only cares that a character is printable.
-}
newtype Printable = Printable Char deriving stock (Show)

instance Arbitrary Printable where arbitrary = Printable <$> printableChars

-- | A Unicode letter ('UppercaseLetter', 'LowercaseLetter', etc.).
newtype Letter = Letter Char deriving stock (Show)

instance Arbitrary Letter where arbitrary = Letter <$> letterChars

-- | A Unicode decimal digit.
newtype Digit = Digit Char deriving stock (Show)

instance Arbitrary Digit where arbitrary = Digit <$> digitChars

-- | An uppercase Unicode letter.
newtype Upper = Upper Char deriving stock (Show)

instance Arbitrary Upper where arbitrary = Upper <$> inCategories [UppercaseLetter] allChars

-- | A lowercase Unicode letter.
newtype Lower = Lower Char deriving stock (Show)

instance Arbitrary Lower where arbitrary = Lower <$> inCategories [LowercaseLetter] allChars

{- | A string of words drawn from 'commonASCIIChars'.
Words are separated by spaces; no leading or trailing space is guaranteed.
-}
newtype AsciiWord = AsciiWord String deriving stock (Show)

instance Arbitrary AsciiWord where arbitrary = AsciiWord <$> wordsOf commonASCIIChars

{- | A string of words made up of Unicode letters.
Words are separated by spaces.
-}
newtype LetterWord = LetterWord String deriving stock (Show)

instance Arbitrary LetterWord where arbitrary = LetterWord <$> wordsOf letterChars

{- | A string of words made up of Unicode decimal digits.
Words are separated by spaces.
-}
newtype DigitWord = DigitWord String deriving stock (Show)

instance Arbitrary DigitWord where arbitrary = DigitWord <$> wordsOf digitChars

{- | A multi-line string drawn from 'commonASCIIChars'.
Useful for testing parsers and text-processing functions.
-}
newtype AsciiLine = AsciiLine String deriving stock (Show)

instance Arbitrary AsciiLine where arbitrary = AsciiLine <$> linesOf commonASCIIChars

instance Arbitrary Char where
  arbitrary = allChars

{- | Generate strings of words drawn from the given character generator,
including single words, multi-word phrases (@'unwords'@), and all lengths.
-}
wordsOf :: TestCases Char -> TestCases String
wordsOf chars = stringsOf <> (unwords <$> replicateM 2 stringsOf) <> foldMap (\k -> unwords <$> replicateM k stringsOf) [3 ..]
  where
    stringsOf = foldMap (`replicateM` chars) [1 ..]

{- | Generate multi-line strings drawn from the given character generator,
including everything 'wordsOf' produces plus multi-line documents
(@'unlines' . fmap 'unwords'@).
-}
linesOf :: TestCases Char -> TestCases String
linesOf chars = wordsOf chars <> foldMap (\k -> unlines . fmap unwords <$> replicateM k (replicateM 2 (wordsOf chars))) [1 ..]

-- | A 'String' generator using 'allChars': single strings, multi-word, and multi-line strings.
string :: TestCases String
string = linesOf allChars

-- Other base types
instance Arbitrary () where arbitrary = pure ()

deriving via Generically Bool instance Arbitrary Bool

deriving via Generically Ordering instance Arbitrary Ordering

-- Container instances
deriving via Generically [a] instance (Arbitrary a) => Arbitrary [a]

deriving via Generically (Maybe a) instance (Arbitrary a) => Arbitrary (Maybe a)

deriving via Generically (Either a b) instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)

-- Tuple instances
deriving via Generically (a, b) instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b)

deriving via Generically (a, b, c) instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c)

deriving via Generically (a, b, c, d) instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (a, b, c, d)

deriving via Generically (a, b, c, d, e) instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e) => Arbitrary (a, b, c, d, e)

-- Function instance
instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = coArbitrary

{- | Route 'Arbitrary' through the generic representation.
Use @deriving ('Arbitrary') via 'Generically' MyType@ to get a free instance
for any 'Generic' type.
-}
instance (Generic a, Arbitrary (Rep a ())) => Arbitrary (Generically a) where
  arbitrary = fmap (Generically . to) (arbitrary :: TestCases (Rep a ()))

instance (Arbitrary (fL x), Arbitrary (fR x)) => Arbitrary ((fL :+: fR) x) where
  arbitrary = (L1 <$> arbitrary) <> (R1 <$> arbitrary)

instance (Arbitrary (fL x), Arbitrary (fR x)) => Arbitrary ((fL :*: fR) x) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance (Arbitrary (f x)) => Arbitrary (M1 i t f x) where
  arbitrary = M1 <$> arbitrary

instance (Arbitrary a) => Arbitrary (K1 i a x) where
  arbitrary = K1 <$> arbitrary

instance Arbitrary (U1 x) where
  arbitrary = pure U1

-- | 'V1' is the empty type (no constructors), so it has no test cases.
instance Arbitrary (V1 x) where
  arbitrary = TestCases []

{- | Derive 'CoArbitrary' for any @'Ord' a@ by doing a three-way split on @'compare' argument pivot@ for each generated pivot.
This means the generated function can return different outputs for values below, equal to, or above the pivot.
-}
newtype OrdCoArbitrary a = OrdCoArbitrary a

instance (Ord a, Arbitrary a) => CoArbitrary (OrdCoArbitrary a) where
  coArbitrary = do
    pivot <- coerce <$> (arbitrary :: TestCases a)
    lt <- arbitrary
    eq <- arbitrary
    gt <- arbitrary
    pure $ \(OrdCoArbitrary x) -> case compare x pivot of
      LT -> lt
      EQ -> eq
      GT -> gt

{- | Derive 'CoArbitrary' for any @'Integral' a@ by splitting on sign
and on odd\/even parity, giving four independent outcome branches.
This captures both the sign structure and fine-grained parity of integers.
-}
newtype IntegralCoArbitrary a = IntegralCoArbitrary a

instance (Integral a, Arbitrary a) => CoArbitrary (IntegralCoArbitrary a) where
  coArbitrary = do
    posEven <- arbitrary
    posOdd <- arbitrary
    negEven <- arbitrary
    negOdd <- arbitrary
    zero <- arbitrary
    pure $ \(IntegralCoArbitrary n) -> case (compare n 0, even n) of
      (EQ, _) -> zero
      (GT, True) -> posEven
      (GT, False) -> posOdd
      (LT, True) -> negEven
      (LT, False) -> negOdd

{- | Derive 'CoArbitrary' for any @'Enum' a@ by converting to 'Int' via
'fromEnum' and then splitting on sign\/parity via 'IntegralCoArbitrary'.
Suitable for small bounded enums such as 'Char'.
-}
newtype EnumCoArbitrary a = EnumCoArbitrary a

instance (Enum a, Arbitrary a) => CoArbitrary (EnumCoArbitrary a) where
  coArbitrary = do
    f <- coArbitrary -- TestCases (IntegralCoArbitrary Int -> b)
    pure $ \(EnumCoArbitrary x) -> f (IntegralCoArbitrary (fromEnum x))

-- | Derive 'CoArbitrary' for any @'RealFrac' a@ by splitting on sign and whether the value is @< 1@, @== 1@, or @> 1@, giving six branches.
newtype RealFracCoArbitrary a = RealFracCoArbitrary a

instance (RealFrac a, Arbitrary a) => CoArbitrary (RealFracCoArbitrary a) where
  coArbitrary = do
    negSmall <- arbitrary -- x < -1
    negOne <- arbitrary -- x == -1
    negFrac <- arbitrary -- -1 < x < 0
    zero <- arbitrary -- x == 0
    posFrac <- arbitrary -- 0 < x < 1
    posOne <- arbitrary -- x == 1
    posLarge <- arbitrary -- x > 1
    pure $ \(RealFracCoArbitrary x) ->
      if
        | x < -1 -> negSmall
        | x == -1 -> negOne
        | x < 0 -> negFrac
        | x == 0 -> zero
        | x < 1 -> posFrac
        | x == 1 -> posOne
        | otherwise -> posLarge

{- | An instance @'CoArbitrary' a@ provides a 'TestCases' of functions @a -> b@ for any @'Arbitrary' b@,
by case-splitting on the structure of @a@.
-}
class CoArbitrary a where
  coArbitrary :: (Arbitrary b) => TestCases (a -> b)

-- Base CoArbitrary instances; all use the newtype wrappers above rather than
-- 'const', so that generated functions actually vary on their argument.
instance CoArbitrary () where
  coArbitrary = (\b () -> b) <$> arbitrary

instance CoArbitrary Bool where
  coArbitrary = do
    t <- arbitrary
    f <- arbitrary
    pure $ \case
      True -> t
      False -> f

instance CoArbitrary Ordering where
  coArbitrary = do
    lt <- arbitrary
    eq <- arbitrary
    gt <- arbitrary
    pure $ \case
      LT -> lt
      EQ -> eq
      GT -> gt

-- Integral types: split on sign × parity via 'IntegralCoArbitrary',
-- also combined with 'OrdCoArbitrary' for pivot-based splitting.
deriving via IntegralCoArbitrary Int instance CoArbitrary Int

deriving via IntegralCoArbitrary Integer instance CoArbitrary Integer

deriving via IntegralCoArbitrary Int8 instance CoArbitrary Int8

deriving via IntegralCoArbitrary Int16 instance CoArbitrary Int16

deriving via IntegralCoArbitrary Int32 instance CoArbitrary Int32

deriving via IntegralCoArbitrary Int64 instance CoArbitrary Int64

deriving via IntegralCoArbitrary Word instance CoArbitrary Word

deriving via IntegralCoArbitrary Word8 instance CoArbitrary Word8

deriving via IntegralCoArbitrary Word16 instance CoArbitrary Word16

deriving via IntegralCoArbitrary Word32 instance CoArbitrary Word32

deriving via IntegralCoArbitrary Word64 instance CoArbitrary Word64

-- Natural has no negatives so OrdCoArbitrary is the right fit.
deriving via OrdCoArbitrary Natural instance CoArbitrary Natural

-- Floating types and rationals: split on sign × magnitude via 'RealFracCoArbitrary'.
deriving via RealFracCoArbitrary Float instance CoArbitrary Float

deriving via RealFracCoArbitrary Double instance CoArbitrary Double

deriving via RealFracCoArbitrary (Ratio Integer) instance CoArbitrary (Ratio Integer)

deriving via RealFracCoArbitrary (Ratio Int) instance CoArbitrary (Ratio Int)

-- Char: an enum, so use 'EnumCoArbitrary'.
deriving via EnumCoArbitrary Char instance CoArbitrary Char

-- Container CoArbitrary instances via Generically
deriving via Generically [a] instance (CoArbitrary a) => CoArbitrary [a]

deriving via Generically (Maybe a) instance (CoArbitrary a) => CoArbitrary (Maybe a)

deriving via Generically (Either a b) instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)

-- Tuple CoArbitrary instances via Generically
deriving via Generically (a, b) instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a, b)

deriving via Generically (a, b, c) instance (CoArbitrary a, CoArbitrary b, CoArbitrary c) => CoArbitrary (a, b, c)

deriving via Generically (a, b, c, d) instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d) => CoArbitrary (a, b, c, d)

{- | Route 'CoArbitrary' through the generic representation.
Use @deriving ('CoArbitrary') via 'Generically' MyType@ for any 'Generic' type.
-}
instance (Generic a, CoArbitrary (Rep a ())) => CoArbitrary (Generically a) where
  coArbitrary = fmap (. (\(Generically a) -> (from a :: Rep a ()))) coArbitrary

instance CoArbitrary (U1 x) where
  coArbitrary = (\b U1 -> b) <$> arbitrary

-- | 'V1' is uninhabited; a function from it is trivially total.
instance CoArbitrary (V1 x) where
  coArbitrary = pure (\case {})

instance (CoArbitrary (f x)) => CoArbitrary (M1 i c f x) where
  coArbitrary = (. unM1) <$> coArbitrary

instance (CoArbitrary (fL x), CoArbitrary (fR x)) => CoArbitrary ((fL :+: fR) x) where
  coArbitrary = do
    fL <- coArbitrary
    fR <- coArbitrary
    pure $ \case
      L1 l -> fL l
      R1 r -> fR r

-- | A function from a product @(fL :*: fR)@ is isomorphic to a curried function @fL -> fR -> b@.
instance (CoArbitrary (fL x), CoArbitrary (fR x)) => CoArbitrary ((fL :*: fR) x) where
  coArbitrary = do
    f <- coArbitrary -- TestCases (fL x -> fR x -> b)
    pure $ \(l :*: r) -> f l r

instance (CoArbitrary a) => CoArbitrary (K1 i a x) where
  coArbitrary = (. unK1) <$> coArbitrary

-- * Instances for base datatypes

--
-- Instances for 'Generic' types are derived via 'Generically'.
-- Instances for types without 'Generic' are written manually.

-- ** Data.Void

-- | 'Void' is uninhabited; there are no test cases and functions from it are trivial.
deriving via Generically Void instance Arbitrary Void

deriving via Generically Void instance CoArbitrary Void

-- ** Data.Proxy

deriving via Generically (Proxy t) instance Arbitrary (Proxy t)

deriving via Generically (Proxy t) instance CoArbitrary (Proxy t)

-- ** Data.Ord

deriving via Generically (Down a) instance (Arbitrary a) => Arbitrary (Down a)

deriving via Generically (Down a) instance (CoArbitrary a) => CoArbitrary (Down a)

-- ** Data.List.NonEmpty

deriving via Generically (NonEmpty a) instance (Arbitrary a) => Arbitrary (NonEmpty a)

deriving via Generically (NonEmpty a) instance (CoArbitrary a) => CoArbitrary (NonEmpty a)

-- ** System.Exit

deriving via Generically ExitCode instance Arbitrary ExitCode

deriving via Generically ExitCode instance CoArbitrary ExitCode

-- ** Data.Complex

deriving via Generically (Complex a) instance (Arbitrary a) => Arbitrary (Complex a)

deriving via Generically (Complex a) instance (CoArbitrary a) => CoArbitrary (Complex a)

-- ** Data.Version

deriving via Generically Version instance Arbitrary Version

deriving via Generically Version instance CoArbitrary Version

-- ** Data.Functor.Identity

deriving via Generically (Identity a) instance (Arbitrary a) => Arbitrary (Identity a)

deriving via Generically (Identity a) instance (CoArbitrary a) => CoArbitrary (Identity a)

-- ** Data.Functor.Const

deriving via Generically (Const a b) instance (Arbitrary a) => Arbitrary (Const a b)

deriving via Generically (Const a b) instance (CoArbitrary a) => CoArbitrary (Const a b)

-- ** Data.Functor.Compose

deriving via
  Generically (Compose f g a)
  instance
    (Arbitrary (f (g a))) => Arbitrary (Compose f g a)

deriving via
  Generically (Compose f g a)
  instance
    (CoArbitrary (f (g a))) => CoArbitrary (Compose f g a)

-- ** Data.Functor.Product

deriving via
  Generically (FP.Product f g a)
  instance
    (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (FP.Product f g a)

deriving via
  Generically (FP.Product f g a)
  instance
    (CoArbitrary (f a), CoArbitrary (g a)) => CoArbitrary (FP.Product f g a)

-- ** Data.Functor.Sum (functor-level, distinct from numeric Sum)

deriving via
  Generically (FS.Sum f g a)
  instance
    (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (FS.Sum f g a)

deriving via
  Generically (FS.Sum f g a)
  instance
    (CoArbitrary (f a), CoArbitrary (g a)) => CoArbitrary (FS.Sum f g a)

-- ** Data.Monoid newtypes

deriving via Generically All instance Arbitrary All

deriving via Generically All instance CoArbitrary All

deriving via Generically Any instance Arbitrary Any

deriving via Generically Any instance CoArbitrary Any

deriving via Generically (Dual a) instance (Arbitrary a) => Arbitrary (Dual a)

deriving via Generically (Dual a) instance (CoArbitrary a) => CoArbitrary (Dual a)

deriving via Generically (Monoid.Sum a) instance (Arbitrary a) => Arbitrary (Monoid.Sum a)

deriving via Generically (Monoid.Sum a) instance (CoArbitrary a) => CoArbitrary (Monoid.Sum a)

deriving via Generically (Monoid.Product a) instance (Arbitrary a) => Arbitrary (Monoid.Product a)

deriving via Generically (Monoid.Product a) instance (CoArbitrary a) => CoArbitrary (Monoid.Product a)

deriving via Generically (First a) instance (Arbitrary a) => Arbitrary (First a)

deriving via Generically (First a) instance (CoArbitrary a) => CoArbitrary (First a)

deriving via Generically (Last a) instance (Arbitrary a) => Arbitrary (Last a)

deriving via Generically (Last a) instance (CoArbitrary a) => CoArbitrary (Last a)

-- | 'Endo' wraps a function @a -> a@; 'Arbitrary' requires 'CoArbitrary' and 'Arbitrary' on @a@.
deriving via
  Generically (Endo a)
  instance
    (CoArbitrary a, Arbitrary a) => Arbitrary (Endo a)

-- | 'CoArbitrary' for 'Endo' requires the caller to supply 'CoArbitrary' for @a -> a@.
deriving via
  Generically (Endo a)
  instance
    (CoArbitrary (a -> a)) => CoArbitrary (Endo a)

deriving via Generically (Alt f a) instance (Arbitrary (f a)) => Arbitrary (Alt f a)

deriving via Generically (Alt f a) instance (CoArbitrary (f a)) => CoArbitrary (Alt f a)

deriving via Generically (Ap f a) instance (Arbitrary (f a)) => Arbitrary (Ap f a)

deriving via Generically (Ap f a) instance (CoArbitrary (f a)) => CoArbitrary (Ap f a)

-- ** Data.Semigroup newtypes

deriving via Generically (Semigroup.Min a) instance (Arbitrary a) => Arbitrary (Semigroup.Min a)

deriving via Generically (Semigroup.Min a) instance (CoArbitrary a) => CoArbitrary (Semigroup.Min a)

deriving via Generically (Semigroup.Max a) instance (Arbitrary a) => Arbitrary (Semigroup.Max a)

deriving via Generically (Semigroup.Max a) instance (CoArbitrary a) => CoArbitrary (Semigroup.Max a)

deriving via Generically (Semigroup.First a) instance (Arbitrary a) => Arbitrary (Semigroup.First a)

deriving via Generically (Semigroup.First a) instance (CoArbitrary a) => CoArbitrary (Semigroup.First a)

deriving via Generically (Semigroup.Last a) instance (Arbitrary a) => Arbitrary (Semigroup.Last a)

deriving via Generically (Semigroup.Last a) instance (CoArbitrary a) => CoArbitrary (Semigroup.Last a)

deriving via Generically (Semigroup.Arg a b) instance (Arbitrary a, Arbitrary b) => Arbitrary (Semigroup.Arg a b)

deriving via Generically (Semigroup.Arg a b) instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Semigroup.Arg a b)

deriving via Generically (Semigroup.WrappedMonoid m) instance (Arbitrary m) => Arbitrary (Semigroup.WrappedMonoid m)

deriving via Generically (Semigroup.WrappedMonoid m) instance (CoArbitrary m) => CoArbitrary (Semigroup.WrappedMonoid m)

-- ** Data.Fixed

-- 'Fixed' has no 'Generic' instance. We reuse the 'Integer' generator via 'coerce'.
deriving via SignedArbitrary (Fixed a) instance (HasResolution a) => Arbitrary (Fixed a)

deriving via
  OrdCoArbitrary (Fixed a)
  instance
    (HasResolution a) => CoArbitrary (Fixed a)
