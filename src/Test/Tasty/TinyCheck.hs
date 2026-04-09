{- | Tasty integration for tinycheck.

Usage:

>>> import Test.Tasty
>>> import Test.Tasty.TinyCheck
>>> import Data.List (nub, reverse)
>>> :{
defaultMain $ testGroup "My suite"
[ testProperty "reverse . reverse == id" $
    \(xs :: [Int]) -> reverse (reverse xs) == xs
, testProperty "length . nub <= length" $
    \(xs :: [Int]) -> length (nub xs) <= length xs
, testProperty "manual generators" $ do
    xs <- forAllShow "xs" (arbitrary :: TestCases [Int])
    ys <- forAllShow "ys" (arbitrary :: TestCases [Int])
    assert $ length (xs ++ ys) == length xs + length ys
]
:}
-}
module Test.Tasty.TinyCheck (
  -- * Defining properties
  Property,
  property,
  assert,
  assertEqual,
  forAll,
  forAllWith,
  forAllShow,
  debug,
  (==>),

  -- * Checking properties
  Testable (..),

  -- * Creating test trees
  testProperty,
  testPropertyWith,
  expectFailureWith,
  expectFailureWithN,

  -- * Options
  TinyCheckTests (..),
)
where

-- base
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))

-- transformers
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer.Strict (WriterT (..), runWriter, writer)

-- tagged
import Data.Tagged (Tagged (..))

-- tasty
import Test.Tasty (localOption)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), lookupOption, safeRead)
import Test.Tasty.Providers (IsTest (..), TestName, TestTree, singleTest, testFailed, testPassed)
import Test.Tasty.Runners (Outcome (..), Result (..))

-- tinycheck
import Data.TestCases (Arbitrary, TestCases (..), arbitrary)

-- * Property

{- | Enumerate test cases, log, and assert conditions.

Build properties with @do@-notation:

> testProperty "commutativity" $ do
>   -- Enumerate test cases
>   x <- forAll generatorX
>   -- Log the generated case
>   y <- forAllShow "y" generatorY
>   -- Log debug information
>   debug $ "x + y = " <> show (x + y)
>   -- Assert the property holds for the generated case
>   assert $ x + y == (y :: Int) + x

For a plain 'Bool' or a function, you can pass it directly to 'testProperty' via the 'Testable' class.
-}
newtype Property a = Property (TestCases (Either String a, [String]))
  deriving (Functor, Applicative, Monad) via (ExceptT String (WriterT [String] TestCases))

instance MonadFail Property where
  fail = Property . pure . (,[]) . Left

-- | Promote a 'Bool' to a 'Property ()'.  Failures carry no extra message.
property :: Bool -> Property ()
property True = pure ()
property False = fail "falsified"

{- | Assert a boolean condition within a 'Property' @do@-block.
Fails with @\"falsified\"@ if the condition is 'False'.

> testProperty "commutativity" $ do
>   x <- forAll arbitrary
>   y <- forAll arbitrary
>   assert $ x + y == (y :: Int) + x
-}
assert :: Bool -> Property ()
assert = property

-- | Like 'assert', logging the expected and actual values on failure.
assertEqual :: (Eq a, Show a) => a -> a -> Property ()
assertEqual expected actual = do
  debug $ "Expected: " <> show expected
  debug $ "Actual: " <> show actual
  assert (expected == actual)

{- | Sample from a 'TestCases' generator in the 'Property' monad.

> testProperty "my test" $ do
>   x <- forAll generator1
>   y <- forAll generator2
>   assert $ somePredicate x y
-}
forAll :: TestCases a -> Property a
forAll gen = Property $ fmap ((,[]) . Right) gen

{- | Like 'forAll', but prepend the rendered value to any failure message produced by the continuation.

Useful when the generator is not 'Show'-able or when you want a custom rendering:

> testProperty "sorted" $ do
>   xs <- forAllWith (\xs -> "First 3 generated elements = " <> show (take 3 xs)) (arbitrary :: TestCases [Int])
>   assert $ sort xs == xs
-}
forAllWith :: (a -> String) -> TestCases a -> Property a
forAllWith showFn gen = Property $ fmap (\a -> (Right a, [showFn a])) gen

{- | Like 'forAllWith', specialised to 'show', with a tag prepended to the rendered value.

In a @do@-block, the tag and value appear in the failure log as @"tag: value"@:

> testProperty "sorted" $ do
>   xs <- forAllShow "xs" (arbitrary :: TestCases [Int])
>   assert $ sort xs == xs
-}
forAllShow :: (Show a) => String -> TestCases a -> Property a
forAllShow tag = forAllWith (\a -> tag <> ": " <> show a)

{- | Append a line of debug information to the log for the current test case.
The line appears in the failure message if the property fails; it is discarded on success.

> do
>   x <- forAll arbitrary
>   debug $ "x = " <> show (x :: Int)
>   assert $ x > 0
-}
debug :: String -> Property ()
debug msg = Property $ pure (Right (), [msg])

{- | Conditional property: if the precondition is 'False' the test case is
/discarded/ (treated as a pass), just like QuickCheck's @==>@.
-}
(==>) :: Bool -> Property () -> Property ()
True ==> p = p
False ==> _ = pure ()

infixr 0 ==>

-- * Testable class

-- | Types that can be converted to a 'Property ()'.
class Testable a where
  toProperty :: a -> Property ()

instance Testable Bool where
  toProperty = property

instance Testable (Property ()) where
  toProperty = id

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  toProperty f = forAllWith show arbitrary >>= toProperty . f

-- * Option: number of test cases

{- | How many test cases tinycheck should check per property.
Defaults to 10 000.  Can be set via @--tinycheck-tests N@ on the command line.
-}
newtype TinyCheckTests = TinyCheckTests Int
  deriving stock (Show, Eq, Ord)

instance IsOption TinyCheckTests where
  defaultValue = TinyCheckTests 10_000
  parseValue s = TinyCheckTests <$> safeRead s
  optionName = Tagged "tinycheck-tests"
  optionHelp = Tagged "Number of test cases for TinyCheck properties (default: 10000)"

-- * IsTest instance

-- | A named property ready to be run as a tasty test.
data TinyCheckTest = forall a. (Testable a) => TinyCheckTest a

instance IsTest TinyCheckTest where
  testOptions = Tagged [Option (Proxy @TinyCheckTests)]

  run opts (TinyCheckTest prop) _progress = do
    let TinyCheckTests n = lookupOption opts
        Property cases = toProperty prop
        results = take n (getTestCases cases)
    case runWriter $ runExceptT $ mapM_ (ExceptT . writer) results of
      (Right (), _) -> pure $ testPassed $ "OK, checked " <> show (length results) <> " cases"
      (Left msg, logs) -> pure $ testFailed $ unlines logs <> msg

-- * Public API

{- | Create a 'TestTree' leaf from a 'Testable' property.

The property can be a plain 'Bool', a 'Property', or a function
@a -> Bool@ / @a -> Property@ for any @'Arbitrary' a@.
-}
testProperty :: (Testable a) => TestName -> a -> TestTree
testProperty name prop = singleTest name (TinyCheckTest prop)

-- | Like 'testProperty', but run exactly @n@ test cases instead of the suite-wide default.
testPropertyWith :: (Testable a) => Int -> TestName -> a -> TestTree
testPropertyWith n name prop = localOption (TinyCheckTests n) $ testProperty name prop

{- | Assert that a property /fails/ and that the failure message contains
the given substring.  The test passes iff the property fails with a
matching message; it fails if the property passes, or if it fails with
an unexpected message.
-}
expectFailureWith :: (Testable a) => String -> TestName -> a -> TestTree
expectFailureWith needle name prop = singleTest name (ExpectFailure needle (TinyCheckTest prop))

-- | Like 'expectFailureWith', but check exactly @n@ test cases instead of the suite-wide default.
expectFailureWithN :: (Testable a) => Int -> String -> TestName -> a -> TestTree
expectFailureWithN n needle name prop = localOption (TinyCheckTests n) $ expectFailureWith needle name prop

-- | Internal wrapper used by 'expectFailureWith'.
data ExpectFailure = ExpectFailure String TinyCheckTest

instance IsTest ExpectFailure where
  testOptions = Tagged [Option (Proxy @TinyCheckTests)]

  run opts (ExpectFailure needle inner) progress = do
    result <- run opts inner progress
    pure $ case resultOutcome result of
      Failure _ ->
        let msg = resultDescription result
         in if needle `isInfixOf` msg
              then testPassed $ "Got expected failure containing " <> show needle
              else
                testFailed $
                  "Property failed as expected, but message\n"
                    <> show msg
                    <> "\ndoes not contain "
                    <> show needle
      Success ->
        testFailed "Expected the property to fail, but it passed"
