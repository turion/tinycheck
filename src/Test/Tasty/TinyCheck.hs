{-# LANGUAGE TypeApplications #-}

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
]
:}
-}
module Test.Tasty.TinyCheck (
  -- * Defining properties
  Property,
  property,
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
import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))

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

{- | A testable property: a 'TestCases' of outcomes, each being either
'Right ()' for a pass or 'Left msg' for a failure with a message.

Build one with 'property' (or its operator alias '==>') or simply
use a function @a -> Bool@ / @a -> Property@ directly via 'testProperty'.
-}
newtype Property = Property (TestCases (Either String ()))

-- | Promote a 'Bool' to a 'Property'.  Failures carry no extra message.
property :: Bool -> Property
property True = Property $ TestCases [Right ()]
property False = Property $ TestCases [Left "falsified"]

{- | Conditional property: if the precondition is 'False' the test case is
/discarded/ (treated as a pass), just like QuickCheck's @==>@.
-}
(==>) :: Bool -> Property -> Property
True ==> p = p
False ==> _ = Property $ TestCases [Right ()]

infixr 0 ==>

-- * Testable class

-- | Types that can be converted to a 'Property'.
class Testable a where
  toProperty :: a -> Property

instance Testable Bool where
  toProperty = property

instance Testable Property where
  toProperty = id

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  toProperty f = Property $ do
    a <- arbitrary
    let Property outcomes = toProperty (f a)
    -- Annotate failures with the offending input.
    fmap (void . prependInput a) outcomes
    where
      prependInput a (Left msg) = Left $ show a <> "\n" <> msg
      prependInput _ r = r

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
    case sequence_ results of
      Right () -> return $ testPassed $ "OK, checked " <> show (length results) <> " cases"
      Left msg -> return $ testFailed msg

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
    return $ case resultOutcome result of
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
