{-# LANGUAGE UndecidableInstances #-}
module Data.TestCases (module Data.TestCases) where

-- base
import Control.Monad (ap, forM_, unless)
import GHC.Generics

newtype TestCases a = TestCases {getTestCases :: [a]}
    deriving newtype (Show, Foldable, Functor)

testCase :: a -> TestCases a
testCase = TestCases . pure

instance Semigroup (TestCases a) where
    TestCases as1 <> TestCases as2 = TestCases $ mingle as1 as2
      where
        mingle [] as = as
        mingle (a : as1') as2' = a : mingle as2' as1'

instance Monoid (TestCases a) where
    mempty = TestCases mempty

instance Applicative TestCases where
    pure = testCase
    (<*>) = ap

instance Monad TestCases where
    as1 >>= f = foldMap f as1

test :: (Show a, Arbitrary a) => String -> (a -> (Bool, String)) -> IO ()
test msg f = do
    forM_ (take 1000000 $ getTestCases arbitrary) $ \a ->
        let (passed, dbg) = f a
         in unless passed $
                error $
                    unlines
                        [ "Test failure:"
                        , msg
                        , show a
                        , dbg
                        ]
    putStrLn $
        unlines
            [ "Test passed:"
            , msg
            ]

class Arbitrary a where
    arbitrary :: TestCases a

instance Arbitrary Int where
    arbitrary = TestCases [0 ..] <> ((* (-1)) <$> TestCases [1 ..])

instance (Arbitrary a) => Arbitrary [a] where
    arbitrary = pure [] <> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary Char where
    arbitrary = TestCases ['a' .. 'z']

instance (Arbitrary k, Arbitrary a) => Arbitrary (k, a) where
    arbitrary = (,) <$> arbitrary <*> arbitrary

instance (Generic a, Arbitrary (Rep a ())) => Arbitrary (Generically a) where
  arbitrary = fmap (Generically . to) (arbitrary :: TestCases (Rep a ()))

instance (Arbitrary (fL x), Arbitrary (fR x)) => Arbitrary ((fL :+: fR) x) where
  arbitrary = (L1 <$> arbitrary) <> (R1 <$> arbitrary)

instance (Arbitrary (fL x), Arbitrary (fR x)) => Arbitrary ((fL :*: fR) x) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary (f x) => Arbitrary ((M1 i t f) x) where
  arbitrary = M1 <$> arbitrary

instance Arbitrary a => Arbitrary (K1 i a f) where
  arbitrary = K1 <$> arbitrary

instance Arbitrary (U1 f) where
  arbitrary = pure U1

deriving via (Generically (Either a b)) instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
deriving via (Generically (Maybe a)) instance (Arbitrary a) => Arbitrary (Maybe a)
deriving via (Generically Bool) instance Arbitrary Bool
