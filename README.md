# tinycheck

A lightweight, **deterministic** property testing library for Haskell.

Instead of generating random inputs, `tinycheck` enumerates test cases from a
canonical, fairly-interleaved ordering.  Tests are reproducible, require no
seeds, and cover small values first — no shrinking required.

## Quick start

```haskell
import Test.Tasty
import Test.Tasty.TinyCheck

main :: IO ()
main = defaultMain $ testGroup "my suite"
  [ testProperty "reverse . reverse == id" $
      \(xs :: [Int]) -> reverse (reverse xs) == xs
  , testProperty "abs x >= 0" $
      \(x :: Int) -> abs x >= 0
  ]
```

Run with `cabal test`.

## How it works

The core type is `TestCases a` — a newtype over `[a]` whose `Semigroup`,
`Applicative`, and `Monad` instances use **fair interleaving** instead of
concatenation and cartesian product.

```
TestCases [1,2,3] <> TestCases [10,20,30]  ==  TestCases [1,10,2,20,3,30]
```

With infinite generators this ensures neither side is starved:

```
(Left <$> TestCases [1..]) <> (Right <$> TestCases [1..])
  ==  TestCases [Left 1, Right 1, Left 2, Right 2, ...]
```

`Applicative` interleaves function-argument pairs, so all parts of the input
space are explored immediately rather than exhausting one argument before
moving to the next.

Use `interleaveN` to interleave any number of generators fairly:

```
interleaveN [TestCases [1,2,3], TestCases [10,20,30], TestCases [100,200,300]]
  ==  TestCases [1,10,100, 2,20,200, 3,30,300]
```

## Defining generators

Implement `Arbitrary` for your types, or derive it via `Generically`:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic, Generically (..))
import Data.TestCases (Arbitrary)

data Colour = Red | Green | Blue
  deriving stock (Show, Generic)
  deriving (Arbitrary) via Generically Colour
```

Newtype wrappers are provided for common patterns:

| Wrapper | Suitable for |
|---|---|
| `SignedArbitrary` | `Num` + `Enum` (e.g. `Int`, `Integer`) |
| `BoundedArbitrary` | `Bounded` + `Enum` (e.g. `Bool`, `Word8`) |
| `RealFracArbitrary` | `Fractional` + `Enum` (e.g. `Float`, `Double`) |

## Preconditions

Use `==>` to skip inputs that don't satisfy a precondition:

```haskell
testProperty "n > 0 implies n * 2 > 0" $
  \(n :: Int) -> (n > 0) ==> property (n * 2 > 0)
```

## Modules

| Module | Purpose |
|---|---|
| `Data.TestCases` | Core `TestCases` type, `Arbitrary`, `CoArbitrary` |
| `Test.Tasty.TinyCheck` | Tasty integration (`testProperty`, `testPropertyWith`, …) |

## License

BSD-3-Clause
