module Main (main) where

-- tasty
import Test.Tasty

-- tinycheck-test
import Test.Base qualified
import Test.Combinators qualified
import Test.Generic qualified
import Test.HybridSort qualified
import Test.Strings qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "tinycheck"
      [ Test.Base.tests
      , Test.Combinators.tests
      , Test.Strings.tests
      , Test.HybridSort.tests
      , Test.Generic.tests
      ]
