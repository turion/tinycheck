module Test.Strings (tests) where

-- base
import Data.Char (GeneralCategory (..), generalCategory, isAlpha, isLower, isPrint, isUpper, ord)

-- tasty
import Test.Tasty
import Test.Tasty.TinyCheck

-- tinycheck
import Data.TestCases

tests :: TestTree
tests =
  testGroup
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
