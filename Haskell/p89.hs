import Data.List.Split
import Data.List

import Lib (flatten, padL)

data Numeral = I | V | X | L | C | D | M
    deriving (Show, Eq, Ord, Read)

data RomanNumeral = Roman [Numeral]

instance Show RomanNumeral where
    show (Roman xs) = flatten $ map show xs

numeralValue :: Numeral -> Int
numeralValue I = 1
numeralValue V = 5
numeralValue X = 10
numeralValue L = 50
numeralValue C = 100
numeralValue D = 500
numeralValue M = 1000

romanToArabic :: RomanNumeral -> Int
romanToArabic (Roman []) = 0
romanToArabic (Roman (n1:[])) = numeralValue n1
romanToArabic (Roman (n1:n2:ns))
    | n1 < n2 = (-(numeralValue n1)) + (romanToArabic $ Roman $ n2 : ns)
    | otherwise = numeralValue n1 + (romanToArabic $ Roman $ n2 : ns)

arabicThousands :: Char -> [Numeral]
arabicThousands '0' = []
arabicThousands '1' = [M]
arabicThousands '2' = [M,M]
arabicThousands '3' = [M,M,M]
arabicThousands '4' = [M,M,M,M]

arabicHundreds :: Char -> [Numeral]
arabicHundreds '0' = []
arabicHundreds '1' = [C]
arabicHundreds '2' = [C,C]
arabicHundreds '3' = [C,C,C]
arabicHundreds '4' = [C,D]
arabicHundreds '5' = [D]
arabicHundreds '6' = [D,C]
arabicHundreds '7' = [D,C,C]
arabicHundreds '8' = [D,C,C,C]
arabicHundreds '9' = [C,M]

arabicTens :: Char -> [Numeral]
arabicTens '0' = []
arabicTens '1' = [X]
arabicTens '2' = [X,X]
arabicTens '3' = [X,X,X]
arabicTens '4' = [X,L]
arabicTens '5' = [L]
arabicTens '6' = [L,X]
arabicTens '7' = [L,X,X]
arabicTens '8' = [L,X,X,X]
arabicTens '9' = [X,C]

arabicUnits :: Char -> [Numeral]
arabicUnits '0' = []
arabicUnits '1' = [I]
arabicUnits '2' = [I,I]
arabicUnits '3' = [I,I,I]
arabicUnits '4' = [I,V]
arabicUnits '5' = [V]
arabicUnits '6' = [V,I]
arabicUnits '7' = [V,I,I]
arabicUnits '8' = [V,I,I,I]
arabicUnits '9' = [I,X]

arabicToRoman :: Int -> RomanNumeral
arabicToRoman n = Roman $ flatten $ ns
    where ns = zipWith (\a b -> a b) [arabicThousands, arabicHundreds, arabicTens, arabicUnits] nDigits
          nDigits = padL '0' 4 $ show n

readRomanNumeral :: String -> RomanNumeral
readRomanNumeral = Roman . map read . chunksOf 1

optimalRomanNumeral :: RomanNumeral -> String
optimalRomanNumeral = show . arabicToRoman . romanToArabic

p89 = do
    contents <- readFile "p089_roman.txt"
    let numerals = map (\i -> (i, readRomanNumeral i)) $ lines contents
    let results = map (\(i, numeral) -> length i - (length $ optimalRomanNumeral numeral)) numerals
    return $ sum $ results
