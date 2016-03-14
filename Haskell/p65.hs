import Lib (nubOnSorted, flatten, separateList, evalRatio, sumDigits)

import Data.List
import Data.Ratio

data ContinuedFraction = ContinuedFraction Integer (Integer -> Integer)

instance Show ContinuedFraction where
    show (ContinuedFraction a f) = "[" ++ show a ++ ";(" ++ (show $ map f [1..6]) ++ ")]"

cycleCF :: Integer -> [Integer] -> ContinuedFraction
cycleCF a bs = ContinuedFraction a ((bs !!) . fromIntegral . (`mod` (toInteger $ length bs)))

e :: ContinuedFraction
e = ContinuedFraction 2 e'
    where e' n = ([1,1] ++ intercalate [1,1] (separateList [2,4..])) !! (fromIntegral n)

-- evalCF :: Integeregral a => ContinuedFraction -> Integer -> Ratio a
evalCF (ContinuedFraction a _) 0 = a % 1
evalCF (ContinuedFraction a f) terms = (a % 1) + (1 / (evalCF' 1))
    where evalCF' i
            | i < terms = ((f i) % 1) + (1 / (evalCF' (i + 1)))
            | otherwise = (f i) % 1

p65 = sumDigits $ numerator $ evalCF e 99
