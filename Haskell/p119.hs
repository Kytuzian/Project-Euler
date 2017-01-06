import MathLib (toDigits, printFilterMap)
import Lib (zipTo, nubOnSorted)

import Data.List

-- isDigitPowerSum :: Integral a => a -> Bool
isDigitPowerSum n
    | n < 10 = ((0,0), False)
    | s <= 1 = ((0,0), False)
    | otherwise = ((s,p), check == n)
    where s = sum $ toDigits n
          (p, check) = head $ dropWhile ((< n) . snd) $ zipTo (s^) [2..]

allPowers = [a^b | a <- [2..], b <- [2..20]]


-- It's a little cheaty, but pretty much I just generated a bunch of powers (up to the 20th for a bunch of numbers)
-- Then just tried all of those
-- The cheaty bit is because it's quite possible I might have missed some.
-- Luckily, I didn't.
main = do
    -- printFilterMap (snd . isDigitPowerSum) (\n -> (n, fst $ isDigitPowerSum n)) id allPowers
    printFilterMap (snd . isDigitPowerSum) (\n -> (n, fst $ isDigitPowerSum n)) id $ nubOnSorted $ sort $ take (10^5) allPowers
