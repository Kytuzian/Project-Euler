import Data.Ratio

import Lib (sequences, countDuplicatesBy, sumPairs)

getDiceSums :: [Integer] -> Integer -> [(Integer, Integer)]
getDiceSums xs n = countDuplicatesBy sum $ sequences xs n

diceSumChance :: [(Integer, Integer)] -> Integer -> Ratio Integer
diceSumChance diceSums goal = a % total
    where (a, b) = sumPairs $ filter (\(_, value) -> value > goal) diceSums
          (total, _) = sumPairs diceSums

p205 = fromRational a
    where diceSums = getDiceSums [1..4] 9
          diceSums2 = getDiceSums [1..6] 6
          (total, _) = sumPairs diceSums2
          a = sum $ map (\(i, goal) -> i % total * (diceSumChance diceSums goal)) diceSums2

main = print $ p205
