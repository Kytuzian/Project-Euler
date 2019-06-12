import Data.List (maximum)

palindrome xs = reverse xs == xs

digits x
    | x < 10 = [x]
    | otherwise = let (q, r) = x `quotRem` 10 in
                      r : digits q

main = print $ maximum [x * y | x <- [100..999], y <- [100..999], palindrome $ digits $ x * y]

