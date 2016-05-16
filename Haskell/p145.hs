import Lib (digits, fromDigits, count, zipTo, showProgress)

import System.ProgressBar

isReversible :: Integral a => a -> Bool
isReversible n
    | n `mod` 10 == 0 = False --This means there were leading zeros (doesn't matter how many)
    | otherwise = all odd $ digits $ (fromDigits $ digits n) + n

-- main = print $ isReversible 1001
main = do
    let limit = 10^9
    let res = filter isReversible [1..limit]
    showProgress limit res
    -- mapM_ print res
    print $ length res
