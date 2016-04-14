import Lib (incrementDigitsToIf, incrementDigitsIf, zipTo, unduplicate, countDuplicates)

isProductSum :: Integral a => [a] -> Bool
isProductSum ns = sum ns == product ns

-- minProductSum :: Integral a => a -> [a]
minProductSum n = minProductSum' [n]
    where minProductSum' ns
            | isProductSum $ res ns = [ns]
            | otherwise = ns : minProductSum' (nextHead : nextTail)
            where nextTail = incrementDigitsIf ((< n) . sum) $ tail ns
                  nextHead = n - sum nextTail
                  res xs = unduplicate $ zip ns [1..]

-- main = do
--     let res = zipTo minProductSum [3..30]
--     let sums = map (sum . snd) res
--     mapM_ print res
--     print $ countDuplicates sums
