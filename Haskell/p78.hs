{-def test(m):
    sums = [0] * (m + 1)

    sums[0] = 1
    for i in xrange(1, m):
        for j in xrange(i, m + 1):
            sums[j] += sums[j - i]
    return sums

test(1000)-}

import Data.List

partitions :: Integral a => a -> [a]
partitions 0 = [1]
partitions 1 = [1,1]
partitions n = prev ++ [sum $ map sum $ tail $ tails prev]
    where prev = partitions (n - 1)