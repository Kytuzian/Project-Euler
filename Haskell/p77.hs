import Lib (memoize, constructAggregates, insertReplace, firstWhere, nubOnSorted)
import Data.Numbers.Primes (isPrime)

import Data.List

sumPairs 1 = [[1]]
sumPairs n = map (\i -> [i, n - i]) [1..n `div` 2]

buildSums f ns = concat $ map sums (zip [0..] (nub ns))
    where sums (_, 1) = []
          sums (i, n) = map replaceNs (zip (repeat i) (f n))
          replaceNs (i, pair) = insertReplace ns pair i
          maxN = maximum ns

partitions 0 = []
partitions 1 = [[1]]
partitions 2 = [[2], [1,1]]
partitions 3 = [[3],[1,2],[1,1,1]]
partitions 4 = [[4],[1,3],[2,2],[1,1,2],[1,1,1,1]]
partitions 5 = [[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]
partitions n = nubOnSorted $ sort $ map sort $ vals
    where vals = [n] : (concat $ map (buildSums partitionsMemo) (sumPairs n))

partitionsMemo = memoize partitions

p = zip [0..] (map (length . partitionsMemo) [0..])

primePartitions 0 = []
primePartitions 1 = []
primePartitions 2 = [[2]]
primePartitions 3 = [[3]]
primePartitions 4 = [[2,2]]
primePartitions 5 = [[5],[2,3]]
primePartitions n = filter (all isPrime) parts
    where parts = nubOnSorted $ sort $ map sort ([n] : (sumPairs n) ++ (concat $ map (buildSums primePartitionsMemo) (sumPairs n)))

primePartitionsMemo = memoize primePartitions

primeP = zip [0..] (map (length . primePartitions) [0..])

main = print $ firstWhere (\i -> (snd i) > 5000) primeP
