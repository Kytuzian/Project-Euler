import Data.List
import System.IO

elemBy :: Eq b => (a -> b) -> a -> [a] -> Bool
elemBy f e xs = (f e) `elem` (map f xs)

isProductSum :: Integral a => [a] -> Bool
isProductSum ns = sum ns == product ns

allProds n = reverse $ allProds' n 2
    where allProds' 1 _ = [[]]
          allProds' x mind = [d : p| d <- [mind..x], x `mod` d == 0, p <- allProds' (x `div` d) d]

minProdSumsOf :: Int -> [[Int]]
minProdSumsOf n = map (\xs -> replicate (product xs - sum xs) 1 ++ xs) $ allProds n

findProdSums :: Int -> [(Int, Int)]
findProdSums limit = nubBy (\(_, a) (_, b) -> a == b) $ findProdSums' [2..limit] 1 []
    where findProdSums' [] _ current = []
          findProdSums' remaining i current = newSums ++ findProdSums' missing (i + 1) (current ++ newSums)
            where res = tail $ minProdSumsOf i
                  sums = map (\ns -> (length ns, sum ns)) res
                  missing = remaining \\ (map fst sums)
                  newSums = filter (\a -> (not $ elemBy fst a current) && (fst a < limit)) sums

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    let res = findProdSums 12000
    print $ sum $ map snd res
