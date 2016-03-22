import Lib (pairOverlap)

maxPathSum :: (Num a, Ord a) => [[a]] -> [a]
maxPathSum ls = maxPathSum' $ reverse ls
    where maxPathSum' (ns:[]) = ns
          maxPathSum' (ns1:ns2:nss) = maxPathSum' (nextRow:nss)
            where nextRow = zipWith (\e (a, b) -> max (e + a) (e + b)) ns2 (pairOverlap ns1)

readNumberLines :: (Num a, Read a) => FilePath -> IO [[a]]
readNumberLines path = do
    contents <- readFile path
    let ls = map (\line -> map read $ words line) $ lines contents
    return ls

main = do
    nsLines <- readNumberLines "p067_triangle.txt"
    print $ maxPathSum nsLines
