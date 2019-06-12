import Data.List
import Data.Ratio
import Data.Tree
import Data.Ord

import System.IO
import System.ProgressBar

import Lib (sumPairs, count)

showProgressZipped limit res = mapM_ (\(i, r) -> progressBar (msg (show i ++ " of " ++ show limit ++ " (" ++ show r ++ ")")) percentage 80 i limit) res

reducedProperFractions d = map reducedProperFractions' [2..d]
    where reducedProperFractions' i
            | i <= d = count (\n -> gcd i n == 1) [1..i - 1]
            | otherwise = 0

doGcd = gcd

p72 limit = scanl (+) 0 vs
    where vs = [sum [1 | b <- [a + 1..limit], doGcd a b == 1] | a <- [1..limit - 1]]

ltFrac (a,b) (c,d) = a * d < b * c

-- sternBrocot :: Tree String
sternBrocot = unfoldTree sternBrocot' ((1,1),[], 0)
    where sternBrocot' (cur@(a,b), ancestors, i)
            | i > 100 = (a%b, [])
            | otherwise = (a%b, [(lval, (cur : ancestors), i + 1), (rval, (cur : ancestors), i + 1)])
            where low@(c,d) = case find (`ltFrac` cur) ancestors of
                                Nothing -> (0,1)
                                Just x -> x
                  high@(e,f) = case find (cur `ltFrac`) ancestors of
                                Nothing -> (1,0)
                                Just x -> x
                  lval = (n1 `div` (gcd n1 d1), d1 `div` (gcd n1 d1))
                    where (n1,d1) = (a + c, b + d)
                  rval = (n1 `div` (gcd n1 d1), d1 `div` (gcd n1 d1))
                    where (n1,d1) = (a + e, b + f)

main = do
    hSetBuffering stdout NoBuffering

    let limit = 5000
    let allFractions = p72 limit

    showProgressZipped limit $ zip [1..] allFractions

    print $ last allFractions
