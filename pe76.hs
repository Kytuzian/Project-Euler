import Lib (doMemoization, znonzero, sumPattern)

p_test f n
    | n < 5 = [1,1,2,3,5] !! n
    | otherwise = sumPattern (map f (map ((-) n) (pentagonalNumbers n))) [1,1,-1,-1]

pMemo = doMemoization p_test

main :: IO ()
main = print $ pMemo 100
