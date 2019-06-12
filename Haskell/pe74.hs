import Lib (factorial, digits, memoize)

--memoize fChain somehow
fChain n = fChain' n []
fChain' n prevs
    | nextVal `elem` prevs = n : prevs
    | otherwise = fChain' nextVal (n : prevs)
    where nextVal = nextFChain n

fChainMemo = memoize fChain

nextFChain = (sum . map factorial . digits)

fChains = map fChain [1..]

main = print $ fChainMemo 1974
