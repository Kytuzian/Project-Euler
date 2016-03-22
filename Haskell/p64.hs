import Lib

import System.ProgressBar

main = do
    let cfs = zipTo makeCF [2..10000]
    mapM_ (\(i, _) -> progressBar (msg "Working") percentage 80 i 10000) cfs
    print $ count (\(_, cf) -> period cf `mod` 2 > 0) cfs
