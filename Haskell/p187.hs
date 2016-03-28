import Lib (primeTau, zipTo, count, allProducts)

import System.ProgressBar
import Math.NumberTheory.Primes

main = do
    let limit = 10^8
    let ps = takeWhile (< limit) primes
    let res = allProducts limit ps
    print $ length res
