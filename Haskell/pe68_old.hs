import Lib
import Data.SBV
import Data.List

uniqueSolutionSets :: (Ord t) => [[(t, t, t)]] -> [[(t, t, t)]]
uniqueSolutionSets = nub . map sort

makeSolutionSet3 :: [t] -> [(t, t, t)]
makeSolutionSet3 (a:b:c:d:e:f:n:rs) = [(a, b, c), (d, c, e), (b, e, f)]

solutions :: [[Integer]] -> [[(Integer, Integer, Integer)]]
solutions = uniqueSolutionSets . map makeSolutionSet3

magic3Gon = do
    a <- intRange exists "a" 0 7
    b <- intRange exists "b" 0 7
    c <- intRange exists "c" 0 7
    d <- intRange exists "d" 0 7
    e <- intRange exists "e" 0 7
    f <- intRange exists "f" 0 7
    n <- natural exists "n"

    constrainUnique [a, b, c, d, e, f]

    constrain $ a + b + c .== n
    constrain $ d + c + e .== n
    return $ b + e + f .== n

magic5Gon = do
    a <- intRange exists "a" 0 11
    b <- intRange exists "b" 0 11
    c <- intRange exists "c" 0 11
    d <- intRange exists "d" 0 11
    e <- intRange exists "e" 0 11
    f <- intRange exists "f" 0 11
    g <- intRange exists "g" 0 11
    h <- intRange exists "h" 0 11
    i <- intRange exists "i" 0 11
    j <- intRange exists "j" 0 11
    n <- natural exists "n"

    constrainUnique [a, b, c]
    constrainUnique [d, c, e]
    constrainUnique [f, e, h]
    constrainUnique [g, h, i]
    constrainUnique [j, i, b]

    constrain $ a + b + c .== n
    constrain $ d + c + e .== n
    constrain $ f + e + h .== n
    constrain $ g + h + i .== n
    return $ j + i + b .== n
