import MathLib

main = print $ sum $ sieve (10^6)

class SquareRootable a where
    toFloat :: a -> Float

instance SquareRootable Int where toFloat = fromIntegral
instance SquareRootable Integer where toFloat = fromIntegral
instance SquareRootable Float where toFloat = id
instance SquareRootable Double where toFloat = realToFrac
