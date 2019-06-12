cancel :: Integral a => a -> a -> (a, a)
cancel 0 0 = (0, 0)
cancel n d = let g = gcd n d in (n `div` g, d `div` g)

fromDigits :: Integral a => [a] -> a
fromDigits [] = 0
fromDigits (d:ds) = foldl (\a b -> 10 * a + b) d ds

toDigits :: Integral a => a -> [a]
toDigits n = reverse $ toDigits' n
    where toDigits' 0 = []
          toDigits' v = r : toDigits' d
            where (d, r) = v `divMod` 10

remove :: Eq a => [a] -> a -> [a]
remove xs e = filter (/= e) xs

p33 :: (Num a, Eq a) => [a] -> [a] -> ([a], [a])
p33 n d = (foldl remove n inBoth, foldl remove d inBoth)
    where inBoth = filter (\i -> i /= 0 && i `elem` d) n
    
isP33 :: Integral a => a -> a -> Bool
isP33 n d = (cancel n d == cancel (fromDigits newN) (fromDigits newD)) && (fromDigits newN /= n) && (fromDigits newD /= d)
    where (newN, newD) = p33 (toDigits n) (toDigits d)
    
doP33 = [(a,b) | a <- [10..100], b <- [a + 1..100], isP33 a b]

cFracApprox :: (Integral a, Fractional b) => [a] -> b
cFracApprox (a:[]) = fromIntegral a
cFracApprox (a:as) = fromIntegral a + 1 / cFracApprox as

cFracReal :: (RealFrac a, Integral b) => Int -> a -> [b]
cFracReal 0 x = []
cFracReal prec x = case x - fromIntegral a of
                    0 -> [a]
                    t -> a : cFracReal (prec - 1) (1 / t)
    where a = floor x

cFrac :: Integral a => a -> a -> [a]
cFrac a b = as a b
    where as rn 0 = []
          as rn rn1 = v : as rn1 rn2
            where (v, rn2) = rn `quotRem` rn1

a `isSqMod` p = legendre a p == 1

legendre :: Integral a => a -> a -> a
legendre inA p
    | a `mod` p == 0 = 0
    | otherwise = case a^((p - 1) `div` 2) `mod` p == p - 1 of
                    True -> -1
                    False -> 1
    where a = inA `mod` p
    
jacobi :: Integral a => a -> a -> a
jacobi a n = product $ map (legendre a) $ factor n
    
isPrime :: Integral a => a -> Bool
isPrime n = (length $ factor n) == 1

primes :: Integral a => [a]
primes = filter isPrime [1..]
    
factor :: Integral a => a -> [a]
factor 1 = [1]
factor n = case nextFactor 2 of
            (i, 1) -> [i]
            (i, nextN) -> i : factor nextN
    where nextFactor i
            | n `mod` i == 0 = (i, n `div` i)
            | otherwise = nextFactor (i + 1)