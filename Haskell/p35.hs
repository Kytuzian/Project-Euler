import Data.List (permutations)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n $ cycle xs

rotations xs = take (length xs) $ iterate (rotate 1) xs

toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits n = reverse $ toDigits' n
    where toDigits' 0 = []
          toDigits' v = (v `mod` 10) : toDigits' (v `div` 10)

fromDigits :: (Num a, Foldable f) => f a -> a
fromDigits = foldl (\n d -> n * 10 + d) 0

a `divides` b = rem b a == 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not $ any (`divides` n) $ takeWhile (\d -> d^2 <= n) (2:[3,5..])

isCircularPrime = all isPrime . map fromDigits . rotations . toDigits

main = do
    let limit = 10^6
    putStrLn $ "There are " ++ show (length (filter isCircularPrime [1..limit])) ++ " circular primes less than " ++ show limit
