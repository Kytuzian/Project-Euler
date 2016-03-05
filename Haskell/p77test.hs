import Data.List
import Data.Numbers.Primes
m_summant :: Int -> [[Int]]
m_summant = (map _summant [1..] !!)
    where _summant 1 = [[1]]
          _summant 2 = [[2],[1,1]]
          _summant n = ([n]: [ i:q | i<-[1..n `div` 2], q<- filter (\x-> i<=head x) (m_summant (n-i-1))])

summant n = m_summant (n-1)


main = do

  print $head $filter (\x-> snd x >=5000)[ (n,ps) | n<-[5..],let ps = length $filter (\x-> all isPrime x) $summant n]
