import Lib

g = 0 : 0 : 0 : 0 : g' 13 5
    where g' v n = v : g' (v + gcd v n) (n + 1)
