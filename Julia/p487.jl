f(n, k, p) = sum([(i^k) % p for i=1:n])

S(n, k, p) = sum([f(i, k, p) % p for i=1:n]) % p
