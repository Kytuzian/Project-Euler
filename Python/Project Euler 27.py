def testcoefficients(a, b):
    n = 0
    while (n^2 + a * n + b) in Primes():
        n += 1
    return n

max([(testcoefficients(a, b), a, b) for a in xrange(-1000, 999) for b in xrange(-1000, 999)])