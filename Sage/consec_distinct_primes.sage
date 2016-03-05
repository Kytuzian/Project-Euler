def is_valid(i, n):
    for k in xrange(n):
        factorization = list(set(factor(i + k)))

        if len(factorization) != n:
            return False

    return True

def consecutive_distinct_primes(n):
    i = 1

    while not is_valid(i, n):
        i += 1

    return i
