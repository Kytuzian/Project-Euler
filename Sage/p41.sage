def is_pandigital(n, digits=-1):
    strn = str(n)

    if digits == -1:
        digits = len(strn)

    for i in xrange(1, digits + 1):
        if not (str(i) in strn):
            return False

    return True

def run():
    factoring.create_primes(10**8)

    return [i for i in factoring.primes if is_pandigital(i)]
