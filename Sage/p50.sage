from sage.all import *

attach("mathlib.sage")

def run():
    factoring.create_primes(10**6)
    sums = [sum(factoring.primes[n:]) for n in xrange(len(factoring.primes))]
    
    return sums