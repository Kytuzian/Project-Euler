from mathlib import *

def get_solution(n, primes, squares):
    for p in primes:
        for s in squares:
            if p + 2 * s == n:
                return true

def solve():
    f = Factoring()

    n = 0

    squares =

    while True:
        f.create_primes(n)

        n += 1
