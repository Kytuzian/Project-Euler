import utility

import math
import time

def replace_infix(cs, es, xs):
    for i in xrange(len(xs)):
        if cs == xs[i:i + len(cs)]:
            return xs[:i] + es + xs[i + len(cs):]

    return []

def factor(n):
    res = []

    test_primes = [2,3,5,7,11,13,17,19,23,27,29,31]

    i = test_primes[-1] + 2
    while n != 1:
        for p in test_primes:
            if n % p == 0:
                res.append(p)

                n /= p

                break
        if n % i == 0:
            res.append(i)

            n /= i
            i = test_primes[-1] + 2
        else:
            i += 2

    return res

def is_prime(n):
    return len(factor(n)) == 1

def unique(xs):
    res = []
    for x in xs:
        if not x in res:
            res.append(x)

    return res

def divisor_pairs(n):
    res = []

    for i in xrange(1, n / 2 + 1):
        if n % i == 0:
            res.append([i, n / i])

    return res

def all_products(n):
    res = [[n]]

    products = divisor_pairs(n)

    # print(n)

    while len(products) > 0:
        # print(products)
        prod = filter(lambda p: p != 1, products.pop(0))

        for p in prod:
            if p != 1 and not is_prime(p):
                pairs = divisor_pairs(p)[1:]
                # print(prod, pairs)

                for pair in pairs:
                    # print([p], pair, prod)

                    nextProd = replace_infix([p], pair, prod)
                    products.append(nextProd)
                    res.append(nextProd)

    return res

def can_be_product_sum_of(n, ns):
    return len(ns) + utility.product(ns) - sum(ns) == n

def min_prod_sums_of(n):
    res = []
    for prod in all_products(n):
        res.append([1] * (utility.product(prod) - sum(prod)) + prod)

    return res

def min_prod_sum(n):
    i = 1
    while True:
        for prod in all_products(i):
            if can_be_product_sum_of(n, prod):
                return [1] * (utility.product(prod) - sum(prod)) + prod
        i += 1

def generate_sums_less_than(limit):
    res = {}

    start_time = time.time()

    for n in xrange(limit):
        vals = min_prod_sums_of(n)
        # print(len(vals))

        for prod_sum in vals:
            utility.show_bar(n, limit, message='Finding ({}, {}). '.format(n, len(res)), start_time=start_time)
            if not len(prod_sum) in res:
                res[len(prod_sum)] = sum(prod_sum)
            # elif res[len(prod_sum)] > sum(prod_sum):
            #     res[len(prod_sum)] = sum(prod_sum)

    print('')

    return sorted(res.items(), key=utility.snd)

def brute_force(limit):
    res = []

    start_time = time.time()

    for i in xrange(1, limit):
        utility.show_bar(i, limit, message='Finding min prod sums. ', start_time=start_time)

        res.append(min_prod_sum(i))

    print('')

    return res

def command_line(args):
    limit = int(args.get('limit', '200'))
    method = args.get('method', 'brute')

    res = []
    if method == 'brute':
        res = brute_force(limit)
    elif method == 'new':
        res = generate_sums_less_than(limit)

    print(res)

if __name__ == '__main__':
    command_line(utility.command_line_args())
