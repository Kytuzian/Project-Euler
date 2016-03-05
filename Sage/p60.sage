import cProfile

def is_prime_pair_set(s):
    for first in s:
        for second in s[1:]:
            if not (Integer(first + second).is_pseudoprime() and Integer(second + first).is_pseudoprime()):
                return False

    return True

def is_prime_pair_set_reason(s):
    for first in s:
        for second in s:
            if first == second:
                continue
            if not (Integer(first + second).is_pseudoprime() and Integer(second + first).is_pseudoprime()):
                return (False, (first, second))

    return (True, None)

def find_prime_pair_sets(length, primes_max, verbose=0):
    ps = map(str, list(primes(primes_max)))[1:] #The number two can't be in any of these, of course, because numbers ending in two aren't prime.
    ress = 0

    num_to_check = binomial(len(ps), length)

    ps_combination_fail = {p: {} for p in ps}

    res = []

    print('There are {} possible sets'.format(num_to_check))

    if length > 2:
        prime_pair_set_candidates = [] #list(find_prime_pair_sets(length - 1, primes_max, verbose=verbose)) * len(ps)
        prev_candidates, ps_combination_fail = find_prime_pair_sets(length - 1, primes_max, verbose=verbose)

        for i1, candidate in enumerate(prev_candidates):
            for i2, p in enumerate(ps):
                if not p in candidate:
                    #prime_pair_set_candidates[i2 + i1 * len(prev_candidates)] += (p)
                    prime_pair_set_candidates.append(tuple(list(candidate) + [p]))
    else:
        prime_pair_set_candidates = list(it.combinations(ps, length))

    if verbose > 0:
        interval = max(1, int(len(prime_pair_set_candidates) / 1000))
    for i, prime_pair_set_check in it.izip(xrange(num_to_check), prime_pair_set_candidates):
        #print(ps_combination_fail)
        if verbose > 0 and i % interval == 0:
            show_bar(i, len(prime_pair_set_candidates), message='Checking ({} of {}): '.format(i, len(prime_pair_set_candidates)), number_limit=True)
            #print(map(lambda x: (x, len(ps_combination_fail[x])), ps_combination_fail))
        for p in prime_pair_set_check:
            skip = False
            for p_check in prime_pair_set_check[1:]:
                if p_check in ps_combination_fail[p]:# or p in ps_combination_fail[p_check]:
                    skip = True
                    break
            if skip:
                break
        if skip:
            continue

        success, reason = is_prime_pair_set_reason(prime_pair_set_check)

        if success:
            res.append(prime_pair_set_check)

            ress += 1
        else:
            ps_combination_fail[reason[0]][reason[1]] = True
            ps_combination_fail[reason[1]][reason[0]] = True

    if verbose > 0:
        print('')

    return (res, ps_combination_fail)

def min_set_sum(sets):
    return min(map(sum, sets))

def p60(length, max_prime):
    try:
        time result = find_prime_pair_sets(length, max_prime, 1)

        time fres = take(result)(1)

        return fres
    except:
        return []
