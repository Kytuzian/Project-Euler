def polygonal_number(s, limit):
    return get_ns_less_than(lambda x: (x^2 * (s - 2) - x*(s - 4)) / 2, limit)

def is_polygonal_number(s, n):
    return n in polygonal_number(s, n + 1)

def get_all_p61_cyclic(n, ns):
    res = []

    for test in ns:
        if pair_is_p61_cyclic(n, test):
            res.append(test)

    return res

def build_p61_cyclic_lists(ns, length=0, verbose=0):
    nsstr = map(str, ns)
    res = map(lambda x: [x], nsstr)
    for l in xrange(length - 1):
        prev_len = len(res)
        prev_res = list(enumerate(res))
        for i, numbers in prev_res:
            if verbose > 0:
                show_bar(i, prev_res, message='Making lists of length {}. '.format(l + 2))
            for an in get_all_p61_cyclic(numbers[-1], nsstr):
                res.append(numbers + [an])

        res = res[prev_len:]

        if verbose > 0:
            print('')
            print('{} lists so far.'.format(len(res)))

    if verbose > 0:
        print('Done.')

    if verbose > 0:
        return map_with_progress(lambda i: map(int, i), filter_with_progress(is_p61_cyclic, res))
    else:
        return map(lambda i: map(int, i), filter(is_p61_cyclic, res))

def polygonal_type(n, mint, maxt):
    for i in xrange(maxt, mint, -1):
        if n in polygonal_number(i, n + 1):
            return i

def distinct_polygonals(ns, mint, maxt):
    return is_distinct(map(lambda x: polygonal_type(x, mint, maxt), ns))

def p61(length):
    all_polygonals = set(combine_lists(map(lambda i: filter(lambda x: x >= 1000, polygonal_number(i, 10000)), [3..length + 2])))
    res = build_p61_cyclic_lists(all_polygonals, length, verbose=1)

    distinct = filter_with_progress(lambda x: distinct_polygonals(x, 3, length + 2), res)

    return set(map(tuple, map(sorted, distinct)))

def pair_is_p61_cyclic(f, s):
    return f[-2:] == s[:2]

def is_p61_cyclic(ns):
    return is_cyclic(map(str, ns), pair_is_p61_cyclic, True)

def is_cyclic(s, f, wrap=False):
    for i, v in enumerate(s):
        if i > 0:
            if not f(v, s[i - 1]):
                return False
        elif wrap:
            if not f(v, s[-1]):
                return False

    return True
