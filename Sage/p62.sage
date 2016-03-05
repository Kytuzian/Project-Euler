def cubes(limit):
    return take_while(number_list(lambda n: n^3), lambda n: n < limit)

def all_cubes():
    return number_list(lambda n: n^3)

def is_permutation(n, test):
    strn = str(n)
    strtest = str(test)
    if len(strn) != len(strtest):
        return False
    for v1, v2 in it.izip(sorted(strn), sorted(strtest)):
        if v1 != v2:
            return False
    return True

def cube_permutations(n, verbose=0):
    return filter(lambda i: is_permutation(n, i), cubes(n * 10))
