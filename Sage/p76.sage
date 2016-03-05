def p_un(n):
    if n < 0:
        return 0
    elif n < 5:
        return [1,1,2,3,5][n]
    else:
        pentagonals = general_pentagonal(n + 1, minn=0)
        args = map_f(n, map(sub, pentagonals))
        vals = map(p, args)

        print(args)

        res = alternating_sums(vals, [1, 1, -1, -1], init_from_first=False)

        return res

p = memoize(p_un)
