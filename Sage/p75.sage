def sum_pythag_triples(limit):
    prim_triples = matrix_gen_triples(limit) #combine_lists(list(gen_prim_pythag_triples(limit, verbose=verbose)), verbose=verbose)

    prim_triples_sums = set(map_with_progress(sum, prim_triples, desc='Summing primitive triples. '))

    prim_triples = map_with_progress(np.array, prim_triples, desc='Making np.arrays. ')
    triples = map_with_progress(lambda trip: take_while(number_list(lambda x: trip * x), lambda ns: sum(ns) < limit), prim_triples, desc='Generating triple multiples. ')
    triples = combine_lists(triples, verbose=1)
    triples = map_with_progress(tuple, triples, desc='Making tuples. ')

    return sorted(set(map_with_progress(lambda i: (sum(i), i), triples, desc='Summing triples. ')), reverse=True)

def matrix_gen_triples(sum_limit):
    open_triples = [matrix([[3],[4],[5]]), matrix([[3],[4],[5]])]
    closed_triples = []

    A = matrix([[1, -2, 2], [2, -1, 2], [2, -2, 3]])
    B = matrix([[1, 2, 2], [2, 1, 2], [2, 2, 3]])
    C = matrix([[-1, 2, 2], [-2, 1, 2], [-2, 2, 3]])

    max_sum = 0

    current_triple = open_triples.pop()
    while len(open_triples) > 0:
        current_triple = open_triples.pop()

        last_sum = sum(current_triple)[0]

        sys.stdout.write('\rSum: {}. {} in closed list. {} in open list. '.format(last_sum, len(closed_triples), len(open_triples)))
        sys.stdout.flush()

        if last_sum < sum_limit:
            open_triples.append(A * current_triple)
            open_triples.append(B * current_triple)
            open_triples.append(C * current_triple)

            closed_triples.append(current_triple)

    print('')

    res = map_with_progress(lambda i: tuple(i.transpose()), closed_triples, desc='Making triple tuples. ')
    res = combine_lists(res, verbose=1)
    return res

def p75(limit):
    triple_sums = sum_pythag_triples(limit)

    triple_sum_dict = {k: 0 for k in [1..limit]}

    for i, (tsum, triple) in enumerate(triple_sums):
        show_bar(i, triple_sums, message='Building triple sum dict. ')
        triple_sum_dict[tsum] += 1

    print('')

    result = count(lambda i: i == 1, triple_sum_dict.values(), verbose=1)

    return result
