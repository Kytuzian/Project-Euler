import time

def replace_insert_tuple(ls, index, vals):
    return tuple(replace_insert(list(ls), index, list(vals)))

def replace_insert(ls, index, vals):
    return ls[:index] + vals + ls[index + 1:]
    
def add_to_closed_list(closed_list, current, i, sums):
    for rep in sums:
        if len(rep) > 1:
            rep = tuple(sorted(replace_insert(current, i, rep)))
            closed_list[rep] = True

sums_of_n = {}
def build_sums(n):
    if n in sums_of_n:
        return sums_of_n[n]

    open_list = {(n,): True}
    closed_list = {}
    res = 0

    while len(open_list) > 0:
        current, _ = open_list.popitem()
        current = list(current)

        for i, number in enumerate(current):
            if number > 1:
                if number in sums_of_n:
                    add_to_closed_list(closed_list, current, i, sums_of_n[number])
                else:
                    for rep in sum_pairs(number):
                        open_list[tuple(sorted(replace_insert(current, i, rep)))] = True

        closed_list[tuple(current)] = True

    if not n in sums_of_n:
        sums_of_n[n] = map(list, closed_list.keys())

    return sums_of_n[n]

def sum_pairs(n):
    result = []

    for i in xrange(1, n // 2 + 1):
        result.append([i, n - i])

    return result

def p76():
    res = range(1, 53)
    
    start = time.time()
    
    for i in range(1, 53):
        res[i - 1] = len(build_sums(i))
        print(i)
        
    print('Elapsed: {}'.format(time.time() - start))
    
    return res
    
print(p76())
 
 
 
 