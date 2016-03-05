def d(n):
    prod = 1
    
    for i in factor(n):
        prod *= i[1] + 1
    
    return prod

def M(n, k):
    max = 0
    dmax = 0
    
    max_i = 0
    
    for i in xrange(n, n + k):
        if i == 1:
            dval = 1
        else:
            dval = d(i)
    
        if dval > dmax:
            max = i
            dmax = dval
    
    print("Found max {0} {1}% of the way through".format(max, float(max) / float(n + k - n)))
    
    return dmax

def S(u, k):
    total = 0
    
    for n in xrange(1, u - k + 2):
        val = M(n, k)
    
        total += val
        
    return total