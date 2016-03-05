def same_digits(n, digits):
    for i in xrange(1, digits + 1):
        test = str(n * i)
        
        for b in str(n):
            test = test.replace(b, "", 1)
                
        if len(test) > 0:
            return False
    
    return True

def permuted_multiples(multiples):
    n = 1
    
    while not same_digits(n, multiples):
        n += 1
        
    return n