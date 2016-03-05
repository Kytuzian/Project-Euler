import numpy

def isPandigital(n, lastDigit):
    digits = range(1, lastDigit + 1)
    
    val = [i for i in str(n)]
    
    for i in digits:
        if str(i) in val:
            val.remove(str(i))
        else:
            return False
    
    return len(val) == 0
    
def isPandigitalProduct(a, b, lastDigit):
    return isPandigital(str(a) + str(b) + str(a * b), lastDigit)
    
def getPandigitalProducts(limit, lastDigit):
    result = []

    for x in xrange(limit):
        for y in xrange(x, limit):
            if isPandigitalProduct(x, y, lastDigit):
                result.append((x, y, x * y))
    
    return result

print(numpy.unique([i[2] for i in getPandigitalProducts(10000, 9)]))