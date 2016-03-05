from sage.all import *

attach("mathlib.sage")
    
def f(k, n):
    return (k^(n + 1)-k)/(k-1)

def Solve(k):
    n = 1/27*k^3 + 1/2*k^2
    h = 1/6*sqrt(1/3)*sqrt((4*k^8 + 27*k^7 - 18*k^5 - k^3 + 4)/k)
    g = (n + h/k - 1/6)^(1/3)
    f = g * k
    
    k3rd = k^3 - 3
    nI = (-I*sqrt(3) + 1)
    pI = (I*sqrt(3) + 1)
    
    #print(k, n, h)
    
    a = -1/2 * g * pI + 1/3 * k - 1/18*k3rd * nI/f
    b = -1/2 * g * nI + 1/3 * k - 1/18*k3rd * pI/f
    c = 1/3 * k + g + 1/9 * k3rd / f

    return (a, b, c)
    
def test_solutions(n):
    solutions = []
    
    for k in xrange(0, n):
        solutions.append(-k^2-2*k)
        
    return solutions
    
def get_solutions(n):
    solutions = []
    
    for k in xrange(1, n + 1):
        a,b,c = Solve(k)
        solutions.append((a + b) * (b + c) * (a + c))
        
    return solutions
    
def testS(n):
    sum = []
    
    solutions = test_solutions(n)
    
    for k in solutions:
        sum.append(f(k, n))
            
    return sum
    
def S(n):
    sum = 0
    
    #solutions = get_solutions(n)
    solutions = test_solutions(n)

    '''for k in xrange(1, n + 1):
        product = solutions[k]
            
        sum += product
        
        for p in xrange(2, n + 1):
            product *= solutions[k]
        
            sum += product'''
            
    for k in solutions:
        #print("({0}^({1} + 1) - {0}) / ({0} - 1) = {2}".format(k, n, (k^(n + 1)-k)/(k-1)))
    
        sum += f(k, n)
            
    return sum