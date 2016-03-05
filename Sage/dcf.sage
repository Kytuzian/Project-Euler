def is_digit_cancelling(n, d):
    try:
        cancelled_n = float("".join([i for i in str(n) if not i in str(d)]))
        cancelled_d = float("".join([i for i in str(d) if not i in str(n)]))
    except:
        return False
    
    if cancelled_d == 0:
        return False
        
    lengths = len(str(cancelled_n)) - 2 < len(str(n))
    
    return (float(n) / d) == (cancelled_n / cancelled_d) and lengths
    
result = []

for n in xrange(12, 100):
    for d in xrange(n + 1, 100):
        if is_digit_cancelling(n, d):
            result.append((n, d))

result = [i for i in result if not "0" in str(i[0])]
result = [Integer(i[0]) / Integer(i[1]) for i in result]
print(result)
result = reduce(lambda a, b: a * b, result)
print(result)