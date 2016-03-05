def next_factorial_c2hain(n):
    return sum(map(factorial, digits(n)))

def next_factorial_chain(n):
	answer = 0
	current = n
	while current > 0:
        print(current)
        answer += factorial(current % 10)
        current /= 10
	return answer

def factorial_chain(n, prev=None):
    if prev == None:
        prev = [n]
    next_val = next_factorial_chain(n)

    if next_val in prev:
        return [next_val]
    else:
        return [next_val] + factorial_chain_mem(next_val, prev + [next_val])

factorial_chain_mem = memoize_cycle(factorial_chain)
