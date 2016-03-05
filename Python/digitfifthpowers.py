def get_digits(n):
    return [int(i) for i in str(n)]

def get_power_sum(digits, power):
    return sum([i**power for i in digits])

start, end = 0, 2000

total = 0
power = 5

i = 2
while i < 1000000:
    matches = get_power_sum(get_digits(i), 5) == i
    
    if (matches):
        print(i)
        
        total += i

    i += 1
    
print("Total: " + str(total))