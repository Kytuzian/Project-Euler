import slow

def is_odd_number(n):
    for i in str(n):
        if i in "0468":
            return False
    
    return True
    
def is_truncatable_prime_left(n):
    if slow.f.isprime(n):
        if len(str(n)) == 1:
            return True
        else:
            return is_truncatable_prime_left(int(str(n)[1:]))
    else:
        return False
        
def is_truncatable_prime_right(n):
    if slow.f.isprime(n):
        if len(str(n)) == 1:
            return True
        else:
            return is_truncatable_prime_right(int(str(n)[:-1]))
    else:
        return False
    
def get_truncatable_primes():
    result = []
    count = 0
    
    n = 0

    for i in slow.f.primes:
        if len(str(i)) == 1:
            continue
            
        if not is_odd_number(i):
            continue
        
        if is_truncatable_prime_left(i) and is_truncatable_prime_right(i):
            count += 1
            
            print(count, i)
            
            result.append(i)
        
            if (count == 11):
                break
            
        n += 1
                
    return result