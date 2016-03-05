def all_odd_digits(n):
    for i in str(n):
        if i in "0468":
            return False

    return True

def is_circular_prime(n):
    if not all_odd_digits(n):
        return False

    strn = str(n)

    for i in xrange(len(str(n))):
        if not is_prime(Integer(strn[i:] + strn[:i])):
            return False

    return True
