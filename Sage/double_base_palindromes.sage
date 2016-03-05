def reverse(n):
    if len(n) == 1:
        return n
    else:
        return n[-1] + reverse(n[:-1])

def isPalindrome(n):
    return str(n) == reverse(str(n))
    
palindromes = [i for i in IntegerRange(1000000) if isPalindrome(i) and isPalindrome(i.str(2))]
print(sum(palindromes))