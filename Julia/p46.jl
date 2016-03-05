function get_solution(n, primes, squares)
    for p in primes
        for s in squares
            if p + 2 * s == n
                #println("$p + 2 * $(sqrt(s))^2 == $n")
            
                return true
            end
        end
    end
    
    return false
end

function solve()
    n = 9
    
    squares = [i^2 for i=1:n]
    
    while true
        p = primes(n)
    
        squares = [i^2 for i=1:n]
        
        if !get_solution(n, p, squares) && (n in p == false)
            return n
        end
        
        n += 2
    end
end