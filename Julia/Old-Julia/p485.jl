function d2(n)
    count = 0
    
    i = 1
    
    while i ^ 2 < n
        if n % i == 0
            count += 1
            
            if div(n, i) != i
                count += 1
            end
        end
        
        i += 1
    end
    
    return count
end

d(n) = reduce(*, map(i -> i + 1, values(factor(n))))

function M(n, k)
    max = 0
    dmax = 0
    
    for i = n:n + k - 1
        if i == 1
            dval = 1
        else
            dval = d(i)
        end
    
        if dval > dmax
            max = i
            dmax = dval
        end
    end
    
    return dmax
end

S(u, k) = sum(map(n -> M(n, k), 1:u - k + 1))