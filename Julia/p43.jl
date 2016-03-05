function is_answer(n)
    num = "$n"
    checks = [2,3,5,7,11,13,17]
    
    #println("$(length(num)): $n")
    
    for i=1:7
        if int(num[i + 1:i + 3]) % checks[i] > 0
            return false
        end
    end
    
    return true
end

function solve()
    numbers = [int(i) for i in permutations("1234567890")]
    
    numbers = filter((i) -> length("$i") == 10, numbers)
    numbers = filter((i) -> is_answer(i), numbers)
    
    return numbers
end