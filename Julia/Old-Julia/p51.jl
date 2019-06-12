function replace_positions(seq::Array{Int64, 1}, positions::Array{Int64, 1}, digit::Int64)
    val = deepcopy(seq)

    for i = positions
        val[i] = digit
    end
    
    return val
end

function next_prime(n)
    if n % 2 == 0
        n += 1
    end

    i = 2
    
    while !isprime(n + i)
        i += 2
    end
    
    return n + i
end

construct_number(digits::Array{Int64, 1}) = length(digits) == 0 ? 0 : digits[1] * 10^(length(digits) - 1) + construct_number(digits[2:end])

function prime_digit_replacements(n, max_count)
    n = reverse(digits(n))
    
    comb = reduce(vcat, map(i -> collect(combinations(1:length(n) - 1, i)), 1:length(n) - 1))
    
    max_prime_count = 0
    
    for (i, c) = enumerate(comb)
        if length(Set(n[comb[i]])) > 1
            continue
        end
    
        prime_count = 0
    
        start = c[1][1] == 1 ? 1 : 0
    
        for digit = start:9
            if (prime_count + 9 - digit + 1) < max_count
                break
            end
        
            if isprime(construct_number(replace_positions(n, c, digit)))
                prime_count += 1
            end
        end
        
        if prime_count > max_prime_count
            max_prime_count = prime_count
        end
    end
    
    return max_prime_count
end

function first_where(test, generator, init, optional=nothing)
    i = init

    while !test(i, optional)
        i = generator(i)
    end
    
    return i
end

p51() = first_where((i, k) -> prime_digit_replacements(i, k) == 8, next_prime, 11, 8)