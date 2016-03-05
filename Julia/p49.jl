function is_permutation(terms)
    string_terms = ASCIIString[]
    
    for i in terms
        push!(string_terms, "$i")
    end

    p = [i for i in permutations(string_terms[1])]
    
    for i in string_terms
        if !(i in p)
            return false
        end
    end
    
    return true
end

function test_sequence(start, increment, stop, prime_list)
    terms = [i for i=start:increment:9999]

    if !is_permutation(terms)
        return false
    end
    
    if length(terms) != 3
        return false
    end
    
    return length(filter((i) -> i in prime_list, terms)) == length(terms)
end

function solve()
    prime_list = primes(10000)
    
    results = (Int64, Int64)[]

    for start=1000:9999
        if start % 100 == 0
            println("Trying with start value: $start")
        end
        
        if !(start in prime_list)
            continue
        end
    
        for increment=div(prime_list[end]-start,3):div(prime_list[end]-start, 2)
            if increment <= 0
                continue
            end
        
            if test_sequence(start, increment, 9999, prime_list)
                println(start:increment:9999)
            
                push!(results, (start, increment))
            end
        end
    end
    
    return results
end