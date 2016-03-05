function get_solutions(n::BigInt)
    result = BigInt[]
    
    for i in [0:n - 1]
        push!(result, -(i^2+2i))
    end
    
    return result
end

function f(k::BigInt, n::BigInt)
    return div((k^(n + 1) - k), (k - 1))
end

function S(n::BigInt)
    sum = BigInt(0)
    
    solutions = get_solutions(n)
    
    @everywhere include("p479.jl")
    
    sum = @parallel (+) for k in solutions
        f(k, n)
    end
    
    return sum % BigInt(1000000007)
end