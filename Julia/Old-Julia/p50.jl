function solve(f, filter_f, collection)
    @everywhere include("p50.jl")

    result = @parallel vcat for (i, k) = collect(enumerate(collection))
        f(i, k)
    end
    
    result = maximum(map(i -> (length(i[2]), i[1]), filter(filter_f, result)))
    
    return result
end

function consecutive_sum_to_target(target, items::Array)
    items = sort(items)
    
    for start = 1:length(items) - 1
        val = items[start]
    
        for i = start + 1:length(items)
            val += items[i]
    
            if val == target
                return items[start:i]
            elseif val > target
                break
            end
        end
    end
    
    return false
end