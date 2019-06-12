function pentagonal(x)
    return div(x * (3*x - 1), 2)
end

function try_section(new_numbers, old_numbers)
    for i in new_numbers
        for o in old_numbers
            if (i - o) in old_numbers && ((i + o) in old_numbers || (i + o) in new_numbers)
                return (o, i, i - o)
            end
        end
    end

    return -1
end

function solve()
    n = 1
    processor = 1
    
    numbers = [pentagonal(i) for i=1:n]
    
    processes = RemoteRef[]
    
    @everywhere include("p44.jl")

    while true
        old_n = n
        n += 1000
        
        new_numbers = [pentagonal(i) for i=old_n:n]
        
        push!(processes, @spawnat processor try_section(new_numbers, numbers))
        
        println("New process created at processor $processor for range $old_n to $n.")
        
        processor += 1
        
        if processor > nprocs()
            processor = 1
            
            result = take!(processes[processor])
            
            deleteat!(processes, processor)
        end
        
        x = 1
        while x < length(processes)
            if isready(processes[x])
                result = take!(processes[x])
                
                if result != -1
                    return result
                end
                
                deleteat!(processes, x)
            else
                x += 1
            end
        end
        
        append!(numbers, new_numbers)
    end
    
    return "Failed"
end