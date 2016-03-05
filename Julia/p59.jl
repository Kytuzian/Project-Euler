function crypt(message::Array{Int64, 1}, key::Array{Int64, 1})
    result = Int64[]

    for (i,k) in enumerate(message)
        push!(result, k $ key[(i - 1) % length(key) + 1])
    end
    
    return result
end

function to_ascii(message::ASCIIString)
    return [int(i) for i in message]
end

function get_ascii(message::Array{Int64, 1})
    return reduce((a, b) -> "$a$b", [char(i) for i in message])
end

function solve()
    f = open("p059_cipher.txt", "r")
    message=Int64[int(i) for i=split(readall(f), ",")]
    
    s = "qwertyuiopasdfghjklzxcvbnm"
    for i=["$a$b$c" for a=s, b=s, c=s]
        result = crypt(message, to_ascii(i))
        
        if !(int('{') in result) && !(int('}') in result) && !(int('~') in result) && !(int('/') in result)
            color = (sum(to_ascii(i)) % 2 == 0) ? :red : :blue
            print_with_color(color, "$i\n")
            print_with_color(color, "$(get_ascii(result))\n")
        end
    end
end