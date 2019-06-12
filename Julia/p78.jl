sums_list(n) = [(i, n - i) for i=1:(n - 1)]

function p(n)
    if n == 1
        return 1
    else
        result = 0

        for (x, y) in sums_list(n)
            result += p(x) + p(y)
        end

        return result
    end
end
