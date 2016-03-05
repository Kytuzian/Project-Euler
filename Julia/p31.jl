function find_sum(target, coins::Array)
    ways = 0

    for i = target:-coins[1]:0.0
        if length(coins) == 1
            ways += 1
        else
            ways += find_sum(i, coins[2:end])
        end
    end
    
    return ways
end