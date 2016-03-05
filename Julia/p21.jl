d(n) = n < 3 ? n - 1 : sum([i + div(n, i) for i = filter(i -> n % i == 0, 2:int(sqrt(n)))]) + 1
is_amicable(n) = d(d(n)) == n && d(n) != n
p21_answer() = sum(filter(is_amicable, 1:10000))