def prime_partitions(n):
    partitions = build_sums(n)

    return filter(lambda i: all(map(is_prime, i)), partitions)
