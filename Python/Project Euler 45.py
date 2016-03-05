start, end = 0, 100000

triangular = [n * (n + 1) / 2 for n in xrange(start, end)]
pentagonal = [n * (3 * n - 1) / 2 for n in xrange(start, end)]
hexagonal = [n * (2 * n - 1) for n in xrange(start, end)]

intersect1 = [n for n in triangular if n in pentagonal]
intersect2 = [n for n in intersect1 if n in hexagonal]

print(intersect1)
print(intersect2)