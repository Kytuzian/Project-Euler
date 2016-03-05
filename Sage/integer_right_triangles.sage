def is_integral_rtriangle(a, b, c, squares):
    c_squared = c^2

    return (a^2 + b^2 == c_squared) and c_squared in squares and not (a == c) and not (b == c)

def generate_squares(limit):
    squares = {1: 1}

    for i in xrange(limit + 1):
        squares[i^2] = True

    return squares

def solve_pe39(max_p):
    result = []

    squares = generate_squares(max_p)

    for p in xrange(max_p):
        result.append([])

        for a in xrange(p):
            for b in xrange(a, p):
                c = p - a - b

                if is_integral_rtriangle(a,b,c,squares):
                    result[p].append((a,b,c))

    return result
