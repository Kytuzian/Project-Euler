def is_square(n):
    return n in {k: True for k in polygonal_number(4, n + 1)}

def find_minimal_diophantine_solution_x(f, D):
    f = f.subs(D=D)

    x = 1
    while True:
        #sys.stdout.write('\r({}, {}) '.format(x, ((x^2 - 1) / D).n()))
        #sys.stdout.flush()
        if (x^2 - 1) / D in squares:
            y = sqrt((x^2 - 1) / D)

            if y > 0:
                return (x, y)

        x += 1

def find_minimal_diophantine_solution_y(f, D):
    f = f.subs(D=D)

    y = 1
    while True:
        sys.stdout.write('\r({}, {}) '.format(y, (D*y^2 + 1).n()))
        sys.stdout.flush()
        if D*y^2 + 1 in squares:
            x = sqrt(D*y^2 + 1)

            if x > 0:
                return (x, y)

        y += 1

def p66(Ds):
    f = dix^2 - D*diy^2

    Ds = filter_with_progress(lambda x: not is_square(x), Ds)
    sols = map_with_progress(lambda Dv: (Dv, find_minimal_diophantine_solution(f.subs(D, Dv) - 1)), Ds, show_current=True)

    return sols
