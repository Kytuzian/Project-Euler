class MagicSide:
    def __init__(self, maxn):
        self.prev = None

        self.a = [1..maxn]
        self.b = [1..maxn]
        self.c = [1..maxn]

    def setup(self, prev):
        self.prev = prev

        self.b = prev.c

class MagicNGon:
    def __init__(self, n, maxn):
        self.sides = [MagicSide(maxn) for i in xrange(n)]
        for i, side in enumerate(self.sides):
            side.setup(self.sides[i - 1])
