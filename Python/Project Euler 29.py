import numpy

powers = [a^b for a in xrange(2, 101) for b in xrange(2, 101)]
unique = list(numpy.unique(powers))
unique.sort()
len(unique)