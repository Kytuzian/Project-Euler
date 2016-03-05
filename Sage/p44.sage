sn5=set([x*(3*x-1)/2 for x in range(1,5000)])
for x in sn5:
	for y in sn5:
		if (x+y) in sn5 and(x-y) in sn5:
			print x-y
