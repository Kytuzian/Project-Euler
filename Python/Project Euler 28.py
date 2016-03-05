def spiral(f, startNumber, size):
    spiral = []
    
    for x in xrange(0, size[0]):
        spiral.append([])
    
        for y in xrange(0, size[1]):
            spiral.append(0)
    
    width = 1
    currentwidth = 0
    dx, dy = 1, 0
    x, y = int(float(size[0] / 2) + 0.5), int(float(size[1] / 2) + 0.5)
    
    maxWidth = size[0], maxHeight = size[1]
    i = startNumber
    
    while True:
        x, y = x + dx, y + dy
        currentwidth += 1
        
        spiral[x][y] = f(i)
        
        if (currentwidth == width):
            true = (dx + 1, dy + 1)
            true[0] += 1
            true[1] = true[0] / 3
            true[0] = true[0] % 3
            
            dx, dy = true[0] - 1, true[1] - 1
        
        if (x == maxWidth - 1) and (y == 0):
            break
        
        i += 1
    return spiral