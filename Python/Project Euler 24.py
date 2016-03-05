def permutations(characters):
    currentIndices = range(characters)
    
    current = ""
    
    result = []
    
    while not (current in result):
        current = [characters[i] for i in currentIndices]
        
        currentIndices = currentIndices[1:] + [currentIndices[0]]
    
    return result