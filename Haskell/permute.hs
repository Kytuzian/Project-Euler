permute :: [[a]] -> [[[a]]]
permute [] = []
permute (x:[]) = [[x]]
permute (x:xs) = 