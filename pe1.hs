main :: IO()
main = print (sum ([5,10..999] ++ [3,6..999]) - sum [15,30..999])
