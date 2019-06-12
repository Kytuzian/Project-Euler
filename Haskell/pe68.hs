import Data.List

data Side a = Side (a, a, a)
    deriving (Show)

data NGon a = NGon (Side a) (NGon a) | End
    deriving (Show)
