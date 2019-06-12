{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

natural quantifier name = do
    n :: SInteger <- quantifier name
    constrain (n .> 0)
    return n

p72 = do
    let limit = 1000000
    d <- natural exists "d"
    n <- natural exists "n"
    a <- natural exists "a"

    constrain $ d .<= limit
    constrain $ n .< d
    constrain $ a * n .== d
    return $ a 

    return $ a * n ./= d
