module Batcher where

generateNet :: Int -> [(Int, Int)]
generateNet n = fb [0..(n-1)]

fb :: [Int] -> [(Int, Int)]
fb [] = []
fb [x] = []
fb xs = let n = length xs `div` 2; (up, down) = splitAt n xs in
    fb up ++ fb down ++ fs up down

fs :: [Int] -> [Int] -> [(Int, Int)]
fs [] [] = []
fs up down = let conc = up ++ down; in
        case length conc of
    1 -> []
    2 -> [firstPair conc]
    n ->
        let
            (up_odd, up_even) = splitParity up;
            (down_odd, down_even) = splitParity down
        in
            fs up_odd down_odd ++ fs up_even down_even ++ [firstPair xx | xx <- map (`drop` conc) [1, 3.. n], length xx > 1]

firstPair :: [b] -> (b, b)
firstPair xs = (head xs, (head.tail) xs)

splitParity :: [a] -> ([a], [a])
splitParity xs = par' 1 [] [] xs where
    par' n od ev [] = (reverse od, reverse ev)
    par' n od ev (x:xs) = if odd n then
                            par' (n+1) (x:od) ev xs
                            else
                            par' (n+1) od (x:ev) xs


