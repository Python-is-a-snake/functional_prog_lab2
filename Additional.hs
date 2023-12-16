module Additional where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad.State
type ThreadChan = Chan (Either Bool ([Int], Bool))
type CustomChan = (ThreadChan, ThreadChan)

splitEveryLimit :: Int -> [a] -> Int -> [[a]]
splitEveryLimit _ [] m = []
splitEveryLimit _ xs 0 = [xs]
splitEveryLimit n list m = first : splitEveryLimit n rest (m-1)
  where
    (first,rest) = splitAt n list


subdivideArray :: [a1] -> Int -> [[a1]]
subdivideArray arr m = let n = length arr; p = div n m in
    reverse (splitEveryLimit p arr m)


mergeArrays :: Ord a => [a] -> [a] -> [a]
mergeArrays xs [] = xs
mergeArrays [] ys = ys
mergeArrays (x:xs) (y:ys) = if x < y then x : mergeArrays xs (y:ys)
                                     else y : mergeArrays ys (x:xs)


preprocessArray :: [Int] -> Int -> ([[Int]], Int)
preprocessArray arr n = let m = length arr; md = mod m n in
                        if md == 0 then
                            (subdivideArray arr n, 0)
                        else
                            let mn = minimum arr; cnt = n - md  ; in
                                (subdivideArray (replicate cnt mn ++arr) n, cnt)


makeNChannels :: Int -> IO [CustomChan]
makeNChannels 0 = return []
makeNChannels n = do
    rest <- makeNChannels (n-1)
    cur_in <- newChan
    cur_out <- newChan
    return ((cur_in, cur_out):rest)
