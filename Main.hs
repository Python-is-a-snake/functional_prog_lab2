module Main where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)

import Control.Concurrent (forkIO, threadDelay, takeMVar, MVar, putMVar, newEmptyMVar)
import Language.Haskell.TH (safe)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import Additional
import Threads


unsortedArray = [1000000,999999..1] ++ [16,15,4,5,13,7,8,11,12,2,1,3,10,6,9]

lab2 :: Int -> IO ()
lab2 n = do
    ioMutex  <- newEmptyMVar
    putMVar ioMutex True
    channels <- makeNChannels n
    let (arrs, rmv) = preprocessArray unsortedArray n
    finished <- newChan
    startWorkingThreads n channels arrs ioMutex

    forkIO (do
        mainThread finished n channels ioMutex
        return ())

    result <- readChan finished
    unboxed <- result
    let originalSortedArray = drop rmv unboxed
    print $ head originalSortedArray 
    print "..."
    print $ last originalSortedArray 
    return ()


main :: IO()
main = do
    let n = 1
    start  <- getSystemTime
    lab2 n
    end <- getSystemTime
    print $ "time: " ++ show (systemNanoseconds end - systemNanoseconds start)




