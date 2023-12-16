module Threads where
import Additional
import Batcher
import Control.Concurrent (forkIO, threadDelay, takeMVar, MVar, putMVar, newEmptyMVar)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (sort)

sorterThread ::  [Int] -> CustomChan -> MVar Bool -> IO()
sorterThread array = workerThread (sort array)

sleepMs n = threadDelay (n * 1000)

startWorkingThreads :: Int -> [CustomChan] -> [[Int]] -> MVar Bool -> IO()
startWorkingThreads n channels arrs ioMutex = do
    for_ [0..n-1] (startFork channels arrs) where
        startFork chans ars i = forkIO(
            do
                sorterThread (ars !! i) (chans !! i) ioMutex)

safePrint :: MVar Bool -> String -> IO()
safePrint ioMutex s = 
    when debugMode $ do 
        available <- takeMVar ioMutex
        putStrLn s
        putMVar ioMutex available
        return ()

mainThread :: Chan (IO [Int]) -> Int -> [CustomChan] -> MVar Bool -> IO ()
mainThread fin n chans ioMutex = do
    let pairs = generateNet n
    goThroughPairs pairs
    writeChan fin $ concatMap fst <$> mapM getFinal [0..n-1]
    return ()
        where
            goThoughPair (up, down) = do
                let (to_up, from_up) = chans !! up
                let (to_down, from_down) = chans !! down
                writeChan to_up (Left True)
                writeChan to_down (Left True)
                Right (vu,_) <- readChan from_up
                Right (vd,_) <- readChan from_down

                safePrint ioMutex $ show up ++ " " ++ show vu
                safePrint ioMutex $ show down ++ " " ++ show vd

                writeChan to_up (Right (vd, True))
                writeChan to_down (Right (vu, False))
            getFinal x = do
                let (to_chan, from_chan) = chans !! x
                writeChan to_chan (Left False)
                Right res <- readChan from_chan
                return res
            goThroughPairs prs = do
                 for_ [0..length prs-1] go where
                    go i = goThoughPair (prs !! i)


workerThread ::  [Int] -> CustomChan -> MVar Bool -> IO()
workerThread array chans@(chan_in, chan_out) ioMutex = do
    safePrint ioMutex $ "started with " ++ show array
    Left go_on <- readChan chan_in
    safePrint ioMutex $ "Got " ++ show go_on
    writeChan chan_out (Right (array, True))
    when go_on $ do
        --sleepMs 100
        Right (other, am_I_top) <- readChan chan_in
        let n = length array
        let m = length other
        let mixed = mergeArrays array other
        if am_I_top then
            workerThread (take n mixed) chans ioMutex
        else
            workerThread (drop m mixed) chans ioMutex
