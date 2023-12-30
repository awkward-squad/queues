{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import RealTimeQueue (RealTimeQueue)
import RealTimeQueue qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "RealTimeQueue" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (RealTimeQueue.enqueue 0 RealTimeQueue.empty)
  where
    loop1 :: Int -> Int -> RealTimeQueue Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case RealTimeQueue.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (RealTimeQueue.enqueue n (RealTimeQueue.enqueue (n + 1) queue'))

    loop2 :: Int -> RealTimeQueue Int -> Int
    loop2 !n queue =
      case RealTimeQueue.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
