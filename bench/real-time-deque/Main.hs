{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Deque.RealTime (RealTimeDeque)
import Deque.RealTime qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "RealTimeDeque" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (Deque.RealTime.enqueue 0 Deque.RealTime.empty)
  where
    loop1 :: Int -> Int -> RealTimeDeque Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case Deque.RealTime.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (Deque.RealTime.enqueue n (Deque.RealTime.enqueue (n + 1) queue'))

    loop2 :: Int -> RealTimeDeque Int -> Int
    loop2 !n queue =
      case Deque.RealTime.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
