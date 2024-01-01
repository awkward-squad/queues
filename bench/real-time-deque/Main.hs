{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import RealTimeDeque (RealTimeDeque)
import RealTimeDeque qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "RealTimeDeque" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (RealTimeDeque.enqueue 0 RealTimeDeque.empty)
  where
    loop1 :: Int -> Int -> RealTimeDeque Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case RealTimeDeque.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (RealTimeDeque.enqueue n (RealTimeDeque.enqueue (n + 1) queue'))

    loop2 :: Int -> RealTimeDeque Int -> Int
    loop2 !n queue =
      case RealTimeDeque.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
