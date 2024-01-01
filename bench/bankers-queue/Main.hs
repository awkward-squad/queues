{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import AmortizedQueue (AmortizedQueue)
import AmortizedQueue qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "AmortizedQueue" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (AmortizedQueue.enqueue 0 AmortizedQueue.empty)
  where
    loop1 :: Int -> Int -> AmortizedQueue Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case AmortizedQueue.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (AmortizedQueue.enqueue n (AmortizedQueue.enqueue (n + 1) queue'))

    loop2 :: Int -> AmortizedQueue Int -> Int
    loop2 !n queue =
      case AmortizedQueue.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
