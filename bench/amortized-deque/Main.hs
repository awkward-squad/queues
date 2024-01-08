{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Deque (Deque)
import Deque qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "Deque" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (Deque.enqueue 0 Deque.empty)
  where
    loop1 :: Int -> Int -> Deque Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case Deque.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (Deque.enqueue n (Deque.enqueue (n + 1) queue'))

    loop2 :: Int -> Deque Int -> Int
    loop2 !n queue =
      case Deque.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
