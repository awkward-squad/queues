{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import AmortizedDeque (AmortizedDeque)
import AmortizedDeque qualified
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let n = 100000 :: Int
  defaultMain [bench "AmortizedDeque" (whnf theBenchmark n)]

theBenchmark :: Int -> Int
theBenchmark num =
  loop1 0 0 (AmortizedDeque.enqueue 0 AmortizedDeque.empty)
  where
    loop1 :: Int -> Int -> AmortizedDeque Int -> Int
    loop1 !n !acc queue
      | n == num = loop2 acc queue
      | otherwise =
          case AmortizedDeque.dequeue queue of
            Just (m, queue') -> loop1 (n + 1) (acc + m) (AmortizedDeque.enqueue n (AmortizedDeque.enqueue (n + 1) queue'))

    loop2 :: Int -> AmortizedDeque Int -> Int
    loop2 !n queue =
      case AmortizedDeque.dequeue queue of
        Nothing -> n
        Just (m, queue') -> loop2 (n + m) queue'
