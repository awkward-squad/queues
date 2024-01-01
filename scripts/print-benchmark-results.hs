#!/usr/bin/env cabal

{- cabal:
    build-depends: base
-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Text.Printf (printf)

main :: IO ()
main = do
  (ephemeralQueueTime, ephemeralQueueMem) <- readCsvs "ephemeral-queue" 5
  (realTimeDequeTime, realTimeDequeMem) <- readCsvs "real-time-deque" 5
  (realTimeQueueTime, realTimeQueueMem) <- readCsvs "real-time-queue" 5
  (sequenceTime, sequenceMem) <- readCsvs "sequence-queue" 5

  let speedup :: Double -> Double -> String
      speedup old new =
        if old >= new
          then printf "%.2fx faster" (old / new)
          else printf "%.2fx slower" (1 / (old / new))

  let allocImprovement :: Double -> Double -> String
      allocImprovement old new =
        printf "%.2fx" (new / old)

  putStrLn "ephemeral queue..."
  printf "  is %s than and allocates %s as much as real-time deque\n" (speedup realTimeDequeTime ephemeralQueueTime) (allocImprovement realTimeDequeMem ephemeralQueueMem)
  printf "  is %s than and allocates %s as much as real-time queue\n" (speedup realTimeQueueTime ephemeralQueueTime) (allocImprovement realTimeQueueMem ephemeralQueueMem)
  printf "  is %s than and allocates %s as much as sequence queue\n" (speedup sequenceTime ephemeralQueueTime) (allocImprovement sequenceMem ephemeralQueueMem)

  putStrLn "real-time deque..."
  printf "  is %s than and allocates %s as much as ephemeral queue\n" (speedup ephemeralQueueTime realTimeDequeTime) (allocImprovement ephemeralQueueMem realTimeDequeMem)
  printf "  is %s than and allocates %s as much as real-time queue\n" (speedup realTimeQueueTime realTimeDequeTime) (allocImprovement realTimeQueueMem realTimeDequeMem)
  printf "  is %s than and allocates %s as much as sequence queue\n" (speedup sequenceTime realTimeDequeTime) (allocImprovement sequenceMem realTimeDequeMem)

  putStrLn "real-time queue..."
  printf "  is %s than and allocates %s as much as ephemeral queue\n" (speedup ephemeralQueueTime realTimeQueueTime) (allocImprovement ephemeralQueueMem realTimeQueueMem)
  printf "  is %s than and allocates %s as much as real-time deque\n" (speedup realTimeDequeTime realTimeQueueTime) (allocImprovement realTimeDequeMem realTimeQueueMem)
  printf "  is %s than and allocates %s as much as sequence queue\n" (speedup sequenceTime realTimeQueueTime) (allocImprovement sequenceMem realTimeQueueMem)

readCsvs :: [Char] -> Int -> IO (Double, Double)
readCsvs name =
  go (0, 0)
  where
    go (!time, !mem) n
      | n == 0 = pure (time, mem)
      | otherwise = do
          (time1, mem1) <- readCsv (name ++ "-" ++ show n ++ ".csv")
          go (time + time1, mem + mem1) (n - 1)

readCsv :: FilePath -> IO (Double, Double)
readCsv path = do
  [_, line2] <- lines <$> readFile path
  let [_, time, _, alloc, _, _] = words (map (\c -> if c == ',' then ' ' else c) line2)
  pure (read time, read alloc)
