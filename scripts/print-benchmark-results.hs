#!/usr/bin/env cabal

{- cabal:
    build-depends: base
-}

module Main where

import Text.Printf (printf)

main :: IO ()
main = do
  (bankersTime, bankersMem) <- readCsv "bankers-queue.csv"
  (ephemeralTime, ephemeralMem) <- readCsv "ephemeral-queue.csv"
  (realTimeTime, realTimeMem) <- readCsv "real-time-queue.csv"
  (sequenceTime, sequenceMem) <- readCsv "sequence-queue.csv"

  let speedup :: Double -> Double -> String
      speedup old new =
        if old >= new
          then printf "%.2fx faster" (old / new)
          else printf "%.2fx slower" (1 / (old / new))

  putStrLn "banker's queue is..."
  printf "  %s than ephemeral queue\n" (speedup ephemeralTime bankersTime)
  printf "  %s than real-time queue\n" (speedup realTimeTime bankersTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime bankersTime)

  putStrLn "ephemeral queue is..."
  printf "  %s than banker's queue\n" (speedup bankersTime ephemeralTime)
  printf "  %s than real-time queue\n" (speedup realTimeTime ephemeralTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime ephemeralTime)

  putStrLn "real-time queue is..."
  printf "  %s than banker's queue\n" (speedup bankersTime realTimeTime)
  printf "  %s than ephemeral queue\n" (speedup ephemeralTime realTimeTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime realTimeTime)

  let allocImprovement :: Double -> Double -> String
      allocImprovement old new =
        printf "%.2fx" (new / old)

  putStrLn "banker's queue allocates..."
  printf "  %s as much as ephemeral queue\n" (allocImprovement ephemeralMem bankersMem)
  printf "  %s as much as real-time queue\n" (allocImprovement realTimeMem bankersMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem bankersMem)

  putStrLn "ephemeral queue allocates..."
  printf "  %s as much as banker's queue\n" (allocImprovement bankersMem ephemeralMem)
  printf "  %s as much as real-time queue\n" (allocImprovement realTimeMem ephemeralMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem ephemeralMem)

  putStrLn "real-time queue allocates..."
  printf "  %s as much as banker's queue\n" (allocImprovement bankersMem realTimeMem)
  printf "  %s as much as ephemeral queue\n" (allocImprovement ephemeralMem realTimeMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem realTimeMem)

readCsv :: FilePath -> IO (Double, Double)
readCsv path = do
  [_, line2] <- lines <$> readFile path
  let [_, time, _, alloc, _, _] = words (map (\c -> if c == ',' then ' ' else c) line2)
  pure (read time, read alloc)
