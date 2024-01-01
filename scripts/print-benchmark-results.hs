#!/usr/bin/env cabal

{- cabal:
    build-depends: base
-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Text.Printf (printf)

main :: IO ()
main = do
  (amortizedTime, amortizedMem) <- readCsvs "amortized-queue" 5
  (ephemeralTime, ephemeralMem) <- readCsvs "ephemeral-queue" 5
  (realTimeTime, realTimeMem) <- readCsvs "real-time-queue" 5
  (sequenceTime, sequenceMem) <- readCsvs "sequence-queue" 5

  let speedup :: Double -> Double -> String
      speedup old new =
        if old >= new
          then printf "%.2fx faster" (old / new)
          else printf "%.2fx slower" (1 / (old / new))

  putStrLn "amortized queue is..."
  printf "  %s than ephemeral queue\n" (speedup ephemeralTime amortizedTime)
  printf "  %s than real-time queue\n" (speedup realTimeTime amortizedTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime amortizedTime)

  putStrLn "ephemeral queue is..."
  printf "  %s than amortized queue\n" (speedup amortizedTime ephemeralTime)
  printf "  %s than real-time queue\n" (speedup realTimeTime ephemeralTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime ephemeralTime)

  putStrLn "real-time queue is..."
  printf "  %s than amortized queue\n" (speedup amortizedTime realTimeTime)
  printf "  %s than ephemeral queue\n" (speedup ephemeralTime realTimeTime)
  printf "  %s than sequence queue\n" (speedup sequenceTime realTimeTime)

  let allocImprovement :: Double -> Double -> String
      allocImprovement old new =
        printf "%.2fx" (new / old)

  putStrLn "amortized queue allocates..."
  printf "  %s as much as ephemeral queue\n" (allocImprovement ephemeralMem amortizedMem)
  printf "  %s as much as real-time queue\n" (allocImprovement realTimeMem amortizedMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem amortizedMem)

  putStrLn "ephemeral queue allocates..."
  printf "  %s as much as amortized queue\n" (allocImprovement amortizedMem ephemeralMem)
  printf "  %s as much as real-time queue\n" (allocImprovement realTimeMem ephemeralMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem ephemeralMem)

  putStrLn "real-time queue allocates..."
  printf "  %s as much as amortized queue\n" (allocImprovement amortizedMem realTimeMem)
  printf "  %s as much as ephemeral queue\n" (allocImprovement ephemeralMem realTimeMem)
  printf "  %s as much as sequence queue\n" (allocImprovement sequenceMem realTimeMem)

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
