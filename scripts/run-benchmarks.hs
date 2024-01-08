#!/usr/bin/env cabal

{- cabal:
    build-depends: base, bytestring, process, text, text-builder-linear
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as ByteString
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Text (Builder)
import Data.Text.Builder.Linear qualified as Text.Builder
import System.Exit
import System.Process qualified as Process
import Text.Printf (printf)

data Results = Results
  { amortizedDeque :: !Result,
    ephemeralQueue :: !Result,
    realTimeDeque :: !Result,
    realTimeQueue :: !Result,
    sequenceQueue :: !Result
  }

data Result
  = Result {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance Monoid Result where
  mempty = Result 0 0 0

instance Semigroup Result where
  Result a1 b1 c1 <> Result a2 b2 c2 = Result (a1 + a2) (b1 + b2) (c1 + c2)

main :: IO ()
main = do
  Process.callCommand "cabal build --enable-benchmarks all"

  amortizedDequeBenchmark <- List.init <$> Process.readProcess "cabal" ["list-bin", "bench-amortized-deque"] ""
  ephemeralQueueBenchmark <- List.init <$> Process.readProcess "cabal" ["list-bin", "bench-ephemeral-queue"] ""
  realTimeDequeBenchmark <- List.init <$> Process.readProcess "cabal" ["list-bin", "bench-real-time-deque"] ""
  realTimeQueueBenchmark <- List.init <$> Process.readProcess "cabal" ["list-bin", "bench-real-time-queue"] ""
  sequenceQueueBenchmark <- List.init <$> Process.readProcess "cabal" ["list-bin", "bench-sequence-queue"] ""

  let runBenchmark :: FilePath -> IO Result
      runBenchmark path = do
        Process.callProcess
          path
          [ "--csv",
            "results.csv",
            "--quiet",
            "--stdev",
            "0.01",
            "--timeout",
            "3",
            "+RTS",
            "-A32m",
            "-N1",
            "-T"
          ]
        readCsv "results.csv"

  let runAmortizedDequeBenchmark :: Results -> IO Results
      runAmortizedDequeBenchmark results = do
        result <- runBenchmark amortizedDequeBenchmark
        pure $! results {amortizedDeque = amortizedDeque results <> result}

  let runEphemeralQueueBenchmark :: Results -> IO Results
      runEphemeralQueueBenchmark results = do
        result <- runBenchmark ephemeralQueueBenchmark
        pure $! results {ephemeralQueue = ephemeralQueue results <> result}

  let runRealTimeDequeBenchmark :: Results -> IO Results
      runRealTimeDequeBenchmark results = do
        result <- runBenchmark realTimeDequeBenchmark
        pure $! results {realTimeDeque = realTimeDeque results <> result}

  let runRealTimeQueueBenchmark :: Results -> IO Results
      runRealTimeQueueBenchmark results = do
        result <- runBenchmark realTimeQueueBenchmark
        pure $! results {realTimeQueue = realTimeQueue results <> result}

  let runSequenceQueueBenchmark :: Results -> IO Results
      runSequenceQueueBenchmark results = do
        result <- runBenchmark sequenceQueueBenchmark
        pure $! results {sequenceQueue = sequenceQueue results <> result}

  let runBenchmarks :: [Results -> IO Results]
      runBenchmarks =
        cycle
          [ runAmortizedDequeBenchmark,
            runEphemeralQueueBenchmark,
            runRealTimeDequeBenchmark,
            runRealTimeQueueBenchmark,
            runSequenceQueueBenchmark
          ]

  let go results0 (bench : benches) = do
        results1 <- bench results0
        renderResults results1
        go results1 benches

  go (Results mempty mempty mempty mempty mempty) runBenchmarks

renderResults :: Results -> IO ()
renderResults
  Results
    { amortizedDeque = Result amortizedDequeRuns amortizedDequeTime0 amortizedDequeMem0,
      ephemeralQueue = Result ephemeralQueueRuns ephemeralQueueTime0 ephemeralQueueMem0,
      realTimeDeque = Result realTimeDequeRuns realTimeDequeTime0 realTimeDequeMem0,
      realTimeQueue = Result realTimeQueueRuns realTimeQueueTime0 realTimeQueueMem0,
      sequenceQueue = Result sequenceQueueRuns sequenceQueueTime0 sequenceQueueMem0
    } = do
    let amortizedDequeInfo, ephemeralQueueInfo, realTimeDequeInfo, realTimeQueueInfo, sequenceQueueInfo :: (Text.Builder, Double, Double, Double)
        amortizedDequeInfo = ("Deque", amortizedDequeRuns, amortizedDequeTime0 / amortizedDequeRuns, amortizedDequeMem0 / amortizedDequeRuns)
        ephemeralQueueInfo = ("Queue.Ephemeral", ephemeralQueueRuns, ephemeralQueueTime0 / ephemeralQueueRuns, ephemeralQueueMem0 / ephemeralQueueRuns)
        realTimeDequeInfo = ("Deque.RealTime", realTimeDequeRuns, realTimeDequeTime0 / realTimeDequeRuns, realTimeDequeMem0 / realTimeDequeRuns)
        realTimeQueueInfo = ("Queue", realTimeQueueRuns, realTimeQueueTime0 / realTimeQueueRuns, realTimeQueueMem0 / realTimeQueueRuns)
        sequenceQueueInfo = ("Seq", sequenceQueueRuns, sequenceQueueTime0 / sequenceQueueRuns, sequenceQueueMem0 / sequenceQueueRuns)

    let renderComparison (name1, runs1, time1, mem1) (name2, runs2, time2, mem2) =
          if runs1 == 0 || runs2 == 0
            then "\n"
            else
              name2
                <> " is "
                <> renderSpeedup time1 time2
                <> " than and allocates "
                <> renderAllocImprovement mem1 mem2
                <> " as much as "
                <> name1
                <> newline

    (ByteString.putStr . Text.Builder.runBuilderBS) $
      Text.Builder.fromText (Text.replicate 80 "=")
        <> newline
        <> renderComparison ephemeralQueueInfo amortizedDequeInfo
        <> renderComparison realTimeDequeInfo amortizedDequeInfo
        <> renderComparison realTimeQueueInfo amortizedDequeInfo
        <> renderComparison sequenceQueueInfo amortizedDequeInfo
        <> Text.Builder.fromText (Text.replicate 40 "-")
        <> newline
        <> renderComparison amortizedDequeInfo ephemeralQueueInfo
        <> renderComparison realTimeDequeInfo ephemeralQueueInfo
        <> renderComparison realTimeQueueInfo ephemeralQueueInfo
        <> renderComparison sequenceQueueInfo ephemeralQueueInfo
        <> Text.Builder.fromText (Text.replicate 40 "-")
        <> newline
        <> renderComparison amortizedDequeInfo realTimeDequeInfo
        <> renderComparison ephemeralQueueInfo realTimeDequeInfo
        <> renderComparison realTimeQueueInfo realTimeDequeInfo
        <> renderComparison sequenceQueueInfo realTimeDequeInfo
        <> Text.Builder.fromText (Text.replicate 40 "-")
        <> newline
        <> renderComparison amortizedDequeInfo realTimeQueueInfo
        <> renderComparison ephemeralQueueInfo realTimeQueueInfo
        <> renderComparison realTimeDequeInfo realTimeQueueInfo
        <> renderComparison sequenceQueueInfo realTimeQueueInfo
    where
      newline = Text.Builder.fromChar '\n'

renderSpeedup :: Double -> Double -> Text.Builder
renderSpeedup old new =
  if old >= new
    then Text.Builder.fromText (Text.pack (printf "%.2f" (old / new))) <> "x faster"
    else Text.Builder.fromText (Text.pack (printf "%.2f" (1 / (old / new)))) <> "x slower"

renderAllocImprovement :: Double -> Double -> Text.Builder
renderAllocImprovement old new =
  Text.Builder.fromText (Text.pack (printf "%.2f" (new / old))) <> Text.Builder.fromChar 'x'

readCsv :: FilePath -> IO Result
readCsv path = do
  [_, line2] <- lines <$> readFile path
  let [_, time, _, alloc, _, _] = words (map (\c -> if c == ',' then ' ' else c) line2)
  pure (Result 1 (read time) (read alloc))
