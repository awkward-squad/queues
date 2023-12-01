{-# OPTIONS_GHC -ddump-stg-final -ddump-to-file #-}

module Main (main) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import EphemeralQueue qualified
import Queue qualified
import RealTimeDeque qualified
import RealTimeQueue qualified
import Test.Tasty (localOption)
import Test.Tasty.Bench (RelStDev (..), bench, bgroup, defaultMain, whnf)

main :: IO ()
main =
  defaultMain
    [ let n = 10000 :: Int
          dataseq = useQueueInARealisticWay Seq.empty seqEnqueue seqDequeue
          queue = useQueueInARealisticWay Queue.empty Queue.enqueue Queue.dequeue
          rtqueue = useQueueInARealisticWay RealTimeQueue.empty RealTimeQueue.enqueue RealTimeQueue.dequeue
          equeue = useQueueInARealisticWay EphemeralQueue.empty EphemeralQueue.enqueue EphemeralQueue.dequeue
          rtdeque = useQueueInARealisticWay RealTimeDeque.empty RealTimeDeque.enqueue RealTimeDeque.dequeue
       in localOption (RelStDev 0.02) $
            bgroup
              "realistic usage"
              [ bench "Seq" (whnf dataseq n),
                bench "Queue" (whnf queue n),
                bench "RealTimeQueue" (whnf rtqueue n),
                bench "EphemeralQueue" (whnf equeue n),
                bench "RealTimeDeque" (whnf rtdeque n)
              ]
    ]

seqEnqueue :: a -> Seq a -> Seq a
seqEnqueue = \x xs -> xs Seq.|> x
{-# INLINE seqEnqueue #-}

seqDequeue :: Seq a -> Maybe (a, Seq a)
seqDequeue = \xs ->
  case Seq.viewl xs of
    Seq.EmptyL -> Nothing
    y Seq.:< ys -> Just (y, ys)
{-# INLINE seqDequeue #-}

useQueueInARealisticWay ::
  forall queue.
  queue Int ->
  (Int -> queue Int -> queue Int) ->
  (queue Int -> Maybe (Int, queue Int)) ->
  Int ->
  Int
useQueueInARealisticWay empty enqueue dequeue = \num ->
  let -- loop1: dequeue 1, enqueue 2
      loop1 :: Int -> Int -> queue Int -> Int
      loop1 !n !acc queue
        | n == num = loop2 acc queue
        | otherwise =
            case dequeue queue of
              Nothing -> undefined
              Just (m, queue') -> loop1 (n + 1) (acc + m) (enqueue n (enqueue (n + 1) queue'))

      -- loop2: dequeue
      loop2 :: Int -> queue Int -> Int
      loop2 !n queue =
        case dequeue queue of
          Nothing -> n
          Just (m, queue') -> loop2 (n + m) queue'
   in loop1 0 0 (enqueue 0 empty)
{-# INLINE useQueueInARealisticWay #-}
