module Main (main) where

import Data.Sequence qualified as Seq
import Queue qualified
import RealTimeDeque qualified
import RealTimeQueue qualified
import Test.Tasty (localOption)
import Test.Tasty.Bench (RelStDev (..), bcompare, bench, bgroup, defaultMain, whnf)

main :: IO ()
main =
  defaultMain
    [ let n = 10000 :: Int
          queue = useQueueInARealisticWay Queue.empty Queue.enqueue Queue.dequeue
          rtqueue = useQueueInARealisticWay RealTimeQueue.empty RealTimeQueue.enqueue RealTimeQueue.dequeue
          rtdeque = useQueueInARealisticWay RealTimeDeque.empty RealTimeDeque.enqueue RealTimeDeque.dequeue
          dataseq = useQueueInARealisticWay Seq.empty (flip (Seq.|>)) \case
            Seq.Empty -> Nothing
            x Seq.:<| xs -> Just (x, xs)
       in localOption (RelStDev 0.02) $
            bgroup
              "realistic usage"
              [ bench "Seq" (whnf dataseq n),
                bcompare "Seq" $ bench "Queue" (whnf queue n),
                bcompare "Seq" $ bench "RealTimeQueue" (whnf rtqueue n),
                bcompare "Seq" $ bench "RealTimeDeque" (whnf rtdeque n)
              ]
    ]

useQueueInARealisticWay ::
  forall queue.
  queue () ->
  (() -> queue () -> queue ()) ->
  (queue () -> Maybe ((), queue ())) ->
  Int ->
  Int
useQueueInARealisticWay empty enqueue dequeue num =
  let -- `loop1 0` enqueues n elemets
      loop1 :: Int -> queue () -> queue ()
      loop1 !n q
        | n == num = q
        | otherwise = loop1 (n + 1) (enqueue () q)

      -- `loop2 0` dequeues 1 and enqueues 2 n times (leaving queue at 2n elements)
      loop2 :: Int -> queue () -> queue ()
      loop2 !n q
        | n == num = q
        | otherwise = loop2 (n + 1) (enqueue () (enqueue () (dequeue_ q)))

      -- `loop3 0` dequeues 2 and enqueues 1 n times (leaving queue at n elements)
      loop3 :: Int -> queue () -> queue ()
      loop3 !n q
        | n == num = q
        | otherwise = loop3 (n + 1) (enqueue () (dequeue_ (dequeue_ q)))

      -- `loop4 0` dequeues until there's nothing left and returns how many were dequeued
      loop4 :: Int -> queue () -> Int
      loop4 !n q =
        case dequeue q of
          Nothing -> n
          Just (_, q') -> loop4 (n + 1) q'
   in loop4 0 (loop3 0 (loop2 0 (loop1 0 empty)))
  where
    dequeue_ q =
      case dequeue q of
        Nothing -> q
        Just (_, q') -> q'
