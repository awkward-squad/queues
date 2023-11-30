module Main (main) where

import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import Deque (Deque)
import Deque qualified
import Hedgehog
  ( Gen,
    Group (Group),
    PropertyName,
    PropertyT,
    checkParallel,
    forAll,
    property,
    withTests,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main qualified as Hedgehog (defaultMain)
import Hedgehog.Range qualified as Range
import Queue (Queue)
import Queue qualified
import Queue.Amortized qualified

main :: IO ()
main = do
  Hedgehog.defaultMain
    [ tests
        & map (\(name, prop) -> (name, withTests 1000 (property prop)))
        & Group "tests"
        & checkParallel
    ]

tests :: [(PropertyName, PropertyT IO ())]
tests =
  [ ( "[queue amortized-queue deque] `toList . fromList = id`",
      do
        list <- forAll generateList
        (Queue.toList . Queue.fromList) list === list
        (Queue.Amortized.toList . Queue.Amortized.fromList) list === list
        (Deque.toList . Deque.fromList) list === list
    ),
    ( "[queue amortized-queue      ] `enqueue/enqueueFront/dequeue state machine tests`",
      do
        actions <- forAll (generateQueueActions 1000)
        let expected = seqApplyQueueActions actions
        queueApplyActions actions === expected
        amortizedQueueApplyActions actions === expected
    ),
    ( "[                      deque] `enqueue/enqueueFront/dequeue/dequeueBack state machine tests`",
      do
        actions <- forAll (generateDequeActions 1000)
        let expected = seqApplyDequeActions actions
        dequeApplyActions actions === expected
    )
  ]

------------------------------------------------------------------------------------------------------------------------
-- Generators

generateList :: Gen [Word8]
generateList =
  Gen.list (Range.linear 0 200) generateWord8

data QueueAction
  = QueueActionEnqueue !Word8
  | QueueActionEnqueueFront !Word8
  | QueueActionDequeue
  deriving stock (Show)

generateQueueActions :: Int -> Gen [QueueAction]
generateQueueActions n =
  Gen.list
    (Range.linear 0 n)
    ( Gen.frequency
        [ (8, QueueActionEnqueue <$> generateWord8),
          (2, QueueActionEnqueueFront <$> generateWord8),
          (2, pure QueueActionDequeue)
        ]
    )

seqApplyQueueActions :: [QueueAction] -> ([Maybe Word8], [Word8])
seqApplyQueueActions =
  second Foldable.toList . List.foldl' apply ([], Seq.empty)
  where
    apply :: ([Maybe Word8], Seq.Seq Word8) -> QueueAction -> ([Maybe Word8], Seq.Seq Word8)
    apply (dequeues, queue) = \case
      QueueActionEnqueue x -> (dequeues, queue Seq.|> x)
      QueueActionEnqueueFront x -> (dequeues, x Seq.<| queue)
      QueueActionDequeue ->
        case queue of
          Seq.Empty -> (Nothing : dequeues, queue)
          x Seq.:<| queue1 -> (Just x : dequeues, queue1)

queueApplyActions :: [QueueAction] -> ([Maybe Word8], [Word8])
queueApplyActions =
  second Queue.toList . List.foldl' apply ([], Queue.empty)
  where
    apply :: ([Maybe Word8], Queue Word8) -> QueueAction -> ([Maybe Word8], Queue Word8)
    apply (dequeues, queue) = \case
      QueueActionEnqueue x -> (dequeues, Queue.enqueue x queue)
      QueueActionEnqueueFront x -> (dequeues, Queue.enqueueFront x queue)
      QueueActionDequeue ->
        case queue of
          Queue.Empty -> (Nothing : dequeues, queue)
          Queue.Front x queue1 -> (Just x : dequeues, queue1)

amortizedQueueApplyActions :: [QueueAction] -> ([Maybe Word8], [Word8])
amortizedQueueApplyActions =
  second Queue.Amortized.toList . List.foldl' apply ([], Queue.Amortized.empty)
  where
    apply :: ([Maybe Word8], Queue.Amortized.Queue Word8) -> QueueAction -> ([Maybe Word8], Queue.Amortized.Queue Word8)
    apply (dequeues, queue) = \case
      QueueActionEnqueue x -> (dequeues, Queue.Amortized.enqueue x queue)
      QueueActionEnqueueFront x -> (dequeues, Queue.Amortized.enqueueFront x queue)
      QueueActionDequeue ->
        case queue of
          Queue.Amortized.Empty -> (Nothing : dequeues, queue)
          Queue.Amortized.Front x queue1 -> (Just x : dequeues, queue1)

data DequeAction
  = DequeActionEnqueue !Word8
  | DequeActionEnqueueFront !Word8
  | DequeActionDequeue
  | DequeActionDequeueBack
  deriving stock (Show)

generateDequeActions :: Int -> Gen [DequeAction]
generateDequeActions n =
  Gen.list
    (Range.linear 0 n)
    ( Gen.frequency
        [ (10, DequeActionEnqueue <$> generateWord8),
          (10, DequeActionEnqueueFront <$> generateWord8),
          (2, pure DequeActionDequeue),
          (2, pure DequeActionDequeueBack)
        ]
    )

seqApplyDequeActions :: [DequeAction] -> ([Maybe Word8], [Word8])
seqApplyDequeActions =
  second Foldable.toList . List.foldl' apply ([], Seq.empty)
  where
    apply :: ([Maybe Word8], Seq.Seq Word8) -> DequeAction -> ([Maybe Word8], Seq.Seq Word8)
    apply (dequeues, queue) = \case
      DequeActionEnqueue x -> (dequeues, queue Seq.|> x)
      DequeActionEnqueueFront x -> (dequeues, x Seq.<| queue)
      DequeActionDequeue ->
        case queue of
          Seq.Empty -> (Nothing : dequeues, queue)
          x Seq.:<| queue1 -> (Just x : dequeues, queue1)
      DequeActionDequeueBack ->
        case queue of
          Seq.Empty -> (Nothing : dequeues, queue)
          queue1 Seq.:|> x -> (Just x : dequeues, queue1)

dequeApplyActions :: [DequeAction] -> ([Maybe Word8], [Word8])
dequeApplyActions =
  second Deque.toList . List.foldl' apply ([], Deque.empty)
  where
    apply :: ([Maybe Word8], Deque Word8) -> DequeAction -> ([Maybe Word8], Deque Word8)
    apply (dequeues, queue) = \case
      DequeActionEnqueue x -> (dequeues, Deque.enqueue x queue)
      DequeActionEnqueueFront x -> (dequeues, Deque.enqueueFront x queue)
      DequeActionDequeue ->
        case queue of
          Deque.Empty -> (Nothing : dequeues, queue)
          Deque.Front x queue1 -> (Just x : dequeues, queue1)
      DequeActionDequeueBack ->
        case queue of
          Deque.Empty -> (Nothing : dequeues, queue)
          Deque.Back queue1 x -> (Just x : dequeues, queue1)

generateWord8 :: Gen Word8
generateWord8 =
  Gen.word8 Range.constantBounded
