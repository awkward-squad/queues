module Main (main) where

import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Word
import Deque (Deque)
import Deque qualified
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main
import Hedgehog.Range qualified as Range
import Queue (Queue)
import Queue qualified
import Queue.Amortized qualified

main :: IO ()
main = do
  defaultMain [checkParallel (Group "tests" (map (\(name, prop) -> (name, withTests 1000 (property prop))) tests))]

tests :: [(PropertyName, PropertyT IO ())]
tests =
  [ ( "[queue amortized-queue deque] `toList . fromList = id`",
      do
        list <- forAll generateList
        (Queue.toList . Queue.fromList) list === list
        (Queue.Amortized.toList . Queue.Amortized.fromList) list === list
        (Deque.toList . Deque.fromList) list === list
    ),
    ( "[queue amortized-queue      ] `push/pushFront/pop state machine tests`",
      do
        actions <- forAll (generateQueueActions 1000)
        let expected = seqApplyQueueActions actions
        queueApplyActions actions === expected
        amortizedQueueApplyActions actions === expected
    ),
    ( "[                      deque] `push/pushFront/pop/popBack state machine tests`",
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
  = QueueActionPush !Word8
  | QueueActionPushFront !Word8
  | QueueActionPop
  deriving stock (Show)

generateQueueActions :: Int -> Gen [QueueAction]
generateQueueActions n =
  Gen.list
    (Range.linear 0 n)
    ( Gen.frequency
        [ (8, QueueActionPush <$> generateWord8),
          (2, QueueActionPushFront <$> generateWord8),
          (2, pure QueueActionPop)
        ]
    )

seqApplyQueueActions :: [QueueAction] -> ([Maybe Word8], [Word8])
seqApplyQueueActions =
  second Foldable.toList . List.foldl' apply ([], Seq.empty)
  where
    apply :: ([Maybe Word8], Seq.Seq Word8) -> QueueAction -> ([Maybe Word8], Seq.Seq Word8)
    apply (pops, queue) = \case
      QueueActionPush x -> (pops, queue Seq.|> x)
      QueueActionPushFront x -> (pops, x Seq.<| queue)
      QueueActionPop ->
        case queue of
          Seq.Empty -> (Nothing : pops, queue)
          x Seq.:<| queue1 -> (Just x : pops, queue1)

queueApplyActions :: [QueueAction] -> ([Maybe Word8], [Word8])
queueApplyActions =
  second Queue.toList . List.foldl' apply ([], Queue.empty)
  where
    apply :: ([Maybe Word8], Queue Word8) -> QueueAction -> ([Maybe Word8], Queue Word8)
    apply (pops, queue) = \case
      QueueActionPush x -> (pops, Queue.push x queue)
      QueueActionPushFront x -> (pops, Queue.pushFront x queue)
      QueueActionPop ->
        case queue of
          Queue.Empty -> (Nothing : pops, queue)
          Queue.Front x queue1 -> (Just x : pops, queue1)

amortizedQueueApplyActions :: [QueueAction] -> ([Maybe Word8], [Word8])
amortizedQueueApplyActions =
  second Queue.Amortized.toList . List.foldl' apply ([], Queue.Amortized.empty)
  where
    apply :: ([Maybe Word8], Queue.Amortized.Queue Word8) -> QueueAction -> ([Maybe Word8], Queue.Amortized.Queue Word8)
    apply (pops, queue) = \case
      QueueActionPush x -> (pops, Queue.Amortized.push x queue)
      QueueActionPushFront x -> (pops, Queue.Amortized.pushFront x queue)
      QueueActionPop ->
        case queue of
          Queue.Amortized.Empty -> (Nothing : pops, queue)
          Queue.Amortized.Front x queue1 -> (Just x : pops, queue1)

data DequeAction
  = DequeActionPush !Word8
  | DequeActionPushFront !Word8
  | DequeActionPop
  | DequeActionPopBack
  deriving stock (Show)

generateDequeActions :: Int -> Gen [DequeAction]
generateDequeActions n =
  Gen.list
    (Range.linear 0 n)
    ( Gen.frequency
        [ (10, DequeActionPush <$> generateWord8),
          (10, DequeActionPushFront <$> generateWord8),
          (2, pure DequeActionPop),
          (2, pure DequeActionPopBack)
        ]
    )

seqApplyDequeActions :: [DequeAction] -> ([Maybe Word8], [Word8])
seqApplyDequeActions =
  second Foldable.toList . List.foldl' apply ([], Seq.empty)
  where
    apply :: ([Maybe Word8], Seq.Seq Word8) -> DequeAction -> ([Maybe Word8], Seq.Seq Word8)
    apply (pops, queue) = \case
      DequeActionPush x -> (pops, queue Seq.|> x)
      DequeActionPushFront x -> (pops, x Seq.<| queue)
      DequeActionPop ->
        case queue of
          Seq.Empty -> (Nothing : pops, queue)
          x Seq.:<| queue1 -> (Just x : pops, queue1)
      DequeActionPopBack ->
        case queue of
          Seq.Empty -> (Nothing : pops, queue)
          queue1 Seq.:|> x -> (Just x : pops, queue1)

dequeApplyActions :: [DequeAction] -> ([Maybe Word8], [Word8])
dequeApplyActions =
  second Deque.toList . List.foldl' apply ([], Deque.empty)
  where
    apply :: ([Maybe Word8], Deque Word8) -> DequeAction -> ([Maybe Word8], Deque Word8)
    apply (pops, queue) = \case
      DequeActionPush x -> (pops, Deque.push x queue)
      DequeActionPushFront x -> (pops, Deque.pushFront x queue)
      DequeActionPop ->
        case queue of
          Deque.Empty -> (Nothing : pops, queue)
          Deque.Front x queue1 -> (Just x : pops, queue1)
      DequeActionPopBack ->
        case queue of
          Deque.Empty -> (Nothing : pops, queue)
          Deque.Back queue1 x -> (Just x : pops, queue1)

generateWord8 :: Gen Word8
generateWord8 =
  Gen.word8 Range.constantBounded
