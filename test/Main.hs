module Main (main) where

import AmortizedQueue qualified
import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import EphemeralQueue qualified
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
import RealTimeDeque qualified
import RealTimeQueue qualified

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
  [ ( "toList . fromList = id",
      do
        let test :: (Eq a, Show a) => Iface a -> [a] -> PropertyT IO ()
            test Iface {toList, fromList} list = toList (fromList list) === list
        list <- forAll generateList
        test amortizedQueueIface list
        test realTimeQueueIface list
        test ephemeralQueueIface list
        test realTimeDequeIface list
    ),
    ( "enqueue/enqueueFront/dequeue state machine tests",
      do
        actions <- forAll (generateQueueActions 1000)
        let expected = applyQueueActions seqIface actions
        applyQueueActions amortizedQueueIface actions === expected
        applyQueueActions realTimeQueueIface actions === expected
        applyQueueActions ephemeralQueueIface actions === expected
    ),
    ( "`enqueue/enqueueFront/dequeue/dequeueBack state machine tests`",
      do
        actions <- forAll (generateDequeActions 1000)
        let expected = applyDequeActions seqIface actions
        applyDequeActions realTimeDequeIface actions === expected
    )
  ]

------------------------------------------------------------------------------------------------------------------------
-- Queue interface

data Iface a = forall queue.
  Iface
  { empty :: queue a,
    enqueue :: a -> queue a -> queue a,
    dequeue :: queue a -> Maybe (a, queue a),
    enqueueFront :: a -> queue a -> queue a,
    dequeueBack :: queue a -> Maybe (queue a, a),
    toList :: queue a -> [a],
    fromList :: [a] -> queue a
  }

amortizedQueueIface :: Iface a
amortizedQueueIface =
  Iface
    AmortizedQueue.empty
    AmortizedQueue.enqueue
    AmortizedQueue.dequeue
    AmortizedQueue.enqueueFront
    (error "AmortizedQueue has no dequeueBack")
    AmortizedQueue.toList
    AmortizedQueue.fromList

realTimeQueueIface :: Iface a
realTimeQueueIface =
  Iface
    RealTimeQueue.empty
    RealTimeQueue.enqueue
    RealTimeQueue.dequeue
    RealTimeQueue.enqueueFront
    (error "RealTimeQueue has no dequeueBack")
    RealTimeQueue.toList
    RealTimeQueue.fromList

ephemeralQueueIface :: Iface a
ephemeralQueueIface =
  Iface
    EphemeralQueue.empty
    EphemeralQueue.enqueue
    EphemeralQueue.dequeue
    EphemeralQueue.enqueueFront
    (error "EphemeralQueue has no dequeueBack")
    EphemeralQueue.toList
    EphemeralQueue.fromList

realTimeDequeIface :: Iface a
realTimeDequeIface =
  Iface
    RealTimeDeque.empty
    RealTimeDeque.enqueue
    RealTimeDeque.dequeue
    RealTimeDeque.enqueueFront
    RealTimeDeque.dequeueBack
    RealTimeDeque.toList
    RealTimeDeque.fromList

seqIface :: Iface a
seqIface =
  Iface
    Seq.empty
    (\x xs -> xs Seq.|> x)
    ( \case
        Seq.Empty -> Nothing
        x Seq.:<| xs -> Just (x, xs)
    )
    (Seq.<|)
    ( \case
        Seq.Empty -> Nothing
        xs Seq.:|> x -> Just (xs, x)
    )
    Foldable.toList
    Seq.fromList

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

applyQueueActions :: Iface Word8 -> [QueueAction] -> ([Maybe Word8], [Word8])
applyQueueActions Iface {empty, enqueue, dequeue, enqueueFront, toList} =
  second toList . List.foldl' apply ([], empty)
  where
    apply (dequeues, queue) = \case
      QueueActionEnqueue x -> (dequeues, enqueue x queue)
      QueueActionEnqueueFront x -> (dequeues, enqueueFront x queue)
      QueueActionDequeue ->
        case dequeue queue of
          Nothing -> (Nothing : dequeues, queue)
          Just (x, queue1) -> (Just x : dequeues, queue1)

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

applyDequeActions :: Iface Word8 -> [DequeAction] -> ([Maybe Word8], [Word8])
applyDequeActions Iface {empty, enqueue, dequeue, enqueueFront, dequeueBack, toList} =
  second toList . List.foldl' apply ([], empty)
  where
    apply (dequeues, queue) = \case
      DequeActionEnqueue x -> (dequeues, enqueue x queue)
      DequeActionEnqueueFront x -> (dequeues, enqueueFront x queue)
      DequeActionDequeue ->
        case dequeue queue of
          Nothing -> (Nothing : dequeues, queue)
          Just (x, queue1) -> (Just x : dequeues, queue1)
      DequeActionDequeueBack ->
        case dequeueBack queue of
          Nothing -> (Nothing : dequeues, queue)
          Just (queue1, x) -> (Just x : dequeues, queue1)

generateWord8 :: Gen Word8
generateWord8 =
  Gen.word8 Range.constantBounded
