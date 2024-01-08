module Main (main) where

import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import Deque qualified
import Queue.Ephemeral qualified
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
import Queue qualified
import Deque.RealTime qualified

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
        test realTimeQueueIface list
        test ephemeralQueueIface list
        test realTimeDequeIface list
    ),
    ( "enqueue/enqueueFront/dequeue state machine tests",
      do
        actions <- forAll (generateQueueActions 1000)
        let expected = applyQueueActions seqIface actions
        applyQueueActions ephemeralQueueIface actions === expected
        applyQueueActions realTimeQueueIface actions === expected
    ),
    ( "`enqueue/enqueueFront/dequeue/dequeueBack state machine tests`",
      do
        actions <- forAll (generateDequeActions 1000)
        let expected = applyDequeActions seqIface actions
        applyDequeActions amortizedDequeIface actions === expected
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

realTimeQueueIface :: Iface a
realTimeQueueIface =
  Iface
    Queue.empty
    Queue.enqueue
    Queue.dequeue
    Queue.enqueueFront
    (error "Queue has no dequeueBack")
    Queue.toList
    Queue.fromList

ephemeralQueueIface :: Iface a
ephemeralQueueIface =
  Iface
    Queue.Ephemeral.empty
    Queue.Ephemeral.enqueue
    Queue.Ephemeral.dequeue
    Queue.Ephemeral.enqueueFront
    (error "EphemeralQueue has no dequeueBack")
    Queue.Ephemeral.toList
    Queue.Ephemeral.fromList

amortizedDequeIface :: Iface a
amortizedDequeIface =
  Iface Deque.empty Deque.enqueue Deque.dequeue Deque.enqueueFront Deque.dequeueBack Deque.toList Deque.fromList

realTimeDequeIface :: Iface a
realTimeDequeIface =
  Iface
    Deque.RealTime.empty
    Deque.RealTime.enqueue
    Deque.RealTime.dequeue
    Deque.RealTime.enqueueFront
    Deque.RealTime.dequeueBack
    Deque.RealTime.toList
    Deque.RealTime.fromList

seqIface :: Iface a
seqIface =
  Iface Seq.empty seqEnqueue seqDequeue seqEnqueueFront seqDequeueBack Foldable.toList Seq.fromList
  where
    seqEnqueue :: a -> Seq a -> Seq a
    seqEnqueue =
      flip (Seq.|>)

    seqDequeue :: Seq a -> Maybe (a, Seq a)
    seqDequeue = \case
      Seq.Empty -> Nothing
      x Seq.:<| xs -> Just (x, xs)

    seqEnqueueFront :: a -> Seq a -> Seq a
    seqEnqueueFront =
      (Seq.<|)

    seqDequeueBack :: Seq a -> Maybe (Seq a, a)
    seqDequeueBack = \case
      Seq.Empty -> Nothing
      xs Seq.:|> x -> Just (xs, x)

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
