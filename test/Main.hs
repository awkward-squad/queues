module Main (main) where

import Control.Applicative ((<|>))
import Data.List qualified as List
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
  [ ( "real-time queue: toList . fromList = id",
      do
        list <- forAll generateList
        Queue.toList (Queue.fromList list) === list
    ),
    ( "real-time queue: push + pop",
      do
        (queue, x) <- forAll ((,) <$> generateQueue <*> generateWord8)
        queueLast (Queue.push x queue) === Just x
    ),
    ( "real-time queue: pushFront + pop",
      do
        (queue, x) <- forAll ((,) <$> generateQueue <*> generateWord8)
        Queue.pop (Queue.pushFront x queue) === Just (x, queue)
    ),
    ( "amortized queue: toList . fromList = id",
      do
        list <- forAll generateList
        Queue.Amortized.toList (Queue.Amortized.fromList list) === list
    ),
    ( "amortized queue: push + pop",
      do
        (queue, x) <- forAll ((,) <$> generateAmortizedQueue <*> generateWord8)
        amortizedQueueLast (Queue.Amortized.push x queue) === Just x
    ),
    ( "amortized queue: pushFront + pop",
      do
        (queue, x) <- forAll ((,) <$> generateAmortizedQueue <*> generateWord8)
        Queue.Amortized.pop (Queue.Amortized.pushFront x queue) === Just (x, queue)
    ),
    ( "real-time deque: toList . fromList = id",
      do
        list <- forAll generateList
        Deque.toList (Deque.fromList list) === list
    ),
    ( "real-time deque: push + pop",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        dequeSlowLast (Deque.push x deque) === Just x
    ),
    ( "real-time deque: push + popBack",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        Deque.popBack (Deque.push x deque) === Just (deque, x)
    ),
    ( "real-time deque: pushFront + pop",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        Deque.pop (Deque.pushFront x deque) === Just (x, deque)
    ),
    ( "real-time deque: pushFront + popBack",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        dequeSlowHead (Deque.pushFront x deque) === Just x
    )
  ]

------------------------------------------------------------------------------------------------------------------------
-- Supplemental API

-- O(n). Get the last element of a queue.
queueLast :: Queue a -> Maybe a
queueLast =
  go Nothing
  where
    go :: Maybe a -> Queue a -> Maybe a
    go acc = \case
      Queue.Empty -> acc
      Queue.Front x xs -> go (Just x <|> acc) xs

-- O(n). Get the last element of a queue.
amortizedQueueLast :: Queue.Amortized.Queue a -> Maybe a
amortizedQueueLast =
  go Nothing
  where
    go :: Maybe a -> Queue.Amortized.Queue a -> Maybe a
    go acc = \case
      Queue.Amortized.Empty -> acc
      Queue.Amortized.Front x xs -> go (Just x <|> acc) xs

-- O(n). Get the first element of a deque by popping from the back.
dequeSlowHead :: Deque a -> Maybe a
dequeSlowHead =
  go Nothing
  where
    go :: Maybe a -> Deque a -> Maybe a
    go acc = \case
      Deque.Empty -> acc
      Deque.Back xs x -> go (Just x <|> acc) xs

-- O(n). Get the last element of a deque by popping from the front.
dequeSlowLast :: Deque a -> Maybe a
dequeSlowLast =
  go Nothing
  where
    go :: Maybe a -> Deque a -> Maybe a
    go acc = \case
      Deque.Empty -> acc
      Deque.Front x xs -> go (Just x <|> acc) xs

------------------------------------------------------------------------------------------------------------------------
-- Generators

generateList :: Gen [Word8]
generateList =
  Gen.list (Range.linear 0 200) generateWord8

generateQueue :: Gen (Queue Word8)
generateQueue = do
  List.foldl' applyQueueAction Queue.empty <$> generateQueueActions
  where
    applyQueueAction queue = \case
      QueueActionPush x -> Queue.push x queue
      QueueActionPop -> maybe queue snd (Queue.pop queue)

generateAmortizedQueue :: Gen (Queue.Amortized.Queue Word8)
generateAmortizedQueue = do
  List.foldl' applyQueueAction Queue.Amortized.empty <$> generateQueueActions
  where
    applyQueueAction queue = \case
      QueueActionPush x -> Queue.Amortized.push x queue
      QueueActionPop -> maybe queue snd (Queue.Amortized.pop queue)

data QueueAction
  = QueueActionPush !Word8
  | QueueActionPop
  deriving stock (Show)

generateQueueActions :: Gen [QueueAction]
generateQueueActions =
  Gen.list
    (Range.linear 0 1000)
    ( Gen.frequency
        [ (10, QueueActionPush <$> generateWord8),
          (2, pure QueueActionPop)
        ]
    )

generateDeque :: Gen (Deque Word8)
generateDeque = do
  List.foldl' applyDequeAction Deque.empty <$> generateDequeActions
  where
    applyDequeAction deque = \case
      DequeActionPush x -> Deque.push x deque
      DequeActionPushFront x -> Deque.pushFront x deque
      DequeActionPop -> maybe deque snd (Deque.pop deque)
      DequeActionPopBack -> maybe deque fst (Deque.popBack deque)

data DequeAction
  = DequeActionPush !Word8
  | DequeActionPushFront !Word8
  | DequeActionPop
  | DequeActionPopBack
  deriving stock (Show)

generateDequeActions :: Gen [DequeAction]
generateDequeActions =
  Gen.list
    (Range.linear 0 1000)
    ( Gen.frequency
        [ (10, DequeActionPush <$> generateWord8),
          (10, DequeActionPushFront <$> generateWord8),
          (2, pure DequeActionPop),
          (2, pure DequeActionPopBack)
        ]
    )

generateWord8 :: Gen Word8
generateWord8 =
  Gen.word8 Range.constantBounded
