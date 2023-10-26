module Main (main) where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Deque (Deque)
import Data.Deque qualified as Deque
import Data.List qualified as List
import Data.Queue (Queue)
import Data.Queue qualified as Queue
import Data.Word
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main
import Hedgehog.Range qualified as Range

main :: IO ()
main = do
  defaultMain [checkParallel (Group "tests" (map (\(name, prop) -> (name, withTests 1000 (property prop))) tests))]

tests :: [(PropertyName, PropertyT IO ())]
tests =
  [ ( "queue: toList . fromList = id",
      do
        list <- forAll generateList
        Queue.toList (Queue.fromList list) === list
    ),
    ( "queue: push + pop",
      do
        (queue, x) <- forAll ((,) <$> generateQueue <*> generateWord8)
        queueLast (Queue.push x queue) === Just x
    ),
    ( "queue: pushFront + pop",
      do
        (queue, x) <- forAll ((,) <$> generateQueue <*> generateWord8)
        let cast = coerce :: Maybe (Word8, Queue Word8) -> Maybe (Word8, QueueWithEq Word8)
        cast (Queue.pop (Queue.pushFront x queue)) === cast (Just (x, queue))
    ),
    ( "deque: toList . fromList = id",
      do
        list <- forAll generateList
        Deque.toList (Deque.fromList list) === list
    ),
    ( "deque: push + pop",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        dequeSlowLast (Deque.push x deque) === Just x
    ),
    ( "deque: push + popBack",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        let cast = coerce :: Maybe (Deque Word8, Word8) -> Maybe (DequeWithEq Word8, Word8)
        cast (Deque.popBack (Deque.push x deque)) === cast (Just (deque, x))
    ),
    ( "deque: pushFront + pop",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        let cast = coerce :: Maybe (Word8, Deque Word8) -> Maybe (Word8, DequeWithEq Word8)
        cast (Deque.pop (Deque.pushFront x deque)) === cast (Just (x, deque))
    ),
    ( "deque: pushFront + popBack",
      do
        (deque, x) <- forAll ((,) <$> generateDeque <*> generateWord8)
        dequeSlowHead (Deque.pushFront x deque) === Just x
    )
  ]

------------------------------------------------------------------------------------------------------------------------
-- Supplemental API

newtype QueueWithEq a = QueueWithEq (Queue a)
  deriving newtype (Show)

instance (Eq a) => Eq (QueueWithEq a) where
  QueueWithEq Queue.Empty == QueueWithEq Queue.Empty = True
  QueueWithEq (Queue.Pop x xs) == QueueWithEq (Queue.Pop y ys) | x == y = QueueWithEq xs == QueueWithEq ys
  _ == _ = False

-- O(n). Get the last element of a queue.
queueLast :: Queue a -> Maybe a
queueLast =
  go Nothing
  where
    go :: Maybe a -> Queue a -> Maybe a
    go acc = \case
      Queue.Empty -> acc
      Queue.Pop x xs -> go (Just x <|> acc) xs

newtype DequeWithEq a = DequeWithEq (Deque a)
  deriving newtype (Show)

instance (Eq a) => Eq (DequeWithEq a) where
  DequeWithEq Deque.Empty == DequeWithEq Deque.Empty = True
  DequeWithEq (Deque.Pop x xs) == DequeWithEq (Deque.Pop y ys) | x == y = DequeWithEq xs == DequeWithEq ys
  _ == _ = False

-- O(n). Get the first element of a deque by popping from the back.
dequeSlowHead :: Deque a -> Maybe a
dequeSlowHead =
  go Nothing
  where
    go :: Maybe a -> Deque a -> Maybe a
    go acc = \case
      Deque.Empty -> acc
      Deque.PopBack xs x -> go (Just x <|> acc) xs

-- O(n). Get the last element of a deque by popping from the front.
dequeSlowLast :: Deque a -> Maybe a
dequeSlowLast =
  go Nothing
  where
    go :: Maybe a -> Deque a -> Maybe a
    go acc = \case
      Deque.Empty -> acc
      Deque.Pop x xs -> go (Just x <|> acc) xs

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
