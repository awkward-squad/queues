-- | A queue data structure with \(\mathcal{O}(1)^*\) (amortized under ephemeral usage only) operations, as described in
--
--   * Okasaki, Chris. \"Simple and efficient purely functional queues and deques.\" /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
--
-- A queue can be thought to have a \"back\" where new elements are enqueued, and a \"front\" where elements are
-- dequeued in the order that they were enqueued.
--
-- This queue also supports a \"enqueue at front\" operation, because the underlying representation happens to trivially
-- support it. (For a variant that also supports a \"dequeue from back\" operation, see "RealTimeDeque".
--
-- In this implementation, it is more helpful to think of the \"front\" being on the /left/, because (though the
-- decision is arbitrary) we are consistent throughout, where it matters:
--
--   * List conversion functions associate the head of a list with the front of a queue.
--   * The append operator @xs <> ys@ creates a queue with @xs@ in front of @ys@.
--
-- Performance comparison to other types:
--
--   +---+------------------------------+---------+
--   |   | @EphemeralQueue@             |         |
--   +===+==============================+=========+
--   | ✔ | is @2.50x@ faster than       | "Queue" |
--   +---+------------------------------+         |
--   | ✔ | allocates @0.50x@ as much as |         |
--   +---+------------------------------+---------+
--   | ✔ | is @6.30x@ faster than       | @Seq@   |
--   +---+------------------------------+         |
--   | ✔ | allocates @0.20x@ as much as |         |
--   +---+------------------------------+---------+
module Queue.Ephemeral
  ( -- * Ephemeral queue
    EphemeralQueue (Empty, Front),

    -- ** Initialization
    empty,
    singleton,
    fromList,

    -- * Basic interface
    enqueue,
    dequeue,

    -- ** Extended interface
    enqueueFront,
    dequeueWhile,

    -- * Queries
    isEmpty,

    -- * Transformations
    map,
    traverse,

    -- * Conversions
    toList,
  )
where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import Prelude hiding (foldMap, length, map, span, traverse)

------------------------------------------------------------------------------------------------------------------------
-- Queue type and instances

-- | A queue data structure with \(\mathcal{O}(1)^⧧\) (amortized under ephemeral usage only) operations.
data EphemeralQueue a
  = Q [a] [a]
  deriving stock (Functor)

instance (Eq a) => Eq (EphemeralQueue a) where
  (==) :: EphemeralQueue a -> EphemeralQueue a -> Bool
  xs == ys =
    Queue.Ephemeral.toList xs == Queue.Ephemeral.toList ys

instance Foldable EphemeralQueue where
  foldMap :: (Monoid m) => (a -> m) -> EphemeralQueue a -> m
  foldMap f =
    go
    where
      go = \case
        Empty -> mempty
        Front x xs -> f x <> go xs

  elem :: (Eq a) => a -> EphemeralQueue a -> Bool
  elem x (Q xs ys) =
    List.elem x xs || List.elem x ys

  null :: EphemeralQueue a -> Bool
  null =
    isEmpty

  toList :: EphemeralQueue a -> [a]
  toList =
    Queue.Ephemeral.toList

instance Monoid (EphemeralQueue a) where
  mempty :: EphemeralQueue a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the second argument.
instance Semigroup (EphemeralQueue a) where
  (<>) :: EphemeralQueue a -> EphemeralQueue a -> EphemeralQueue a
  Q as bs <> Q cs ds =
    Q as (ds ++ reverse cs ++ bs)

instance (Show a) => Show (EphemeralQueue a) where
  show :: EphemeralQueue a -> String
  show =
    show . Queue.Ephemeral.toList

instance Traversable EphemeralQueue where
  traverse :: (Applicative f) => (a -> f b) -> EphemeralQueue a -> f (EphemeralQueue b)
  traverse =
    Queue.Ephemeral.traverse

------------------------------------------------------------------------------------------------------------------------
-- Patterns

-- | An empty queue.
pattern Empty :: EphemeralQueue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> EphemeralQueue a -> EphemeralQueue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

------------------------------------------------------------------------------------------------------------------------
-- Initialization

-- | An empty queue.
empty :: EphemeralQueue a
empty =
  Q [] []

-- | A singleton queue.
singleton :: a -> EphemeralQueue a
singleton x =
  Q [x] []

-- | \(\mathcal{O}(1)\). Construct a queue from a list. The head of the list corresponds to the front of the queue.
fromList :: [a] -> EphemeralQueue a
fromList xs =
  Q xs []

------------------------------------------------------------------------------------------------------------------------
-- Basic interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> EphemeralQueue a -> EphemeralQueue a
enqueue y (Q xs ys) =
  Q xs (y : ys)

-- | \(\mathcal{O}(1)^*\) front, \(\mathcal{O}(1)^*\) rest. Dequeue an element from the front of a queue.
dequeue :: EphemeralQueue a -> Maybe (a, EphemeralQueue a)
dequeue = \case
  Q [] ys ->
    case reverse ys of
      [] -> Nothing
      x : xs -> Just (x, Q xs [])
  Q (x : xs) ys -> Just (x, Q xs ys)

------------------------------------------------------------------------------------------------------------------------
-- Extended interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> EphemeralQueue a -> EphemeralQueue a
enqueueFront x (Q xs ys) =
  Q (x : xs) ys

-- | Dequeue elements from the front of a queue while a predicate is satisfied.
dequeueWhile :: (a -> Bool) -> EphemeralQueue a -> ([a], EphemeralQueue a)
dequeueWhile p queue0 =
  case span p empty queue0 of
    (queue1, queue2) -> (toList queue1, queue2)

span :: (a -> Bool) -> EphemeralQueue a -> EphemeralQueue a -> (EphemeralQueue a, EphemeralQueue a)
span p =
  go
  where
    go acc = \case
      Empty -> (acc, empty)
      Front x xs
        | p x -> go (enqueue x acc) xs
        | otherwise -> (acc, enqueueFront x xs)

------------------------------------------------------------------------------------------------------------------------
-- Queries

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: EphemeralQueue a -> Bool
isEmpty = \case
  Q [] [] -> True
  _ -> False

------------------------------------------------------------------------------------------------------------------------
-- Transformations

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
map :: (a -> b) -> EphemeralQueue a -> EphemeralQueue b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
traverse :: (Applicative f) => (a -> f b) -> EphemeralQueue a -> f (EphemeralQueue b)
traverse f (Q xs ys) =
  Q
    <$> Traversable.traverse f xs
    <*> backwards ys
  where
    backwards =
      go
      where
        go = \case
          [] -> pure []
          z : zs -> flip (:) <$> go zs <*> f z

------------------------------------------------------------------------------------------------------------------------
-- Conversions

-- | \(\mathcal{O}(n)\). Construct a list from a queue. The head of the list corresponds to the front of the queue.
toList :: EphemeralQueue a -> [a]
toList (Q xs ys) =
  xs ++ reverse ys
