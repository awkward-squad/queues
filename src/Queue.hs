-- | A queue data structure with \(\mathcal{O}(1)\) (worst-case) operations, as described in
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
--   +---+------------------------------+------------------+
--   |   | @Queue@                      |                  |
--   +===+==============================+==================+
--   | ✘ | is @2.50x@ slower than       | "EphemeralQueue" |
--   +---+------------------------------+                  |
--   | ✘ | allocates @2.10x@ as much as |                  |
--   +---+------------------------------+------------------+
--   | ✔ | is @2.50x@ faster than       | @Seq@            |
--   +---+------------------------------+                  |
--   | ✔ | allocates @0.40x@ as much as |                  |
--   +---+------------------------------+------------------+
module Queue
  ( -- * Queue
    Queue (Empty, Front),

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
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, map, span, traverse)

------------------------------------------------------------------------------------------------------------------------
-- Queue type and instances

-- | A queue data structure with \(\mathcal{O}(1)\) (worst-case) operations.
data Queue a
  = Q
      -- The front of the queue.
      -- Invariant: not shorter than the back
      [a]
      -- The back of the queue, in reverse order.
      [a]
      -- Some tail of the front of the queue.
      -- Invariant: length = length of front - length of back
      Schedule

instance (Eq a) => Eq (Queue a) where
  (==) :: Queue a -> Queue a -> Bool
  xs == ys =
    Queue.toList xs == Queue.toList ys

instance Foldable Queue where
  foldMap :: (Monoid m) => (a -> m) -> Queue a -> m
  foldMap f =
    go
    where
      go = \case
        Empty -> mempty
        Front x xs -> f x <> go xs

  null :: Queue a -> Bool
  null =
    isEmpty

  toList :: Queue a -> [a]
  toList =
    Queue.toList

instance Functor Queue where
  fmap :: (a -> b) -> Queue a -> Queue b
  fmap =
    map

instance Monoid (Queue a) where
  mempty :: Queue a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the second argument.
instance Semigroup (Queue a) where
  (<>) :: Queue a -> Queue a -> Queue a
  xs <> Empty = xs
  xs <> Front y ys = enqueue y xs <> ys

instance (Show a) => Show (Queue a) where
  show :: Queue a -> String
  show =
    show . Queue.toList

instance Traversable Queue where
  traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
  traverse =
    Queue.traverse

------------------------------------------------------------------------------------------------------------------------
-- Patterns

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

------------------------------------------------------------------------------------------------------------------------
-- Internal smart constructor utils

-- `queue xs ys zs` is always called when |zs| = |xs| - |ys| + 1 (i.e. just after a enqueue or dequeue)
makeQueue :: [a] -> [a] -> Schedule -> Queue a
makeQueue xs ys = \case
  NoMoreWorkToDo -> let xs1 = rotate ys [] xs in Q xs1 [] (schedule xs1)
  DidWork zs -> Q xs ys zs

-- rotate ys zs xs = xs ++ reverse ys ++ zs
-- Precondition: |ys| = |xs| + 1
rotate :: NonEmptyList a -> [a] -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

------------------------------------------------------------------------------------------------------------------------
-- Initialization

-- | An empty queue.
empty :: Queue a
empty =
  Q [] [] NoMoreWorkToDo

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Q xs [] (schedule xs)
  where
    xs = [x]

-- | \(\mathcal{O}(1)\). Construct a queue from a list. The head of the list corresponds to the front of the queue.
fromList :: [a] -> Queue a
fromList xs =
  Q xs [] (schedule xs)

------------------------------------------------------------------------------------------------------------------------
-- Basic interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> Queue a -> Queue a
enqueue y (Q xs ys zs) =
  makeQueue xs (y : ys) zs

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)\) rest. Dequeue an element from the front of a queue.
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Q [] _ _ -> Nothing
  Q (x : xs) ys zs -> Just (x, makeQueue xs ys zs)

------------------------------------------------------------------------------------------------------------------------
-- Extended interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> Queue a -> Queue a
enqueueFront x (Q xs ys zs) =
  -- smart constructor not needed here
  -- we also add useless work to the schedule to maintain the convenient rotate-on-empty-schedule trigger
  Q (x : xs) ys (delay x zs)

-- | Dequeue elements from the front of a queue while a predicate is satisfied.
dequeueWhile :: (a -> Bool) -> Queue a -> ([a], Queue a)
dequeueWhile p queue0 =
  case span p empty queue0 of
    (queue1, queue2) -> (toList queue1, queue2)

span :: (a -> Bool) -> Queue a -> Queue a -> (Queue a, Queue a)
span p =
  go
  where
    go acc queue =
      case queue of
        Empty -> (acc, empty)
        Front x xs
          | p x -> go (enqueue x acc) xs
          | otherwise -> (acc, queue)

------------------------------------------------------------------------------------------------------------------------
-- Queries

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty = \case
  Q [] _ _ -> True
  _ -> False

------------------------------------------------------------------------------------------------------------------------
-- Transformations

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
map :: (a -> b) -> Queue a -> Queue b
map f =
  fromList . List.map f . toList

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
traverse f =
  fmap fromList . Traversable.traverse f . toList

------------------------------------------------------------------------------------------------------------------------
-- Conversions

-- | \(\mathcal{O}(n)\). Construct a list from a queue. The head of the list corresponds to the front of the queue.
toList :: Queue a -> [a]
toList =
  List.unfoldr dequeue

------------------------------------------------------------------------------------------------------------------------
-- Schedule utils

type Schedule =
  [Any]

pattern NoMoreWorkToDo :: Schedule
pattern NoMoreWorkToDo = []

pattern DidWork :: Schedule -> Schedule
pattern DidWork xs <- _ : xs

{-# COMPLETE NoMoreWorkToDo, DidWork #-}

schedule :: [a] -> Schedule
schedule =
  unsafeCoerce

delay :: a -> Schedule -> Schedule
delay x =
  (unsafeCoerce x :)

------------------------------------------------------------------------------------------------------------------------
-- Non-empty list utils

-- A list that we know is non-empty somehow.
type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}
