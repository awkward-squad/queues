-- It seems this is only needed on GHC <= 9.4
{-# LANGUAGE UndecidableInstances #-}

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
--   |   | @RealTimeQueue@              |                  |
--   +===+==============================+==================+
--   | ✔ | is @2.50x@ faster than       | @Seq@            |
--   +---+------------------------------+                  |
--   | ✔ | allocates @0.40x@ as much as |                  |
--   +---+------------------------------+------------------+
--   | ✘ | is @2.50x@ slower than       | "EphemeralQueue" |
--   +---+------------------------------+                  |
--   | ✘ | allocates @2.10x@ as much as |                  |
--   +---+------------------------------+------------------+
module RealTimeQueue
  ( -- * Queue
    RealTimeQueue (Empty, Front),

    -- ** Initialization
    empty,
    singleton,

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

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import GHC.Exts (Any)
import QueuesPrelude (NonEmptyList, listFoldMapBackwards, pattern NonEmptyList)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, map, span, traverse)

-- | A queue data structure with \(\mathcal{O}(1)\) (worst-case) operations.
data RealTimeQueue a
  = Q
      -- The front of the queue.
      -- Invariant: length >= length of back
      [a]
      -- The back of the queue, in reverse order.
      [a]
      -- Some tail of the front of the queue.
      -- Invariant: length = length of front - length of back
      Schedule

instance (Eq a) => Eq (RealTimeQueue a) where
  (==) :: RealTimeQueue a -> RealTimeQueue a -> Bool
  xs == ys =
    toList xs == toList ys

instance Foldable RealTimeQueue where
  foldMap :: (Monoid m) => (a -> m) -> RealTimeQueue a -> m
  foldMap f (Q xs ys _) =
    Foldable.foldMap f xs <> listFoldMapBackwards f ys

  elem :: (Eq a) => a -> RealTimeQueue a -> Bool
  elem x (Q xs ys _) =
    elem x xs || elem x ys

  null :: RealTimeQueue a -> Bool
  null =
    isEmpty

  toList :: RealTimeQueue a -> [a]
  toList =
    toList

instance Functor RealTimeQueue where
  fmap :: (a -> b) -> RealTimeQueue a -> RealTimeQueue b
  fmap =
    map

instance Monoid (RealTimeQueue a) where
  mempty :: RealTimeQueue a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the first argument.
instance Semigroup (RealTimeQueue a) where
  (<>) :: RealTimeQueue a -> RealTimeQueue a -> RealTimeQueue a
  Empty <> ys = ys
  Front x xs <> ys = enqueue x (xs <> ys)

instance (Show a) => Show (RealTimeQueue a) where
  show :: RealTimeQueue a -> String
  show =
    show . toList

instance Traversable RealTimeQueue where
  traverse :: (Applicative f) => (a -> f b) -> RealTimeQueue a -> f (RealTimeQueue b)
  traverse =
    traverse

-- | An empty queue.
pattern Empty :: RealTimeQueue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> RealTimeQueue a -> RealTimeQueue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

-- Queue smart constructor.
--
-- `queue xs ys zs` is always called when |zs| = |xs| - |ys| + 1 (i.e. just after a enqueue or dequeue)
makeQueue :: [a] -> [a] -> Schedule -> RealTimeQueue a
makeQueue xs ys = \case
  NoMoreWorkToDo -> let xs1 = rotate ys [] xs in Q xs1 [] (schedule xs1)
  DidWork zs -> Q xs ys zs
{-# INLINE makeQueue #-}

-- rotate ys zs xs = xs ++ reverse ys ++ zs
-- Precondition: |ys| = |xs| + 1
rotate :: NonEmptyList a -> [a] -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

-- | An empty queue.
empty :: RealTimeQueue a
empty =
  Q [] [] NoMoreWorkToDo

-- | A singleton queue.
singleton :: a -> RealTimeQueue a
singleton x =
  Q xs [] (schedule xs)
  where
    xs = [x]

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> RealTimeQueue a -> RealTimeQueue a
enqueue y (Q xs ys zs) =
  makeQueue xs (y : ys) zs
{-# INLINEABLE enqueue #-}

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)\) rest. Dequeue an element from the front of a queue.
dequeue :: RealTimeQueue a -> Maybe (a, RealTimeQueue a)
dequeue = \case
  Q [] _ _ -> Nothing
  Q (x : xs) ys zs -> Just (x, makeQueue xs ys zs)
{-# INLINEABLE dequeue #-}

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> RealTimeQueue a -> RealTimeQueue a
enqueueFront x (Q xs ys zs) =
  -- smart constructor not needed here
  -- we also add useless work to the schedule to maintain the convenient rotate-on-empty-schedule trigger
  Q (x : xs) ys (delay x zs)
{-# INLINEABLE enqueueFront #-}

-- | Dequeue elements from the front of a queue while a predicate is satisfied.
dequeueWhile :: (a -> Bool) -> RealTimeQueue a -> ([a], RealTimeQueue a)
dequeueWhile p queue0 =
  case span p empty queue0 of
    (queue1, queue2) -> (toList queue1, queue2)
{-# INLINEABLE dequeueWhile #-}

span :: (a -> Bool) -> RealTimeQueue a -> RealTimeQueue a -> (RealTimeQueue a, RealTimeQueue a)
span p =
  go
  where
    go acc queue =
      case queue of
        Empty -> (acc, empty)
        Front x xs
          | p x -> go (enqueue x acc) xs
          | otherwise -> (acc, queue)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: RealTimeQueue a -> Bool
isEmpty = \case
  Q [] _ _ -> True
  _ -> False
{-# INLINE isEmpty #-}

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
map :: (a -> b) -> RealTimeQueue a -> RealTimeQueue b
map f =
  fromList . List.map f . toList
{-# INLINEABLE map #-}

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
traverse :: (Applicative f) => (a -> f b) -> RealTimeQueue a -> f (RealTimeQueue b)
traverse f =
  fmap fromList . Traversable.traverse f . toList
{-# INLINEABLE traverse #-}

-- | \(\mathcal{O}(1)\). Construct a queue from a list. The head of the list corresponds to the front of the queue.
fromList :: [a] -> RealTimeQueue a
fromList xs =
  Q xs [] (schedule xs)
{-# INLINE fromList #-}

-- | \(\mathcal{O}(n)\). Construct a list from a queue. The head of the list corresponds to the front of the queue.
toList :: RealTimeQueue a -> [a]
toList (Q xs ys _) =
  xs ++ List.reverse ys
{-# INLINEABLE toList #-}

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
{-# INLINE delay #-}
