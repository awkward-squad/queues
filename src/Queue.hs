-- | A queue data structure with \(\mathcal{O}(1)\) amortized enqueue and dequeue, as described in
--
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
--
-- A queue can be thought to have a \"back\" where new elements are enqueued, and a \"front\" where elements are
-- dequeued in the order that they were enqueued.
--
-- This queue also supports a \"enqueue at front\" operation, because the underlying representation happens to trivially
-- support it. (For a variant that also supports a \"dequeue from back\" operation, see "RealTimeDeque".)
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
--   | ✔ | is @1.35x@ faster than       | @Seq@            |
--   +---+------------------------------+                  |
--   | ✔ | allocates @0.60x@ as much as |                  |
--   +---+------------------------------+------------------+
--   | ✘ | is @4.71x@ slower than       | "EphemeralQueue" |
--   +---+------------------------------+                  |
--   | ✘ | allocates @3.25x@ as much as |                  |
--   +---+------------------------------+------------------+
--   | ✘ | is @1.85x@ slower than       | "RealTimeQueue"  |
--   +---+------------------------------+                  |
--   | ✘ | allocates @1.54x@ as much as |                  |
--   +---+------------------------------+------------------+
module Queue
  ( -- * Queue
    Queue (Empty, Front),

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
    length,

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
import Queue.Internal.Prelude
import Prelude hiding (foldMap, length, map, span, traverse)

-- Implementation note: there are a number of similar options presented in Okasaki's works:
--
--   1. Banker's queue
--   2. Bootstrapped banker's queue
--   3. Not-exactly-bootstrapped banker's queue that just uses lists of rotations instead of a queue of rotations
--
-- In various benchmarks I've put together, which should probably be included in this repo but aren't, (3) appears to be
-- the fastest, so that's what we use.

-- | A queue data structure with \(\mathcal{O}(1)\) amortized enqueue and dequeue.
data Queue a
  = Q
      -- The head of the queue, e.g. [1,2,3]
      -- Invariant: empty iff queue is empty
      [a]
      -- A list of rotations, e.g. [ reverse [6,5,4], reverse [12,11,10,9,8,7] ]
      [NonEmptyList a]
      -- Length of head + all elems in queue of rotations
      {-# UNPACK #-} !Int
      -- The reversed tail of the queue, e.g. [50,49,48]
      -- Invariant: not longer than head + all elems in queue of rotations
      [a]
      -- Length of tail
      {-# UNPACK #-} !Int
  deriving stock (Functor)

instance (Eq a) => Eq (Queue a) where
  (==) :: Queue a -> Queue a -> Bool
  xs == ys =
    length xs == length ys && toList xs == toList ys

instance Foldable Queue where
  foldMap :: (Monoid m) => (a -> m) -> Queue a -> m
  foldMap f (Q xs ms _ ys _) =
    Foldable.foldMap f xs <> Foldable.foldMap (Foldable.foldMap f) ms <> listFoldMapBackwards f ys

  elem :: (Eq a) => a -> Queue a -> Bool
  elem x (Q xs ms _ ys _) =
    elem x xs || any (elem x) ms || elem x ys

  length :: Queue a -> Int
  length =
    length

  null :: Queue a -> Bool
  null =
    isEmpty

  toList :: Queue a -> [a]
  toList =
    toList

instance Monoid (Queue a) where
  mempty :: Queue a
  mempty =
    empty

  mappend :: Queue a -> Queue a -> Queue a
  mappend =
    (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (Queue a) where
  (<>) :: Queue a -> Queue a -> Queue a
  xs <> ys
    -- Either enqueue xs onto the front of ys, or ys onto the back of xs, depending on which one would be fewer
    -- enqueues.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (Queue a) where
  show :: Queue a -> String
  show =
    show . toList

instance Traversable Queue where
  traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
  traverse =
    traverse

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

-- Queue smart constructor.
makeQueue :: [a] -> [NonEmptyList a] -> Int -> [a] -> Int -> Queue a
makeQueue [] [] _ ys ylen = Q ys [] ylen [] 0
makeQueue [] (m : ms) xlen ys ylen = makeQueue1 m ms xlen ys ylen
makeQueue xs ms xlen ys ylen = makeQueue1 xs ms xlen ys ylen
{-# INLINE makeQueue #-}

-- Queue smart constructor.
makeQueue1 :: [a] -> [NonEmptyList a] -> Int -> [a] -> Int -> Queue a
makeQueue1 xs ms xlen ys ylen
  | ylen <= xlen = Q xs ms xlen ys ylen
  | otherwise = Q xs (ms ++ [reverse ys]) (xlen + ylen) [] 0
{-# INLINE makeQueue1 #-}

-- | An empty queue.
empty :: Queue a
empty =
  Q [] [] 0 [] 0

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Q [x] [] 1 [] 0

-- | \(\mathcal{O}(1)\) amortized. Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> Queue a -> Queue a
enqueue y (Q xs ms xlen ys ylen) =
  makeQueue xs ms xlen (y : ys) (ylen + 1)
{-# INLINEABLE enqueue #-}

-- | \(\mathcal{O}(1)\) amortized. Dequeue an element from the front of a queue.
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Q [] _ _ _ _ -> Nothing
  Q (x : xs) ms xlen ys ylen -> Just (x, makeQueue xs ms (xlen - 1) ys ylen)
{-# INLINEABLE dequeue #-}

-- | \(\mathcal{O}(1)\) amortized. Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> Queue a -> Queue a
enqueueFront x (Q xs ms xlen ys ylen) =
  -- smart constructor not needed here
  Q (x : xs) ms (xlen + 1) ys ylen
{-# INLINEABLE enqueueFront #-}

-- | Dequeue elements from the front of a queue while a predicate is satisfied.
dequeueWhile :: (a -> Bool) -> Queue a -> ([a], Queue a)
dequeueWhile p queue0 =
  case span p empty queue0 of
    (queue1, queue2) -> (toList queue1, queue2)
{-# INLINEABLE dequeueWhile #-}

span :: (a -> Bool) -> Queue a -> Queue a -> (Queue a, Queue a)
span p =
  go
  where
    go acc = \case
      Empty -> (acc, empty)
      Front x xs
        | p x -> go (enqueue x acc) xs
        | otherwise -> (acc, enqueueFront x xs)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty (Q _ _ xlen _ _) =
  xlen == 0
{-# INLINEABLE isEmpty #-}

-- | \(\mathcal{O}(1)\). How many elements are in a deque?
length :: Queue a -> Int
length (Q _ _ xlen _ ylen) =
  xlen + ylen
{-# INLINEABLE length #-}

-- @append xs ys@ enqueues @ys@ at the back of @ys@.
append :: Queue a -> Queue a -> Queue a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

-- @prepend xs ys@ enqueues @xs@ at the front of @ys@.
prepend :: Queue a -> Queue a -> Queue a
prepend Empty ys = ys
prepend (Front x xs) ys = enqueueFront x (prepend xs ys)

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
map :: (a -> b) -> Queue a -> Queue b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
traverse f (Q xs ms xlen ys ylen) =
  Q
    <$> Traversable.traverse f xs
    <*> Traversable.traverse (Traversable.traverse f) ms
    <*> pure xlen
    <*> backwards ys
    <*> pure ylen
  where
    backwards =
      go
      where
        go = \case
          [] -> pure []
          z : zs -> flip (:) <$> go zs <*> f z
{-# INLINEABLE traverse #-}

-- | \(\mathcal{O}(n)\). Construct a queue from a list, where the head of the list corresponds to the front of the
-- queue.
fromList :: [a] -> Queue a
fromList xs =
  Q xs [] (List.length xs) [] 0
{-# INLINEABLE fromList #-}

-- | \(\mathcal{O}(n)\). Construct a list from a queue, where the head of the list corresponds to the front of the
-- queue.
toList :: Queue a -> [a]
toList (Q xs ms _ ys _) =
  xs ++ concat ms ++ reverse ys
{-# INLINEABLE toList #-}
