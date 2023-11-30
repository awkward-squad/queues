-- | A queue data structure with \(\mathcal{O}(1)\) amortized enqueue and dequeue, as described in
--
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
--
-- A queue can be thought to have a "back" where new elements are enqueued, and a "front" where elements are dequeued in
-- the order that they were enqueued.
--
-- This queue also supports a "enqueue at front" operation, because the underlying representation happens to trivially
-- support it. For a variant that also supports a "dequeue from back" operation, see "Data.Deque".
--
-- In this implementation, it is more helpful to think of the "front" being on the /left/, because (though the decision
-- is arbitrary) we are consistent throughout, where it matters:
--
--   * List conversion functions associate the head of a list with the front of a queue.
--   * The append operator @xs <> ys@ creates a queue with @xs@ in front of @ys@.
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
  = Queue
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
  xs == ys =
    length xs == length ys && toList xs == toList ys

instance Foldable Queue where
  foldMap f (Queue xs ms _ ys _) =
    Foldable.foldMap f xs <> Foldable.foldMap (Foldable.foldMap f) ms <> listFoldMapBackwards f ys
  elem x (Queue xs ms _ ys _) = elem x xs || any (elem x) ms || elem x ys
  length = length
  null = isEmpty
  toList = toList

instance Monoid (Queue a) where
  mempty = empty
  mappend = (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (Queue a) where
  xs <> ys
    -- Either enqueue xs onto the front of ys, or ys onto the back of xs, depending on which one would be fewer
    -- enqueues.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (Queue a) where
  show = show . toList

instance Traversable Queue where
  traverse = traverse

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

-- Queue smart constructor.
makeQueue :: [a] -> [NonEmptyList a] -> Int -> [a] -> Int -> Queue a
makeQueue [] [] _ ys ylen = Queue ys [] ylen [] 0
makeQueue [] (m : ms) xlen ys ylen = makeQueue1 m ms xlen ys ylen
makeQueue xs ms xlen ys ylen = makeQueue1 xs ms xlen ys ylen

-- Queue smart constructor.
makeQueue1 :: [a] -> [NonEmptyList a] -> Int -> [a] -> Int -> Queue a
makeQueue1 xs ms xlen ys ylen
  | ylen <= xlen = Queue xs ms xlen ys ylen
  | otherwise = Queue xs (ms ++ [reverse ys]) (xlen + ylen) [] 0

-- | An empty queue.
empty :: Queue a
empty =
  Queue [] [] 0 [] 0

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Queue [x] [] 1 [] 0

-- | \(\mathcal{O}(1)\) amortized. Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> Queue a -> Queue a
enqueue y (Queue xs ms xlen ys ylen) =
  makeQueue xs ms xlen (y : ys) (ylen + 1)

-- | \(\mathcal{O}(1)\) amortized. Dequeue an element from the front of a queue.
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Queue [] _ _ _ _ -> Nothing
  Queue (x : xs) ms xlen ys ylen -> Just (x, makeQueue xs ms (xlen - 1) ys ylen)

-- | \(\mathcal{O}(1)\) amortized. Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> Queue a -> Queue a
enqueueFront x (Queue xs ms xlen ys ylen) =
  -- smart constructor not needed here
  Queue (x : xs) ms (xlen + 1) ys ylen

-- | Dequeue elements from the front of a queue while a predicate is satisfied.
dequeueWhile :: (a -> Bool) -> Queue a -> ([a], Queue a)
dequeueWhile p queue0 =
  case span p queue0 of
    (queue1, queue2) -> (toList queue1, queue2)

span :: (a -> Bool) -> Queue a -> (Queue a, Queue a)
span p =
  go empty
  where
    go acc = \case
      Empty -> (acc, empty)
      Front x xs
        | p x -> go (enqueue x acc) xs
        | otherwise -> (acc, enqueueFront x xs)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty (Queue _ _ xlen _ _) =
  xlen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a deque?
length :: Queue a -> Int
length (Queue _ _ xlen _ ylen) =
  xlen + ylen

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
traverse f (Queue xs ms xlen ys ylen) =
  Queue
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

-- | \(\mathcal{O}(n)\). Construct a queue from a list, where the head of the list corresponds to the front of the
-- queue.
fromList :: [a] -> Queue a
fromList xs =
  Queue xs [] (List.length xs) [] 0

-- | \(\mathcal{O}(n)\). Construct a list from a queue, where the head of the list corresponds to the front of the
-- queue.
toList :: Queue a -> [a]
toList (Queue xs ms _ ys _) =
  xs ++ concat ms ++ reverse ys
