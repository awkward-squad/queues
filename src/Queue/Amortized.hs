-- | A queue data structure with \(\mathcal{O}(1)\) amortized push and pop, as described in
--
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
--
-- A queue can be thought to have a "back" where new elements are pushed, and a "front" where elements are popped in the
-- order that they were pushed.
--
-- This queue also supports a "push to front" operation, because the underlying representation happens to trivially
-- support it. For a variant that also supports a "pop from back" operation, see "Data.Deque".
--
-- In this implementation, it is more helpful to think of the "front" being on the /left/, because (though the decision
-- is arbitrary) we are consistent throughout, where it matters:
--
--   * List conversion functions associate the head of a list with the front of a queue.
--   * The append operator @xs <> ys@ creates a queue with @xs@ in front of @ys@.
--
-- This module is intended to be imported qualified:
--
-- > import Queue.Amortized (Queue)
-- > import Queue.Amortized qualified as Queue
module Queue.Amortized
  ( -- * Queue
    Queue (Empty, Front),

    -- ** Initialization
    empty,
    singleton,

    -- * Basic interface
    push,
    pop,

    -- * Extended interface
    pushFront,
    popWhile,

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

-- | A queue data structure with \(\mathcal{O}(1)\) amortized push and pop.
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
    -- Either push xs onto the front of ys, or ys onto the back of xs, depending on which one would be fewer pushes.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (Queue a) where
  show = show . toList

instance Traversable Queue where
  traverse = traverse

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (pop -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (pop -> Just (x, xs))

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

-- | \(\mathcal{O}(1)\). Push an element onto the back of a queue, to be popped last.
push :: a -> Queue a -> Queue a
push y (Queue xs ms xlen ys ylen) =
  makeQueue xs ms xlen (y : ys) (ylen + 1)

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ _ _ -> Nothing
  Queue (x : xs) ms xlen ys ylen -> Just (x, makeQueue xs ms (xlen - 1) ys ylen)

-- | \(\mathcal{O}(1)\). Push an element onto the front of a queue, to be popped next.
pushFront :: a -> Queue a -> Queue a
pushFront x (Queue xs ms xlen ys ylen) =
  -- smart constructor not needed here
  Queue (x : xs) ms (xlen + 1) ys ylen

-- | Pop elements off of the front of a queue while a predicate is satisfied.
popWhile :: (a -> Bool) -> Queue a -> ([a], Queue a)
popWhile p queue0 =
  case span p queue0 of
    (queue1, queue2) -> (toList queue1, queue2)

span :: (a -> Bool) -> Queue a -> (Queue a, Queue a)
span p =
  go empty
  where
    go acc = \case
      Empty -> (acc, empty)
      Front x xs
        | p x -> go (push x acc) xs
        | otherwise -> (acc, pushFront x xs)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty (Queue _ _ xlen _ _) =
  xlen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a deque?
length :: Queue a -> Int
length (Queue _ _ xlen _ ylen) =
  xlen + ylen

-- @append xs ys@ pushes @ys@ onto the back of @ys@.
append :: Queue a -> Queue a -> Queue a
append xs Empty = xs
append xs (Front y ys) = append (push y xs) ys

-- @prepend xs ys@ pushes @xs@ onto the front of @ys@.
prepend :: Queue a -> Queue a -> Queue a
prepend Empty ys = ys
prepend (Front x xs) ys = pushFront x (prepend xs ys)

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
toList =
  List.unfoldr pop
