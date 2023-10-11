-- | A queue data structure with \(\mathcal{O}(1)\) worst-case push and pop, as described in
--
--   * Chris Okasaki, /"Simple and efficient purely functional queues and deques"/, Journal of Functional Programming, 5(4):583–592, October 1995.
--
-- A queue can be thought to have a "back", like the back of a line (or queue), where new elements are pushed, and a
-- "front", like the front of a line (or queue), where elements are popped in the order that they were pushed.
--
-- This queue also supports a "push to front" operation, which is like cutting the line (or queue), because it is
-- trivial to implement.
module Data.Queue
  ( -- * Queue
    Queue,
    empty,

    -- * Basic interface
    push,
    pushFront,
    pop,

    -- * List conversions
    fromList,
    toList,
  )
where

import qualified Data.List as List

-- | A queue.
data Queue a
  = Queue [a] [a] [a]
  deriving stock (Functor)

instance Monoid (Queue a) where
  mempty = empty
  mappend = (<>)

instance Semigroup (Queue a) where
  xs <> ys =
    case pop ys of
      Nothing -> xs
      Just (y, ys1) -> push y xs <> ys1

instance (Show a) => Show (Queue a) where
  show = show . toList

queue :: [a] -> [a] -> [a] -> Queue a
queue xs ys = \case
  [] -> let xs1 = rotate ys [] xs in Queue xs1 [] xs1
  _ : zs -> Queue xs ys zs

-- rotate ys zs xs = xs ++ reverse ys ++ zs
rotate :: [a] -> NonEmptyList a -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

-- | An empty queue.
empty :: Queue a
empty =
  Queue [] [] []

-- | \(\mathcal{O}(1)\). Push an element onto the back of a queue.
push :: a -> Queue a -> Queue a
push y (Queue xs ys zs) =
  queue xs (y : ys) zs

-- | \(\mathcal{O}(1)\). Push an element onto the front of a queue.
pushFront :: a -> Queue a -> Queue a
pushFront x (Queue xs ys zs) =
  Queue (x : xs) ys (x : zs) -- n.b. smart constructor not needed here

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, queue xs ys zs)

-- | \(\mathcal{O}(1)\). Construct a queue from a list, where the head of the list corresponds to the front of the
-- queue.
fromList :: [a] -> Queue a
fromList xs =
  Queue xs [] xs

-- | \(\mathcal{O}(n)\). Construct a list from a queue, where the head of the list corresponds to the front of the
-- queue.
toList :: Queue a -> [a]
toList =
  List.unfoldr pop

type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}
