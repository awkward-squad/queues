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
    Queue (Empty, Full),

    -- ** Initialization
    empty,
    singleton,

    -- * Basic interface
    push,
    pushFront,
    pop,

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

import qualified Data.List as List
import qualified Data.Traversable as Traversable
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (map, traverse)

-- | A queue.
data Queue a
  = Queue
      [a] -- xs = front of queue
      [a] -- ys = back of queue, in reverse order
      [Any] -- zs = some suffix of front of queue, |zs| = |xs| - |ys|. it's Any to show we don't care about the values
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

pattern Empty :: Queue a
pattern Empty <- Queue [] _ _

pattern Full :: a -> Queue a -> Queue a
pattern Full x xs <- (pop -> Just (x, xs))

{-# COMPLETE Empty, Full #-}

queue :: [a] -> [a] -> [Any] -> Queue a
queue xs ys = \case
  [] -> let xs1 = rotate ys [] xs in Queue xs1 [] (unsafeCoerce xs1)
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

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Queue [x] [] [undefined]

-- | \(\mathcal{O}(1)\). Push an element onto the back of a queue, to be popped after the other elements.
push :: a -> Queue a -> Queue a
push y (Queue xs ys zs) =
  queue xs (y : ys) zs

-- | \(\mathcal{O}(1)\). Push an element onto the front of a queue, to be popped next.
pushFront :: a -> Queue a -> Queue a
pushFront x (Queue xs ys zs) =
  Queue (x : xs) ys (undefined : zs) -- n.b. smart constructor not needed here

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, queue xs ys zs)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty = \case
  Empty -> True
  Full _ _ -> False

-- | \(\mathcal{O}(n)\). Apply a function to each element in a queue.
map :: (a -> b) -> Queue a -> Queue b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function (in a context) to each element in a queue.
traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
traverse f (Queue xs ys zs) =
  Queue
    <$> Traversable.traverse f xs
    <*> traverseBackwards f ys
    <*> pure zs

traverseBackwards :: (Applicative f) => (a -> f b) -> [a] -> f [b]
traverseBackwards f = \case
  [] -> pure []
  x : xs -> flip (:) <$> traverseBackwards f xs <*> f x

-- | \(\mathcal{O}(1)\). Construct a queue from a list, where the head of the list corresponds to the front of the
-- queue.
fromList :: [a] -> Queue a
fromList xs =
  Queue xs [] (unsafeCoerce xs)

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
