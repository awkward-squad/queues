-- | A queue data structure with \(\mathcal{O}(1)\) worst-case push and pop, as described in
--
--   * Okasaki, Chris. "Simple and efficient purely functional queues and deques." /Journal of functional programming/ 5.4 (1995): 583-592.
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
module Data.Queue
  ( -- * Queue
    Queue (Empty, Pop),

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
import Prelude hiding (map, span, traverse)

-- | A queue.
data Queue a
  = Queue
      -- The front of the queue.
      -- Invariant: length >= length of back
      [a]
      -- The back of the queue, in reverse order.
      [a]
      -- Some tail of the front of the queue. `Any` to show we don't care about the values, just the spine.
      -- Invariant: length = length of front - length of back
      [Any]
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

pattern Pop :: a -> Queue a -> Queue a
pattern Pop x xs <- (pop -> Just (x, xs))

{-# COMPLETE Empty, Pop #-}

-- Queue smart constructor.
--
-- `queue xs ys zs` is always called when |zs| = |xs| - |ys| + 1 (i.e. just after a push or pop)
queue :: [a] -> [a] -> [Any] -> Queue a
queue xs ys = \case
  [] -> let xs1 = rotate ys [] xs in Queue xs1 [] (schedule xs1)
  _ : zs -> Queue xs ys zs

schedule :: [a] -> [Any]
schedule =
  unsafeCoerce

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

-- | \(\mathcal{O}(1)\). Push an element onto the back of a queue, to be popped last.
push :: a -> Queue a -> Queue a
push y (Queue xs ys zs) =
  queue xs (y : ys) zs

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, queue xs ys zs)

-- | \(\mathcal{O}(1)\). Push an element onto the front of a queue, to be popped next.
pushFront :: a -> Queue a -> Queue a
pushFront x (Queue xs ys zs) =
  Queue (x : xs) ys (undefined : zs) -- n.b. smart constructor not needed here

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
      Pop x xs
        | p x -> go (push x acc) xs
        | otherwise -> (acc, pushFront x xs)

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty = \case
  Empty -> True
  Pop _ _ -> False

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
  Queue xs [] (schedule xs)

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
