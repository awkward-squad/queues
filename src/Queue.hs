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
--
-- This module is intended to be imported qualified:
--
-- > import Queue (Queue)
-- > import Queue qualified
module Queue
  ( -- * Queue
    Queue (Empty, Front),

    -- ** Initialization
    empty,
    singleton,

    -- * Basic interface
    push,
    pop,

    -- ** Extended interface
    pushFront,
    popWhile,

    -- * Queries
    isEmpty,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Foldable qualified as Foldable
import GHC.Exts (Any)
import GHC.TypeError qualified as TypeError
import Queue.Internal.Prelude
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, span)

-- | A queue data structure with \(\mathcal{O}(1)\) worst-case push and pop.
data Queue a
  = Queue
      -- The front of the queue.
      -- Invariant: length >= length of back
      [a]
      -- The back of the queue, in reverse order.
      [a]
      -- Some tail of the front of the queue.
      -- Invariant: length = length of front - length of back
      Schedule

instance (Eq a) => Eq (Queue a) where
  xs == ys =
    toList xs == toList ys

instance Foldable Queue where
  foldMap f (Queue xs ys _) =
    Foldable.foldMap f xs <> listFoldMapBackwards f ys
  elem x (Queue xs ys _) = elem x xs || elem x ys
  null = isEmpty
  toList = toList

instance
  ( TypeError.TypeError
      ( 'TypeError.Text "The real-time queue does not admit a Functor instance."
          'TypeError.:$$: 'TypeError.Text "Perhaps you would like to use the amortized queue instead?"
      )
  ) =>
  Functor Queue
  where
  fmap = undefined

instance Monoid (Queue a) where
  mempty = empty
  mappend = (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the first argument.
instance Semigroup (Queue a) where
  Empty <> ys = ys
  Front x xs <> ys = push x (xs <> ys)

instance (Show a) => Show (Queue a) where
  show = show . toList

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (pop -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (pop -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

-- Queue smart constructor.
--
-- `queue xs ys zs` is always called when |zs| = |xs| - |ys| + 1 (i.e. just after a push or pop)
makeQueue :: [a] -> [a] -> Schedule -> Queue a
makeQueue xs ys = \case
  NoMoreWorkToDo -> let xs1 = rotate ys [] xs in Queue xs1 [] (schedule xs1)
  DidWork zs -> Queue xs ys zs

-- rotate ys zs xs = xs ++ reverse ys ++ zs
-- Precondition: |ys| = |xs| + 1
rotate :: NonEmptyList a -> [a] -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

-- | An empty queue.
empty :: Queue a
empty =
  Queue [] [] NoMoreWorkToDo

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Queue xs [] (schedule xs)
  where
    xs = [x]

-- | \(\mathcal{O}(1)\). Push an element onto the back of a queue, to be popped last.
push :: a -> Queue a -> Queue a
push y (Queue xs ys zs) =
  makeQueue xs (y : ys) zs

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, makeQueue xs ys zs)

-- | \(\mathcal{O}(1)\). Push an element onto the front of a queue, to be popped next.
pushFront :: a -> Queue a -> Queue a
pushFront x (Queue xs ys zs) =
  -- smart constructor not needed here
  -- we also add useless work to the schedule to maintain the convenient rotate-on-empty-schedule trigger
  Queue (x : xs) ys (delay x zs)

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
isEmpty = \case
  Queue [] _ _ -> True
  _ -> False

-- | \(\mathcal{O}(1)\). Construct a queue from a list, where the head of the list corresponds to the front of the
-- queue.
fromList :: [a] -> Queue a
fromList xs =
  Queue xs [] (schedule xs)

-- | \(\mathcal{O}(n)\). Construct a list from a queue, where the head of the list corresponds to the front of the
-- queue.
toList :: Queue a -> [a]
toList (Queue xs ys _) =
  xs ++ reverse ys

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
