{-# LANGUAGE InstanceSigs #-}
-- It seems this is only needed on GHC <= 9.4
{-# LANGUAGE UndecidableInstances #-}

-- | A queue data structure with \(\mathcal{O}(1)\) worst-case enqueue and dequeue, as described in
--
--   * Okasaki, Chris. "Simple and efficient purely functional queues and deques." /Journal of functional programming/ 5.4 (1995): 583-592.
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
    enqueue,
    dequeue,

    -- ** Extended interface
    enqueueFront,
    dequeueWhile,

    -- * Queries
    isEmpty,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Constraint)
import GHC.Exts (Any)
import GHC.TypeError qualified as TypeError
import Queue.Internal.Prelude (NonEmptyList, listFoldMapBackwards, pattern NonEmptyList)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, span)

-- | A queue data structure with \(\mathcal{O}(1)\) worst-case enqueue and dequeue.
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
  (==) :: Queue a -> Queue a -> Bool
  xs == ys =
    toList xs == toList ys

instance Foldable Queue where
  foldMap :: (Monoid m) => (a -> m) -> Queue a -> m
  foldMap f (Queue xs ys _) =
    Foldable.foldMap f xs <> listFoldMapBackwards f ys

  elem :: (Eq a) => a -> Queue a -> Bool
  elem x (Queue xs ys _) =
    elem x xs || elem x ys

  null :: Queue a -> Bool
  null =
    isEmpty

  toList :: Queue a -> [a]
  toList =
    toList

type NoFunctorInstance :: Constraint
type NoFunctorInstance =
  TypeError.TypeError
    ( 'TypeError.Text "The real-time queue does not admit a Functor instance."
        'TypeError.:$$: 'TypeError.Text "Perhaps you would like to use the amortized queue instead?"
    )

instance (NoFunctorInstance) => Functor Queue where
  fmap = undefined

instance Monoid (Queue a) where
  mempty = empty
  mappend = (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the first argument.
instance Semigroup (Queue a) where
  Empty <> ys = ys
  Front x xs <> ys = enqueue x (xs <> ys)

instance (Show a) => Show (Queue a) where
  show = show . toList

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Front :: a -> Queue a -> Queue a
pattern Front x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Front #-}

-- Queue smart constructor.
--
-- `queue xs ys zs` is always called when |zs| = |xs| - |ys| + 1 (i.e. just after a enqueue or dequeue)
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

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> Queue a -> Queue a
enqueue y (Queue xs ys zs) =
  makeQueue xs (y : ys) zs

-- | \(\mathcal{O}(1)\). Dequeue an element from the front of a queue.
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, makeQueue xs ys zs)

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> Queue a -> Queue a
enqueueFront x (Queue xs ys zs) =
  -- smart constructor not needed here
  -- we also add useless work to the schedule to maintain the convenient rotate-on-empty-schedule trigger
  Queue (x : xs) ys (delay x zs)

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
