module AmortizedDeque
  ( -- * Amortized deque
    AmortizedDeque (Empty, Front, Back),

    -- ** Initialization
    empty,

    -- * Basic interface
    enqueue,
    enqueueFront,
    dequeue,
    dequeueBack,

    -- * Queries
    isEmpty,
    length,

    -- * Transformations
    map,
    traverse,
    reverse,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Bits (unsafeShiftR)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import QueuesPrelude (listFoldMapBackwards)
import Prelude hiding (drop, foldMap, length, map, reverse, span, take, traverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) amortized enqueue and dequeue.
data AmortizedDeque a
  = Q
      [a]
      {-# UNPACK #-} !Int
      [a]
      {-# UNPACK #-} !Int
  deriving stock (Functor)

instance (Eq a) => Eq (AmortizedDeque a) where
  (==) :: AmortizedDeque a -> AmortizedDeque a -> Bool
  xs == ys =
    -- FIXME make this faster
    length xs == length ys && toList xs == toList ys

instance Foldable AmortizedDeque where
  foldMap :: (Monoid m) => (a -> m) -> AmortizedDeque a -> m
  foldMap f (Q xs _ ys _) =
    Foldable.foldMap f xs <> listFoldMapBackwards f ys

  elem :: (Eq a) => a -> AmortizedDeque a -> Bool
  elem x (Q xs _ ys _) =
    List.elem x xs || List.elem x ys

  length :: AmortizedDeque a -> Int
  length =
    AmortizedDeque.length

  null :: AmortizedDeque a -> Bool
  null =
    isEmpty

  toList :: AmortizedDeque a -> [a]
  toList =
    AmortizedDeque.toList

instance Monoid (AmortizedDeque a) where
  mempty :: AmortizedDeque a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (AmortizedDeque a) where
  (<>) :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
  xs <> ys
    -- Either enqueue xs at the front of ys, or ys onto the back of xs, depending on which one would be fewer enqueues.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (AmortizedDeque a) where
  show :: AmortizedDeque a -> String
  show =
    show . toList

instance Traversable AmortizedDeque where
  traverse :: (Applicative f) => (a -> f b) -> AmortizedDeque a -> f (AmortizedDeque b)
  traverse =
    traverse

-- | An empty double-ended queue.
pattern Empty :: AmortizedDeque a
pattern Empty <-
  (dequeue -> Nothing)

-- | The front of a double-ended queue, and the rest of it.
pattern Front :: a -> AmortizedDeque a -> AmortizedDeque a
pattern Front x xs <-
  (dequeue -> Just (x, xs))

-- | The back of a double-ended queue, and the rest of it.
pattern Back :: AmortizedDeque a -> a -> AmortizedDeque a
pattern Back xs x <-
  (dequeueBack -> Just (xs, x))

{-# COMPLETE Empty, Front #-}

{-# COMPLETE Empty, Back #-}

-- Deque smart constructor.
makeDeque :: [a] -> Int -> [a] -> Int -> AmortizedDeque a
makeDeque xs xlen ys ylen
  | xlen > (3 * ylen + 1) = Q (List.take xlen1 xs) xlen1 (ys ++ List.reverse (List.drop xlen1 xs)) ylen1
  | ylen > (3 * xlen + 1) = Q (xs ++ List.reverse (List.drop ylen1 ys)) xlen1 (List.take ylen1 ys) ylen1
  | otherwise = Q xs xlen ys ylen
  where
    xlen1 = (xlen + ylen) `unsafeShiftR` 1
    ylen1 = xlen + ylen - xlen1
{-# INLINE makeDeque #-}

-- | An empty double-ended queue.
empty :: AmortizedDeque a
empty =
  Q [] 0 [] 0

-- | \(\mathcal{O}(1)^*\). Enqueue an element at the back of a double-ended queue.
enqueue :: a -> AmortizedDeque a -> AmortizedDeque a
enqueue y (Q xs xlen ys ylen) =
  makeDeque xs xlen (y : ys) (ylen + 1)
{-# INLINEABLE enqueue #-}

-- | \(\mathcal{O}(1)^*\). Enqueue an element at the front of a double-ended queue.
enqueueFront :: a -> AmortizedDeque a -> AmortizedDeque a
enqueueFront x (Q xs xlen ys ylen) =
  makeDeque (x : xs) (xlen + 1) ys ylen
{-# INLINEABLE enqueueFront #-}

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)^*\) rest. Dequeue an element from the front of a double-ended queue.
dequeue :: AmortizedDeque a -> Maybe (a, AmortizedDeque a)
dequeue = \case
  Q [] _ [] _ -> Nothing
  Q [] _ (y : _) _ -> Just (y, empty)
  Q (x : xs) xlen ys ylen -> Just (x, makeDeque xs (xlen - 1) ys ylen)
{-# INLINEABLE dequeue #-}

-- | \(\mathcal{O}(1)\) back, \(\mathcal{O}(1)^*\) rest. Dequeue an element from of the back of a double-ended queue.
dequeueBack :: AmortizedDeque a -> Maybe (AmortizedDeque a, a)
dequeueBack = \case
  Q [] _ [] _ -> Nothing
  Q (x : _) _ [] _ -> Just (empty, x)
  Q xs xlen (y : ys) ylen -> Just (makeDeque xs xlen ys (ylen - 1), y)
{-# INLINEABLE dequeueBack #-}

-- | \(\mathcal{O}(1)\). Is a double-ended queue empty?
isEmpty :: AmortizedDeque a -> Bool
isEmpty (Q _ xlen _ ylen) =
  xlen == 0 && ylen == 0
{-# INLINEABLE isEmpty #-}

-- | \(\mathcal{O}(1)\). How many elements are in a double-ended queue?
length :: AmortizedDeque a -> Int
length (Q _ xlen _ ylen) =
  xlen + ylen
{-# INLINEABLE length #-}

-- | \(\mathcal{O}(1)\). Reverse a double-ended queue.
reverse :: AmortizedDeque a -> AmortizedDeque a
reverse (Q xs xlen ys ylen) =
  Q ys ylen xs xlen
{-# INLINE reverse #-}

append :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

prepend :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (enqueueFront x ys)

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
map :: (a -> b) -> AmortizedDeque a -> AmortizedDeque b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
traverse :: (Applicative f) => (a -> f b) -> AmortizedDeque a -> f (AmortizedDeque b)
traverse f (Q xs xlen ys ylen) =
  (\xs1 ys1 -> Q xs1 xlen ys1 ylen) <$> Traversable.traverse f xs <*> backwards ys
  where
    backwards =
      go
      where
        go = \case
          [] -> pure []
          z : zs -> flip (:) <$> go zs <*> f z
{-# INLINEABLE traverse #-}

-- | \(\mathcal{O}(n)\). Construct a double-ended queue from a list. The head of the list corresponds to the front of
-- the double-ended queue.
fromList :: [a] -> AmortizedDeque a
fromList =
  foldr enqueueFront empty
{-# INLINE fromList #-}

-- | \(\mathcal{O}(n)\). Construct a list from a double-ended queue. The head of the list corresponds to the front of
-- the double-ended queue.
toList :: AmortizedDeque a -> [a]
toList (Q xs _ ys _) =
  xs ++ List.reverse ys
{-# INLINEABLE toList #-}
