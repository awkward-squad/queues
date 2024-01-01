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

import Data.Foldable qualified as Foldable
import Data.Traversable qualified as Traversable
import Prelude hiding (foldMap, length, map, reverse, span, traverse)

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
  xs == ys = undefined

instance Foldable AmortizedDeque where
  foldMap :: (Monoid m) => (a -> m) -> AmortizedDeque a -> m
  foldMap = undefined

instance Monoid (AmortizedDeque a) where
  mempty :: AmortizedDeque a
  mempty =
    empty

instance Semigroup (AmortizedDeque a) where
  (<>) :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
  xs <> ys = undefined

instance (Show a) => Show (AmortizedDeque a) where
  show :: AmortizedDeque a -> String
  show = undefined

instance Traversable AmortizedDeque where
  traverse :: (Applicative f) => (a -> f b) -> AmortizedDeque a -> f (AmortizedDeque b)
  traverse = undefined

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

-- | An empty double-ended queue.
empty :: AmortizedDeque a
empty =
  Q [] 0 [] 0

enqueue :: a -> AmortizedDeque a -> AmortizedDeque a
enqueue = undefined

enqueueFront :: a -> AmortizedDeque a -> AmortizedDeque a
enqueueFront = undefined

dequeue :: AmortizedDeque a -> Maybe (a, AmortizedDeque a)
dequeue = undefined

dequeueBack :: AmortizedDeque a -> Maybe (AmortizedDeque a, a)
dequeueBack = undefined

isEmpty :: AmortizedDeque a -> Bool
isEmpty = undefined

length :: AmortizedDeque a -> Int
length = undefined

reverse :: AmortizedDeque a -> AmortizedDeque a
reverse = undefined

append :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

prepend :: AmortizedDeque a -> AmortizedDeque a -> AmortizedDeque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (enqueueFront x ys)

map :: (a -> b) -> AmortizedDeque a -> AmortizedDeque b
map = undefined

traverse :: (Applicative f) => (a -> f b) -> AmortizedDeque a -> f (AmortizedDeque b)
traverse = undefined

fromList :: [a] -> AmortizedDeque a
fromList = undefined

toList :: AmortizedDeque a -> [a]
toList = undefined
