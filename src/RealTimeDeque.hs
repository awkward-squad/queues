-- It seems this is only needed on GHC <= 9.4
{-# LANGUAGE UndecidableInstances #-}

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) (worst-case) operations, as described in
--
--   * Okasaki, Chris. \"Simple and efficient purely functional queues and deques.\" /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
module RealTimeDeque
  ( -- * Deque
    RealTimeDeque (Empty, Front, Back),

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
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (drop, foldMap, length, map, reverse, take, traverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) (worst-case) operations.
data RealTimeDeque a
  = Q
      [a]
      {-# UNPACK #-} !Int
      Schedule
      [a]
      {-# UNPACK #-} !Int
      Schedule

instance (Eq a) => Eq (RealTimeDeque a) where
  (==) :: RealTimeDeque a -> RealTimeDeque a -> Bool
  xs == ys =
    length xs == length ys && toList xs == toList ys

instance Foldable RealTimeDeque where
  foldMap :: (Monoid m) => (a -> m) -> RealTimeDeque a -> m
  foldMap f =
    go
    where
      go = \case
        Empty -> mempty
        Front x xs -> f x <> go xs

  elem :: (Eq a) => a -> RealTimeDeque a -> Bool
  elem x (Q xs _ _ ys _ _) =
    List.elem x xs || List.elem x ys

  length :: RealTimeDeque a -> Int
  length =
    RealTimeDeque.length

  null :: RealTimeDeque a -> Bool
  null =
    isEmpty

  toList :: RealTimeDeque a -> [a]
  toList =
    RealTimeDeque.toList

instance Functor RealTimeDeque where
  fmap :: (a -> b) -> RealTimeDeque a -> RealTimeDeque b
  fmap =
    map

instance Monoid (RealTimeDeque a) where
  mempty :: RealTimeDeque a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (RealTimeDeque a) where
  (<>) :: RealTimeDeque a -> RealTimeDeque a -> RealTimeDeque a
  xs <> ys
    -- Either enqueue xs at the front of ys, or ys onto the back of xs, depending on which one would be fewer enqueues.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (RealTimeDeque a) where
  show :: RealTimeDeque a -> String
  show =
    show . toList

instance Traversable RealTimeDeque where
  traverse :: (Applicative f) => (a -> f b) -> RealTimeDeque a -> f (RealTimeDeque b)
  traverse =
    traverse

-- | An empty double-ended queue.
pattern Empty :: RealTimeDeque a
pattern Empty <-
  (dequeue -> Nothing)

-- | The front of a double-ended queue, and the rest of it.
pattern Front :: a -> RealTimeDeque a -> RealTimeDeque a
pattern Front x xs <-
  (dequeue -> Just (x, xs))

-- | The back of a double-ended queue, and the rest of it.
pattern Back :: RealTimeDeque a -> a -> RealTimeDeque a
pattern Back xs x <-
  (dequeueBack -> Just (xs, x))

{-# COMPLETE Empty, Front #-}

{-# COMPLETE Empty, Back #-}

-- Deque smart constructor.
makeDeque :: [a] -> Int -> [Any] -> [a] -> Int -> [Any] -> RealTimeDeque a
makeDeque xs xlen xc ys ylen yc
  | xlen > (3 * ylen + 1) =
      let xs1 = List.take xlen1 xs
          ys1 = rotate1 xlen1 ys xs
       in Q xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | ylen > (3 * xlen + 1) =
      let xs1 = rotate1 ylen1 xs ys
          ys1 = List.take ylen1 ys
       in Q xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | otherwise = Q xs xlen xc ys ylen yc
  where
    xlen1 = (xlen + ylen) `unsafeShiftR` 1
    ylen1 = xlen + ylen - xlen1

rotate1 :: Int -> [a] -> [a] -> [a]
rotate1 i (x : xs) ys | i >= 3 = x : rotate1 (i - 3) xs (List.drop 3 ys)
rotate1 i xs ys = rotate2 xs (List.drop i ys) []

rotate2 :: [a] -> [a] -> [a] -> [a]
rotate2 [] ys zs = List.reverse ys ++ zs
rotate2 (x : xs) ys zs = x : rotate2 xs (List.drop 3 ys) (List.reverse (List.take 3 ys) ++ zs)

-- | An empty double-ended queue.
empty :: RealTimeDeque a
empty =
  Q [] 0 [] [] 0 []

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a double-ended queue.
enqueue :: a -> RealTimeDeque a -> RealTimeDeque a
enqueue y (Q xs xlen xc ys ylen yc) =
  makeDeque xs xlen (execute1 xc) (y : ys) (ylen + 1) (execute1 yc)

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a double-ended queue.
enqueueFront :: a -> RealTimeDeque a -> RealTimeDeque a
enqueueFront x (Q xs xlen xc ys ylen yc) =
  makeDeque (x : xs) (xlen + 1) (execute1 xc) ys ylen (execute1 yc)

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)\) rest. Dequeue an element from the front of a double-ended queue.
dequeue :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
dequeue = \case
  Q [] _ _ [] _ _ -> Nothing
  Q [] _ _ (y : _) _ _ -> Just (y, empty)
  Q (x : xs) xlen xc ys ylen yc -> Just (x, makeDeque xs (xlen - 1) (execute2 xc) ys ylen (execute2 yc))

-- | \(\mathcal{O}(1)\) back, \(\mathcal{O}(1)\) rest. Dequeue an element from of the back of a double-ended queue.
dequeueBack :: RealTimeDeque a -> Maybe (RealTimeDeque a, a)
dequeueBack = \case
  Q [] _ _ [] _ _ -> Nothing
  Q (x : _) _ _ [] _ _ -> Just (empty, x)
  Q xs xlen xc (y : ys) ylen yc -> Just (makeDeque xs xlen (execute2 xc) ys (ylen - 1) (execute2 yc), y)

-- | \(\mathcal{O}(1)\). Is a double-ended queue empty?
isEmpty :: RealTimeDeque a -> Bool
isEmpty (Q _ xlen _ _ ylen _) =
  xlen == 0 && ylen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a double-ended queue?
length :: RealTimeDeque a -> Int
length (Q _ xlen _ _ ylen _) =
  xlen + ylen

-- | \(\mathcal{O}(1)\). Reverse a double-ended queue.
reverse :: RealTimeDeque a -> RealTimeDeque a
reverse (Q xs xlen xc ys ylen yc) =
  Q ys ylen yc xs xlen xc

-- O(ys). @append xs ys@ enqueues @ys@ onto the back of @ys@.
append :: RealTimeDeque a -> RealTimeDeque a -> RealTimeDeque a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

-- O(xs). @prepend xs ys@ enqueues @xs@ onto the front of @ys@.
prepend :: RealTimeDeque a -> RealTimeDeque a -> RealTimeDeque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (enqueueFront x ys)

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
map :: (a -> b) -> RealTimeDeque a -> RealTimeDeque b
map f =
  fromList . List.map f . toList

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
traverse :: (Applicative f) => (a -> f b) -> RealTimeDeque a -> f (RealTimeDeque b)
traverse f =
  fmap fromList . Traversable.traverse f . toList

-- | \(\mathcal{O}(n)\). Construct a double-ended queue from a list. The head of the list corresponds to the front of
-- the double-ended queue.
fromList :: [a] -> RealTimeDeque a
fromList =
  foldr enqueueFront empty

-- | \(\mathcal{O}(n)\). Construct a list from a double-ended queue. The head of the list corresponds to the front of
-- the double-ended queue.
toList :: RealTimeDeque a -> [a]
toList (Q xs _ _ ys _ _) =
  xs ++ List.reverse ys

------------------------------------------------------------------------------------------------------------------------
-- Schedule utils

type Schedule =
  [Any]

schedule :: [a] -> Schedule
schedule =
  unsafeCoerce

execute1 :: Schedule -> Schedule
execute1 = \case
  [] -> []
  _ : xs -> xs

execute2 :: Schedule -> Schedule
execute2 = \case
  _ : _ : xs -> xs
  _ -> []
