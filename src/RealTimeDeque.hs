-- It seems this is only needed on GHC <= 9.4
{-# LANGUAGE UndecidableInstances #-}

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) worst-case enqueue and dequeue, as described in
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
    reverse,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Bits (unsafeShiftR)
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint)
import Data.List qualified as List
import GHC.Exts (Any)
import GHC.TypeError qualified as TypeError
import Queue.Internal.Prelude
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, reverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) worst-case enqueue and dequeue.
data RealTimeDeque a
  = RealTimeDeque
      [a]
      {-# UNPACK #-} !Int
      Schedule
      [a]
      {-# UNPACK #-} !Int
      Schedule

instance (Eq a) => Eq (RealTimeDeque a) where
  xs == ys =
    length xs == length ys && toList xs == toList ys

instance Foldable RealTimeDeque where
  foldMap f (RealTimeDeque xs _ _ ys _ _) =
    Foldable.foldMap f xs <> listFoldMapBackwards f ys
  elem x (RealTimeDeque xs _ _ ys _ _) = elem x xs || elem x ys
  length = length
  null = isEmpty
  toList = toList

type NoFunctorInstance :: Constraint
type NoFunctorInstance = TypeError.TypeError ('TypeError.Text "The real-time deque does not admit a Functor instance.")

instance (NoFunctorInstance) => Functor RealTimeDeque where
  fmap = undefined

instance Monoid (RealTimeDeque a) where
  mempty = empty
  mappend = (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (RealTimeDeque a) where
  xs <> ys
    -- Either enqueue xs at the front of ys, or ys onto the back of xs, depending on which one would be fewer enqueues.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (RealTimeDeque a) where
  show = show . toList

-- | An empty deque.
pattern Empty :: RealTimeDeque a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a deque, and the rest of it.
pattern Front :: a -> RealTimeDeque a -> RealTimeDeque a
pattern Front x xs <- (dequeue -> Just (x, xs))

-- | The back of a deque, and the rest of it.
pattern Back :: RealTimeDeque a -> a -> RealTimeDeque a
pattern Back xs x <- (dequeueBack -> Just (xs, x))

{-# COMPLETE Empty, Front #-}

{-# COMPLETE Empty, Back #-}

c :: Int
c = 3

-- Deque smart constructor.
makeDeque :: [a] -> Int -> [Any] -> [a] -> Int -> [Any] -> RealTimeDeque a
makeDeque xs xlen xc ys ylen yc
  | xlen > (c * ylen + 1) =
      let xs1 = take xlen1 xs
          ys1 = rotate1 xlen1 ys xs
       in RealTimeDeque xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | ylen > (c * xlen + 1) =
      let xs1 = rotate1 ylen1 xs ys
          ys1 = take ylen1 ys
       in RealTimeDeque xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | otherwise = RealTimeDeque xs xlen xc ys ylen yc
  where
    xlen1 = (xlen + ylen) `unsafeShiftR` 1
    ylen1 = xlen + ylen - xlen1

rotate1 :: Int -> [a] -> [a] -> [a]
rotate1 i (x : xs) ys | i >= c = x : rotate1 (i - c) xs (drop c ys)
rotate1 i xs ys = rotate2 xs (drop i ys) []

rotate2 :: [a] -> [a] -> [a] -> [a]
rotate2 [] ys zs = List.reverse ys ++ zs
rotate2 (x : xs) ys zs = x : rotate2 xs (drop c ys) (List.reverse (take c ys) ++ zs)

-- | An empty deque.
empty :: RealTimeDeque a
empty =
  RealTimeDeque [] 0 [] [] 0 []

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a deque.
enqueue :: a -> RealTimeDeque a -> RealTimeDeque a
enqueue y (RealTimeDeque xs xlen xc ys ylen yc) =
  makeDeque xs xlen (execute1 xc) (y : ys) (ylen + 1) (execute1 yc)

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a deque.
enqueueFront :: a -> RealTimeDeque a -> RealTimeDeque a
enqueueFront x (RealTimeDeque xs xlen xc ys ylen yc) =
  makeDeque (x : xs) (xlen + 1) (execute1 xc) ys ylen (execute1 yc)

-- | \(\mathcal{O}(1)\). Dequeue an element from the front of a deque.
dequeue :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
dequeue = \case
  RealTimeDeque [] _ _ [] _ _ -> Nothing
  RealTimeDeque [] _ _ (y : _) _ _ -> Just (y, empty)
  RealTimeDeque (x : xs) xlen xc ys ylen yc -> Just (x, makeDeque xs (xlen - 1) (execute2 xc) ys ylen (execute2 yc))

-- | \(\mathcal{O}(1)\). Dequeue an element from of the back of a deque.
dequeueBack :: RealTimeDeque a -> Maybe (RealTimeDeque a, a)
dequeueBack = \case
  RealTimeDeque [] _ _ [] _ _ -> Nothing
  RealTimeDeque (x : _) _ _ [] _ _ -> Just (empty, x)
  RealTimeDeque xs xlen xc (y : ys) ylen yc -> Just (makeDeque xs xlen (execute2 xc) ys (ylen - 1) (execute2 yc), y)

-- | \(\mathcal{O}(1)\). Is a deque empty?
isEmpty :: RealTimeDeque a -> Bool
isEmpty (RealTimeDeque _ xlen _ _ ylen _) =
  xlen == 0 && ylen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a deque?
length :: RealTimeDeque a -> Int
length (RealTimeDeque _ xlen _ _ ylen _) =
  xlen + ylen

-- | \(\mathcal{O}(1)\). Reverse a deque.
reverse :: RealTimeDeque a -> RealTimeDeque a
reverse (RealTimeDeque xs xlen xc ys ylen yc) =
  RealTimeDeque ys ylen yc xs xlen xc

-- O(ys). @append xs ys@ enqueues @ys@ onto the back of @ys@.
append :: RealTimeDeque a -> RealTimeDeque a -> RealTimeDeque a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

-- O(xs). @prepend xs ys@ enqueues @xs@ onto the front of @ys@. O(xs).
prepend :: RealTimeDeque a -> RealTimeDeque a -> RealTimeDeque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (enqueueFront x ys)

-- | \(\mathcal{O}(n)\). Construct a deque from a list, where the head of the list corresponds to the front of the
-- deque.
fromList :: [a] -> RealTimeDeque a
fromList =
  foldr enqueueFront empty

-- | \(\mathcal{O}(n)\). Construct a list from a deque, where the head of the list corresponds to the front of the
-- deque.
toList :: RealTimeDeque a -> [a]
toList (RealTimeDeque xs _ _ ys _ _) =
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
execute2 =
  execute1 . execute1
