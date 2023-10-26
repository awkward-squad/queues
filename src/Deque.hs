-- | A double-ended queue data structure with \(\mathcal{O}(1)\) worst-case push and pop, as described in
--
--   * Okasaki, Chris. "Simple and efficient purely functional queues and deques." /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
--
-- This module is intended to be imported qualified:
--
-- > import Deque (Deque)
-- > import Deque qualified
module Deque
  ( -- * Deque
    Deque (Empty, Front, Back),

    -- ** Initialization
    empty,

    -- * Basic interface
    push,
    pushFront,
    pop,
    popBack,

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
import Data.List qualified as List
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (length, reverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) worst-case push and pop.
data Deque a
  = Deque
      [a]
      {-# UNPACK #-} !Int
      Schedule
      [a]
      {-# UNPACK #-} !Int
      Schedule

instance Monoid (Deque a) where
  mempty = empty
  mappend = (<>)

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (Deque a) where
  xs <> ys
    -- Either push xs onto the front of ys, or ys onto the back of xs, depending on which one would be fewer pushes.
    | length xs < length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (Deque a) where
  show = show . toList

-- | An empty deque.
pattern Empty :: Deque a
pattern Empty <- (pop -> Nothing)

-- | The front of a deque, and the rest of it.
pattern Front :: a -> Deque a -> Deque a
pattern Front x xs <- (pop -> Just (x, xs))

-- | The back of a deque, and the rest of it.
pattern Back :: Deque a -> a -> Deque a
pattern Back xs x <- (popBack -> Just (xs, x))

{-# COMPLETE Empty, Front #-}

{-# COMPLETE Empty, Back #-}

c :: Int
c = 3

-- Deque smart constructor.
deque :: [a] -> Int -> [Any] -> [a] -> Int -> [Any] -> Deque a
deque xs xlen xc ys ylen yc
  | xlen > (c * ylen + 1) =
      let xs1 = take xlen1 xs
          ys1 = rotate1 xlen1 ys xs
       in Deque xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | ylen > (c * xlen + 1) =
      let xs1 = rotate1 ylen1 xs ys
          ys1 = take ylen1 ys
       in Deque xs1 xlen1 (schedule xs1) ys1 ylen1 (schedule ys1)
  | otherwise = Deque xs xlen xc ys ylen yc
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
empty :: Deque a
empty =
  Deque [] 0 [] [] 0 []

-- | \(\mathcal{O}(1)\). Push an element onto the back of a deque, to be popped last.
push :: a -> Deque a -> Deque a
push y (Deque xs xlen xc ys ylen yc) =
  deque xs xlen (execute1 xc) (y : ys) (ylen + 1) (execute1 yc)

-- | \(\mathcal{O}(1)\). Push an element onto the front of a deque, to be popped next.
pushFront :: a -> Deque a -> Deque a
pushFront x (Deque xs xlen xc ys ylen yc) =
  deque (x : xs) (xlen + 1) (execute1 xc) ys ylen (execute1 yc)

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a deque.
pop :: Deque a -> Maybe (a, Deque a)
pop = \case
  Deque [] _ _ [] _ _ -> Nothing
  Deque [] _ _ (y : _) _ _ -> Just (y, empty)
  Deque (x : xs) xlen xc ys ylen yc -> Just (x, deque xs (xlen - 1) (execute2 xc) ys ylen (execute2 yc))

-- | \(\mathcal{O}(1)\). Pop an element off of the back of a deque.
popBack :: Deque a -> Maybe (Deque a, a)
popBack = \case
  Deque [] _ _ [] _ _ -> Nothing
  Deque (x : _) _ _ [] _ _ -> Just (empty, x)
  Deque xs xlen xc (y : ys) ylen yc -> Just (deque xs xlen (execute2 xc) ys (ylen - 1) (execute2 yc), y)

-- | \(\mathcal{O}(1)\). Is a deque empty?
isEmpty :: Deque a -> Bool
isEmpty (Deque _ xlen _ _ ylen _) =
  xlen == 0 && ylen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a deque?
length :: Deque a -> Int
length (Deque _ xlen _ _ ylen _) =
  xlen + ylen

-- | \(\mathcal{O}(1)\). Reverse a deque.
reverse :: Deque a -> Deque a
reverse (Deque xs xlen xc ys ylen yc) =
  Deque ys ylen yc xs xlen xc

-- @append xs ys@ pushes @ys@ onto the back of @ys@.
append :: Deque a -> Deque a -> Deque a
append xs Empty = xs
append xs (Front y ys) = append (push y xs) ys

-- @prepend xs ys@ pushes @xs@ onto the front of @ys@.
prepend :: Deque a -> Deque a -> Deque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (pushFront x ys)

-- | \(\mathcal{O}(n)\). Construct a deque from a list, where the head of the list corresponds to the front of the
-- deque.
fromList :: [a] -> Deque a
fromList =
  foldr pushFront empty

-- | \(\mathcal{O}(n)\). Construct a list from a deque, where the head of the list corresponds to the front of the
-- deque.
toList :: Deque a -> [a]
toList =
  List.unfoldr pop

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
