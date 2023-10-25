-- | A double-ended queue data structure with \(\mathcal{O}(1)\) worst-case push and pop, as described in
--
--   * Okasaki, Chris. "Simple and efficient purely functional queues and deques." /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
module Data.Deque
  ( -- * Deque
    Deque (Empty, Pop, PopBack),

    -- ** Initialization
    empty,

    -- * Basic interface
    push,
    pushFront,
    pop,
    popBack,

    -- * Queries
    isEmpty,

    -- * Transformations
    map,
    reverse,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Bits (unsafeShiftR)
import qualified Data.List as List
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (map, reverse, traverse)

-- | A deque.
data Deque a
  = Deque
      [a]
      {-# UNPACK #-} !Int
      [Any]
      [a]
      {-# UNPACK #-} !Int
      [Any]
  deriving stock (Functor)

instance (Show a) => Show (Deque a) where
  show = show . toList

pattern Empty :: Deque a
pattern Empty <- (pop -> Nothing)

pattern Pop :: a -> Deque a -> Deque a
pattern Pop x xs <- (pop -> Just (x, xs))

pattern PopBack :: Deque a -> a -> Deque a
pattern PopBack xs x <- (popBack -> Just (xs, x))

{-# COMPLETE Empty, Pop #-}

{-# COMPLETE Empty, PopBack #-}

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

schedule :: [a] -> [Any]
schedule =
  unsafeCoerce

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
  deque xs xlen (drop1 xc) (y : ys) (ylen + 1) (drop1 yc)

-- | \(\mathcal{O}(1)\). Push an element onto the front of a deque, to be popped next.
pushFront :: a -> Deque a -> Deque a
pushFront x (Deque xs xlen xc ys ylen yc) =
  deque (x : xs) (xlen + 1) (drop1 xc) ys ylen (drop1 yc)

-- | \(\mathcal{O}(1)\). Pop an element off of the front of a deque.
pop :: Deque a -> Maybe (a, Deque a)
pop = \case
  Deque [] _ _ [] _ _ -> Nothing
  Deque [] _ _ (y : _) _ _ -> Just (y, empty)
  Deque (x : xs) xlen xc ys ylen yc -> Just (x, deque xs (xlen - 1) (drop2 xc) ys ylen (drop2 yc))

-- | \(\mathcal{O}(1)\). Pop an element off of the back of a deque.
popBack :: Deque a -> Maybe (Deque a, a)
popBack = \case
  Deque [] _ _ [] _ _ -> Nothing
  Deque (x : _) _ _ [] _ _ -> Just (empty, x)
  Deque xs xlen xc (y : ys) ylen yc -> Just (deque xs xlen (drop2 xc) ys (ylen - 1) (drop2 yc), y)

-- | \(\mathcal{O}(1)\). Is a deque empty?
isEmpty :: Deque a -> Bool
isEmpty (Deque _ xlen _ _ ylen _) =
  xlen == 0 && ylen == 0

-- | \(\mathcal{O}(n)\). Apply a function to each element in a deque.
map :: (a -> b) -> Deque a -> Deque b
map =
  fmap

-- | \(\mathcal{O}(1)\). Reverse a deque.
reverse :: Deque a -> Deque a
reverse (Deque xs xlen xc ys ylen yc) =
  Deque ys ylen yc xs xlen xc

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
-- List utils

drop1 :: [a] -> [a]
drop1 = \case
  [] -> []
  _ : xs -> xs

drop2 :: [a] -> [a]
drop2 =
  drop1 . drop1
