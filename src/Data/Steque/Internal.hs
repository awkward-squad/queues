{-# LANGUAGE BangPatterns
           , PatternSynonyms
           , ViewPatterns #-}

module Data.Steque.Internal
  ( Steque (Empty, (:<|), ..)

  , empty
  , singleton
  , fromListF

  , (<|)
  , (|>)
  , ViewF (..)
  , viewF

  , Data.Steque.Internal.null
  , Data.Steque.Internal.size

  , Data.Steque.Internal.reverse

  , Data.Steque.Internal.map
  , map'

  , Data.Steque.Internal.foldl
  , Data.Steque.Internal.foldl'

  , Data.Steque.Internal.foldr
  , Data.Steque.Internal.foldr'

  , Data.Steque.Internal.foldMap

  , Data.Steque.Internal.traverse

  , toList
  , toListF
  , toListB
  ) where

import           Data.List.Strict.Internal (List' (..))
import qualified Data.List.Strict.Internal as List'

import           Control.DeepSeq
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Text.Show



-- | Spine-strict stack-ended queue.
data Steque a =
       Steque
         !(List' a) -- ^ Front of the queue, in normal order.
         !(List' a) -- ^ Back of the queue, in reverse order.

{-# COMPLETE Empty, (:<|) #-}

-- | \(\mathcal{O}(1)\). Bidirectional pattern synonym matching an empty queue.
pattern Empty :: Steque a
pattern Empty <- (Data.Steque.Internal.null -> True)
  where
    Empty = empty

infixr 5 :<|
-- | \(\mathcal{O}(1)\) construction and amortized match,
--   \(\mathcal{O}(n)\) worst-case match.
--   Bidirectional pattern synonym analyzing the front of a non-empty queue.
pattern (:<|) :: a -> Steque a -> Steque a
pattern x :<| q <- (viewF -> Just (x :< q))
  where
    (:<|) = (<|)


-- | Uses 'toListF'.
instance Show a => Show (Steque a) where
  showsPrec = liftShowsPrec showsPrec showList

-- | Uses 'toListF'.
instance Show1 Steque where
  liftShowsPrec _shows _ _ = showListWith (_shows 0) . toListF


instance NFData a => NFData (Steque a) where
  rnf = liftRnf rnf

instance NFData1 Steque where
  liftRnf nf (Steque xs ys) = liftRnf nf xs `seq` liftRnf nf ys


instance Functor Steque where
  fmap = Data.Steque.Internal.map

-- | The order of entries is arbitrary.
instance Foldable Steque where
  foldl  = Data.Steque.Internal.foldl
  foldl' = Data.Steque.Internal.foldl

  foldr  = Data.Steque.Internal.foldr
  foldr' = Data.Steque.Internal.foldr'

  foldMap = Data.Steque.Internal.foldMap

  null = Data.Steque.Internal.null

  toList = Data.Steque.Internal.toList

  length = Data.Steque.Internal.size

instance Traversable Steque where
  traverse = Data.Steque.Internal.traverse



-- | \(\mathcal{O}(1)\). Empty queue.
empty :: Steque a
empty = Steque Nil' Nil'

-- | \(\mathcal{O}(1)\). Queue with a single element.
singleton :: a -> Steque a
singleton x = Steque (Cons' x Nil') Nil'

-- | \(\mathcal{O}(n)\). Construct a queue from a list.
--   Element order matches list order, with
--   the front of the queue corresponding to the head of the list.
fromListF :: [a] -> Steque a
fromListF xs = Steque (List'.fromList xs) Nil'



infixr 5 <|
-- | \(\mathcal{O}(1)\). Add an element to the front of the queue.
(<|) :: a -> Steque a -> Steque a
(<|) x (Steque xs ys) = Steque (Cons' x xs) ys

infixl 5 |>
-- | \(\mathcal{O}(1)\). Add an element to the back of the queue.
(|>) :: Steque a -> a -> Steque a
(|>) (Steque xs ys) y = Steque xs (Cons' y ys)



-- | View of the front of the queue.
data ViewF a = a :< !(Steque a)
               deriving Show

{-# INLINE viewF #-}
-- | \(\mathcal{O}(1)\) amortized, \(\mathcal{O}(n)\) worst-case.
--   Analyze the front of the queue.
viewF :: Steque a -> Maybe (ViewF a)
viewF (Steque xs ys) =
  case xs of
    Cons' x xx -> Just $! x :< Steque xx ys
    Nil'       ->
      case ys of
        Nil'       -> Nothing
        Cons' y yy -> Just $! viewF1 y yy

viewF1 :: a -> List' a -> ViewF a
viewF1 = revAppend1 Nil'
  where
    revAppend1 rs v  Nil'        = v :< Steque rs Nil'
    revAppend1 rs v (Cons' u us) = revAppend1 (Cons' v rs) u us



-- | \(\mathcal{O}(1)\). Check if the queue is empty.
null :: Steque a -> Bool
null (Steque Nil' Nil') = True
null _                  = False



-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the queue.
--   The returned number is guaranteed to be non-negative.
size :: Steque a -> Int
size (Steque xs ys) = List'.length xs + List'.length ys



-- | \(\mathcal{O}(1)\). Reverse the element order.
--
--   Amortized time complexity for 'viewF' when used on a reversed queue is no longer
--   \(\mathcal{O}(1)\), deteriorating to \(\mathcal{O}(n)\) in the extreme case of
--   using the steque as a double-ended queue.
reverse :: Steque a -> Steque a
reverse (Steque xs ys) = Steque ys xs



-- | \(\mathcal{O}(n)\). Apply a function to every element in the queue.
map :: (a -> b) -> Steque a -> Steque b
map f (Steque xs ys) = Steque (List'.map f xs) (List'.map f ys)

-- | \(\mathcal{O}(n)\). Apply a function to every element in the queue and
--   evaluate every resulting element to weak head normal form.
map' :: (a -> b) -> Steque a -> Steque b
map' f (Steque xs ys) = Steque (List'.map' f xs) (List'.map' f ys)



-- | \(\mathcal{O}(n_R)\). Fold the queue left-to-right.
foldl :: (b -> a -> b) -> b -> Steque a -> b
foldl f = \z (Steque xs ys) -> List'.foldl f (List'.foldl f z xs) ys

-- | \(\mathcal{O}(n)\). Fold the queue left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> Steque a -> b
foldl' f = \ !z (Steque xs ys) -> List'.foldl' f (List'.foldl' f z xs) ys

-- | \(\mathcal{O}(n_L)\). Fold the queue right-to-left.
foldr :: (a -> b -> b) -> b -> Steque a -> b
foldr f = \z (Steque xs ys) -> List'.foldr f (List'.foldr f z ys) xs

-- | \(\mathcal{O}(n)\). Fold the queue right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Steque a -> b
foldr' f = \ !z (Steque xs ys) -> List'.foldr' f (List'.foldr' f z ys) xs

-- | \(\mathcal{O}(n_M)\). Map each element of the queue into a monoid,
--   and combine the results.
foldMap :: Monoid m => (a -> m) -> Steque a -> m
foldMap f (Steque xs ys) = List'.foldMap f xs <> List'.foldMap f ys



-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue. Element order is arbitrary.
toList :: Steque a -> [a]
toList (Steque xs ys) = xs `List'.appendToList` List'.appendToList ys []

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the front of the queue.
toListF :: Steque a -> [a]
toListF (Steque xs ys) = xs `List'.appendToList` List'.revAppendToList ys []

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the back of the queue.
toListB :: Steque a -> [a]
toListB (Steque xs ys) = ys `List'.appendToList` List'.revAppendToList xs []



-- | \(\mathcal{O}(n)\). Map each element of a queue to an action,
--   evaluate these actions from front to back, and collect the results.
traverse :: Applicative f => (a -> f b) -> Steque a -> f (Steque b)
traverse f (Steque xs ys) = liftA2 Steque (List'.traverse f xs) (List'.traverse f ys)
