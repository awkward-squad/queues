{-# LANGUAGE BangPatterns
           , PatternSynonyms
           , ViewPatterns #-}

module Data.Queue.Internal
  ( Queue (Empty, (:<|), ..)

  , MalformedQueue (..)

  , (|>)
  , ViewF (..)
  , viewF

  , (<|)

  , empty
  , singleton
  , fromListF

  , Data.Queue.Internal.null
  , Data.Queue.Internal.size

  , Data.Queue.Internal.foldl
  , Data.Queue.Internal.foldl'

  , Data.Queue.Internal.foldr
  , Data.Queue.Internal.foldr'

  , Data.Queue.Internal.foldMap

  , Data.Queue.Internal.toList
  , toListF
  , toListB
  ) where

import           Data.List.Strict.Internal (List' (..))
import qualified Data.List.Strict.Internal as List'

import           Control.DeepSeq
import           Control.Exception
import qualified Data.Foldable    as Foldable
import           Data.Functor.Classes
import qualified Data.List        as List
import           Text.Show



-- | Spine-strict queue.
data Queue a =
       -- | Invariants: \(|S| = |F| - |R|\).
       Queue
         ![a]       -- ^ \(F\). Front of the queue, in normal order.
         !(List' a) -- ^ \(R\). Rear of the queue, in reverse order.
         [a]        -- ^ \(S\). Schedule: reference to some point in \(F\).

{-# COMPLETE (:<|), Empty #-}

-- | \(\mathcal{O}(1)\). Bidirectional pattern matching an empty queue.
pattern Empty :: Queue a
pattern Empty <- (Data.Queue.Internal.null -> True)
  where
    Empty = empty

infixr 5 :<|
-- | \(\mathcal{O}(1)\).
--   Unidirectional pattern analyzing the front of a non-empty queue.
pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| q <- (viewF -> Just (x :< q))


-- | Uses 'toListF'.
instance Show a => Show (Queue a) where
  showsPrec = liftShowsPrec showsPrec showList

-- | Uses 'toListF'.
instance Show1 Queue where
  liftShowsPrec _shows _ _ = showListWith (_shows 0) . toListF


instance NFData a => NFData (Queue a) where
  rnf = liftRnf rnf

instance NFData1 Queue where
  liftRnf nf (Queue fs rs _) = liftRnf nf fs `seq` liftRnf nf rs


-- | The order of entries is arbitrary.
--
--   Folding over the queue implies full evaluation of the
--   suspended part of the queue.
instance Foldable Queue where
  foldl  = Data.Queue.Internal.foldl
  foldl' = Data.Queue.Internal.foldl'

  foldr  = Data.Queue.Internal.foldr
  foldr' = Data.Queue.Internal.foldr'

  foldMap = Data.Queue.Internal.foldMap

  null = Data.Queue.Internal.null

  toList = Data.Queue.Internal.toList

  length = Data.Queue.Internal.size



-- | Exception thrown if the internal length invariant is broken, i.e. \(|F| \lt |R|\).
data MalformedQueue = MalformedQueue
                      deriving Show

instance Exception MalformedQueue

unsafeRotate :: [a] -> List' a -> [a] -> [a]
unsafeRotate !fs rs !sd =
  case rs of
    Cons' r rr -> rotate1 fs r rr sd
    Nil'       -> throw MalformedQueue

rotate1 :: [a] -> a -> List' a -> [a] -> [a]
rotate1 fs r !rr !sd =
  case fs of
    []   -> r : sd
    f:ff -> f : unsafeRotate ff rr (r:sd)



infixl 5 |>
-- | \(\mathcal{O}(1)\). Add an element to the back of the queue.
(|>) :: Queue a -> a -> Queue a
Queue fs rs sd |> x =
  case sd of
    _:ss -> Queue fs (Cons' x rs) ss
    []   ->
      let fs' = rotate1 fs x rs []
      in Queue fs' Nil' fs'


-- | View of the front of the queue.
data ViewF a = a :< !(Queue a)
               deriving Show

{-# INLINEABLE viewF #-}
-- | \(\mathcal{O}(1)\). View the front of the queue.
viewF :: Queue a -> Maybe (ViewF a)
viewF (Queue fs rs sd) =
  case fs of
    []   -> Nothing
    f:ff -> Just $! f :< viewF1 ff rs sd

viewF1 :: [a] -> List' a -> [a] -> Queue a
viewF1 ff rs sd =
  case sd of
    _:ss -> Queue ff rs ss
    []   ->
      let fs' = unsafeRotate ff rs []
      in Queue fs' Nil' fs'



infixr 5 <|
-- | \(\mathcal{O}(1)\). Add an element to the front of the queue.
--
--   Makes \(S\) diverge from \(F\) to preserve the invariant, doubling
--   allocation overhead for this element.
(<|) :: a -> Queue a -> Queue a
x <| Queue fs rs sd = Queue (x:fs) rs (x:sd)



-- | \(\mathcal{O}(1)\). Empty queue.
empty :: Queue a
empty = Queue [] Nil' []

-- | \(\mathcal{O}(1)\). Queue with a single element.
singleton :: a -> Queue a
singleton x =
  let fs = [x]
  in Queue fs Nil' fs

-- | \(\mathcal{O}(1)\). Construct a queue from a list.
--   Element order matches list order, with
--   the front of the queue corresponding to the head of the list.
fromListF :: [a] -> Queue a
fromListF fs = Queue fs Nil' fs



-- | \(\mathcal{O}(1)\). Check if the queue is empty.
null :: Queue a -> Bool
null (Queue [] _ _) = True
null _              = False

-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the queue.
--   The returned number is guaranteed to be non-negative.
size :: Queue a -> Int
size (Queue fs rs _) = List.length fs + List'.length rs



-- | \(\mathcal{O}(n_R)\). Fold the queue left-to-right.
foldl :: (b -> a -> b) -> b -> Queue a -> b
foldl f = \z (Queue fs rs _) -> List'.foldl f (Foldable.foldl f z fs) rs

-- | \(\mathcal{O}(n)\). Fold the queue left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> Queue a -> b
foldl' f = \ !z (Queue fs rs _) -> List'.foldl' f (Foldable.foldl' f z fs) rs

-- | \(\mathcal{O}(n_L)\). Fold the queue right-to-left.
foldr :: (a -> b -> b) -> b -> Queue a -> b
foldr f = \z (Queue fs rs _) -> Foldable.foldr f (List'.foldr f z rs) fs

-- | \(\mathcal{O}(n)\). Fold the queue right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Queue a -> b
foldr' f = \ !z (Queue fs rs _) -> Foldable.foldr' f (List'.foldr' f z rs) fs



-- | \(\mathcal{O}(n_M)\). Map each element of the queue into a monoid,
--   and combine the results.
foldMap :: Monoid m => (a -> m) -> Queue a -> m
foldMap f (Queue fs rs _) = Foldable.foldMap f fs <> List'.foldMap f rs


-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue. Element order is arbitrary.
toList :: Queue a -> [a]
toList (Queue fs rs _) = fs List.++ List'.appendToList rs []

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the front of the queue.
toListF :: Queue a -> [a]
toListF (Queue fs rs _) = fs List.++ List'.revAppendToList rs []

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the back of the queue.
toListB :: Queue a -> [a]
toListB (Queue fs rs _) = rs `List'.appendToList` List.reverse fs
