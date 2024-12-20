{-# LANGUAGE BangPatterns
           , PatternSynonyms
           , UnboxedTuples
           , ViewPatterns #-}

module Data.Deque.Internal
  ( Deque (Empty, (:<|), (:|>), ..)

  , MalformedDeque (..)

  , (|>)
  , (<|)
  , ViewF (..)
  , viewF
  , ViewB (..)
  , viewB

  , empty
  , singleton
  , fromListF
  , fromListB

  , Data.Deque.Internal.null
  , Data.Deque.Internal.size

  , Data.Deque.Internal.reverse

  , Data.Deque.Internal.foldl
  , Data.Deque.Internal.foldl'

  , Data.Deque.Internal.foldr
  , Data.Deque.Internal.foldr'

  , Data.Deque.Internal.foldMap
  , toList
  , toListF
  , toListB
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Data.Bits
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import qualified Data.List as List
import           Text.Show



-- | Spine-strict double-ended queue.
data Deque a =
       -- | Invariants: \(|F| \le 2 |R| + 1\), \(|R| \le 2 |F| + 1\).
       Deque
         ![a]                -- ^ \(F\). Front of the queue, in normal order.
         {-# UNPACK #-} !Int -- ^ \(|F|\). Length of the front of the queue.
         [a]                 -- ^ \(S_F\). Front schedule: reference to some point in \(F\).
         [a]                 -- ^ \(S_R\). Back schedule: reference to some point in \(R\).
         {-# UNPACK #-} !Int -- ^ \(|R|\). Length of the back of the queue.
         ![a]                -- ^ \(R\). Back of the queue, in reverse order.

{-# COMPLETE (:<|), Empty #-}
{-# COMPLETE (:|>), Empty #-}

-- | \(\mathcal{O}(1)\). Bidirectional pattern matching an empty queue.
pattern Empty :: Deque a
pattern Empty <- (Data.Deque.Internal.null -> True)
  where
    Empty = empty

infixr 5 :<|
-- | \(\mathcal{O}(1)\). Bidirectional pattern analyzing the front of a non-empty queue.
pattern (:<|) :: a -> Deque a -> Deque a
pattern x :<| q <- (viewF -> Just (x :< q))
  where
    (:<|) = (<|)

infixl 5 :|>
-- | \(\mathcal{O}(1)\). Bidirectional pattern analyzing the back of a non-empty queue.
pattern (:|>) :: Deque a -> a -> Deque a
pattern q :|> x <- (viewB -> Just (q :> x))
  where
    (:|>) = (|>)


-- | Uses 'toListF'.
instance Show a => Show (Deque a) where
  showsPrec _ = showList . toListF

-- | Uses 'toListF'.
instance Show1 Deque where
  liftShowsPrec _shows _ _ = showListWith (_shows 0) . toListF


instance NFData a => NFData (Deque a) where
  rnf = liftRnf rnf

instance NFData1 Deque where
  liftRnf nf (Deque fs _ _ _ _ rs) = liftRnf nf fs `seq` liftRnf nf rs


-- | The order of entries is arbitrary.
--
--   Folding over the queue implies full evaluation of the
--   suspended parts of the queue.
instance Foldable Deque where
  foldl  = Data.Deque.Internal.foldl
  foldl' = Data.Deque.Internal.foldl'

  foldr  = Data.Deque.Internal.foldr
  foldr' = Data.Deque.Internal.foldr'

  foldMap = Data.Deque.Internal.foldMap

  null = Data.Deque.Internal.null

  toList = Data.Deque.Internal.toList

  length = Data.Deque.Internal.size



rotateDrop :: Int -> [a] -> [a] -> [a]
rotateDrop i !fs !rs =
  if i < 2
    then rotateRev fs (drop i rs) []
    else
      case fs of
        f:ff -> let !(# rs' #) = drop2 rs
                in f : rotateDrop (i - 2) ff rs'

        []   -> throw MalformedDeque

rotateRev :: [a] -> [a] -> [a] -> [a]
rotateRev fs !rs !sd =
  case fs of
    []   -> revAppend rs sd
    f:ff -> f : case rs of
                  r0:r1:rr -> rotateRev ff rr (r1:r0:sd)
                  _        -> revAppend rs $ case rs of
                                               [r] -> r:sd
                                               []  -> sd

revAppend :: [a] -> [a] -> [a]
revAppend (a:as) bs = revAppend as (a:bs)
revAppend    []  bs = bs

drop2 :: [a] -> (# [a] #)
drop2 (_:_:as) = (# as #)
drop2      _   = (# [] #)

drop1 :: [a] -> (# [a] #)
drop1 (_:as) = (# as #)
drop1    []  = (# [] #)



-- | Exception thrown if the internal length invariant is broken,
--   i.e. \(|F| \gt 2 |R| + 1\) or \(|R| \gt 2 |F| + 1\)
data MalformedDeque = MalformedDeque
                      deriving Show

instance Exception MalformedDeque



infixr 5 <|
-- | \(\mathcal{O}(1)\). Add an element to the front of the queue.
(<|) :: a -> Deque a -> Deque a
x <| Deque fs nf sf sr nr rs =
  let !(# fs', nf', sf', sr', nr', rs' #) = insert_ x fs nf sf sr nr rs
  in Deque fs' nf' sf' sr' nr' rs'

infixl 5 |>
-- | \(\mathcal{O}(1)\). Add an element to the back of the queue.
(|>) :: Deque a -> a -> Deque a
Deque fs nf sf sr nr rs |> x =
  let !(# rs', nr', sr', sf', nf', fs' #) = insert_ x rs nr sr sf nf fs
  in Deque fs' nf' sf' sr' nr' rs'

insert_
  :: a
  -> [a] -> Int -> [a] -> [a] -> Int -> [a]
  -> (# [a], Int, [a], [a], Int, [a] #)
insert_ x fs _nf _sf _sr nr rs =
  let !ff = x : fs
      nf = _nf + 1
      !(# sf #) = drop1 _sf
      !(# sr #) = drop1 _sr

  in if nf > unsafeShiftL nr 1 + 1
       then
         let ll = nf + nr
             !i = ll `unsafeShiftR` 1
             !j = ll - i

             !fs' = take j ff
             !rs' = rotateDrop j rs ff

         in (# fs', j, fs', rs', i, rs' #)

       else (# ff, nf, sf, sr, nr, rs #)



-- | View of the front of the queue.
data ViewF a = a :< !(Deque a)
               deriving Show

{-# INLINE viewF #-}
-- | \(\mathcal{O}(1)\). Analyze the front of the queue.
viewF :: Deque a -> Maybe (ViewF a)
viewF (Deque fs nf sf sr nr rs) =
  case fs of
    []   ->
      case rs of
        []  -> Nothing
        r:_ -> Just $! r :< empty

    f:ff -> Just $! f :< viewF1 ff nf sf sr nr rs

{-# NOINLINE viewF1 #-}
viewF1 :: [a] -> Int -> [a] -> [a] -> Int -> [a] -> Deque a
viewF1 ff nf sf sr nr rs =
  let !(# fs', nf', sf', sr', nr', rs' #) = view_ ff nf sf sr nr rs
  in Deque fs' nf' sf' sr' nr' rs'


-- | View of the back of the queue.
data ViewB a = !(Deque a) :> a
               deriving Show

{-# INLINE viewB #-}
-- | \(\mathcal{O}(1)\). Analyze the back of the queue.
viewB :: Deque a -> Maybe (ViewB a)
viewB (Deque fs nf sf sr nr rs) =
  case rs of
    []   ->
      case fs of
        []  -> Nothing
        f:_ -> Just $! empty :> f

    r:rr -> Just $! viewB1 fs nf sf sr nr rr :> r

{-# NOINLINE viewB1 #-}
viewB1 :: [a] -> Int -> [a] -> [a] -> Int -> [a] -> Deque a
viewB1 fs nf sf sr nr rr =
  let !(# rs', nr', sr', sf', nf', fs' #) = view_ rr nr sr sf nf fs
  in Deque fs' nf' sf' sr' nr' rs'


view_ :: [a] -> Int -> [a] -> [a] -> Int -> [a] -> (# [a], Int, [a], [a], Int, [a] #)
view_ ff _nf _sf _sr nr rs =
  let nf = _nf - 1
      !(# sf #) = drop2 _sf
      !(# sr #) = drop2 _sr

  in if nr > unsafeShiftL nf 1 + 1
       then
         let ll = nf + nr
             !i = ll `unsafeShiftR` 1
             !j = ll - i

             !fs' = rotateDrop i ff rs
             !rs' = take i rs

         in (# fs', j, fs', rs', i, rs' #)

       else (# ff, nf, sf, sr, nr, rs #)



-- | \(\mathcal{O}(1)\). Empty queue.
empty :: Deque a
empty = Deque [] 0 [] [] 0 []

-- | \(\mathcal{O}(1)\). Queue with a single element.
singleton :: a -> Deque a
singleton x = Deque [x] 1 [] [] 0 []



-- | \(\mathcal{O}(n)\). Construct a queue from a list.
--   Element order matches list order, with the front of the queue
--   corresponding to the head of the list.
fromListF :: [a] -> Deque a
fromListF xs =
  let !(# fs, nf, nr, rs #) = fromListF_ xs
  in Deque fs nf [] [] nr rs

-- | \(\mathcal{O}(n)\). Construct a queue from a list.
--   Element order matches list order, with the front of the queue
--   corresponding to the tail of the list.
fromListB :: [a] -> Deque a
fromListB xs =
  let !(# fs, nf, nr, rs #) = fromListF_ xs
  in Deque rs nr [] [] nf fs

fromListF_ :: [a] -> (# [a], Int, Int, [a] #)
fromListF_ as = reduce 0 id as as
  where
    reduce !nf !fs xs !ys =
      case xs of
        []   -> (# [], 0, 0, [] #)
        x:xx ->
          case ys of
            _:_:yy -> reduce (nf + 1) (fs . (:) x) xx yy
            _      ->
              let !fs' = fs []

                  !nr = case ys of
                          [] -> nf
                          _  -> nf + 1

                  !rs = List.reverse xs

              in (# fs', nf, nr, rs #)



-- | \(\mathcal{O}(1)\). Check if the queue is empty.
null :: Deque a -> Bool
null (Deque [] _ _ _ _ []) = True
null _                     = False

-- | \(\mathcal{O}(1)\). Calculate the number of elements in the queue.
--    The returned number is guaranteed to be non-negative.
size :: Deque a -> Int
size (Deque _ nf _ _ nr _) = nf + nr



-- | \(\mathcal{O}(1)\). Reverse the element order.
reverse :: Deque a -> Deque a
reverse (Deque fs nf sf sr nr rs) = Deque rs nr sr sf nf fs



-- | \(\mathcal{O}(n_R)\). Fold the queue left-to-right.
foldl :: (b -> a -> b) -> b -> Deque a -> b
foldl f = \z (Deque fs _ _ _ _ rs) -> Foldable.foldl f (Foldable.foldl f z fs) rs

-- | \(\mathcal{O}(n)\). Fold the queue left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> Deque a -> b
foldl' f = \ !z (Deque fs _ _ _ _ rs) -> Foldable.foldl' f (Foldable.foldl' f z fs) rs

-- | \(\mathcal{O}(n_L)\). Fold the queue right-to-left.
foldr :: (a -> b -> b) -> b -> Deque a -> b
foldr f = \z (Deque fs _ _ _ _ rs) -> Foldable.foldr f (Foldable.foldr f z rs) fs

-- | \(\mathcal{O}(n)\). Fold the queue right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Deque a -> b
foldr' f = \ !z (Deque fs _ _ _ _ rs) -> Foldable.foldr' f (Foldable.foldr' f z rs) fs



-- | \(\mathcal{O}(n_M)\). Map each element of the queue into a monoid,
--   and combine the results with '(<>)'.
foldMap :: Monoid m => (a -> m) -> Deque a -> m
foldMap f (Deque fs _ _ _ _ rs) = Foldable.foldMap f fs <> Foldable.foldMap f rs



-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue. Element order is arbitrary.
toList :: Deque a -> [a]
toList (Deque fs _ _ _ _ rs) = fs List.++ rs

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the front of the queue.
toListF :: Deque a -> [a]
toListF (Deque fs _ _ _ _ rs) = fs List.++ List.reverse rs

-- | \(\mathcal{O}(n)\).
--   Construct a list from a queue.
--   Element order matches queue order, with the head of the list corresponding
--   to the back of the queue.
toListB :: Deque a -> [a]
toListB (Deque fs _ _ _ _ rs) = rs List.++ List.reverse fs
