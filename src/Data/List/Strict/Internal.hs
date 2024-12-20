{-# LANGUAGE BangPatterns #-}

module Data.List.Strict.Internal
  ( List' (..)

  , Data.List.Strict.Internal.length

  , append
  , revAppend

  , Data.List.Strict.Internal.map
  , map'

  , Data.List.Strict.Internal.foldl
  , Data.List.Strict.Internal.foldl'

  , Data.List.Strict.Internal.foldr
  , Data.List.Strict.Internal.foldr'

  , Data.List.Strict.Internal.foldMap

  , fromList
  , fromRevList
  , appendToList
  , revAppendToList

  , Data.List.Strict.Internal.traverse
  ) where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Data.Foldable
import           Data.Functor.Classes



-- | Spine-strict list.
data List' a = Nil'
             | Cons' a !(List' a)

instance Show a => Show (List' a) where
  showsPrec d = showsPrec d . (\xs -> appendToList xs [])

instance Show1 List' where
  liftShowsPrec _showsPrec _showsList _ =
    _showsList . (\xs -> appendToList xs [])


instance Eq a => Eq (List' a) where
  (==) = liftEq (==)

instance Eq1 List' where
  liftEq eq = go
    where
      go (Cons' x xs) (Cons' y ys) = eq x y && go xs ys
      go Nil'         Nil'         = True
      go _            _            = False

instance NFData a => NFData (List' a) where
  rnf = liftRnf rnf

instance NFData1 List' where
  liftRnf nf = go
    where
      go (Cons' a as) = nf a `seq` go as
      go Nil'         = ()


instance Functor List' where
  fmap = Data.List.Strict.Internal.map

instance Foldable List' where
  foldl = Data.List.Strict.Internal.foldl
  foldl' = Data.List.Strict.Internal.foldl'

  foldr = Data.List.Strict.Internal.foldr
  foldr' = Data.List.Strict.Internal.foldr'

  foldMap = Data.List.Strict.Internal.foldMap

  length = Data.List.Strict.Internal.length

  null Nil' = True
  null _    = False

instance Traversable List' where
  traverse = Data.List.Strict.Internal.traverse



length :: List' a -> Int
length (Cons' _ bs) = let !n = Data.List.Strict.Internal.length bs in n + 1
length  Nil'        = 0



append :: List' a -> List' a -> List' a
append (Cons' a bs) cs = Cons' a (append bs cs)
append  Nil'        cs = cs

revAppend :: List' a -> List' a -> List' a
revAppend (Cons' a as) bs = revAppend as (Cons' a bs)
revAppend  Nil'        bs = bs



map :: (a -> b) -> List' a -> List' b
map f = go
  where
    go  Nil'        = Nil'
    go (Cons' a bs) = Cons' (f a) (go bs)

map' :: (a -> b) -> List' a -> List' b
map' f = go
  where
    go  Nil'        = Nil'
    go (Cons' a bs) = let !b = f a
                      in Cons' b (go bs)



foldl :: (b -> a -> b) -> b -> List' a -> b
foldl f = go
  where
    go z0 Nil'         = z0
    go z0 (Cons' a bs) = go (f z0 a) bs

foldl' :: (b -> a -> b) -> b -> List' a -> b
foldl' f = go
  where
    go !z0 Nil'         = z0
    go  z0 (Cons' a bs) = go (f z0 a) bs



foldr :: (a -> b -> b) -> b -> List' a -> b
foldr f = \z ->
  let go Nil'         = z
      go (Cons' a bs) = f a $ go bs

  in go

foldr' :: (a -> b -> b) -> b -> List' a -> b
foldr' f = \ !z ->
  let go Nil'         = z
      go (Cons' a bs) = f a $! go bs

  in go



foldMap :: Monoid m => (a -> m) -> List' a -> m
foldMap f = go
  where
    go (Cons' a bs) = f a <> go bs
    go  Nil'        = mempty



fromList :: [a] -> List' a
fromList (a:bs) = Cons' a (fromList bs)
fromList    []  = Nil'

fromRevList :: [a] -> List' a
fromRevList = go Nil'
  where
    go rs (a:bs) = go (Cons' a rs) bs
    go rs    []  = rs



appendToList :: List' a -> [a] -> [a]
appendToList xs ys = go xs
  where
    go (Cons' a bs) = a : go bs
    go  Nil'        = ys

revAppendToList :: List' a -> [a] -> [a]
revAppendToList (Cons' x xx) ys = revAppendToList xx (x:ys)
revAppendToList  Nil'        ys = ys



traverse :: Applicative f => (a -> f b) -> List' a -> f (List' b)
traverse f = go
  where
    go (Cons' a bs) = liftA2 Cons' (f a) (go bs)
    go  Nil'        = pure Nil'
