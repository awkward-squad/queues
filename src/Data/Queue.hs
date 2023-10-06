module Data.Queue
  ( Queue,
    empty,
    push,
    pop,
  )
where

data Queue a
  = Queue [a] [a] [a]
  deriving stock (Functor)

queue :: [a] -> [a] -> [a] -> Queue a
queue xs ys = \case
  [] -> let xs1 = rotate [] ys xs in Queue xs1 [] xs1
  _ : zs -> Queue xs ys zs

empty :: Queue a
empty =
  Queue [] [] []

rotate :: [a] -> NonEmptyList a -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

push :: a -> Queue a -> Queue a
push x (Queue xs ys zs) =
  queue xs (x : ys) zs

pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, queue xs ys zs)

type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}
