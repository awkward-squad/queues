module QueuesPrelude
  ( NonEmptyList,
    pattern NonEmptyList,
    listFoldMapBackwards,
  )
where

-- A list that we know is non-empty somehow.
type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}

listFoldMapBackwards :: (Monoid m) => (a -> m) -> [a] -> m
listFoldMapBackwards f =
  go mempty
  where
    go acc = \case
      [] -> acc
      z : zs -> go (f z <> acc) zs
