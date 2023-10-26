module NonEmptyList
  ( NonEmptyList,
    pattern NonEmptyList,
  )
where

-- A list that we know is non-empty somehow.
type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}
