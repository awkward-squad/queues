module QueuesPrelude
  ( listFoldMapBackwards,
  )
where

listFoldMapBackwards :: (Monoid m) => (a -> m) -> [a] -> m
listFoldMapBackwards f =
  go mempty
  where
    go acc = \case
      [] -> acc
      z : zs -> go (f z <> acc) zs
