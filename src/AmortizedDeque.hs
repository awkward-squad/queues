module AmortizedDeque
  ( -- * Deque
    AmortizedDeque,
  )
where

import Prelude hiding (foldMap, length, map, span, traverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)\) amortized enqueue and dequeue.
data AmortizedDeque a
  = Q
      [a]
      {-# UNPACK #-} !Int
      [a]
      {-# UNPACK #-} !Int
  deriving stock (Functor)
