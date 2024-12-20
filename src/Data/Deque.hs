{-| A t'Deque' is a spine-strict double-ended queue that incrementally rebalances
    itself when elements are added or removed.

    == Laziness

    Evaluating the root of the queue (i.e. @(_ :: t'Deque' a)@) to weak head normal form
    evaluates equal extents of the back and the front of the queue to normal form,
    while the remaining parts of both sides are represented as at most two thunks.

    Functions do not perform any additional evaluations unless their documentation
    directly specifies so.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of elements in the queue.
    Parts of the heap are denoted using subscripts:
    \(n_L\) refers to the left side, \(n_R\) to the right side,
    and \(n_M\) refers to the total number of elements collected with the 'Monoid'.

    == Implementation

    The implementation is specialized for \(c = 2\).

    Description of the real-time double-ended queue can be found within the
    following paper:

    - Chris Okasaki, /"Purely Functional Data Structures"/, September 1996, pages 57-59,
      https://www.cs.cmu.edu/~rwh/students/okasaki.pdf
 -}

module Data.Deque
  ( -- * Itself
    Deque (Empty, (:<|), (:|>))

    -- * Construct
  , empty
  , singleton
  , fromListF
  , fromListB

    -- * Front
    -- ** Insert
  , (<|)

    -- ** View
  , ViewF (..)
  , viewF

    -- * Back
    -- ** Insert
  , (|>)

    -- ** View
  , ViewB (..)
  , viewB

    -- * Full queue
    -- ** Size
  , Data.Deque.Internal.null
  , Data.Deque.Internal.size

    -- ** Reverse
  , Data.Deque.Internal.reverse

    -- ** Fold
    -- | === Left-to-right
  , Data.Deque.Internal.foldl
  , Data.Deque.Internal.foldl'

    -- | === Right-to-left
  , Data.Deque.Internal.foldr
  , Data.Deque.Internal.foldr'

    -- | === Monoid
  , Data.Deque.Internal.foldMap

    -- | === List
  , toList
  , toListF
  , toListB
  ) where

import           Data.Deque.Internal
