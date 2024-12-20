{-| A t'Queue' is a spine-strict queue that incrementally rotates itself when
    elements are added or removed.

    == Laziness

    Evaluating the root of the queue (i.e. @(_ :: t'Queue' a)@) to weak head normal form
    evaluates both the entire back of the queue and the extent of the front the queue
    equal to the size of the back to normal form, while the remainder of
    the front of the queue is represented as at most one thunk.

    Functions do not perform any additional evaluations unless their documentation
    directly specifies so.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of elements in the queue.
    Parts of the heap are denoted using subscripts:
    \(n_L\) refers to the left side, \(n_R\) to the right side,
    and \(n_M\) refers to the total number of elements collected with the 'Monoid'.

    == Implementation

    Description of the real-time queue can be found within the following paper:

    - Chris Okasaki, /"Purely Functional Data Structures"/, September 1996, pages 41-44,
      https://www.cs.cmu.edu/~rwh/students/okasaki.pdf
 -}

module Data.Queue
  ( -- * Itself
    Queue (Empty, (:<|))

    -- * Construct
  , empty
  , singleton
  , fromListF

    -- * Front
    -- ** View
  , ViewF (..)
  , viewF

    -- * Back
    -- ** Insert
  , (|>)

    -- * Full queue
    -- ** Size
  , Data.Queue.Internal.null
  , Data.Queue.Internal.size

    -- ** Fold
    -- | === Left-to-right
  , Data.Queue.Internal.foldl
  , Data.Queue.Internal.foldl'

    -- | === Right-to-left
  , Data.Queue.Internal.foldr
  , Data.Queue.Internal.foldr'

    -- | === Monoid
  , Data.Queue.Internal.foldMap

    -- | === List
  , toList
  , toListF
  , toListB
  ) where

import           Data.Queue.Internal
