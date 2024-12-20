{-| A t'Steque' is a spine-strict stack-ended queue that reverses its back in one go once
    the front runs dry.

    == Laziness

    Evaluating the root of the queue (i.e. @(_ :: t'Steque' a)@) to weak head normal form
    evaluates the entire spine of the queue to normal form.

    Functions do not perform any additional evaluations unless their documentation
    directly specifies so.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of elements in the queue.
    Parts of the heap are denoted using subscripts:
    \(n_L\) refers to the left side, \(n_R\) to the right side,
    and \(n_M\) refers to the total number of elements collected with the 'Monoid'.

    == Implementation

    Description of the amortized queue can be found within the following paper:

    - Chris Okasaki, /"Purely Functional Data Structures"/, September 1996, pages 15-18,
      https://www.cs.cmu.edu/~rwh/students/okasaki.pdf
 -}

module Data.Steque
  ( -- * Itself
    Steque (Empty, (:<|))

    -- * Construct
  , empty
  , singleton
  , fromListF

    -- * Front
    -- ** Insert
  , (<|)

    -- ** View
  , ViewF (..)
  , viewF

    -- * Back
    -- ** Insert
  , (|>)

    -- * Full queue
    -- ** Size
  , Data.Steque.Internal.null
  , Data.Steque.Internal.size

    -- ** Map
  , Data.Steque.Internal.map
  , map'

    -- ** Fold
    -- | === Left-to-right
  , Data.Steque.Internal.foldl
  , Data.Steque.Internal.foldl'

    -- | === Right-to-left
  , Data.Steque.Internal.foldr
  , Data.Steque.Internal.foldr'

    -- | === Monoid
  , Data.Steque.Internal.foldMap

    -- | === List
  , toList
  , toListF
  , toListB

    -- ** Traverse
  , Data.Steque.Internal.traverse
  ) where

import           Data.Steque.Internal
