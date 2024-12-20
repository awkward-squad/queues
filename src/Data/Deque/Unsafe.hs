{-# OPTIONS_HADDOCK not-home #-}

{-| Data structure internals, helper operations and unsafe functions.
 -}

module Data.Deque.Unsafe
  ( -- * Itself
    Deque (..)

    -- * Invariant
  , MalformedDeque (..)
  ) where

import           Data.Deque.Internal
