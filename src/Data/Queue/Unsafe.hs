{-# OPTIONS_HADDOCK not-home #-}

{-| Data structure internals, helper operations and unsafe functions.
 -}

module Data.Queue.Unsafe
  ( -- * Itself
    Queue (Queue)
  , List' (..)

    -- * Invariant
  , MalformedQueue (..)

    -- * Front
    -- ** Insert
  , (<|)
  ) where

import           Data.List.Strict.Internal (List' (..))
import           Data.Queue.Internal
