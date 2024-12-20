{-# OPTIONS_HADDOCK not-home #-}

{-| Data structure internals, helper operations and unsafe functions.
 -}

module Data.Steque.Unsafe
  ( -- * Itself
    Steque (Steque)
  , List' (..)

    -- * Full queue
    -- ** Reverse
  , Data.Steque.Internal.reverse
  ) where

import           Data.List.Strict.Internal (List' (..))
import           Data.Steque.Internal
