{-| Safe functions for datatype introspection.
 -}

module Data.Queue.Debug
  ( -- * Show
    showQueue
    -- * Eq
  , eqQueue
    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import qualified Data.List.Strict.Internal as List'
import           Data.Queue.Internal

import           Data.Functor.Classes
import           Text.Show



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the queue.
showQueue :: (a -> ShowS) -> Queue a -> ShowS
showQueue _shows (Queue fs rs sf) =
  showString "Queue\n  " . showListWith _shows fs
     . showString "\n  " . showListWith _shows (List'.appendToList rs [])
     . showString "\n  " . showListWith _shows sf



-- | \(\mathcal{O}(n)\).
--   Checks whether the internal representations of two queues are equal.
eqQueue :: (a -> b -> Bool) -> Queue a -> Queue b -> Bool
eqQueue eq (Queue fs rs sf) (Queue fs' rs' sf') =
  liftEq eq fs fs' && liftEq eq rs rs' && liftEq eq sf sf'




-- | Whether the queue is well-formed.
data Validity = Valid
              | Invalid Reason
                deriving (Show, Eq)

-- | Reason for why the queue is considered malformed.
data Reason = -- | \(|R| + |S| \ne |F|\).
              LengthMismatch
              deriving (Show, Eq)

-- | O(n). Checks whether the queue is well-formed.
validate :: Queue a -> Validity
validate (Queue fs rs sf)
  | length fs - length sf /= List'.length rs = Invalid LengthMismatch
  | otherwise                                = Valid
