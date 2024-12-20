{-| Safe functions for datatype introspection.
 -}

module Data.Steque.Debug
  ( -- * Show
    showSteque
    -- * Eq
  , eqSteque
  ) where

import qualified Data.List.Strict.Internal as List'
import           Data.Steque.Internal

import           Data.Functor.Classes
import           Text.Show



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the queue.
showSteque :: (a -> ShowS) -> Steque a -> ShowS
showSteque _shows (Steque fs rs) =
  showString "Steque\n  " . showListWith _shows (List'.appendToList fs [])
      . showString "\n  " . showListWith _shows (List'.appendToList rs [])



-- | \(\mathcal{O}(n)\).
--   Checks whether the internal representations of two queues are equal.
eqSteque :: (a -> b -> Bool) -> Steque a -> Steque b -> Bool
eqSteque eq (Steque fs rs) (Steque fs' rs') =
  liftEq eq fs fs' && liftEq eq rs rs'
