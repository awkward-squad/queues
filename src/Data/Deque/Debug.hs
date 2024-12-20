{-# LANGUAGE BangPatterns #-}

{-| Safe functions for datatype introspection.
 -}

module Data.Deque.Debug
  ( -- * Show
    showDeque
    -- * Eq
  , eqDeque
    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.Deque.Internal

import           Data.Functor.Classes
import           Text.Show



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the queue.
showDeque :: (a -> ShowS) -> Deque a -> ShowS
showDeque _shows (Deque fs nf sf sr nr rs) =
  showString "Deque\n  " . showListWith _shows fs
     . showString "\n  " . shows nf
     . showString "\n  " . showListWith _shows sf
     . showString "\n  " . showListWith _shows sr
     . showString "\n  " . shows nr
     . showString "\n  " . showListWith _shows rs



-- | \(\mathcal{O}(n)\).
--   Checks whether the internal representations of two queues are equal.
eqDeque :: (a -> b -> Bool) -> Deque a -> Deque b -> Bool
eqDeque eq (Deque fs nf sf sr nr rs) (Deque fs' nf' sf' sr' nr' rs') =
     nf == nf' && nr == nr'
  && liftEq eq fs fs' && liftEq eq sf sf' && liftEq eq sr sr' && liftEq eq rs rs'




-- | Whether the queue is well-formed.
data Validity = Valid
              | Invalid Reason
                deriving (Show, Eq)

-- | Reason for why the queue is considered malformed.
data Reason = -- | \(|F| \gt 2 |R| + 1\) or \(|R| \gt 2 |F| + 1\).
              Imbalanced

              -- | Stored length does not match actual length on either side.
            | BadLength

              -- | Either schedule is too large in regards to stored lengths.
            | BadSchedule
              deriving (Show, Eq)

-- | O(n). Checks whether the queue is well-formed.
validate :: Deque a -> Validity
validate (Deque fs nf sf sr nr rs)
  | max nf nr > 2 * min nf nr + 1                 = Invalid Imbalanced
  | length fs /= nf || length rs /= nr            = Invalid BadLength
  | invalidSchedule nf (length sf) (length sr) nr = Invalid BadSchedule
  | otherwise                                     = Valid



invalidSchedule :: Int -> Int -> Int -> Int -> Bool
invalidSchedule !nf nsf nsr !nr
  | nsf == nsr =
      let hi = max nf nr
          lo = min nf nr

      in nsf > abs (abs (hi - lo) - lo)

  | min nsf nsr + 1 == max nsf nsr =
      let hi = max nf nr
          lo = min nf nr + 1

      in nsf > abs (abs (hi - lo) - lo)

  | otherwise = True
