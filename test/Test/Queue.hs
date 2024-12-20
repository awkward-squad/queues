{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Queue
  ( queue
  ) where

import           Data.Queue as Queue
import           Data.Queue.Debug
import           Data.Queue.Unsafe as Queue

import qualified Data.List as List
import           Test.Hspec



instance Eq a => Eq (Queue a) where
  (==) = eqQueue (==)



reduce :: Queue a -> [a]
reduce Empty      = []
reduce (x :<| ys) = x : reduce ys



queue :: Spec
queue = do
  describe "front" $ do
    it "insert/viewF" $ do
      let range = [63,62..1 :: Int]

          step :: [(Int, Validity, Queue Int)] -> IO ()
          step []                 = pure ()
          step ((n, valid, q):ys) =
            case valid of
              Valid ->
                if reduce q == [n..63]
                  then step ys
                  else fail . showChar '#' . shows n . showString ": "
                                . shows (reduce q) . showString " /= " $ show [1..n]

              Invalid reason -> fail . showChar '#' . shows n . showString ": " $
                                         show reason

      step . zipWith (\n q -> (n, validate q, q)) (64:range) $
               scanl (flip (<|)) empty range


  describe "back" $ do
    it "insert/viewF" $ do
      let range = [1..63 :: Int]

          step :: [(Int, Validity, Queue Int)] -> IO ()
          step []                 = pure ()
          step ((n, valid, q):ys) =
            case valid of
              Valid ->
                if reduce q == [1..n]
                  then step ys
                  else fail . showChar '#' . shows n . showString ": "
                                . shows (reduce q) . showString " /= " $ show [1..n]

              Invalid reason -> fail . showChar '#' . shows n . showString ": " $
                                         show reason

      step . zipWith (\n q -> (n, validate q, q)) (0:range) $ scanl (|>) empty range


  it "fromListF" $ do
    let ref = [0..63 :: Int]

    reduce (fromListF ref) `shouldBe` ref


  describe "null" $ do
    it "empty" $
      Queue.null Queue.empty `shouldBe` True

    it "singleton" $
      Queue.null (Queue.singleton @Int 1) `shouldBe` False


  describe "fold" $ do
    let q = (0 <| 1 <| 2 <| 3 <| 4 <| empty) |> 5 |> 6 |> 7 |> 8 |> (9 :: Int)

    it "foldl-foldr" $
      Queue.foldl (flip (:)) [] q `shouldBe` List.reverse (Queue.foldr (:) [] q)

    it "foldl-foldl'" $
      Queue.foldl (flip (:)) [] q `shouldBe` Queue.foldl' (flip (:)) [] q

    it "foldr-foldMap" $
      Queue.foldr (:) [] q `shouldBe` Queue.foldMap pure q

    it "foldr-foldr'" $
      Queue.foldr (:) [] q `shouldBe` Queue.foldr' (:) [] q
