{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Deque
  ( deque
  ) where

import           Data.Deque as Deque
import           Data.Deque.Debug

import qualified Data.List as List
import           Test.Hspec



instance Eq a => Eq (Deque a) where
  (==) = eqDeque (==)



reduceF :: Deque a -> [a]
reduceF Empty      = []
reduceF (x :<| ys) = x : reduceF ys

reduceB :: Deque a -> [a]
reduceB = List.reverse . go
  where
    go Empty      = []
    go (ys :|> x) = x : go ys



deque :: Spec
deque = do
  describe "front" $ do
    it "insert/view" $ do
      let range = [63,62..1 :: Int]

          step :: [(Int, Validity, Deque Int)] -> IO ()
          step []                 = pure ()
          step ((n, valid, q):ys) =
            case valid of
              Valid ->
                if reduceF q == [n..63]
                  then step ys
                  else fail . showChar '#' . shows n . showString ": "
                                . shows (reduceF q) . showString " /= " $ show [1..n]

              Invalid reason -> fail . showChar '#' . shows n . showString ": " $
                                         show reason

      step . zipWith (\n q -> (n, validate q, q)) (64:range) $
               scanl (flip (<|)) empty range


  describe "back" $ do
    it "insert/view" $ do
      let range = [1..63 :: Int]

          step :: [(Int, Validity, Deque Int)] -> IO ()
          step []                 = pure ()
          step ((n, valid, q):ys) =
            case valid of
              Valid ->
                if reduceB q == [1..n]
                  then step ys
                  else fail . showChar '#' . shows n . showString ": "
                                . shows (reduceB q) . showString " /= " $ show [1..n]

              Invalid reason -> fail . showChar '#' . shows n . showString ": " $
                                         show reason

      step . zipWith (\n q -> (n, validate q, q)) (0:range) $ scanl (|>) empty range


  it "fromListF" $ do
    let ref = [0..63 :: Int]

    reduceF (fromListF ref) `shouldBe` ref

  it "fromListB" $ do
    let ref = [0..63 :: Int]

    reduceB (fromListB ref) `shouldBe` List.reverse ref


  describe "null" $ do
    it "empty" $
      Deque.null Deque.empty `shouldBe` True

    it "singleton" $
      Deque.null (Deque.singleton @Int 1) `shouldBe` False


  it "reverse" $ do
    let ref = [0..63 :: Int]

    reduceF (Deque.reverse $ fromListF ref) `shouldBe` List.reverse ref


  describe "fold" $ do
    let q = (0 <| 1 <| 2 <| 3 <| 4 <| empty) |> 5 |> 6 |> 7 |> 8 |> (9 :: Int)

    it "foldl-foldr" $
      Deque.foldl (flip (:)) [] q `shouldBe` List.reverse (Deque.foldr (:) [] q)

    it "foldl-foldl'" $
      Deque.foldl (flip (:)) [] q `shouldBe` Deque.foldl' (flip (:)) [] q

    it "foldr-foldMap" $
      Deque.foldr (:) [] q `shouldBe` Deque.foldMap pure q

    it "foldr-foldr'" $
      Deque.foldr (:) [] q `shouldBe` Deque.foldr' (:) [] q
