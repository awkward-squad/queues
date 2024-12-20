{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Steque
  ( steque
  ) where

import           Data.Steque as Steque
import           Data.Steque.Debug
import           Data.Steque.Unsafe as Steque

import qualified Data.List as List
import           Data.IORef
import           Test.Hspec



instance Eq a => Eq (Steque a) where
  (==) = eqSteque (==)



reduce :: Steque a -> [a]
reduce Empty      = []
reduce (x :<| ys) = x : reduce ys



steque :: Spec
steque = do
  describe "front" $ do
    it "insert" $ do
      let ref = (1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

      reduce (0 <| ref) `shouldBe` 0 : reduce ref

    describe "view" $ do
      it "both full" $ do
        let ref = (0 <| 1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

        case ref of
          x :<| ys -> (x, reduce ys) `shouldBe` (0, [1, 2, 3, 4, 5])
          Empty    -> fail "empty"

      it "left empty" $ do
        let ref = empty |> 3 |> 4 |> (5 :: Int)

        case ref of
          x :<| ys -> (x, reduce ys) `shouldBe` (3, [4, 5])
          Empty    -> fail "empty"


  describe "back" $ do
    it "insert" $ do
      let ref = (0 <| 1 <| 2 <| empty) |> 3 |> (4 :: Int)

      reduce (ref |> 4) `shouldBe` (reduce ref <> [4])


  it "fromListF" $ do
    let ref = [1..5 :: Int]

    reduce (fromListF ref) `shouldBe` ref


  describe "null" $ do
    it "empty" $
      Steque.null Steque.empty `shouldBe` True

    it "singleton" $
      Steque.null (Steque.singleton @Int 1) `shouldBe` False


  it "reverse" $ do
    let ref = (0 <| 1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

    reduce (Steque.reverse ref) `shouldBe` [5, 4, 3, 2, 1, 0]


  describe "map" $ do
    let q = (0 <| 1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

        q' = (0 <| -1 <| -2 <| empty) |> -3 |> -4 |> (-5 :: Int)

    it "map" $
      Steque.map negate q `shouldBe` q'

    it "map'" $
      Steque.map' negate q `shouldBe` q'


  describe "fold" $ do
    let q = (0 <| 1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

    it "foldl-foldr" $
      Steque.foldl (flip (:)) [] q `shouldBe` List.reverse (Steque.foldr (:) [] q)

    it "foldl-foldl'" $
      Steque.foldl (flip (:)) [] q `shouldBe` Steque.foldl' (flip (:)) [] q

    it "foldr-foldMap" $
      Steque.foldr (:) [] q `shouldBe` Steque.foldMap pure q

    it "foldr-foldr'" $
      Steque.foldr (:) [] q `shouldBe` Steque.foldr' (:) [] q


  describe "traverse" $ do
    let q = (0 <| 1 <| 2 <| empty) |> 3 |> 4 |> (5 :: Int)

        q' = (0 <| -1 <| -2 <| empty) |> -3 |> -4 |> (-5 :: Int)

    it "traverse" $ do
      var <- newIORef 0
      res <- Steque.traverse
               ( \a -> do modifyIORef' var (+ a)
                          pure $! negate a
               )
               q

      s <- readIORef var

      (s, res) `shouldBe` (15, q')
