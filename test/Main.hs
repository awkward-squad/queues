module Main where

import           Test.Deque
import           Test.Queue
import           Test.Steque

import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "Deque" deque
    describe "Queue" queue
    describe "Steque" steque
