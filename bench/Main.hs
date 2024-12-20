{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Deque as Deque
import qualified Data.Queue as Queue
import qualified Data.Sequence as Seq
import qualified Data.Steque as Steque

import           Test.Tasty.Bench



deque_populate :: [a] -> Deque.Deque a
deque_populate = foldl' (Deque.|>) Deque.empty

queue_populate :: [a] -> Queue.Queue a
queue_populate = foldl' (Queue.|>) Queue.empty

steque_populate :: [a] -> Steque.Steque a
steque_populate = foldl' (Steque.|>) Steque.empty

seq_populate :: [a] -> Seq.Seq a
seq_populate = foldl' (Seq.|>) Seq.empty



deque_reduce :: Int -> Deque.Deque a -> IO (Deque.Deque a)
deque_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case q of
        Deque.Empty    -> fail $ "Empty at " <> show n
        _ Deque.:<| q' -> deque_reduce (n - 1) q'

queue_reduce :: Int -> Queue.Queue a -> IO (Queue.Queue a)
queue_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case q of
        Queue.Empty    -> fail $ "Empty at " <> show n
        _ Queue.:<| q' -> queue_reduce (n - 1) q'

steque_reduce :: Int -> Steque.Steque a -> IO (Steque.Steque a)
steque_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case q of
        Steque.Empty    -> fail $ "Empty at " <> show n
        _ Steque.:<| q' -> steque_reduce (n - 1) q'

seq_reduce :: Int -> Seq.Seq a -> IO (Seq.Seq a)
seq_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case q of
        Seq.Empty    -> fail $ "Empty at " <> show n
        _ Seq.:<| q' -> seq_reduce (n - 1) q'



deque_wobble :: Deque.Deque a -> [a] -> IO (Deque.Deque a)
deque_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = q Deque.|> x
      in case q' of
           Deque.Empty     -> fail $ "Empty at " <> show n
           _ Deque.:<| q'' -> go (n + 1) q'' ys

queue_wobble :: Queue.Queue a -> [a] -> IO (Queue.Queue a)
queue_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = q Queue.|> x
      in case q' of
           Queue.Empty     -> fail $ "Empty at " <> show n
           _ Queue.:<| q'' -> go (n + 1) q'' ys

steque_wobble :: Steque.Steque a -> [a] -> IO (Steque.Steque a)
steque_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = q Steque.|> x
      in case q' of
           Steque.Empty     -> fail $ "Empty at " <> show n
           _ Steque.:<| q'' -> go (n + 1) q'' ys

seq_wobble :: Seq.Seq a -> [a] -> IO (Seq.Seq a)
seq_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = q Seq.|> x
      in case q' of
           Seq.Empty     -> fail $ "Empty at " <> show n
           _ Seq.:<| q'' -> go (n + 1) q'' ys



sizes :: [Int]
sizes = [100, 1000, 10000, 100000]

withSizes :: (Int -> [Benchmark]) -> [Benchmark]
withSizes f = fmap (\n -> bgroup (show n) (f n)) sizes



main :: IO ()
main =
  defaultMain
    [ bgroup "insert" $
        withSizes $ \n ->
          let load = [1..n]

          in [ bench "Steque" $ do
                 whnf steque_populate load

             , bench "Queue" $ do
                 whnf queue_populate load

             , bench "Deque" $ do
                 whnf deque_populate load

             , bench "Seq" $ do
                 whnf seq_populate load
             ]

    , bgroup "reduce" $
        withSizes $ \n ->
          let load = [1..n]

          in [ bench "Steque" $ do
                 whnfAppIO (steque_reduce n) $ steque_populate load

             , bench "Queue" $ do
                 whnfAppIO (queue_reduce n) $ queue_populate load

             , bench "Deque" $ do
                 whnfAppIO (deque_reduce n) $ deque_populate load

             , bench "Seq" $ do
                 whnfAppIO (seq_reduce n) $ seq_populate load
             ]

    , bgroup "wobble" $
        withSizes $ \n ->
          let base = [1..n]
              load = [1..100000]

          in [ bench "Steque" $ do
                 flip whnfAppIO (steque_populate base, load) $
                   uncurry steque_wobble

             , bench "Queue" $ do
                 flip whnfAppIO (queue_populate base, load) $
                   uncurry queue_wobble

             , bench "Deque" $ do
                 flip whnfAppIO (deque_populate base, load) $
                   uncurry deque_wobble

             , bench "Seq" $ do
                 flip whnfAppIO (seq_populate base, load) $
                   uncurry seq_wobble
             ]
    ]
