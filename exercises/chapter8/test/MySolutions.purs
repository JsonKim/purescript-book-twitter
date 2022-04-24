module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (modify, new, read, write)
import Data.Array (foldM, head, sort, tail)
import Data.Int (toNumber)
import Data.List (List(..), snoc, (:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Math (pow)

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third xs = do
  t1 <- tail xs
  t2 <- tail t1
  head t2

possibleSums :: Array Int -> Array Int
possibleSums xs = sort $ foldM (\acc cur -> [acc, acc + cur]) 0 xs

filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' f xs = go xs Nil 
  where
    go :: List a -> List a -> m (List a)
    go  Nil acc = pure acc
    go (Cons h t) acc = bind (f h) (\b -> if b then go t (snoc acc h) else go t acc)

filterM2 :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM2 _ Nil = pure Nil
filterM2 f (h:t) = do
  b <- f h
  xs <- filterM f t
  pure if b then h:xs else xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f xs = go xs Nil 
  where
    go :: List a -> List a -> m (List a)
    go  Nil acc = pure acc
    go (Cons h t) acc = (f h) >>= (\b -> 
      let
        xs' :: m (List a)
        xs' = go t acc
      in
        xs' >>= \xs'' -> pure if b then h:xs'' else xs''
    )

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide x y = pure $ x / y

estimatePi :: Int -> Number
estimatePi n =
  run do
    ref <- new 0.0
    for 1 n \k' ->
      let k = toNumber k' in
      modify 
        (\acc -> acc + (pow (-1.0) (k + 1.0)) / (2.0 * k - 1.0))
        ref
    result <- read ref
    pure $ result * 4.0

fibonacci :: Int -> Int
fibonacci n =
  run do
    ref0 <- new 0
    ref1 <- new 1
    for 0 n (\_ -> do
      v0 <- read ref0
      v1 <- read ref1
      _ <- write v1 ref0
      write (v0 + v1) ref1)
    read ref0

fibonacci' :: Int -> Int
fibonacci' n =
  run do
    ref <- new (Tuple 0 1)
    for 0 n \_ ->
      modify 
        (\(Tuple x y) -> Tuple y (x + y))
        ref
    result <- read ref
    pure $ fst result
