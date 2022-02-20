module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, foldl, head, last, sortBy, tail, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, size)
import Test.Examples (allFiles, factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (if n < 0 then n + 2 else n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven arr = (if nIsEven == true then 1 else 0) + (countEven $ fromMaybe [] $ tail arr)
  where
    nIsEven = isEven $ fromMaybe 0 $ head arr

squared :: Array Number -> Array Number
squared xs = (\x -> x * x) <$> xs

keepNonNegative :: Array Number -> Array Number
keepNonNegative xs = filter (\x -> x >= 0.0) xs

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = factors n == [[1, n]]

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  c <- 2 .. n
  a <- 1 .. c
  b <- a .. c
  guard $ (a * a) + (b * b) == (c * c)
  pure [a, b, c]

primeFactors :: Int -> Array Int
primeFactors 1 = []
primeFactors n = cons factor $ primeFactors n1
  where
    findNextFactor :: Int -> Int -> Int
    findNextFactor x y =
      if x >= y then
        y 
      else if (y `mod` x) == 0 then
        x
      else
        findNextFactor (x + 1) y

    factor = findNextFactor 2 n
    n1 = n / factor

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true
 
{-
(foldl (==) false xs) == true
xs에 있는 false가 홀수이면 참이다.
-}

fibTailRec :: Int -> Int
fibTailRec n = go 0 1 0
  where
  go acc1 acc2 x = 
    if x == n then
      acc1
    else
      go acc2 (acc1 + acc2) (x + 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles paths = do
  path <- allFiles paths
  guard $ not $ isDirectory path
  pure path

whereIs :: Path -> String -> Maybe Path
whereIs path name = head $ do
  path' <- allFiles path
  file <- ls path'
  guard $ filename file == filename path' <> name
  pure path'

largestSmallest :: Path -> Array Path
largestSmallest path = ret
  where
    paths = sortBy (\a z -> compare (size a) (size z)) $ onlyFiles path
    h = head paths
    l = last paths
    ret = case [h, l] of
      [Just p1, Just p2] -> 
        if filename p1 == filename p2 then
          [p1]
        else
          [p1, p2]
      [Just p1, Nothing] -> [p1]
      [Nothing, Just p2] -> [p2]
      _ -> []
