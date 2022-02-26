module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Picture (Bounds, Picture, Point, Shape(..), bounds, getCenter, intersect, origin, shapeBounds)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = (factorial n) / ((factorial k) * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = (pascal (n - 1) k) + (pascal (n - 1) (k - 1))

type City = forall a b.  { address :: { city :: String | a } | b }

sameCity :: forall a b c d e. Eq a =>
  { address :: { city :: a | b } | c } ->
  { address :: { city :: a | d } | e } ->
  Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _   = a

circleAtOrigin :: Shape
circleAtOrigin = (Circle { x: 0.0, y : 0.0} 10.0)

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (r * 2.0)
doubleScaleAndCenter (Rectangle _ w h)  = Rectangle origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line p1 p2) = 
  let scale = { x: 2.0, y: 2.0 }
      l1 = Line (p1 * scale) (p2 * scale)
      d = getCenter l1
  in Line (p1 * scale - d) (p2 * scale - d)
doubleScaleAndCenter (Text _ s) = Text origin s

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp amp) (Volt volt) = Watt $ amp * volt

area :: Shape -> Number
area (Circle _ r) = 3.14 * r * r
area (Rectangle _ w h) = w * h
area _ = 0.0

data ShapeExt
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBoundsExt :: ShapeExt -> Bounds
shapeBoundsExt (Clipped pic pt w h) = intersect (bounds pic) (shapeBounds (Rectangle pt w h))
shapeBoundsExt (Shape shape) = shapeBounds shape
