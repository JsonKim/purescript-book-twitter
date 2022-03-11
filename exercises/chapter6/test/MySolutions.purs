module Test.MySolutions where

import Prelude

import Data.Array (nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hashCode, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }
derive newtype instance Eq Complex

instance Show Complex where
  show (Complex {real, imaginary}) | imaginary > 0.0 = show real <> "+" <> show imaginary <> "i"
  show (Complex {real, imaginary}) = show real <> show imaginary <> "i"

instance Semiring Complex where
  add (Complex x) (Complex y) = Complex { real: x.real + y.real, imaginary: x.imaginary + y.imaginary } 
  zero = Complex { real: 0.0, imaginary: 0.0 }
  mul
    (Complex { real: r1, imaginary: i1 })
    (Complex { real: r2, imaginary: i2 })
      = Complex
          { real:      r1 * r2 - i1 * i2
          , imaginary: r1 * i2 + r2 * i1
          }
  one = Complex { real: 1.0, imaginary: 0.0 }

derive newtype instance Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance Generic Shape _

instance Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

-- derive instance Eq a => Eq (NonEmpty a)
instance Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  append (NonEmpty h1 t1) (NonEmpty h2 t2) = NonEmpty h1 (t1 <> [h2] <> t2)

instance Functor NonEmpty where
  map f (NonEmpty head tail) = NonEmpty (f head) (map f tail)

instance Show a => Show (NonEmpty a) where
  show (NonEmpty head tail) = show head <> " " <> show tail

data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

instance Foldable NonEmpty where
  foldr f init (NonEmpty head tail) = foldr f init ([head] <> tail)
  foldl f init (NonEmpty head tail) = foldl f init ([head] <> tail)
  foldMap f (NonEmpty head tail) = foldMap f ([head] <> tail)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f init (OneMore one more) = f one (foldr f init more)
  foldl f init (OneMore one more) = foldl f (f init one) more
  foldMap f (OneMore one more) = f one <> foldMap f more

derive instance Eq Point
derive instance Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance Ord Point
derive instance Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just x -> x

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply n) m = n * m

instance Action Multiply String where
  act (Multiply n) s = power s n

instance Action m a => Action m (Array a) where
  act m xs = act m <$> xs

newtype Self m = Self m

instance Monoid m => Action m (Self m) where
  act m (Self n) = Self (m <> n)

derive newtype instance Show Multiply
derive newtype instance Eq Multiply

derive newtype instance Eq m => Eq (Self m)
derive newtype instance Show m => Show (Self m)

arrayHasDuplicates :: forall a. Hashable a => Ord a => Array a -> Boolean
arrayHasDuplicates arr = nubByEq hashEqual arr /= arr && nub arr /= arr

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance Hashable Hour where
  hash (Hour h) =  hashCode (mod h 12)
