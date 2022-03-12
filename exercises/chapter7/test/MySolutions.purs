module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file
lift3 :: forall a b c d f
       . Apply f
      => (a -> b -> c -> d)
      -> f a
      -> f b
      -> f c
      -> f d
lift3 f x y z = ado
  a <- x
  b <- y
  c <- z
  in f a b c

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

traverseMaybe :: forall f a b. Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
traverseMaybe _ Nothing = pure Nothing
traverseMaybe f (Just x) = Just <$> f x

traverseMaybe' :: forall f a b. Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
traverseMaybe' f x = combineMaybe $ f <$> x

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex a.street
  city   <- matches "City" nonEmptyRegex a.city
  state  <- matches "State" stateRegex a.state
  in address street city state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance Eq a => Eq (Tree a)

instance Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch l x r) = "(Branch " <> show l <> " " <> show x <> " " <> show r <> ")" 

instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Branch l x r) = foldr f (f x (foldr f z r)) l

  foldl _ z Leaf = z
  foldl f z (Branch l x r) = foldl f (f (foldl f z l) x) r

  foldMap _ Leaf = mempty
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r

-- (a -> List b) -> Tree a -> List (Tree b)
instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  -- traverse f (Branch l x r) = Branch <$> sequence (f <$> l) <*> f x <*> sequence (f <$> r)
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r
  -- sequence Leaf = pure Leaf
  -- sequence (Branch l x r) = Branch <$> sequence l <*> x <*> sequence r
  sequence = traverse identity

traversePreOrder' :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder' _ Leaf = pure Leaf
traversePreOrder' f (Branch l x r) = result (f x)
  where
    result = \x'' -> ado
      x' <- x''
      l' <- traversePreOrder' f l
      r' <- traversePreOrder' f r
      in Branch l' x' r'

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l x r) = 
  (\x' l' r' -> Branch l' x' r') <$> f x <*> traversePreOrder f l <*> traversePreOrder f r

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l x r) = ado
  l' <- traversePostOrder f l
  r' <- traversePostOrder f r
  x' <- f x
  in Branch l' x' r'

type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

person :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p = ado
  firstName <- nonEmpty "First Name" p.firstName
  lastName  <- nonEmpty "Last Name" p.lastName
  address   <- traverse validateAddress p.homeAddress
  numbers   <- validatePhoneNumbers "Phone Numbers" p.phones
  in person firstName lastName address numbers

sequenceUsingTraverse :: forall t f a. Traversable t => Applicative f => t (f a) -> f (t a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall t f a b. Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)
traverseUsingSequence f x = sequence (f <$> x)
