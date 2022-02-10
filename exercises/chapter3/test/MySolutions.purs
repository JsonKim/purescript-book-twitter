module Test.MySolutions where

import Prelude

import Control.Plus (empty)
import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (filter, head, nubByEq)
import Data.Maybe (Maybe, isJust)

-- Note to reader: Add your solutions to this file

-- head :: AddressBook -> Maybe Entry
-- filter :: (Entry -> Boolean) -> AddressBook -> AddressBook

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((street == _) <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq (\x y -> x.firstName == y.firstName && x.lastName == y.lastName)
