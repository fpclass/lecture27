--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Type families                                                     --
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedLists, TypeFamilies #-}

module OverloadedList where

--------------------------------------------------------------------------------

import GHC.Exts
import qualified LabRecDataTypes as L

--------------------------------------------------------------------------------

-- This class is defined in GHC.Exts:
--
-- class IsList l where
--     type family Item l :: *
--
--     fromList :: [Item l] -> l
--     fromListN :: Int -> [Item l] -> l
--     fromListN _ xs = fromList xs
--     toList :: l -> [Item l]

instance Ord a => IsList (L.Tree a) where 
    type Item (L.Tree a) = a 

    toList = L.toList 

    fromList xs = foldr (flip L.insert) L.empty xs 

poultree :: L.Tree String 
poultree = ["Duck", "Goose", "Chicken", "Pigeon"]

digits :: L.Tree Int 
digits = [8, 15, 4, 42, 23]

--------------------------------------------------------------------------------
