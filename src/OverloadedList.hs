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

--------------------------------------------------------------------------------
