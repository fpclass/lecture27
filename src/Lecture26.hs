--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 26: Type families                                                  --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Lecture26 where

--------------------------------------------------------------------------------

import qualified Lab4 as L

--------------------------------------------------------------------------------
-- Previous lecture

-- natural numbers
data Nat = Zero | Succ Nat

data Vector (n :: Nat) a where
    Nil  :: Vector 'Zero a
    Cons :: a -> Vector n a -> Vector ('Succ n) a

vhead :: Vector ('Succ n) a -> a
vhead (Cons x _) = x

vzip :: Vector n a -> Vector n b -> Vector n (a,b)
vzip Nil         Nil         = Nil
vzip (Cons x xs) (Cons y ys) = Cons (x,y) (vzip xs ys)

data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)

vreplicate :: SNat n -> a -> Vector n a
vreplicate SZero     _ = Nil
vreplicate (SSucc n) x = Cons x (vreplicate n x)

vlength :: Vector n a -> Int
vlength Nil         = 0
vlength (Cons _ xs) = 1 + vlength xs

data NatProxy (a :: Nat) = MkProxy

zeroProxy :: NatProxy 'Zero
zeroProxy = MkProxy

oneProxy :: NatProxy ('Succ 'Zero)
oneProxy = MkProxy

class FromNat (n :: Nat) where
    fromNat :: NatProxy n -> Int

instance FromNat 'Zero where
    fromNat _ = 0

instance FromNat n => FromNat ('Succ n) where
    fromNat _ = 1 + fromNat (MkProxy :: NatProxy n)

vlength' :: forall n a . FromNat n => Vector n a -> Int
vlength' _ = fromNat (MkProxy :: NatProxy n)

--------------------------------------------------------------------------------
-- Closed type families

type family (n :: Nat) + (m :: Nat) :: Nat where
    'Zero   + m = m
    'Succ n + m = 'Succ (n + m)

vappend :: Vector n a -> Vector m a -> Vector (n+m) a
vappend Nil         ys = ys
vappend (Cons x xs) ys = Cons x (vappend xs ys)

--------------------------------------------------------------------------------
-- Associated (open) type families

class Collection c where
    type Elem c :: *

    empty  :: c
    insert :: Elem c -> c -> c
    member :: Elem c -> c -> Bool

instance Eq a => Collection [a] where
    type Elem [a] = a

    empty = []
    insert x xs = x : xs
    member x xs = x `elem` xs

instance Ord a => Collection (L.Tree a) where
    type Elem (L.Tree a) = a

    empty      = L.Leaf
    insert x t = L.insert t x
    member x t = L.member x t

--------------------------------------------------------------------------------
