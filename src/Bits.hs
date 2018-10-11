module Bits where

import Vec

-- Bit strings -----------------------------------------------------------------
-- (as vectors of booleans)

type Bits n = Vec n Bool

xorBits :: forall (n :: Nat). Bits n -> Bits n -> Bits n
xorBits None None = None
xorBits (Some b bs) (Some b' bs') = Some (xor b b') $ xorBits bs bs'
  where xor :: Bool -> Bool -> Bool
        xor True False = True
        xor False True = True
        xor _ _ = False

-- Permutation tables ----------------------------------------------------------
-- (as vectors of valid indices in a source vector)

type Perm n m = Vec n (Fin m)

permuteBits :: forall (n :: Nat). Perm ('Succ n) n -> Bits ('Succ n) -> Bits ('Succ n)
permuteBits perm bs = go perm
  where getBit :: Fin n -> Bool
        getBit i = indexVec i bs
        go :: forall (m :: Nat). Perm m n -> Bits m
        go None = None
        go (Some p ps) = Some (getBit p) $ go ps
