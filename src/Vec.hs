module Vec where

import Data.Kind (Type)

-- Type-level Peano arithmetic -------------------------------------------------

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

type family Add (x :: Nat) (y :: Nat) :: Nat
type instance Add 'Zero x = x
type instance Add ('Succ x) y = 'Succ (Add x y)

type One = 'Succ 'Zero
type Two = Add One One
type Three = 'Succ Two
type Four = Add Two Two
type Five = 'Succ Four
type Seven = Add Four Three
type Eight = Add Four Four
type Nine = 'Succ Eight
type Ten = Add Five Five

-- Finite types of n elements --------------------------------------------------
-- (effectively, natural numbers less than or equal to some maximum)

data Fin (n :: Nat) :: Type where
  FZero :: forall (m :: Nat). Fin m
  FSucc :: forall (m :: Nat). Fin m -> Fin ('Succ m)
deriving instance Show (Fin n)

f0 :: forall (n :: Nat). Fin n
f0 = FZero
f1 :: forall (n :: Nat). Fin ('Succ n)
f1 = FSucc f0
f2 :: forall (n :: Nat). Fin ('Succ ('Succ n))
f2 = FSucc f1
f3 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ n)))
f3 = FSucc f2
f4 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ n))))
f4 = FSucc f3
f5 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ ('Succ n)))))
f5 = FSucc f4
f6 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ n))))))
f6 = FSucc f5
f7 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ n)))))))
f7 = FSucc f6
f8 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ n))))))))
f8 = FSucc f7
f9 :: forall (n :: Nat). Fin ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ n)))))))))
f9 = FSucc f8

-- Vectors ---------------------------------------------------------------------
-- (polymorphic lists of cons cells with size encoded at the type level)

-- Representing our bit strings this way has nice properties: it is impossible
-- to have an out-of-bounds error at runtime! The compiler will force us to
-- prove that any index is within the proper bounds. This can make certain
-- things verbose, but it's the only way to formally ensure correctness.

data Vec (n :: Nat) (a :: Type) :: Type where
  None :: forall (a :: Type). Vec 'Zero a
  Some :: forall (m :: Nat) (a :: Type). a -> Vec m a -> Vec ('Succ m) a

lengthVec :: forall (n :: Nat) (a :: Type). Vec n a -> Int
lengthVec None = 0
lengthVec (Some _ bs) = 1 + lengthVec bs

indexVec :: forall (n :: Nat) (a :: Type). Fin n -> Vec ('Succ n) a -> a
indexVec FZero (Some b _) = b
indexVec (FSucc x) (Some _ bs) = indexVec x bs

concatVec :: forall (n :: Nat) (m :: Nat) (a :: Type). Vec n a -> Vec m a -> Vec (Add n m) a
concatVec None x = x
concatVec (Some b bs) x = Some b $ concatVec bs x

popVec :: forall (n :: Nat) (a :: Type). Vec ('Succ n) a -> Vec n a
popVec (Some _ bs) = bs

-- Notice that there are three split functions for different vector lenghts.
-- We cannot define a general interface for splitting arbitrary-length vectors
-- easily in Haskell, as without true dependent types it is difficult to move
-- the length parameter between the term and type levels. We could fake it
-- using a singleton type, but this would be overly verbose for this
-- assignment.
splitVec4 :: forall (a :: Type). Vec Four a -> (Vec Two a, Vec Two a)
splitVec4 (Some a (Some b rest)) = (Some a $ Some b None, rest)

splitVec8 :: forall (a :: Type). Vec Eight a -> (Vec Four a, Vec Four a)
splitVec8 (Some a (Some b (Some c (Some d rest)))) = (Some a . Some b . Some c $ Some d None, rest)

splitVec10 :: forall (a :: Type). Vec Ten a -> (Vec Five a, Vec Five a)
splitVec10 (Some a (Some b (Some c (Some d (Some e rest))))) = (Some a . Some b . Some c . Some d $ Some e None, rest)
