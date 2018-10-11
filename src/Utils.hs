module Utils where

import Data.Kind (Type)

-- Reversed function composition
(>>>) :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)

-- Swapping ordered pairs
swap :: forall (a :: Type) (b :: Type). (a, b) -> (b, a)
swap (l, r) = (r, l)

-- Applying a function to the left side of an ordered pair
left :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b) -> ((a, c) -> (b, c))
left f (l, r) = (f l, r)

-- Applying a function to the right side of an ordered pair
right :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b) -> ((c, a) -> (c, b))
right f (l, r) = (l, f r)

pad :: Integer -> [Bool] -> [Bool]
pad n l | fromIntegral (length l) >= n = l
        | otherwise = pad n (False:l)
