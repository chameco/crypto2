module DES where

import Data.Kind (Type)
import Data.Word (Word8)
import qualified Data.ByteString as BS

import Vec
import Bits
import Utils

-- The initial permutation
initialPermutation :: Bits Eight -> Bits Eight
initialPermutation = permuteBits table
  where table :: Perm Eight Seven
        table = Some f1 . Some f5 . Some f2 . Some f0 . Some f3 . Some f7 . Some f4 $ Some f6 None

-- Compute the value of an f-box from a given 8-bit partial key.
-- Notice how verbose the substitution tables are: given the nonlinearity of the
-- function and the details of our bitstring representation, it is easiest to
-- simply match every possibe 4-bit value. In a full DES implementation, we would
-- likely want to write the necessary conversion functions to convert arbitrary
-- bitstrings into indices in a constant vector of output values (the C way, but
-- safer), but for now, we choose to avoid writing the (verbose) type-level
-- machinery.
-- Also notice the elegance of the actual definition from its components using
-- (>>>) and left/right. Working in pointfree/concatenative style makes this
-- definition exactly mirror the diagram given in the lecture notes!
fbox :: Bits Eight -> Bits Four -> Bits Four
fbox key = expansionPermutation >>> xorBits key >>> splitVec8 >>> left substitution1 >>> right substitution2 >>> uncurry concatVec >>> finalPermutation
  where expansionPermutation :: Bits Four -> Bits Eight
        expansionPermutation bs = concatVec (permuteBits table1 bs) (permuteBits table2 bs)
          where table1 :: Perm Four Three
                table1 = Some f3 . Some f0 . Some f1 $ Some f2 None
                table2 :: Perm Four Three
                table2 = Some f1 . Some f2 . Some f3 $ Some f0 None
        substitution1 :: Bits Four -> Bits Two
        substitution1 (Some False (Some False (Some False (Some False None)))) = Some False $ Some True None
        substitution1 (Some False (Some False (Some False (Some True None))))  = Some True  $ Some True None
        substitution1 (Some False (Some False (Some True (Some False None))))  = Some False $ Some False None
        substitution1 (Some False (Some False (Some True (Some True None))))   = Some True  $ Some False None
        substitution1 (Some False (Some True (Some False (Some False None))))  = Some True  $ Some True None
        substitution1 (Some False (Some True (Some False (Some True None))))   = Some False $ Some True None
        substitution1 (Some False (Some True (Some True (Some False None))))   = Some True  $ Some False None
        substitution1 (Some False (Some True (Some True (Some True None))))    = Some False $ Some False None
        substitution1 (Some True (Some False (Some False (Some False None))))  = Some False $ Some False None
        substitution1 (Some True (Some False (Some False (Some True None))))   = Some True  $ Some True None
        substitution1 (Some True (Some False (Some True (Some False None))))   = Some True  $ Some False None
        substitution1 (Some True (Some False (Some True (Some True None))))    = Some False $ Some True None
        substitution1 (Some True (Some True (Some False (Some False None))))   = Some False $ Some True None
        substitution1 (Some True (Some True (Some False (Some True None))))    = Some True  $ Some True None
        substitution1 (Some True (Some True (Some True (Some False None))))    = Some True  $ Some True None
        substitution1 (Some True (Some True (Some True (Some True None))))     = Some True  $ Some False None
        substitution2 :: Bits Four -> Bits Two
        substitution2 (Some False (Some False (Some False (Some False None)))) = Some False $ Some False None
        substitution2 (Some False (Some False (Some False (Some True None))))  = Some True  $ Some False None
        substitution2 (Some False (Some False (Some True (Some False None))))  = Some False $ Some True None
        substitution2 (Some False (Some False (Some True (Some True None))))   = Some False $ Some False None
        substitution2 (Some False (Some True (Some False (Some False None))))  = Some True  $ Some False None
        substitution2 (Some False (Some True (Some False (Some True None))))   = Some False $ Some True None
        substitution2 (Some False (Some True (Some True (Some False None))))   = Some True  $ Some True None
        substitution2 (Some False (Some True (Some True (Some True None))))    = Some True  $ Some True None
        substitution2 (Some True (Some False (Some False (Some False None))))  = Some True  $ Some True None
        substitution2 (Some True (Some False (Some False (Some True None))))   = Some True  $ Some False None
        substitution2 (Some True (Some False (Some True (Some False None))))   = Some False $ Some False None
        substitution2 (Some True (Some False (Some True (Some True None))))    = Some False $ Some True None
        substitution2 (Some True (Some True (Some False (Some False None))))   = Some False $ Some True None
        substitution2 (Some True (Some True (Some False (Some True None))))    = Some False $ Some False None
        substitution2 (Some True (Some True (Some True (Some False None))))    = Some False $ Some False None
        substitution2 (Some True (Some True (Some True (Some True None))))     = Some True  $ Some True None
        finalPermutation :: Bits Four -> Bits Four
        finalPermutation = permuteBits table
          where table :: Perm Four Three
                table = Some f1 . Some f3 . Some f2 $ Some f0 None

-- A single round of Feistel transformation given an 8-bit partial key.
feistel :: Bits Eight -> (Bits Four, Bits Four) -> (Bits Four, Bits Four)
feistel key (l, r) = (r, xorBits l (fbox key r))

-- The final permutation ("undoing" the initial permutation).
inverseInitialPermutation :: Bits Eight -> Bits Eight
inverseInitialPermutation = permuteBits table
  where table :: Perm Eight Seven
        table = Some f3 . Some f0 . Some f2 . Some f4 . Some f6 . Some f1 . Some f7 $ Some f5 None

-- Perform DES given the 8-bit two partial keys.
-- This allows us to abstract the common details of encryption and decryption,
-- which only differ in the order of the partial keys.
doDES :: (Bits Eight, Bits Eight) -> Bits Eight -> Bits Eight
doDES (k1, k2) = initialPermutation >>> splitVec8 >>> feistel k1 >>> feistel k2 >>> swap >>> uncurry concatVec >>> inverseInitialPermutation

-- Compute the two 8-bit partial keys from a full 10-bit key.
splitKey :: Bits Ten -> (Bits Eight, Bits Eight)
splitKey key = (left (uncurry concatVec >>> key8Permutation) >>> right (left rotateLeft >>> right rotateLeft >>> uncurry concatVec >>> key8Permutation)) (fives, fives)
  where fives = (key10Permutation >>> splitVec10 >>> left rotateLeft >>> right rotateLeft) key
        key10Permutation :: Bits Ten -> Bits Ten
        key10Permutation = permuteBits table
          where table :: Perm Ten Nine
                table = Some f2 . Some f4 . Some f1 . Some f6 . Some f3 . Some f9 . Some f0 . Some f8 . Some f7 $ Some f5 None
        key8Permutation :: Bits Ten -> Bits Eight
        key8Permutation = popVec >>> popVec >>> permuteBits table
          where table :: Perm Eight Seven
                table = Some f3 . Some f0 . Some f4 . Some f1 . Some f5 . Some f2 . Some f7 $ Some f6 None 
        rotateLeft :: Bits Five -> Bits Five
        rotateLeft (Some b bs) = concatVec bs (Some b None)

-- Helper for performing encryption of a byte given a 10-bit key.
encryptDES :: Bits Ten -> Bits Eight -> Bits Eight
encryptDES = splitKey >>> doDES

-- Helper for performing decryption of a byte given a 10-bit key.
-- As previously mentioned, the only difference is swapping the partial keys!
decryptDES :: Bits Ten -> Bits Eight -> Bits Eight
decryptDES = splitKey >>> swap >>> doDES

-- Command-line interface ------------------------------------------------------

-- Convert an 8-bit bit string to a Haskell integer.
bitsToInt :: forall (n :: Nat). Bits n -> Int
bitsToInt bits = go (lengthVec bits - 1) bits
  where go :: forall (n :: Nat). Int -> Bits n -> Int
        go _ None = 0
        go l (Some b bs) = (if b then 1 else 0) * 2^l + go (l - 1) bs

-- Convert a Haskell integer to a bit string.
-- If the Haskell integer is too large, use the highest n bits.
intToBits :: forall (a :: Type). ([Bool] -> a) -> Integer -> a
intToBits extract = extract . reverse . rep
  where rep :: Integer -> [Bool]
        rep 0 = []
        rep n = (rem n 2 == 1):rep (quot n 2)

intToBits8 :: Integer -> Bits Eight
intToBits8 = intToBits (extract . pad 10)
  where extract :: [Bool] -> Bits Eight
        extract (b1:b2:b3:b4:b5:b6:b7:b8:_) = Some b1 . Some b2 . Some b3 . Some b4 . Some b5 . Some b6 . Some b7 $ Some b8 None
        extract _ = error "Invalid byte"

intToBits10 :: Integer -> Bits Ten
intToBits10 = intToBits (extract . pad 10)
  where extract :: [Bool] -> Bits Ten
        extract (b1:b2:b3:b4:b5:b6:b7:b8:b9:b10:_) = Some b1 . Some b2 . Some b3 . Some b4 . Some b5
                                                     . Some b6 . Some b7 . Some b8 . Some b9 $ Some b10 None
        extract _ = error "Invalid byte"

intToString10 :: Integer -> String
intToString10 = intToBits (fmap (\x -> if x then '1' else '0') . take 10 . pad 10)

-- Encrypt a single byte using the given key.
encryptByte :: Bits Ten -> Word8 -> Word8
encryptByte key = fromIntegral . bitsToInt . encryptDES key . intToBits8 . fromIntegral

-- Decrypt a single byte using the given key.
decryptByte :: Bits Ten -> Word8 -> Word8
decryptByte key = fromIntegral . bitsToInt . decryptDES key . intToBits8 . fromIntegral

-- Encrypt an entire bytestring using the given key.
encryptByteString :: Bits Ten -> BS.ByteString -> BS.ByteString
--encryptByteString key = BS.map (encryptByte key)
encryptByteString _ = id

-- Decrypt an entire bytestring using the given key.
decryptByteString :: Bits Ten -> BS.ByteString -> BS.ByteString
--decryptByteString key = BS.map (decryptByte key)
decryptByteString _ = id

buildKey :: String -> Bits Ten
buildKey = extract . fmap (=='1')
  where extract :: [Bool] -> Bits Ten
        extract (b1:b2:b3:b4:b5:b6:b7:b8:b9:b10:_) = Some b1 . Some b2 . Some b3 . Some b4 . Some b5
                                                     . Some b6 . Some b7 . Some b8 . Some b9 $ Some b10 None
        extract _ = error "Invalid key"
