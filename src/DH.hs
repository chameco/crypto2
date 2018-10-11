module DH where

import DES

-- Prime from http://www.math.ucla.edu/~baker/40/handouts/rev_DH/node1.html
q :: Integer
q = 0xde9b707d4c5a4633c0290c95ff30a605aeb7ae864ff48370f13cf01c49adb9f23d19a439f743ee7703cf342d87f431105c843c78ca4df639931f3458fae8a94d1687e99a76ed99d0ba87189f42fd31ad8262c54a8cf5914ae6c28c540d714a5f6087a172fb74f4814c6f968d72386ef345a05180c3b3c7ddd5ef6fe76b0531c3 

alpha :: Integer
alpha = 2

stringToKey :: String -> Integer
stringToKey = foldl (\n x -> n * 2 + if x == '1' then 1 else 0) 0

computePublicKey :: Integer -> Integer
computePublicKey private = rem (alpha ^ private) q

computeSessionKey :: Integer -> Integer -> Integer
computeSessionKey public private = rem (public ^ private) q
