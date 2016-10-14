-- Caesar cipher based on Jeremy Singer program.

import Data.Char

shouldCipher :: Char -> Bool
shouldCipher c = isLetter(c) && isAscii(c)

encipherChar :: Int -> Char -> Char
encipherChar n c
  | shouldCipher c = chr(ord(c) + adjustn)
  | otherwise = c
  where adjustn = let n' = n `mod` 26 in
          if (wraparround n' c) then (n' - 26) else n' 

encipher :: Int -> [Char] -> [Char]
encipher _ [] = []
encipher n (x:xs) = (encipherChar n x):(encipher n xs)

decipher :: Int -> [Char] -> [Char]
decipher n text = encipher (-n) text

wraparround :: Int -> Char -> Bool
wraparround n c
  | isLower(c) && ord(c)+n > ord('z') = True
  | isUpper(c) && ord(c)+n > ord('Z') = True
  | otherwise = False




