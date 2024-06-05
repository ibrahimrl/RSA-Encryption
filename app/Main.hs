import Data.Bits (shiftR)
import System.Random (randomRIO)

-- Compute the greatest common divisor (GCD) using Euclid's algorithm
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Extended Euclidean algorithm to find the modular inverse
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b =
  let (g, x1, y1) = extendedEuclid b (a `mod` b)
      x = y1
      y = x1 - (a `div` b) * y1
   in (g, x, y)

-- Find the modular inverse of e mod phi
modInverse :: Integer -> Integer -> Integer
modInverse e phi =
  let (_, x, _) = extendedEuclid e phi
   in (x `mod` phi + phi) `mod` phi

-- Generate a random prime number in the range
randomPrime :: Integer -> Integer -> IO Integer
randomPrime low high = do
  p <- randomRIO (low, high)
  if isPrime p then return p else randomPrime low high

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- [2 .. (floor . sqrt . fromIntegral) n], n `mod` x == 0]

-- Generate RSA keys
generateKeys :: IO ((Integer, Integer), (Integer, Integer))
generateKeys = do
  p <- randomPrime 1000 5000
  q <- randomPrime 1000 5000
  let n = p * q
  let phi = (p - 1) * (q - 1)
  e <- randomRIO (2, phi - 1)
  let d = modInverse e phi
  return ((e, n), (d, n))

-- Modular exponentiation
powMod :: Integer -> Integer -> Integer -> Integer
powMod base exp modulus = powMod' base exp modulus 1
  where
    powMod' _ 0 _ acc = acc
    powMod' b e m acc
      | e `mod` 2 == 1 = powMod' (b * b `mod` m) (e `div` 2) m (acc * b `mod` m)
      | otherwise = powMod' (b * b `mod` m) (e `div` 2) m acc

-- Encrypt a message
encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt m (e, n) = powMod m e n

-- Decrypt a message
decrypt :: Integer -> (Integer, Integer) -> Integer
decrypt c (d, n) = powMod c d n

-- Main function
main :: IO ()
main = do
  (publicKey, privateKey) <- generateKeys
  putStrLn $ "Public Key: " ++ show publicKey
  putStrLn $ "Private Key: " ++ show privateKey

  let message = 1234 -- Example message
  let encryptedMessage = encrypt message publicKey
  putStrLn $ "Encrypted Message: " ++ show encryptedMessage

  let decryptedMessage = decrypt encryptedMessage privateKey
  putStrLn $ "Decrypted Message: " ++ show decryptedMessage
