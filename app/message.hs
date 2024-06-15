module Main where

import Data.Bits (xor)
import Data.Char (chr, ord)
import System.Random
import System.IO (hFlush, stdout)

-- Convert a string to a list of integers (based on ASCII values)
stringToInts :: String -> [Int]
stringToInts = map ord

-- Convert a list of integers back to a string
intsToString :: [Int] -> String
intsToString = map chr

-- Encrypt or decrypt a message with a given key
xorProcess :: String -> [Int] -> String
xorProcess message key = intsToString $ zipWith xor (stringToInts message) key

-- Generate a random key
generateKey :: Int -> IO [Int]
generateKey size = mapM (\_ -> randomRIO (0, 255)) [1 .. size]

-- Main function to demonstrate encryption and decryption
main :: IO ()
main = do
  putStr "Enter the message: "
  hFlush stdout
  message <- getLine

  putStrLn $ "Original message: " ++ message

  -- Generate a random key
  key <- generateKey (length message)
  putStrLn $ "Generated Key: " ++ show key

  -- Encrypt the message
  let encrypted = xorProcess message key
  putStrLn $ "Encrypted message: " ++ encrypted

  -- Decrypt the message
  let decrypted = xorProcess encrypted key
  putStrLn $ "Decrypted message: " ++ decrypted
