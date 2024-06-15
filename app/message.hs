module Main where

import Data.Bits (xor)
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO (hFlush, readFile, stdout, writeFile)
import System.Random

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

-- Save the keys to a file with an index
saveKeysToFile :: [(Int, [Int])] -> FilePath -> IO ()
saveKeysToFile keys filename = writeFile filename (unlines (map show keys))

-- Load the keys from a file
loadKeysFromFile :: FilePath -> IO [(Int, [Int])]
loadKeysFromFile filename = do
  content <- readFile filename
  return (map read (lines content))

-- Save the encrypted messages to a file
saveMessagesToFile :: Map.Map Int String -> FilePath -> IO ()
saveMessagesToFile messages filename = writeFile filename (show (Map.toList messages))

-- Load the encrypted messages from a file
loadMessagesFromFile :: FilePath -> IO (Map.Map Int String)
loadMessagesFromFile filename = do
  content <- readFile filename
  return (Map.fromList (read content))

-- Main function to demonstrate encryption and decryption
main :: IO ()
main = do
  -- Initialize the encrypted messages map
  let encryptedMessages = Map.empty :: Map.Map Int String
      keys = []

  -- Enter the loop to allow multiple entries
  mainLoop encryptedMessages keys 1

-- Loop function to handle multiple entries
mainLoop :: Map.Map Int String -> [(Int, [Int])] -> Int -> IO ()
mainLoop encryptedMessages keys index = do
  putStr "Do you want to encrypt or decrypt a message? (e/d): "
  hFlush stdout
  action <- getLine

  case action of
    "e" -> do
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

      -- Add the encrypted message and key to their respective storages
      let updatedMessages = Map.insert index encrypted encryptedMessages
          updatedKeys = (index, key) : keys

      -- Save the keys to a file
      saveKeysToFile updatedKeys "keys.txt"
      putStrLn "Keys saved to keys.txt"

      -- Save the encrypted messages to a file
      saveMessagesToFile updatedMessages "messages.txt"
      putStrLn "Encrypted messages saved to messages.txt"

      -- Continue the loop
      mainLoop updatedMessages updatedKeys (index + 1)
    "d" -> do
      putStr "Enter the key (e.g., [123, 34, 56, ...]): "
      hFlush stdout
      keyStr <- getLine
      let key = read keyStr :: [Int]

      -- Load the keys from the file
      storedKeys <- loadKeysFromFile "keys.txt"

      -- Load the encrypted messages from the file
      storedMessages <- loadMessagesFromFile "messages.txt"

      -- Find the index corresponding to the given key
      let maybeIndex = lookupKeyByValue storedKeys key

      case maybeIndex of
        Just idx -> do
          let encryptedMessage = fromMaybe "" (Map.lookup idx storedMessages)
          let decrypted = xorProcess encryptedMessage key
          putStrLn $ "Decrypted message: " ++ decrypted
        Nothing -> putStrLn "No message found for the given key."

      -- Continue the loop
      mainLoop encryptedMessages keys index
    _ -> do
      putStrLn "Invalid action. Please choose 'encrypt' or 'decrypt'."
      mainLoop encryptedMessages keys index

-- Helper function to find the key by its value
lookupKeyByValue :: (Eq b) => [(a, b)] -> b -> Maybe a
lookupKeyByValue [] _ = Nothing
lookupKeyByValue ((k, v) : xs) val
  | v == val = Just k
  | otherwise = lookupKeyByValue xs val
