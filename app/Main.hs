module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Random
import Data.Bits (shiftR)
import Data.Char (ord, chr)
import RSA

-- Main function to demonstrate encryption and decryption
main :: IO ()
main = do
  -- Generate RSA keys
  (publicKey, privateKey) <- generateKeys
  putStrLn $ "Public Key: " ++ show publicKey
  putStrLn $ "Private Key: " ++ show privateKey

  -- Save the keys to separate files
  savePublicKeyToFile publicKey "public_key.txt"
  savePrivateKeyToFile privateKey "private_key.txt"
  putStrLn "Public key saved to public_key.txt"
  putStrLn "Private key saved to private_key.txt"

  -- Initialize the encrypted messages map
  let encryptedMessages = Map.empty :: Map.Map String [Integer]

  -- Enter the loop to allow multiple entries
  mainLoop encryptedMessages publicKey privateKey

-- Loop function to handle multiple entries
mainLoop :: Map.Map String [Integer] -> (Integer, Integer) -> (Integer, Integer) -> IO ()
mainLoop encryptedMessages publicKey privateKey = do
  putStr "Do you want to encrypt or decrypt a message? (e/d): "
  hFlush stdout
  action <- getLine

  case action of
    "e" -> do
      putStr "Enter the message: "
      hFlush stdout
      message <- getLine

      putStrLn $ "Original message: " ++ message

      -- Encrypt the message
      let encrypted = encryptString message publicKey
      putStrLn $ "Encrypted message: " ++ show encrypted

      -- Add the encrypted message to the map
      let updatedMessages = Map.insert message encrypted encryptedMessages

      -- Save the encrypted messages to a file
      saveMessagesToFile updatedMessages "messages.txt"
      putStrLn "Encrypted messages saved to messages.txt"

      -- Continue the loop
      mainLoop updatedMessages publicKey privateKey
    "d" -> do
      putStr "Enter the encrypted message: "
      hFlush stdout
      encryptedMessageStr <- getLine
      let encryptedMessage = read encryptedMessageStr :: [Integer]

      -- Decrypt the message
      let decrypted = decryptString encryptedMessage privateKey
      putStrLn $ "Decrypted message: " ++ decrypted

      -- Continue the loop
      mainLoop encryptedMessages publicKey privateKey
    _ -> do
      putStrLn "Invalid action. Please choose 'encrypt' or 'decrypt'."
      mainLoop encryptedMessages publicKey privateKey

-- Save the public key to a file
savePublicKeyToFile :: (Integer, Integer) -> FilePath -> IO ()
savePublicKeyToFile publicKey filename = writeFile filename (show publicKey)

-- Save the private key to a file
savePrivateKeyToFile :: (Integer, Integer) -> FilePath -> IO ()
savePrivateKeyToFile privateKey filename = writeFile filename (show privateKey)

-- Load the public key from a file
loadPublicKeyFromFile :: FilePath -> IO (Integer, Integer)
loadPublicKeyFromFile filename = do
  content <- readFile filename
  return (read content)

-- Load the private key from a file
loadPrivateKeyFromFile :: FilePath -> IO (Integer, Integer)
loadPrivateKeyFromFile filename = do
  content <- readFile filename
  return (read content)

-- Save the encrypted messages to a file
saveMessagesToFile :: Map.Map String [Integer] -> FilePath -> IO ()
saveMessagesToFile messages filename = writeFile filename (show (Map.toList messages))

-- Load the encrypted messages from a file
loadMessagesFromFile :: FilePath -> IO (Map.Map String [Integer])
loadMessagesFromFile filename = do
  content <- readFile filename
  return (Map.fromList (read content))
