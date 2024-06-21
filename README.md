# RSA

## Introduction

The RSA Public Key Encryption System is a Haskell-based implementation of the RSA algorithm, showcasing key generation, encryption, and decryption. Named after its inventors Rivest, Shamir, and Adleman, this system uses a public and a private key to demonstrate public key cryptography and data security practices.

## Project Structure

#### This project includes several components:

- **Haskell Modules:**
  - [Main.hs](app/Main.hs): Contains the main executable logic, handling user inputs and orchestrating the encryption and decryption processes.
  - [RSA.hs](app/RSA.hs): Provides all the necessary cryptographic functions, including key generation, encryption, decryption, and helper functions for handling integers and strings.
  
- **Cabal Configuration:**
  - [RSA-public-key-encryption-system.cabal](RSA-public-key-encryption-system.cabal): Configures the Haskell project, dependencies, and build information.

- **Text Files:**
  - `public_key.txt` and `private_key.txt`: Files where the public and private keys are stored.
  - `messages.txt`: This file stores encrypted messages in a persistable format.

- **Additional Files:**
  - [LICENSE](LICENSE): Provides the licensing information.
  - [CHANGELOG.md](CHANGELOG.md): Documents the chronological development and updates of the project.

## Function Descriptions

### [RSA.hs](app/RSA.hs) Functions

- **`gcd`**: Computes the greatest common divisor of two integers using Euclid's algorithm, ensuring they are coprimes.
- **`extendedEuclid`**: Implements the Extended Euclidean Algorithm to find integers x and y such that they satisfy the equation ax + by = gcd(a, b).
- **`modInverse`**: Calculates the modular inverse of an integer e modulo φ, essential for deriving the private key in RSA.
- **`randomPrime`**: Generates a random prime number within a specified range, crucial for creating the RSA modulus.
- **`isPrime`**: Checks if a number is prime, a basic utility used during the generation of prime numbers for RSA.
- **`generateKeys`**: Generates a pair of RSA keys (public and private) using random prime numbers.
- **`findCoprime`**: Identifies an integer coprime to a given number, used to find a suitable public key exponent.
- **`powMod`**: Performs modular exponentiation, which is fundamental for RSA encryption and decryption operations.
- **`encrypt`**: Encrypts an integer message using a public key, applying the RSA encryption formula.
- **`decrypt`**: Decrypts an encrypted integer using a private key, reversing the RSA encryption process.
- **`stringToInts`**: Converts a string to a list of integers based on ASCII values, preparing text for encryption.
- **`intsToString`**: Converts a list of integers back to a string, reconstructing text from decrypted ASCII values.
- **`encryptString`**: Encrypts a string by converting it to integers and encrypting each integer.
- **`decryptString`**: Decrypts a list of encrypted integers to a string, combining decryption and ASCII conversion.

### [Main.hs](app/Main.hs) Functions

- **`main`**: Initializes the application, generates keys, and starts the interactive encryption/decryption session.
- **`mainLoop`**: Manages user interaction for ongoing encryption and decryption tasks within a loop.
- **`savePublicKeyToFile`**: Saves the RSA public key to a file.
- **`savePrivateKeyToFile`**: Saves the RSA private key to a file.
- **`loadPublicKeyFromFile`**: Loads the RSA public key from a file.
- **`loadPrivateKeyFromFile`**: Loads the RSA private key from a file.
- **`saveMessagesToFile`**: Saves encrypted messages to a file for persistent storage.
- **`loadMessagesFromFile`**: Loads encrypted messages from a file, allowing for decryption or review.


## Setup

To set up the RSA Public Key Encryption System, ensure you have the GHC (Glasgow Haskell Compiler) and Cabal installed. Proceed with the following steps:

1. **Clone the repository**:
    ```bash
    git clone https://github.com/ibrahimrl/RSA-Encryption.git
    ```
2. **Navigate to the project directory:**
   ```bash
    cd RSA-Encryption
   ```
3. **Build the project using Cabal**:
    ```bash
    cabal build
   ```
4. **Run the executable:**:
    ```bash
    cabal run
   ```

## Usage

#### Key Generation

Upon initial startup, the system automatically generates a public and private key, saving them in `public_key.txt` and `private_key.txt`.

#### Encrypting Messages

1. Run the application.

2. Select 'encrypt' by typing `e` when prompted.

3. Enter the message you wish to encrypt and press enter.
4. The encrypted message will be displayed and saved to `messages.txt`.

#### Decrypting Messages

1. Run the application.

2. Select 'decrypt' by typing `d` when prompted.

3. Enter the encrypted message. Copy this from `messages.txt` or your previous output.
4. The program will display the decrypted message, revealing the original text.

#### Managing Keys

- **View Keys**: Open `public_key.txt` and `private_key.txt` to view your RSA keys.

- **Regenerate Keys**: Delete the existing key files and restart the application to generate new keys.

## Additional Information

- **Security Note**: This implementation is intended for demonstration purposes. The generated keys may need to be larger for secure applications.

## Contact

For queries or contributions, please contact the project maintainer at [ibrahim.rahimli20016@gmail.com](mailto:ibrahim.rahimli20016@gmail.com)

## License

This project is licensed under the [MIT License](LICENSE).