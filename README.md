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