# CryptoCobol
Cryptographic modules created on COBOL.

## Why?
Why NOT?
I mean, there's a need to use cryptographic modules on COBOL, and as there's no public solutions, I decided to go by myself.
All this modules are free to download and use, and closed releases are NOT allowed as they are under GNU GPL v3 License. With this "restriction" the goal is to ensure it will be always open to all users.

## What functions cover this repository
 * SHA-1: The classical hash algorithm, fully tested.
 * SHA-512: The newest hash algorithm (it's still on BETA, so be aware of using it). It will be fully tested soon.
 * So many other modules yet to come. Do you need one? Just tell me!

## SHA-1
It works as a standalone module. You may inform data on LINKAGE-SECTION (on ASCII mode), then call module, and it will return the HEX hash.

## SHA-256
It works as a standalone module. The final functionality is yet to be closed and tested but you will need

 * A LRECL 64 file (that's a file with a RECORD LENGHT of 64 characters)
   * This file will host binary-representation of the data to be hashed
   * A interface will be provided to create this file

After you have the file, just put the LOGICAL name on LINKAGE-SECTION and call the module. It will return the HEX hash.
