# CryptoCobol - AES
Cryptographic modules created on COBOL (GNU GPL v3 Liscense).
 
## What it offers?
This AES modules offers full functionality for AES
 * 128 / 192 / 256 BIT mode
 * ECB / CBC / CFB / OFB / CTR mode
 * Cipher / Decipher mode

Any possible combination is available and had been tested.

You can find `AESTEST` module that will test all modes for AES.

## How it works?
This system relays on four default interfaces
 * `DATA-GET`: Interface to get table-related data. If no interface is specified, system will call to `AESTGET`
 * `TEXT-GET`: Interface to get the input text. If no interface is specified, system will call to `AESXGET`
 * `TEXT-PUT`: Interface to put the output text. If no interface is specified, system will call to `AESTPUT`
 * `MSGE-PUT`: Logging interface. If no interface is specified, system will call to `AESMPUT`

Default interfaces works on file-system basis (`AESIDAT` for input file, `AESODAT` for output file, and `AESTAB` for table-related file).
Note that if custom interfaces are built (that's an expected behaviour), those interfaces may meet associated Copybook pattern found on `/AES/Copybook`. Any developer can use the `INPUT` and `OUTPUT` areas (1023 chars wide each) to add custom data on each custom interface.

The main component to call is `AESMAIN` that will delegate on all above mentioned interfaces, and will perform operations thru `AESCORE` module.

All default interfaces and copybook are found on `/AES/Default` and `/AES/Copybook/Default`.


## Table-related data
In order to improve performance, this modules works with pre-calculated data.
There's `AESGEN` module that will craft the default file, that can be read under `AESTAB` Copybook (`/AES/Copybook/AESTAB`).

This data is expected to be provided by `DATA-GET` interface in two separated data items (`CIPHER-DATA` and `DECIPHER-DATA` respectively).
Again, is up to any developer to build a custom interface that holds `AESTAB` in any desired format, but it shall be on `AESTAB` specific format and order (Cipher - Decipher) when it's called from `AESMAIN`.


## Mentions
 * FIPS Paper: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
 * XTime operation (explanation): https://www.usenix.org/legacy/publications/library/proceedings/cardis02/full_papers/valverde/valverde_html/node12.html
 * MixColumns (explanation): https://en.wikipedia.org/wiki/Rijndael_MixColumns
 * AES Modes: https://www.highgo.ca/2019/08/08/the-difference-in-five-modes-in-the-aes-encryption-algorithm/
 * AES Online (used during testing): https://the-x.cn/en-us/cryptography/Aes.aspx

## Contact
As always, you can reach me at doscar.sole@gmail.com or @ESC_ILU at Twitter.
I will be glad to talk to you, or even help you if you have any doubt with the module or any topic related with AES or AES implementation.
