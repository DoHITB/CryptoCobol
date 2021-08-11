# CryptoCobol - AES
Cryptographic modules created on COBOL (GNU GPL v3 Liscense).
 
 
 
## What it offers?
This AES modules offers full functionality for AES
 * 128 / 192 / 256 BIT mode
 * ECB / CBC / CFB / OFB / CTR mode
 * Cipher / Decipher mode

Any possible combination is available and had been tested.

You can find `AESTEST` module that will test all modes for AES, having all results under `out.log`.



## How it works?
This system relays on four default interfaces
 * `DATA-GET`: Interface to get table-related data. If no interface is specified, system will call to `AESTGET`
 * `TEXT-GET`: Interface to get the input text. If no interface is specified, system will call to `AESXGET`
 * `TEXT-PUT`: Interface to put the output text. If no interface is specified, system will call to `AESTPUT`
 * `MSGE-PUT`: Logging interface. If no interface is specified, system will call to `AESMPUT`

Default interfaces works on file-system basis (`AESIDAT` for input file, `AESODAT` for output file, and `AESTAB` for table-related file).

Note that if custom interfaces are built (that's an expected behaviour), those interfaces **shall meet associated Copybook pattern found on `/AES/Copybook`**. Any developer can use the `INPUT` and `OUTPUT` areas (1023 chars wide each) to add custom data on each custom interface.

The main component to call is `AESMAIN` that will delegate on all above mentioned interfaces, and will perform operations thru `AESCORE` module.

All default interfaces and copybook are found on `/AES/Default` and `/AES/Copybook/Default`.


Here a picture of detailed flow can be found:

![AES Flow diagram](https://github.com/DoHITB/CryptoCobol/blob/main/AES/AES_Flow.png?raw=true "Title")


 * `Main program` will call to `AESMAIN` using `AESLMAIN` Copybook.
   * This `AESLMAIN` will have all necessary information to make it all work
     * `LMAIN-DATA-GET`: Interface name for `DATA-GET`.
     * `LMAIN-TEXT-GET`: Interface name for `TEXT-GET`.
     * `LMAIN-TEXT-PUT`: Interface name for `TEXT-PUT`.
     * `LMAIN-PUT-MESSAGE`: Interface name for `MSGE-PUT`.
     * `LMAIN-ACTION`: Can be CIPHER or DECIPHER.
     * `LMAIN-MODE`: Working mode. Can be any of ECB / CBC / CFB / OFB / CTR.
     * `LMAIN-BITS`: BIT length. Can be any of 128 / 192 / 256.
     * `LMAIN-KEY`: Cipher / Decipher key to be used, on HEX format.
     * `LMAIN-IV`: Cipher / Decipher IV to be used, on HEX format.
     * `LMAIN-MESSAGE-LEVEL`: Logging level we want to use. The bigger the number, the fewer output we will see.
     * `LMAIN-STATUS`: Return code for interface.
     * `LMAIN-EXTRA-DG`: Fill here your `DATA-GET` Copybook, it will be parsed on `AESMAIN`.
     * `LMAIN-EXTRA-TG`: Fill here your `TEXT-GET` Copybook, it will be parsed on `AESMAIN`.
     * `LMAIN-EXTRA-TP`: Fill here your `TEXT-PUT` Copybook, it will be parsed on `AESMAIN`.
     * `LMAIN-EXTRA-PM`: Fill here your `MSGE-PUT` Copybook, it will be parsed on `AESMAIN`.
 * `AESMAIN` will parse all interface data and will call first to `DATA-GET` to retreive both CIPHER-DATA and DECIPHER-DATA.
 * Then, it will call to `TEXT-GET` to get the input, and will relay on `AESCORE` to operate it, then put it on the output using `TEXT-PUT`.
   * This operation will repeat until `TEXT-GET` returns and EOF.
   * Additionally, `TEXT-GET` may return SKP value to make `AESMAIN` to skip the cycle and try again without ending the process. This can be usefull if the input text is a stream and we don't want to end the main routine.
 * Communication with `AESCORE` will be made using `AESLCOR` Copybook:
   * IO-ACTION: Can be (C)ipher, (D)ecipher, or (K)ey-Schedule.
   * IO-MODE: Can be any of ECB / CBC / CFB / OFB / CTR.
   * IO-BITS: Can be any of 128 / 192 / 256.
   * IO-TEXT: Input **and** output text.
   * IO-KEY: Key to perform the action.
   * IO-KSCH: Key Schedule output for "K" option.
   * IO-TAB: Cipher / Decipher tables (depending on action)
   * IO-PUTM: `MSGE-PUT` interface for logging



## Table-related data
In order to improve performance, this modules works with pre-calculated data.
There's `AESGEN` module that will craft the default file, that can be read under `AESTAB` Copybook (`/AES/Copybook/AESTAB`).

This data is expected to be provided by `DATA-GET` interface in two separated data items (`CIPHER-DATA` and `DECIPHER-DATA` respectively).
Again, is up to any developer to build a custom interface that holds `AESTAB` in any desired format, but it shall be on `AESTAB` specific format and order (Cipher - Decipher) when it's called from `AESMAIN`.



## Mentions
 * [FIPS Paper!](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf)
 * [XTime operation (explanation)!](https://www.usenix.org/legacy/publications/library/proceedings/cardis02/full_papers/valverde/valverde_html/node12.html)
 * [MixColumns (explanation)!](https://en.wikipedia.org/wiki/Rijndael_MixColumns)
 * [AES Modes (explanation)!](https://www.highgo.ca/2019/08/08/the-difference-in-five-modes-in-the-aes-encryption-algorithm/)
 * [AES Online (used during testing)!](https://the-x.cn/en-us/cryptography/Aes.aspx)



## Contact
As always, you can reach me at doscar.sole@gmail.com or @ESC_ILU at Twitter.
I will be glad to talk to you, or even help you if you have any doubt with the module or any topic related with AES or AES implementation.
