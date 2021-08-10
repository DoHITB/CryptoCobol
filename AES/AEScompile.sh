cobc -m AESMAIN.cbl
cobc -m AESTGET.cbl
cobc -m AESCORE.cbl
cobc -m AESMPUT.cbl
cobc -m AESTPUT.cbl
cobc -x -o AESTEST AESTEST.cbl AESMAIN.cbl AESTGET.cbl AESCORE.cbl AESMPUT.cbl AESTPUT.cbl
