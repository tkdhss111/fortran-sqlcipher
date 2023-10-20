# Fortran Interfaces for SQLCipher 

## SqlCipher Installation

### OpenSSL

    sudo apt update
    sudo apt install openssl
    sudo apt install openssl-dev
    sudo apt install libssl-dev

### Git Clone Source Code

    git clone https://github.com/sqlcipher/sqlcipher.git

### Static Library


N.B.   
The following LDFLAGS has been changed to path obtained from "which libcrypto.a".

    ./configure --enable-tempstore=yes CFLAGS="-DSQLITE_HAS_CODEC" \
    	LDFLAGS="/usr/lib/x86_64-linux-gnu/libcrypto.a"

    make

### Dynamic Library

     ./configure --enable-tempstore=yes CFLAGS="-DSQLITE_HAS_CODEC" \
    	LDFLAGS="-lcrypto"

    make

### Test

    sudo apt install tcl-dev

    cp /usr/include/tcl/*.h ./

    ./configure --enable-tempstore=yes --enable-fts5 CFLAGS="-DSQLITE_HAS_CODEC -DSQLCIPHER_TEST" \
	LDFLAGS="-lcrypto"

    make testfixture

    ./testfixture test/sqlcipher.test

    sudo make install

## Fortran Interfaces

### Compilation
Cmake and Ninja are used for compilation.

    make

### Test

    make test

### Installation

    sudo make install
