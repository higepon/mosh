## What is Mosh?
Mosh is a free and fast interpreter for Scheme as specified in the R6RS.(R6RS is the latest revision of the Scheme standard)
The current release of Mosh supports all of the features R6RS.
See detailed information on http://mosh.monaos.org.

## Building and Installing Mosh
Get a release of Mosh from [Download](https://github.com/higepon/mosh/releases).

### macOS
#### Install Dependences
```sh
% brew install gmp oniguruma
```

#### Build and Install
```sh
% export LDFLAGS=-L/opt/homebrew/lib
% export CFLAGS=-I$/opt/homebrew/include
% export CXXFLAGS=-I/opt/homebrew/include
% ./configure
% make
% make test
% sudo make install
```

### Ubuntu
#### Install Dependences
```sh
# gmp
% apt install libgmp-dev 

# oniguruma
% wget https://github.com/kkos/oniguruma/releases/download/v5.9.6/onig-5.9.6.tar.gz
% tar zvxf onig-5.9.6.tar.gz
% cd onig-5.9.6
% ./configure
% make
% make test
% sudo make install
```

### Other Platforms
Please see [doc](https://github.com/higepon/mosh/tree/master/doc) and [INSTALL](https://github.com/higepon/mosh/blob/master/INSTALL).

## Building mosh.git
To build mosh.git you need release version mosh installed. You can see Please use and check [docker/dev](https://github.com/higepon/mosh/tree/master/docker/) to build.