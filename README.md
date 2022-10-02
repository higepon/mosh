## What is Mosh?
Mosh is a free and fast interpreter for Scheme as specified in the R6RS.
The current release of Mosh supports all of the features R6RS.
See detailed information on http://mosh.monaos.org.

## Building and Installing Mosh
Get a release of Mosh from [Download](https://github.com/higepon/mosh/releases).

### macOS
#### Install Dependences
```sh
% brew install pkg-config gmp oniguruma
```

#### Build and Install
```sh
% ./configure
% make
% make test
% sudo make install
```

### Ubuntu

#### Install Dependences
```sh
% apt install pkg-config libgmp-dev libonig-dev
```

#### Build and Install
```sh
% ./configure
% make
% make test
% sudo make install
```

### Other Platforms
Please see [doc](https://github.com/higepon/mosh/tree/master/doc) and [INSTALL](https://github.com/higepon/mosh/blob/master/INSTALL).

## For mosh developers
### Building mosh.git
To build mosh.git you need to have release version mosh and some other tools installed. Please use and see [docker/dev](https://github.com/higepon/mosh/tree/master/docker/) to build.

### CI
[![Build](https://github.com/higepon/mosh/actions/workflows/build.yml/badge.svg)](https://github.com/higepon/mosh/actions/workflows/build.yml)

### Make a release.
1. Update verions in configur.ac.
1. Make sure GitHub Actions are all green.
1. ```git tag mosh-0.2.8-rc3 -a -m "mosh-0.2.8-rc3"``` and ```git push origin mosh-0.2.8-rc3``` will trigger the release build.

