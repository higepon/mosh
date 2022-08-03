# What is Mosh?
Mosh is a free and fast interpreter for Scheme as specified in the R6RS.(R6RS is the latest revision of the Scheme standard)
The current release of Mosh supports all of the features R6RS.
See detailed information on http://mosh.monaos.org.

# Building and Installing Mosh
Get a release of Mosh from [Download](http://code.google.com/p/mosh-scheme/downloads/list). The [development head version](http://storage.osdev.info/pub/mosh/mosh-current.tar.gz) is also available.

See [INSTALL](https://github.com/higepon/mosh/blob/master/INSTALL) and [doc](https://github.com/higepon/mosh/tree/master/doc) for prerequisite packages.

    % ./configure
    % make
    % make check
    % make install

On Windows, see [Build](http://mosh.monaos.org/files/doc/text/Download-txt.html).

# Building the cutting-edge Mosh
## Docker
We have Docker support to setup Mosh devlopment enviroment. Please see docker/dev directory.
## Requirements

- [Mosh current](http://storage.osdev.info/pub/mosh/mosh-current.tar.gz)
- [Gauche](http://practical-scheme.net/gauche/)
- [re2c](http://re2c.org/) 0.13.5 or higher
- [bison](http://www.gnu.org/software/bison/)
- [subversion](http://subversion.tigris.org/)
- [autoconf](http://www.gnu.org/software/autoconf/) 2.63 or higher
- [automake](http://www.gnu.org/software/automake/)

### auto tools
Following autotools version works.

- autoconf 2.65 and automake 1.11.
- autoconf 2.63 and automake 1.10.

## Building

    % git clone git://github.com/higepon/mosh.git
    % cd mosh
    % ./gen-git-build.sh
    % ./configure
    % make
    % make check
    % make install

If you have issues during the `gen-git-build.sh` step, check that you have all
the build dependencies installed.  Messages about unbound variables, presently
`eq-hashtable-copy`, are symptomatic of this.

## Building on OSX Lion

    % CFLAGS='-arch i386 -m32'  ./configure --prefix=~/lib-for-mosh && make && make install # 32bit oniguruma
    % ABI=32 ./configure --prefix=~/lib-for-mosh && make && make install # 32bit libgmp
    % git clone git://github.com/higepon/mosh.git
    % cd mosh
    % export PATH=/Users/taro/lib-for-mosh/bin:$PATH # for onig-config
    % export CC=clang
    % export CXX=clang++
    % ./gen-git-build.sh
    % CFLAGS='-arch i386' CXXFLAGS='-arch i386' LDFLAGS="-L/Users/higepon/lib-for-mosh/lib"  ./configure --without-nmosh-defaults
    % make
    % make check
    % make install
