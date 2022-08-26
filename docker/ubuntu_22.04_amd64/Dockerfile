FROM ubuntu:22.04

ENV LD_LIBRARY_PATH="/usr/local/lib"

WORKDIR /root

RUN apt update # && apt -y upgrade
RUN apt install -y gauche wget make libgmp-dev gcc g++ autoconf automake make
RUN apt install -y re2c bison subversion autoconf automake git gdb

# Boehm GC in mosh-0.2.7 requires older gcc & g++
RUN apt install -y gcc-9 g++-9

# Oniguruma
RUN wget https://github.com/kkos/oniguruma/releases/download/v5.9.6/onig-5.9.6.tar.gz
RUN tar zvxf onig-5.9.6.tar.gz && cd onig-5.9.6 && rm config.sub && rm config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.sub && ./configure && make && make install

# Checkout Mosh
RUN git clone https://github.com/higepon/mosh.git mosh.git

# Mosh 0.2.8
RUN wget https://github.com/higepon/mosh/releases/download/mosh-0.2.8-rc2/mosh-0.2.8-rc2.tar.gz
RUN tar zvxf mosh-0.2.8-rc2.tar.gz && cd mosh-0.2.8-rc2 && ./configure # && make && make install


# Mosh 0.2.7
RUN wget https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/mosh-scheme/mosh-0.2.7.tar.gz
RUN tar zvxf mosh-0.2.7.tar.gz && cd mosh-0.2.7 && rm config.sub && rm config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.sub && ./configure CC=gcc-9 CXX=g++-9 && make

CMD ["bash"]
