FROM i686/ubuntu

ENV LD_LIBRARY_PATH="/usr/local/lib"

RUN apt update # && apt -y upgrade
RUN apt install -y gauche wget make libgmp-dev gcc g++ autoconf automake make
RUN apt install -y re2c bison subversion autoconf automake git


# Oniguruma
RUN wget https://github.com/kkos/oniguruma/releases/download/v5.9.6/onig-5.9.6.tar.gz
RUN tar zvxf onig-5.9.6.tar.gz && cd onig-5.9.6 && ./configure && make && make install

# Mosh 0.2.7
RUN wget https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/mosh-scheme/mosh-0.2.7.tar.gz
RUN tar zvxf mosh-0.2.7.tar.gz && cd mosh-0.2.7 && ./configure && make && make install

CMD ["mosh"]
