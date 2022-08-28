FROM ubuntu:22.04

ENV LD_LIBRARY_PATH="/usr/local/lib"

WORKDIR /root

RUN apt update # && apt -y upgrade
RUN apt install -y gauche wget make libgmp-dev gcc g++ autoconf automake make
RUN apt install -y re2c bison subversion autoconf automake git gdb g++

# Oniguruma
RUN wget https://github.com/kkos/oniguruma/releases/download/v5.9.6/onig-5.9.6.tar.gz
RUN tar zvxf onig-5.9.6.tar.gz && cd onig-5.9.6 && rm config.sub && rm config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.guess && wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.sub && ./configure && make && make install

# Mosh 0.2.8 RC3
RUN wget https://github.com/higepon/mosh/releases/download/mosh-0.2.8-rc3/mosh-0.2.8-rc3.tar.gz
RUN tar zvxf mosh-0.2.8-rc3.tar.gz && cd mosh-0.2.8-rc3 && ./configure && make && make test && make install

# Checkout Mosh
# Using this inorder not to use cache.
ADD http://worldclockapi.com/api/json/utc/now /etc/builddate
RUN git clone https://github.com/higepon/mosh.git mosh.git

# Try to build mosh.git using mosh binary built above. If this fails, please comment out this line and debug in the container.
RUN cd mosh.git && ./gen-git-build.sh && ./configure && make && make test

CMD ["bash"]
