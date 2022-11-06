FROM ubuntu:22.04

ENV LD_LIBRARY_PATH="/usr/local/lib"

WORKDIR /root

RUN apt update # && apt -y upgrade
RUN apt install -y gauche wget make libgmp-dev gcc g++ autoconf automake make libonig-dev
RUN apt install -y re2c bison subversion autoconf automake git gdb g++ pkg-config rlwrap

# Mosh 0.2.8
RUN wget https://github.com/higepon/mosh/releases/download/mosh-0.2.8-rc8/mosh-0.2.8-rc8.tar.gz
RUN tar zvxf mosh-0.2.8-rc8.tar.gz && cd mosh-0.2.8-rc8 && ./configure && make && make test && make install

# Checkout Mosh
# Using this inorder not to use cache.
#ADD http://worldclockapi.com/api/json/utc/now /etc/builddate
RUN git clone https://github.com/higepon/mosh.git mosh.git

# Try to build mosh.git using mosh binary built above. If this fails, please comment out this line and debug in the container.
RUN cd mosh.git && ./gen-git-build.sh && ./configure && make && make test

# Show git branch name.
RUN touch /root/.bashrc
RUN echo \
"parse_git_branch() {\n\
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\\\\1)/'\n\
}\n\
export PS1='\u@\h \[\e[32m\]\w \[\e[91m\]\$(parse_git_branch)\[\e[00m\]$ '\n" >> /root/.bashrc

CMD ["bash"]
