FROM archlinux:latest

ENV LD_LIBRARY_PATH="/usr/local/lib"

WORKDIR /root

RUN pacman -Sy
# Dependencies to build Mosh 0.2.8.
RUN pacman -S --noconfirm wget
RUN pacman -S --noconfirm gcc
RUN pacman -S --noconfirm make
RUN pacman -S --noconfirm pkg-config
RUN pacman -S --noconfirm oniguruma

# Additional dependencies to build Mosh head.
RUN pacman -S --noconfirm gauche autoconf automake re2c bison git gdb

# Mosh 0.2.8 RC7
RUN wget https://github.com/higepon/mosh/releases/download/mosh-0.2.8-rc7/mosh-0.2.8-rc7.tar.gz
RUN tar zvxf mosh-0.2.8-rc7.tar.gz && cd mosh-0.2.8-rc7 && ./configure && make && make test && make install

# Checkout Mosh
RUN git clone https://github.com/higepon/mosh.git mosh.git
# Using this inorder not to use cache.
ADD http://worldclockapi.com/api/json/utc/now /etc/builddate

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
