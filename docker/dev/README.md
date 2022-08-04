 ## What is this?
 This docker is supposed to be used by Mosh developers who need to bootstrap & build Mosh (See [Building the cutting-edge Mosh](https://github.com/higepon/mosh/blob/master/README.md#building-the-cutting-edge-mosh)
 In this image
 - All required libraries & binaries are pre-installed.
 - Mosh 0.2.7 is pre-installed in /usr/local/bin.

## How to use
### Build the image
```
$ docker-compose up -d
$ docker-compose ps
$ docker-compose exec mosh-0.2.7-bootstrap bash
```
### Build mosh head using pre-installed mosh-0.2.7
```
$ git clone https://github.com/higepon/mosh.git
$ cd mosh
$ ./gen-git-build.sh
$ ./configure
$ make
$ make testR
```
