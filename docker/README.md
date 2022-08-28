### What is this?
Docker containers mainly for Mosh developers. In each subdirectory you should be able to build a) Released stable version of Mosh and b) the git head of Mosh in the architecture. b) may fail due to the change in the head but the containers should help the debugging.

### How to use


```
# Pick the architecture to use.
cd ubuntu_22.04_amd64

# Start and build the container
make start

# Attach to the running container and show bash prompt
make attach

# Stop the container
make stop

# Stop and remove the container
make clean
```

You can also use Visual Studio Code - "Attach to Running Container..." after you run ```make start```.
