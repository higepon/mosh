### What is this?
Docker container to build and test Mosh on Ubuntu 22.04.

### How to use
```
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
