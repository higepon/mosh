## MNIST demo in R7RS
MNIST demo written in R7RS. This work is based on the Python code in https://github.com/oreilly-japan/deep-learning-from-scratch. The demo can classify handwritten digits.

## How to run
-　Dowload the train and test data from [THE MNIST DATABASE](http://yann.lecun.com/exdb/mnist/)
  - train-images-idx3-ubyte.gz:  training set images (9912422 bytes)
  - train-labels-idx1-ubyte.gz:  training set labels (28881 bytes)
  - t10k-images-idx3-ubyte.gz:   test set images (1648877 bytes)
  - t10k-labels-idx1-ubyte.gz:   test set labels (4542 bytes) 
- gunzip the files and place the files in the same directory as mnist.scm
- Run
  - Run on Mosh ```make run-mosh```
  - Run on Gauche ```make run-gosh```