## MNIST demo in R7RS
MNIST demo written in R7RS. This work is based on the Python code in https://github.com/oreilly-japan/deep-learning-from-scratch. The demo can classify handwritten digits.

## How to run
- Download the train and test data from [THE MNIST DATABASE](http://yann.lecun.com/exdb/mnist/)
  - train-images-idx3-ubyte.gz:  training set images (9912422 bytes)
  - train-labels-idx1-ubyte.gz:  training set labels (28881 bytes)
  - t10k-images-idx3-ubyte.gz:   test set images (1648877 bytes)
  - t10k-labels-idx1-ubyte.gz:   test set labels (4542 bytes) 
- gunzip the files and place the files in the same directory as mnist.scm
- Run: It can take ~30min.
  - Run on Mosh ```make run-mosh```
  - Run on Gauche ```make run-gosh```

## How it works
- The model is two layer neural network with hidden_size=50.
- We train the model for 8 epochs with 60000 images and test model performance against 10000 unseen test data.
- For each epoch we show
  - Model accuracy for both train (seen) and test (unseen) data.

In the following example output. The model has 88% accuracy after 3 epochs of training.
```
train accuracy=0.10441666666666667
test accuracy=0.1028
loss= 2.301143138281154
loss= 2.2675479959863867
loss= 2.166248047926419
loss= 1.7091766822067018
loss= 1.3085393948428177
loss= 1.0981788987086087
train accuracy=0.7941333333333334
test accuracy=0.7965
loss= 1.0002654560807318
loss= 0.7240668203914656
loss= 0.6905258822720313
loss= 0.5499173317625818
loss= 0.4996544432883294
loss= 0.48351799414293706
train accuracy=0.8768666666666667
test accuracy=0.882
```
