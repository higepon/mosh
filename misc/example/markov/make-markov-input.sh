#!/bin/bash

# Text file => markov-input.txt
#mecab -Owakati < ./data.txt > markov-input.txt

mecab -Owakati < $1 > markov-input.txt

# Make dictionary
../../mosh markov.ss markov-input.txt
