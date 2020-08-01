#!/bin/bash

stack clean
stack build
stack install
stack exec haskell-fold-exe < input.txt 