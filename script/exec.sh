#!/bin/sh

# Install packages and fetch data
Rscript init.r

# Generate images from data
Rscript make.r

cp *.png ../source/images/
