#!/bin/sh
IMAGE=richfitz/context
Rscript -e 'library(methods); devtools::load_all(".."); write_bootstrap("context")'
docker build -t ${IMAGE} .
