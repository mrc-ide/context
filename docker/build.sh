#!/bin/sh
IMAGE=mrc-ide/context
rm -rf context
Rscript -e 'library(methods); devtools::load_all(".."); setup_bootstrap("context")'
docker build -t ${IMAGE} .
