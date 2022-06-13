#!/bin/bash

VERSION=0.1.1

export DOCKER_BUILDKIT=1

docker build --tag cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION .

#docker push cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION
