#!/bin/bash

VERSION=0.1.25

export DOCKER_BUILDKIT=1

set -ex

docker build --add-host 'beta.quicklisp.org:13.33.243.6' --progress plain --tag cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION .

docker push cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION
