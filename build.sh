#!/bin/bash

VERSION=0.2.0

export DOCKER_BUILDKIT=1

set -ex

find . -name '*fasl' -print0 | xargs -0 rm

docker build --add-host 'beta.quicklisp.org:13.33.243.6' --progress plain --tag cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION .

docker push cr.yandex/crp7b1mum9l62quiuuu0/cloud-analyzer:$VERSION
