#!/bin/bash

# Saves the docker image with the provided name to the provided cache directory.

set -euo pipefail

die() {
    echo >&2 "$@"
    exit 1
}

print_usage() {
    die "usage: $0 IMAGE_NAME CACHE_DIR"
}

[ "$#" -ne 2 ] && print_usage

image="$1"
cache_dir="$2"

#

tarball="${image}.tar.gz"

mkdir -p ${cache_dir}
docker save $(docker history -q ${image} | grep -v '<missing>' | head -n1) | gzip > ${tarball}
mv ${tarball} ${cache_dir}/
