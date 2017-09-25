#!/bin/bash

# Attempts to load the a previously-saved docker cache tarball for the provided image name from the provided cache directory.

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

tarball="${cache_dir}/${image}.tar.gz"

if [ -f "${tarball}" ]
then
    echo "loading docker cache from ${tarball}"
    gunzip -c ${tarball} | docker load
else
    echo "no cache file found: ${tarball}"
fi
