#!/bin/bash

# Extracts the (e.g. deb, rpm) package built from a dockerfile with the provided image name (e.g. ubuntu-trusty) and package type (e.g. deb)

set -euo pipefail

die() {
    echo >&2 "$@"
    exit 1
}

print_usage() {
    die "usage: $0 IMAGE_NAME PACKAGE_TYPE"
}

[ "$#" -ne 2 ] && print_usage

image="$1"
type="$2"

#

container="extract-${image}"
package_name="${image}.${type}"

docker run --name ${container} -t -d ${image} /bin/bash
docker cp ${container}:/usr/local/src/constellation/${package_name} ${package_name}
docker stop ${container}
