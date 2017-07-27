#!/bin/bash

# Upgrades Travis CI's trusty environment to use docker 17.05, which supports using ARG before FROM

set -euo pipefail

sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" > /etc/apt/sources.list.d/docker.list'
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
sudo apt-get update
sudo apt-key update
sudo apt-get -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" install docker-engine=17.05.0~ce-0~ubuntu-trusty
docker -v
