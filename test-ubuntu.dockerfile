FROM ubuntu:trusty

RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository ppa:chris-lea/libsodium && \
    apt-get update

WORKDIR /tmp/constellation
ADD ubuntu.deb /tmp/constellation
RUN dpkg -i ubuntu.deb; apt-get -y -f install

RUN constellation-node --version
