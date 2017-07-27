ARG DISTRO_VERSION
FROM ubuntu:${DISTRO_VERSION}

RUN apt-get update && apt-get install -y lsb-release

# We need a PPA for libsodium on trusty:
RUN bash -c 'if [ "$(lsb_release -sc)" == "trusty" ]; then \
               apt-get install -y software-properties-common && \
               add-apt-repository ppa:chris-lea/libsodium && \
               apt-get update; \
             fi'

WORKDIR /tmp/constellation
ADD ubuntu.deb /tmp/constellation
RUN dpkg -i ubuntu.deb; apt-get -y -f install

RUN constellation-node --version
