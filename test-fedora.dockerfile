ARG DISTRO_VERSION
FROM fedora:${DISTRO_VERSION}

RUN dnf -y update

WORKDIR /tmp/constellation
ADD fedora.rpm /tmp/constellation
RUN dnf -y install fedora.rpm

RUN constellation-node --version
