ARG DISTRO_VERSION
FROM fedora:${DISTRO_VERSION}

RUN dnf -y update

WORKDIR /tmp/constellation
ARG DISTRO_VERSION
ADD fedora-${DISTRO_VERSION}.rpm /tmp/constellation
RUN dnf -y install fedora-${DISTRO_VERSION}.rpm

RUN constellation-node --version
