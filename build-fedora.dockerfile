# DISTRO_VERSION can be something like 24, 25, or 26.
# Supply this using e.g.: --build-arg DISTRO_VERSION=24

ARG DISTRO_VERSION
FROM fedora:${DISTRO_VERSION}

RUN dnf -y update

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN dnf -y install gcc-c++ gmp-devel libdb-devel leveldb-devel libsodium-devel zlib-devel ncurses-devel && \
    dnf -y install redhat-rpm-config rpmdevtools ruby rubygems ruby-devel && \
    gem install --no-ri --no-rdoc fpm

# CONSTELLATION BUILD. If you modify this, change the other dockerfiles too.

ENV SRC /usr/local/src/constellation
WORKDIR $SRC

ADD stack.yaml $SRC/
RUN stack setup

ADD LICENSE constellation.cabal $SRC/
RUN stack build --dependencies-only

# TODO: possibly combine these commands with tar+ADD
ADD README.md CHANGELOG.md Setup.hs $SRC/
COPY bin/ $SRC/bin/
COPY test/ $SRC/test/
COPY Constellation/ $SRC/Constellation/
RUN stack install --local-bin-path /usr/local/bin --test

# END constellation build.

ARG DISTRO_VERSION
# FIXME: This flag is currently causing a failure: --rpm-changelog CHANGELOG.md
RUN fpm -t rpm -d gmp-devel -d libdb-devel -d leveldb-devel -d libsodium-devel -d zlib-devel -d ncurses-devel \
        -s dir \
        -n constellation \
        -p fedora-${DISTRO_VERSION}.rpm \
        -v "$(cat constellation.cabal | grep '^version:' | awk '{print $2}')" \
        --description "$(cat constellation.cabal | grep '^description:' | sed 's/description: *//')" \
        --url "https://github.com/jpmorganchase/constellation" \
        --maintainer "$(cat constellation.cabal | grep 'author:' | sed 's/author: *//')" \
        --license "$(cat constellation.cabal | grep license: | sed 's/license: *//')" \
        --vendor "JPMorgan Chase & Co." \
        $(cat constellation.cabal | grep '^executable ' | awk '{print $2}' | sed 's#.*#/usr/local/bin/&=/usr/local/bin/&#')
