#
# Eventually this chould be parameterized, where the base image can be specified
# for *any* rpm-based distro.
#
# OR we might want to just have multiple FROM statements.
#
FROM fedora:26

RUN dnf -y update

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN dnf -y install gcc-c++ libdb-devel leveldb-devel libsodium-devel zlib-devel ncurses-devel && \
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

# FIXME: This flag is currently causing a failure: --rpm-changelog CHANGELOG.md
RUN fpm -t rpm -d libdb-devel -d leveldb-devel -d libsodium-devel -d zlib-devel -d ncurses-devel \
        -s dir \
        -n constellation \
        -v "$(cat constellation.cabal | grep '^version:' | awk '{print $2}')" \
        --description "$(cat constellation.cabal | grep '^description:' | sed 's/description: *//')" \
        --url "https://github.com/jpmorganchase/constellation" \
        --maintainer "$(cat constellation.cabal | grep 'author:' | sed 's/author: *//')" \
        --license "$(cat constellation.cabal | grep license: | sed 's/license: *//')" \
        --vendor "JPMorgan Chase & Co." \
        $(cat constellation.cabal | grep '^executable ' | awk '{print $2}' | sed 's#.*#/usr/local/bin/&=/usr/local/bin/&#')
