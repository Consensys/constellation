FROM ubuntu:trusty

RUN apt-get update

RUN apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get install -y software-properties-common && \
    add-apt-repository ppa:chris-lea/libsodium && \
    apt-get update && \
    apt-get install -y libdb-dev libleveldb-dev libsodium-dev zlib1g-dev libtinfo-dev && \
    apt-get install -y ruby ruby-dev build-essential && \
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

RUN fpm -t deb --deb-changelog CHANGELOG.md -d libdb-dev -d libleveldb-dev -d libsodium-dev -d zlib1g-dev -d libtinfo-dev \
        -s dir \
        -n constellation \
        -v "$(cat constellation.cabal | grep '^version:' | awk '{print $2}')" \
        --description "$(cat constellation.cabal | grep '^description:' | sed 's/description: *//')" \
        --url "https://github.com/jpmorganchase/constellation" \
        --maintainer "$(cat constellation.cabal | grep 'author:' | sed 's/author: *//')" \
        --license "$(cat constellation.cabal | grep license: | sed 's/license: *//')" \
        --vendor "JPMorgan Chase & Co." \
        $(cat constellation.cabal | grep '^executable ' | awk '{print $2}' | sed 's#.*#/usr/local/bin/&=/usr/local/bin/&#')
