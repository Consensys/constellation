FROM ubuntu:xenial as builder

RUN apt-get update

RUN apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get install -y libgmp-dev libdb-dev libleveldb-dev libsodium-dev zlib1g-dev libtinfo-dev && \
    apt-get install -y ruby ruby-dev build-essential && \
    gem install --no-ri --no-rdoc fpm

ENV SRC /usr/local/src/constellation
WORKDIR $SRC

ADD stack.yaml $SRC/
RUN stack setup

ADD LICENSE constellation.cabal $SRC/
RUN stack build --dependencies-only

ADD README.md CHANGELOG.md Setup.hs $SRC/
COPY bin/ $SRC/bin/
COPY test/ $SRC/test/
COPY Constellation/ $SRC/Constellation/
RUN stack install --local-bin-path /usr/local/bin --test

# Pull binary into a second stage and deploy to container
FROM ubuntu:xenial

RUN mkdir -p /constellation
RUN apt-get update
RUN apt-get install -y libgmp-dev libdb-dev libleveldb-dev libsodium-dev zlib1g-dev libtinfo-dev build-essential curl

COPY --from=builder /usr/local/bin/constellation-node /usr/local/bin/

ENTRYPOINT ["constellation-node"]