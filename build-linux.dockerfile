FROM haskell:8.0.2

RUN apt-get update && \
    # constellation deps:
    apt-get install -y libdb-dev libleveldb-dev libsodium-dev zlib1g-dev libtinfo-dev && \
    # fpm deps:
    apt-get install -y ruby ruby-dev rubygems build-essential && \
    gem install --no-ri --no-rdoc fpm

ENV SRC /usr/local/src/constellation
WORKDIR $SRC

# Incrementally setup/build deps/build to reduce re-build times on changes

ADD stack.yaml $SRC/
RUN stack setup

ADD LICENSE constellation.cabal $SRC/
RUN stack build --dependencies-only

ADD . $SRC/
RUN stack install --local-bin-path /usr/local/bin --test
