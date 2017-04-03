# Dockerfile for building static constellation binaries using musl
# To run: % docker build -f build-static.dockerfile .

# Use image with GHC 8 on Alpine Linux
FROM mitchty/alpine-ghc:latest
RUN apk update
RUN apk add alpine-sdk git ca-certificates gmp-dev zlib-dev libsodium-dev db-dev ncurses-dev ncurses-static

# Compile db statically from source since there's no -static package
WORKDIR /usr/local/src
RUN wget http://download.oracle.com/berkeley-db/db-5.3.28.tar.gz
RUN tar xfz db-5.3.28.tar.gz
WORKDIR /usr/local/src/db-5.3.28/build_unix
RUN ../dist/configure --prefix=/usr      \
	              --enable-compat185 \
	              --enable-dbm       \
	              --enable-static    \
	              --enable-cxx    && \
    make
RUN make docdir=/usr/local/share/doc/db-5.3.28 install

# Install stack
ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack

# Compile constellation
ADD ./ /usr/local/src/constellation
WORKDIR /usr/local/src/constellation
RUN stack install --test --ghc-options '-optl-static -fPIC'

# Optional: Compress with upx for a smaller file size
# ADD https://github.com/lalyos/docker-upx/releases/download/v3.91/upx /usr/local/bin/upx
# RUN chmod 755 /usr/local/bin/upx
# RUN upx --best --ultra-brute /usr/local/bin/constellation-node
# RUN upx --best --ultra-brute /usr/local/bin/constellation-enclave-keygen

# Binaries are available in the container:
# Get the image ID: % docker images
# Start a container: % docker run -t -i <imageid> /bin/bash
# Get the container ID: % docker ps
# % docker cp <containerid>:/root/.local/bin/constellation-node .
# % docker cp <containerid>:/root/.local/bin/constellation-enclave-keygen .
