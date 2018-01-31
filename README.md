# Constellation [<img src="https://travis-ci.org/jpmorganchase/constellation.svg" align="right" />](https://travis-ci.org/jpmorganchase/constellation)

Constellation forms a network of nodes, each of which advertises a list of
public keys that they are the recipient for. Each node exposes an API which
allows the user to send a payload to one or more public keys. That payload
will be encrypted for the public key before being transferred over the wire
to the recipient node. You can think of it as a network of Mail Transfer
Agents (MTAs) exchanging PGP-encrypted emails.

## Installation

### Prerequisites

  1. Install supporting libraries:
    - Ubuntu: `apt-get install libdb-dev libleveldb-dev libsodium-dev zlib1g-dev libtinfo-dev`
    - Red Hat: `dnf install libdb-devel leveldb-devel libsodium-devel zlib-devel ncurses-devel`
    - MacOS: `brew install berkeley-db leveldb libsodium`

### Downloading precompiled binaries

Constellation binaries for most major platforms can be downloaded [here](https://github.com/jpmorganchase/constellation/releases).

### Installation from source

  1. First time only: Install Stack:
    - Linux: `curl -sSL https://get.haskellstack.org/ | sh`
    - MacOS: `brew install haskell-stack`
  2. First time only: run `stack setup` to install the Haskell compiler
    a. If you have manually installed Berkley make sure to include the header and library files.
          
         stack install --extra-include-dirs=/usr/local/BerkleyDB.N.X/include --extra-lib-dirs=/usr/local/BerkleyDB.N.X/lib
  
  3. Run `stack install`

## Generating keys

  1. To generate a key pair "node", run `constellation-node --generatekeys=node`

  If you choose to lock the keys with a password, they will be encrypted using
  a master key derived from the password using Argon2i. This is designed to be
  a very expensive operation to deter password cracking efforts. When
  constellation encounters a locked key, it will prompt for a password after
  which the decrypted key will live in memory until the process ends.

## Running

  1. Run `constellation-node <path to config file>` or specify configuration
     variables as command-line options (see `constellation-node --help`)

For now, please refer to the [Constellation client Go library](https://github.com/jpmorganchase/quorum/blob/master/private/constellation/node.go)
for an example of how to use Constellation. More detailed documentation coming soon!

## Configuration File Format

See [sample.conf](sample.conf).
