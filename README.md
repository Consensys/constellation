# Constellation

Constellation forms a network of nodes, each of which advertises a list of
public keys that they are the recipient for. Each node exposes an API which
allows the user to send a payload to one or more public keys. That payload
will be encrypted for the public key before being transferred over the wire
to the recipient node.

## Downloading precompiled binaries

Constellation binaries for most major platforms can be downloaded [here]().

## Installation from source

### Prerequisites

  1. Install Stack:
    - Linux: `curl -sSL https://get.haskellstack.org/ | sh`
    - MacOS: `brew install haskell-stack`

  2. Install dependencies:
    - Ubuntu: `apt-get install libdb-dev libsodium-dev zlib1g-dev libtinfo-dev`
    - Red Hat: `dnf install libdb-devel sodium-devel zlib-devel ncurses-devel`
    - MacOS: `brew install berkeley-db libsodium`

### Building the Binaries

  1. First time only: run `stack setup` to install the Haskell compiler
  2. Run `stack install`

## Generating keys

  1. To generate two key pairs, one for the node and one for archival, run
     `constellation-enclave-keygen node archival`

  If you choose to lock the keys with a password, they will be encrypted using
  a master key derived from the password using Argon2i. This is designed to be
  a very expensive operation to deter password cracking efforts. When
  constellation encounters a locked key, it will prompt for a password after
  which the decrypted key will live in memory until the process ends.

## Running

  1. Run `constellation-node <path to config file>`

## Configuration File Format

    # Externally accessible URL for this node (this is what's advertised)
    url = "http://127.0.0.1:9001/"

    # Port to listen on
    port = 9001

    # Socket file to use for IPC
    socketPath = "tm1.ipc"

    # Initial (not necessarily complete) list of other nodes in the network.
    # Constellation will automatically connect to other nodes not in this list
    # that are advertised by the nodes below, thus these can be considered the
    # "boot nodes."
    otherNodeUrls = ["http://127.0.0.1:9000/"]

    # This node's public key
    publicKeyPath = "tm1.pub"

    # This node's private key
    privateKeyPath = "tm1.key"

    # This node's archival key (payloads sent by this node are encrypted for
    # this key by default.)
    archivalPublicKeyPath = "tm1a.pub"

    # This node's archival private key
    archivalPrivateKeyPath = "tm1a.key"

    # Where to store payloads and related information
    storagePath = "data/payloads"
