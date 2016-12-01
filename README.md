# Constellation

Constellation forms a network of nodes, each of which advertises a list of
public keys that they are the recipient for. Each node exposes an API which
allows the user to send a payload to one or more public keys. That payload
will be encrypted for the public key before being transferred over the wire
to the recipient node. You can think of it as a network of Mail Transfer
Agents (MTAs) exchanging PGP-encrypted emails.

## Installation

### Prerequisites

  1. Install supporting libraries:
    - Ubuntu: `apt-get install libdb-dev libsodium-dev zlib1g-dev libtinfo-dev`
    - Red Hat: `dnf install libdb-devel libsodium-devel zlib-devel ncurses-devel`
    - MacOS: `brew install berkeley-db libsodium`

### Downloading precompiled binaries

Constellation binaries for most major platforms can be downloaded [here](https://github.com/jpmorganchase/constellation/releases).

### Installation from source

  1. First time only: Install Stack:
    - Linux: `curl -sSL https://get.haskellstack.org/ | sh`
    - MacOS: `brew install haskell-stack`
  2. First time only: run `stack setup` to install the Haskell compiler
  3. Run `stack install`

## Generating keys

  1. To generate a key pair "node", run `constellation-enclave-keygen node`

  If you choose to lock the keys with a password, they will be encrypted using
  a master key derived from the password using Argon2i. This is designed to be
  a very expensive operation to deter password cracking efforts. When
  constellation encounters a locked key, it will prompt for a password after
  which the decrypted key will live in memory until the process ends.

## Running

  1. Run `constellation-node <path to config file>`

For now, please refer to the [Constellation client Go library](https://github.com/jpmorganchase/quorum/blob/master/private/constellation/node.go)
for an example of how to use Constellation. More detailed documentation coming soon!

## Configuration File Format

    # Externally accessible URL for this node (this is what's advertised)
    url = "http://127.0.0.1:9001/"

    # Port to listen on
    port = 9001

    # Optional IP whitelist for the external API. If unspecified/empty,
    # connections from all sources will be allowed (but the private API remains
    # accessible only via the IPC socket below.) To allow connections from
    # localhost when a whitelist is defined, e.g. when running multiple
    # Constellation nodes on the same machine, add "127.0.0.1" and "::1" to
    # this list.
    ipWhitelist = ["10.0.0.1", "2001:0db8:85a3:0000:0000:8a2e:0370:7334"]

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

    # Where to store payloads and related information
    storagePath = "data/payloads"
