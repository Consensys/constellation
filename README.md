# Constellation

Constellation is a self-managing, peer-to-peer system in which each
node:

  - Hosts a number of NaCl (Curve25519) public/private key pairs.

  - Automatically discovers other nodes on the network after
    synchronizing with as little as one other host.

  - Synchronizes a directory of public keys mapped to recipient hosts
    with other nodes on the network.

  - Exposes a public API which allows other nodes to send encrypted
    bytestrings to your node, and to synchronize, retrieving
    information about the nodes that your node knows about.

  - Exposes a private API which:

      - Allows you to send a bytestring to one or more public keys,
        returning a content-addressable identifier. This bytestring is
        encrypted transparently and efficiently (at symmetric
        encryption speeds) before being transmitted over the wire to
        the correct recipient nodes (and only those nodes.) The
        identifier is a hash digest of the encrypted payload that
        every recipient node receives. Each recipient node also
        receives a small blob encrypted for their public key which
        contains the Master Key for the encrypted payload.

      - Allows you to receive a decrypted bytestring
        based on an identifier. Payloads which your node has sent or
        received can be decrypted and retrieved in this way.

      - Exposes methods for deletion, resynchronization, and other
        management functions.

  - Supports a number of storage backends including LevelDB,
    BerkeleyDB, SQLite, and Directory/Maildir-style file storage
    suitable for use with any FUSE adapter, e.g. for AWS S3.

  - Uses mutually-authenticated TLS with modern settings and various trust
    models including hybrid CA/tofu (default), tofu (think OpenSSH), and
    whitelist (only some set of public keys can connect.)

  - Supports access controls like an IP whitelist.

Conceptually, one can think of Constellation as an amalgamation of a
distributed key server, PGP encryption (using modern cryptography,)
and Mail Transfer Agents (MTAs.)

Constellation's current primary application is to implement the
"privacy engine" of Quorum, a fork of Ethereum with support for
private transactions that function exactly as described in this
README. Private transactions in Quorum contain only a flag indicating
that they're private and the content-addressable identifier described
here.

Constellation can be run stand-alone as a daemon via
`constellation-node`, or imported as a Haskell library, which allows
you to implement custom storage and encryption logic.

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

  2. First time only: run `stack setup` to install GHC, the Glasgow
     Haskell Compiler

  3. Run `stack install`

## Generating keys

  1. To generate a key pair "node", run `constellation-node --generatekeys=node`

  If you choose to lock the keys with a password, they will be encrypted using
  a master key derived from the password using Argon2id. This is designed to be
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

## How It Works

Each Constellation node hosts some number of key pairs, and advertises
a publicly accessible FQDN/port for other hosts to connect to.

Nodes can be started with a reference to existing nodes on the network
(with the `othernodes` configuration variable,) or without, in which
case some other node must later be pointed to this node to achieve
synchronization.

When a node starts up, it will reach out to each node in `othernodes`,
and learn about the public keys they host, as well as other nodes in
the network. In short order, the node's public key directory will be
the same as that of all other nodes, and you can start addressing
messages to any of the known public keys.

This is what happens when you use the `send` function of the Private
API to send the bytestring `foo` to the public key
`ROAZBWtSacxXQrOe3FGAqJDyJjFePR5ce4TSIzmJ0Bc=`:

  1. You send a POST API request to the Private API socket like:
     `{"payload": "foo", "from": "mypublickey", to: "ROAZBWtSacxXQrOe3FGAqJDyJjFePR5ce4TSIzmJ0Bc="}`

  2. The local node generates using `/dev/urandom` (or similar):
       - A random Master Key (MK) and nonce
       - A random recipient nonce

  3. The local node encrypts the payload using NaCl `secretbox` using
     the random MK and nonce.

  4. The local node generates an MK container for each recipient
     public key; in this case, simply one container for `ROAZ...`,
     using NaCl `box` and the recipient nonce.

     NaCl `box` works by deriving a shared key based
     on your private key and the recipient's public key. This is known
     as elliptic curve key agreement.

     Note that the sender public key and recipient public key we
     specified above aren't enough to perform the
     encryption. Therefore, the node will check to see that it is
     actually hosting the private key that corresponds to the given
     public key before generating an MK container for each recipient
     based on SharedKey(yourprivatekey, recipientpublickey) and the
     recipient nonce.

     We now have:

       - An encrypted payload which is `foo` encrypted with the random
         MK and a random nonce. This is the same for all recipients.

       - A random recipient nonce that also is the same for all
         recipients.

       - For each recipient, the MK encrypted with the
         shared key of your private key and their public key. This
         MK container is unique per recipient, and is only transmitted to
         that recipient.

  5. For each recipient, the local node looks up the recipient host,
     and transmits to it:

       - The sender's (your) public key

       - The encrypted payload and nonce

       - The MK container for that recipient and the recipient nonce

  6. The recipient node returns a SHA3-512 hash digest of the
     encrypted payload, which represents its storage address.

     (Note that it is not possible for the sender to dictate the
     storage address. Every node generates it independently by hashing
     the encrypted payload.)

  7. The local node stores the payload locally, generating the same
     hash digest.

  8. The API call returns successfully once all nodes have confirmed
     receipt and storage of the payload, and returned a hash digest.

Now, through some other mechanism, you'll inform the recipient that
they have a payload waiting for them with the identifier `owqkrokwr`,
and they will make a call to the `receive` method of their Private
API:

  1. Make a call to the Private API socket `receive` method:
     `{"key": "qrqwrqwr"}`

  2. The local node will look in its storage for the key `qrqwrqwr`,
     and abort if it isn't found.

  3. When found, the node will use the information about the sender as
     well as its private key to derive SharedKey(senderpublickey,
     yourprivatekey) and decrypt the MK container using NaCl `box`
     with the recipient nonce.

  4. Using the decrypted MK, the local node will decrypt the encrypted
     payload using NaCl `secretbox` using the main nonce.

  5. The API call returns the decrypted data.

# Getting Help

Stuck at some step? Have no fear, the help is here <a href="https://clh7rniov2.execute-api.us-east-1.amazonaws.com/Express/" target="_blank" rel="noopener"><img title="Quorum Slack" src="https://clh7rniov2.execute-api.us-east-1.amazonaws.com/Express/badge.svg" alt="Quorum Slack" /></a>
