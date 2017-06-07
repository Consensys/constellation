# Changelog

All major changes to Constellation will be recorded here.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.1.0] - 2017-06-06

This release includes changes to the configuration file
format. Backward compatibility is maintained, but we recommend users
migrate to the new format as soon as it's convenient. Please see
the Changed section below and `sample.conf` for more information.

### Added

- Ability to specify any configuration file parameter on the command
  line, e.g. `--storage=data`. If a configuration file is given as an
  argument, the command line options will override the options
  specified in the file. A configuration file is no longer necessary
  to launch `constellation-node`.

- Ability to set an IPv4/IPv6 whitelist for simple access control
  using `ipwhitelist`.

- Ability to specify `-v [NUM]` or `-vv`, `-vvv` to control verbosity
  level.

- Ability to generate a key pair using `--generatekeys`, removing the
  need for a separate `constellation-enclave-keygen` binary.

- Ability to define a number of public keys that will always be added
  to the recipient list for all transactions, e.g. for backup purposes,
  using `alwayssendto`.

- Ability to output Constellation version and exit using `--version`.

- Ability to specify a file containing passwords for the private keys
  given (if locked) with `passwords`. Each line contains a password
  for a key in the same sequence they were given in `privatekeys`. If
  a key is not locked, but others are, add a blank line for that key's
  entry in the passwords file.

- Support for deleting payloads and addition of `delete` method for
  the private API. (Thanks to Conor Svensson.)

- Addition of `sendRaw` and `receiveRaw` methods to the private API
  which forego base64-encoding and the associated overhead. (Thanks to
  Conor Svensson.)

- `scripts/db-list-keys.py` for listing keys in a payload database,
  for aiding in troubleshooting.

- `build-linux-static.dockerfile` for building binaries statically
  using musl inside an Alpine Linux container.

### Changed
- Most configuration options, e.g. the IPC socket path, are now
  optional, in anticipation of later adding runtime configuration
  ability. `port` must always be specified either in the configuration
  file or with `--port`.

- Ability to specify multiple public/private key pairs using
  `publickeys` and `privatekeys` in the configuration file, or
  `--publickeys` and `--privatekeys` on the command line. (The
  ordering of the public and private keys must match.)

- Some options have been renamed:
  - `socketPath` is now `socket`
  - `otherNodeUrls` is now `othernodes`
  - `publicKeyPath` is now `publickeys` (a list -- see previous)
  - `privateKeyPath` is now `privatekeys` (a list -- see previous)
  - `storagePath` is now `storage`
  - `ipWhitelist` is now `ipwhitelist`

  Constellation remains backwards compatible with configuration files
  using the old naming conventions, however the new and old naming
  should not be mixed, and we recommend switching to the new naming as
  soon as possible.

  If you were previously making use of archival keys, we suggest
  putting the public/private archival keys in `publickeys` and
  `privatekeys` respectively, then adding the archival public key to
  `alwayssendto`, which will emulate the same behavior. (Note,
  however, that this key will be advertised as available on this node
  to the rest of the network.)

### Deprecated
- The camel case configuration options `socketPath`, `otherNodeUrls`,
  `publicKeyPath`, `privateKeyPath`, `storagePath`, `ipWhitelist`
  `archivalPrivateKeyPath` and `archivalPublicKeyPath`.
  (please see Changed section above for details.)

### Fixed
- Any newlines will be ignored in key files.

- Nodes now attempt to synchronize with each other more aggressively
  in the first few minutes after startup, which should address issues
  with "Unknown recipient" errors shortly after startup.

## 0.0.1 - 2016-11-14
### Added
- Initial Constellation release.

[Unreleased]: https://github.com/jpmorganchase/constellation/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/jpmorganchase/constellation/compare/v0.0.1...v0.1.0
