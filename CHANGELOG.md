# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Add Chance.js and FFI Bindings to generate randomized sample trust networks
- Refine EnvVar types by replacing "String" with more accurate representations like "Address" from the `web3` package and `URI` from the `uri` package
- Add a POC that `Protocol`, `State` and `Action` can be defined _at once_ without having to change `stadium`.
  This will reduce boilerplate when adding/modifying the state machine. Currently it's only implemented for the `landing` state.
- Parse environment variables in API script using the typedenv package.
- Introduce `checkouts` just task.
  It checks out third prarty repos (like `circles-docker`) via Nix into the `checkouts` subdirectory.
- Start using a Changelog.

### Changed

- Change publishing pipeline to increment the `rc` (release candidate) part of the version.
- Make `RemoteData` a `Newtype` to avoid redunant type hints
- Improve compiler error messages by starting to use more `Newtype`s instead of type aliases for records. (`CirlesCore.Bindings`)

## [1.0.4] - 2022-05-06

### Added

- NPM package release pipeline
