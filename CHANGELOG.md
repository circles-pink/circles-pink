# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Start using a Changelog.
- Introduce `checkouts` just task.
  It checks out third prarty repos (like `circles-docker`) via Nix into the `checkouts` subdirectory.

### Changed

- Improve compiler error messages by starting to use more `Newtype`s instead of type aliases for records. (`CirlesCore.Bindings`)
- Make `RemoteData` a `Newtype` to avoid redunant type hints

## [1.0.4] - 2022-05-06

### Added

- NPM package release pipeline
