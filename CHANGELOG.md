# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Add a POC that `Protocol`, `State` and `Action` can be defined _at once_ without having to change `stadium`.
  This will reduce boilerplate when adding/modifying the state machine. Currently it's only implemented for the `landing` state.
- Change publishing pipeline to increment the `rc` (release candidate) part of the version.
- Parse environment variables in API script using the typedenv package.
- Introduce `checkouts` just task.
  It checks out third prarty repos (like `circles-docker`) via Nix into the `checkouts` subdirectory.
- Start using a Changelog.

### Changed

- Make `RemoteData` a `Newtype` to avoid redunant type hints
- Improve compiler error messages by starting to use more `Newtype`s instead of type aliases for records. (`CirlesCore.Bindings`)

## [1.0.4] - 2022-05-06

### Added

- NPM package release pipeline
