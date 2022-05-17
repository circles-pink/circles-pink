# Changelog

All notable changes to this project will be documented in this file.

Please add new entries below.

## Unrelesed

### Added

- Add tooltip for dashboard actions
- Add translations for hardcoded placeholders

### Changed

- Better responsive Dashboard styling
- Refactor GunDB bindings to arrow functions and remove unused cancelers
- Fix border-box for storybook and npm package

## [1.0.24]

### Changed

- Tweak UI for first integration demo

## [1.0.21]

### Changed

- Bundling with Babel 7 to transpile and resolve the twin webpack macros

## [1.0.15]

### Changed

- Publish web-client initially

## [1.0.14]

### Changed

- Revert release pipeline

## [1.0.10]

### Added

- Ffi for BN to string - preparation for a better check, when UBI should be requested. (BN.length is not working as expected and will be replaced)

### Changed

- Dashboard layout and button rows with grid, for a more static behaviour
- Balance - better initial fetching and update every 15 sec.
- Re-introduced initLanding

## [1.0.5]

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
