# Changelog

All notable changes to this project will be documented in this file.

Please add new entries below.

## [1.0.29]

- Generate PureScript docs and serve as folder in publicDir
- Refactor finalizeRegisterAccount: token & safe deployment is re-tried until success
- Add Tests for "finalize Account"
- Change TestEnv to be statefule: Enables better simulation of services

## [1.0.28]

### Fixed

- Theme for Pageselector and centered the buttons
- Fix "Username not available" error

### Changed

- StatemachineDebugger on every view
- Deploy token three times, instead of two, to make sure it is deployed after finalize

## [1.0.27]

### Added

- Pagination for trust list
- Scan address mvp in send circles overlay
- Redeploy token and safe option for debug
- Simple validation message for userinput (email, username)
- Simple status message for trusting and pending state

### Changed

- The debug state (magicDebug) is now sorted and filterable
- Search is now triggered on change and pressing the button is obsolete
- Removed extra search component and refactored search results, mapping is still not in sync with trustState from trusts
- Fixed Tooltips
- Trusts from search results now correctly mapped

## [1.0.26]

### Added

- Just task that does a nix-collect-garbage without deleting currently needed derivations. Does not yet work 100%.
- Just default task that does a full ci build and keeps the result in an out-link
- Refactor control modules
- Sync trusts with mapping in state-machine
- Implement trusts in Frontend

### Changed

- Move nix dev-shell to separate file.

## [1.0.25]

### Added

- Add tooltip for dashboard actions
- Add translations for hardcoded placeholders
- Fetch trusts in trust screen

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
- Publish web-client from babel built

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
