# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Start using a Changelog.

### Changed

- Improve compiler error messages by starting to use more `Newtype`s instead of type aliases for records. (`CirlesCore.Bindings`)
- Make `RemoteData` a `Newtype` to avoid redunant type hints

## [1.0.4] - 2022-05-06

### Added

- NPM package release pipeline
