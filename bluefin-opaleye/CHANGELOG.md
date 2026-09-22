# Changelog

All notable changes to `bluefin-opaleye` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org).

## [Unreleased]

## [0.2.0.0] - 22.09.2026

### Changed
- Support `bluefin` versions `0.2.7 && < 0.6` in [#4](https://github.com/fpringle/bluefin-postgresql/pull/4).
- Extract common operation counting code to [postgresql-operation-counting](https://github.com/fpringle/postgresql-operation-counting) in [#6](https://github.com/fpringle/bluefin-postgresql/pull/6). Breaking change.
- Support `bluefin < 0.11` in [#8](https://github.com/fpringle/bluefin-postgresql/pull/8).
- Fix `Bluefin.Opaleye.Count` module docs in [#9](https://github.com/fpringle/bluefin-postgresql/pull/9).

## [0.1.0.0] - 27.02.2026

### Added

- First edition of the package, ready for feedback.
- 100% documentation coverage.
- Reasonably detailed READMEs.
- CI that builds and tests the packages for each version of GHC in the `tested-with` field.

[unreleased]: https://github.com/fpringle/bluefin-postgresql/compare/bluefin-opaleye-0.2.0.0...HEAD
[0.2.0.0]: https://github.com/fpringle/bluefin-postgresql/compare/bluefin-opaleye-0.1.0.0...bluefin-opaleye-0.2.0.0
[0.1.0.0]: https://github.com/fpringle/bluefin-postgresql/releases/tag/bluefin-opaleye-0.1.0.0
