# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Mixed format text no longer parses as the first element [#17](https://github.com/jisantuc/cliffs/pull/17)
- Release script also bumps version of nix derivation [#19](https://github.com/jisantuc/cliffs/pull/19)

## [0.0.1] - 2021-11-15
### Added
- Show help text on script name hover [#5](https://github.com/jisantuc/cliffs/pull/5)
- Created CI workflow and tested parsing a few known inputs [#9](https://github.com/jisantuc/cliffs/pull/9)
- Determined and documented a release process and better ideas about binaries [#15](https://github.com/jisantuc/cliffs/pull/15)

### Changed
- Nix expressions evaluate to a real derivation and a shell [#11](https://github.com/jisantuc/cliffs/pull/11)
- Determined non-link binary path using nix-store [#14](https://github.com/jisantuc/cliffs/pull/14)

[Unreleased]: https://github.com/jisantuc/cliffs/compare/v0.0.1...HEAD
[0.0.1]: https://github.com/jisantuc/cliffs/releases/tags/v0.0.1
