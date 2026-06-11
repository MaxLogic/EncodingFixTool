# Changelog

## [Unreleased]

### Added

- Added opt-in `eol=crlf` line-ending normalization for Delphi source cleanup while keeping `eol=preserve` as the default.
- Added binary `.dfm` detection so binary Delphi forms are skipped unchanged while text DFM files remain repairable.
