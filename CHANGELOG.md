# Changelog

## [v1.4] - 2026-06-21

### Fixed

- Added non-mutating `--help` and `/?` handling so help requests cannot fall through to file scanning.
- Added CLI safety regression coverage to keep unknown options as non-mutating parse failures.

## [v1.3] - 2026-06-16

### Added

- Added `path=<file>` support so EncodingFixTool can process exactly one file while using the file's parent directory as the scan root.
- Added regression coverage for single-file CLI processing to ensure sibling files are not touched.

## [v1.2] - 2026-06-16

### Added

- Added `bin/EncodingFixTool.sh` for invoking the Windows executable from WSL with path argument conversion.

## [v1.1] - 2026-06-11

### Added

- Added opt-in `eol=crlf` line-ending normalization for Delphi source cleanup while keeping `eol=preserve` as the default.
- Added binary `.dfm` detection so binary Delphi forms are skipped unchanged while text DFM files remain repairable.
- Added the opt-in `preset=delphi-ai`, `scope=git-changed`, and `format=json` workflow for AI/editor Delphi cleanup.
- Added configurable JSON presets from explicit, repo-local `.encodingfix.json`, and `%APPDATA%\MaxLogic\EncodingFixTool\config.json` locations.
- Added a repo-local agent skill describing the recommended Delphi cleanup workflow for EncodingFixTool.
- Expanded README guidance for AI/agent usage, CRLF cleanup, binary DFM safety, JSON summaries, and preset configuration.
- Refined the repo-local agent skill with safer scope checks, JSON result handling, non-Git workflow guidance, and evaluation prompts.
- Documented the repo-local `agent-skill/` package and how to use it with Codex or Claude Code.
- Refined README wording to reduce repeated agent/preset documentation and added a Delphi Companion reference.
