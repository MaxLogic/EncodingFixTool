# Changelog

## [Unreleased]

### Added

- Added a native Delphi Linux64 build at `bin/Linux64/EncodingFixTool`, including native regression coverage for encoding repair, CRLF normalization, backups, XDG configuration, Git-changed scope, shell quoting, and case-sensitive paths.

### Changed

- Changed `EncodingFixTool.sh` to launch the native Linux64 executable directly instead of invoking the Windows EXE through WSL interoperability.
- Made Git execution, shell quoting, user config discovery, and path containment platform-correct; Linux avoids treating its UTF-8 default encoding as a legacy ANSI candidate while Windows retains active-code-page compatibility.
- Updated the agent skill with the native Linux runtime layout, optional WSL `PATH` setup, and fresh-shell verification.

### Fixed

- Bounded Linux Git command execution to 30 seconds and terminated the command process group on timeout so child processes are not left running.

## [v1.5] - 2026-06-24

### Fixed

- Fixed dry-run summaries so they report the same files that a real run would change while still leaving file bytes untouched.

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
