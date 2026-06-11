# Changelog

## [Unreleased]

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
