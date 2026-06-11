# Tasks

Next task ID: T-007

## Summary
Open tasks: 6 (In Progress: 0, Next Today: 6, Next This Week: 0, Next Later: 0, Blocked: 0)
Done tasks: 0

## In Progress

## Next - Today

### T-005 [DOC] Create EncodingFix agent skill
Outcome:
- A repo-local skill exists under `agent-skill/` with a `SKILL.md` that teaches AI agents when and how to use EncodingFixTool for Delphi cleanup.
- The skill instructs agents to prefer the AI workflow command after Delphi edits, especially for CRLF and encoding repair caused by code generation tools.
- The skill documents safe defaults, dry-run/check behavior, changed-file scope, JSON summary output, and when not to run the tool.
- The skill includes a concise install/use note so it can be copied into an agent skill directory without needing repo-specific context.
Proof:
- Run: `Test-Path .\agent-skill\SKILL.md`
  Expect: exit=0 and result is `True`
- Run: `Select-String -Path .\agent-skill\SKILL.md -Pattern 'preset=delphi-ai','scope=git-changed','format=json','CRLF','Delphi'`
  Expect: exit=0 and all patterns are found
- Run: `python -c "from pathlib import Path; p=Path('agent-skill/SKILL.md'); b=p.read_bytes(); assert b.count(b'\n') == b.count(b'\r\n')"`
  Expect: exit=0
Touches: agent-skill/SKILL.md
Deps: T-003
Verify: cli-proof
Ceremony: reduced
Notes: This is documentation/agent-instruction work only; no source behavior changes.

### T-006 [DOC] Refresh README for workflow and presets
Outcome:
- README describes the current CLI surface, including CRLF normalization, binary DFM behavior, AI workflow usage, scopes, formats, and configurable presets once implemented.
- README includes a short "AI/agent usage" section with the intended command shape for Delphi projects.
- README explains preset config precedence and the recommended locations for repo-local and user-global JSON configuration.
- README examples stay copy-pasteable on Windows PowerShell and preserve conservative defaults.
Proof:
- Run: `Select-String -Path .\README.md -Pattern 'preset=delphi-ai','scope=git-changed','format=json','.encodingfix.json','%APPDATA%','eol=crlf'`
  Expect: exit=0 and all patterns are found
- Run: `python -c "from pathlib import Path; p=Path('README.md'); b=p.read_bytes(); assert b.count(b'\n') == b.count(b'\r\n')"`
  Expect: exit=0
Touches: README.md
Deps: T-001, T-002, T-003, T-004
Verify: cli-proof
Ceremony: reduced
Notes: Do after the CLI tasks or explicitly mark unreleased sections if documenting ahead of implementation.

### T-004 [CLI] Add configurable presets
Outcome:
- The CLI loads user-defined presets from JSON configuration in a predictable precedence order: CLI args, explicit `config=...`, repo config, user config, built-in defaults.
- Repo-local config is discovered from the scan root upward using a documented filename such as `.encodingfix.json`.
- User-global config is loaded from `%APPDATA%\MaxLogic\EncodingFixTool\config.json` when present.
- Invalid preset names, malformed JSON, and invalid preset option values fail with clear errors before any files are rewritten.
- README documents preset configuration, precedence, and at least one Delphi-focused example.
Proof:
- Run: `msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32`
  Expect: exit=0, zero warnings, zero errors
- Run: `.\bin\EncodingFixTool.Tests.exe --filter=PresetConfig`
  Expect: exit=0, all configurable preset tests pass
- Run: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Expect: exit=0, output contains `EncodingFixTool CLI tests passed.`
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md
Deps: T-003
Verify: integration-test, cli-proof
Notes: Prefer JSON via Delphi's built-in JSON support. Do not use side-by-side exe config as the primary location because install directories may be read-only or shared across users.

### T-003 [CLI] Add AI-friendly Delphi cleanup workflow
Outcome:
- The CLI provides a built-in `preset=delphi-ai` workflow for agent-driven Delphi cleanup.
- `preset=delphi-ai` expands to Delphi source/project extensions, UTF-8 BOM for non-ASCII files, CRLF normalization, recursive scan, and binary DFM safety once the dependent features exist.
- A changed-file scope such as `scope=git-changed` limits cleanup to modified and untracked Delphi files in a Git worktree.
- A concise machine-readable mode such as `format=json` reports scanned, changed, skipped, and failed files without verbose per-file chatter.
Proof:
- Run: `msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32`
  Expect: exit=0, zero warnings, zero errors
- Run: `.\bin\EncodingFixTool.Tests.exe --filter=AiWorkflow`
  Expect: exit=0, all AI workflow preset and scope tests pass
- Run: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Expect: exit=0, output contains `EncodingFixTool CLI tests passed.`
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md
Deps: T-001, T-002
Verify: integration-test, cli-proof
Notes: Target command shape: `EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json`. Keep defaults conservative; this preset is explicitly opt-in for AI/editor cleanup after code generation.

### T-001 [CLI] Add CRLF normalization mode
Outcome:
- The CLI accepts an opt-in `eol=preserve|crlf` option, defaulting to the current `preserve` behavior.
- `eol=crlf` rewrites solitary `LF` and solitary `CR` line separators to Windows `CRLF`, including files that are otherwise valid ASCII or UTF-8.
- Dry-run and verbose output report when a file would change only because of line-ending normalization.
- Existing encoding repair still preserves line endings unless `eol=crlf` is explicitly requested.
Proof:
- Run: `msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32`
  Expect: exit=0, zero warnings, zero errors
- Run: `.\bin\EncodingFixTool.Tests.exe --filter=LineEnding`
  Expect: exit=0, all line-ending normalization tests pass
- Run: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Expect: exit=0, output contains `EncodingFixTool CLI tests passed.`
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md
Verify: integration-test, cli-proof
Notes: Inspired by Fix-CRLF's solitary CR/LF normalization; implement in our byte-based pipeline, not by importing its GUI/string helpers.

### T-002 [CLI] Skip binary DFM files safely
Outcome:
- When `ext` includes `dfm`, binary DFM files are detected from raw bytes before decoding and are skipped without modification.
- Skipped binary DFM files are reported in verbose mode and do not count as failures.
- Text DFM files remain eligible for the normal encoding and optional line-ending repair path.
Proof:
- Run: `msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32`
  Expect: exit=0, zero warnings, zero errors
- Run: `.\bin\EncodingFixTool.Tests.exe --filter=Dfm`
  Expect: exit=0, all binary/text DFM tests pass
- Run: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Expect: exit=0, output contains `EncodingFixTool CLI tests passed.`
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md
Verify: integration-test, cli-proof
Notes: Fix-CRLF skips binary DFM files before repair; our implementation should avoid string-decoding binary content entirely.

## Next - This Week

## Next - Later

## Blocked

## Done
