# Tasks

Next task ID: T-007

## Summary
Open tasks: 2 (In Progress: 0, Next Today: 2, Next This Week: 0, Next Later: 0, Blocked: 0)
Done tasks: 4

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

## Next - This Week

## Next - Later

## Blocked

## Done

### T-004 [CLI] Add configurable presets
Completed: 2026-06-11
Outcome:
- The CLI loads user-defined presets from JSON configuration in a predictable precedence order: CLI args, explicit `config=...`, repo config, user config, built-in defaults.
- Repo-local config is discovered from the scan root upward using `.encodingfix.json`.
- User-global config is loaded from `%APPDATA%\MaxLogic\EncodingFixTool\config.json` when present.
- Invalid preset names, malformed JSON, and invalid preset option values fail with clear errors before any files are rewritten.
- README documents preset configuration, precedence, and a Delphi-focused example.
Proof:
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe --include:PresetConfig`
  Result: exit=0, 8 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 30 passed, 0 failed
- PASS: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
- PASS: `& $env:DAK_EXE build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md, CHANGELOG.md
Deps: T-003
Verify: integration-test, cli-proof
Notes: Config JSON supports named presets under `presets`; explicit config overrides repo config, repo config overrides user config, CLI arguments override all config values.

### T-003 [CLI] Add AI-friendly Delphi cleanup workflow
Completed: 2026-06-11
Outcome:
- The CLI provides a built-in `preset=delphi-ai` workflow for agent-driven Delphi cleanup.
- `preset=delphi-ai` expands to Delphi source/project extensions, UTF-8 BOM for non-ASCII files, CRLF normalization, recursive scan, and binary DFM safety once the dependent features exist.
- `scope=git-changed` limits cleanup to modified and untracked Delphi files in a Git worktree, resolving Git paths from the worktree root and filtering back to the requested scan path.
- `format=json` reports scanned, changed, skipped, and failed files without verbose per-file chatter.
Proof:
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe --include:AiWorkflow`
  Result: exit=0, 2 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 22 passed, 0 failed
- PASS: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
- PASS: `& $env:DAK_EXE build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md, CHANGELOG.md
Deps: T-001, T-002
Verify: integration-test, cli-proof
Notes: Target command shape is `EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json`. The preset is explicitly opt-in for AI/editor cleanup after code generation.

### T-002 [CLI] Skip binary DFM files safely
Completed: 2026-06-11
Outcome:
- When `ext` includes `dfm`, binary DFM files are detected from raw bytes before decoding and are skipped without modification.
- Skipped binary DFM files are reported in verbose mode and do not count as failures.
- Text DFM files remain eligible for the normal encoding and optional line-ending repair path.
Proof:
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe --include:Dfm`
  Result: exit=0, 3 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 20 passed, 0 failed
- PASS: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
- PASS: `& $env:DAK_EXE build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md, CHANGELOG.md
Verify: integration-test, cli-proof
Notes: Fix-CRLF skips binary DFM files before repair; implemented as a raw-byte `TPF0` check before decoding. Text DFM encoding and `eol=crlf` paths remain covered.

### T-001 [CLI] Add CRLF normalization mode
Completed: 2026-06-11
Outcome:
- The CLI accepts an opt-in `eol=preserve|crlf` option, defaulting to the current `preserve` behavior.
- `eol=crlf` rewrites solitary `LF` and solitary `CR` line separators to Windows `CRLF`, including files that are otherwise valid ASCII or UTF-8.
- Dry-run and verbose output report when a file would change only because of line-ending normalization.
- Existing encoding repair still preserves line endings unless `eol=crlf` is explicitly requested.
Proof:
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe --include:LineEnding`
  Result: exit=0, 8 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 17 passed, 0 failed
- PASS: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
- PASS: `& $env:DAK_EXE build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Rebuild --ai`
  Result: success
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md, CHANGELOG.md
Verify: integration-test, cli-proof
Notes: Inspired by Fix-CRLF's solitary CR/LF normalization; implemented in our byte-based pipeline, not by importing its GUI/string helpers.
