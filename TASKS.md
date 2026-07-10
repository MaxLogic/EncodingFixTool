# Tasks

Next task ID: T-011

## Summary
Open tasks: 1 (In Progress: 0, Next Today: 0, Next This Week: 0, Next Later: 1, Blocked: 0)
Done tasks: 9

## In Progress

## Next - Today

## Next - This Week

## Next - Later

### T-010 [BUILD] Repair Delphi 12 command-line Linux64 RTL loading
Outcome:
- Delphi 12 command-line Linux64 builds can load the installed `Posix.Unistd.dcu`.
- DelphiAIKit and raw MSBuild rebuild Linux64 projects as reliably as the RAD Studio IDE.
Proof:
- Run: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Linux64 --config Release --target Rebuild --ai --show-warnings`
  Expect: exit=0, zero warnings, zero errors.
- Run a minimal Linux64 program whose only dependency is `System.SysUtils` through `dcclinux64.exe`.
  Expect: compilation reaches the linker instead of failing with F2063 for `Posix.Unistd`.
Touches: Delphi 12 installation or DelphiAIKit build environment
Verify: build-only
Notes: The project search path contains the expected Delphi 12 Linux64 release RTL folder, and namespace variants do not change the failure. A minimal `uses Posix.Unistd` probe fails under `dcclinux64.exe`; `System.SysUtils` fails transitively on the same unit, while other POSIX DCUs load. Raw MSBuild reproduces DAK, so this is not caused by EncodingFixTool conditionals or DAK command construction. RAD Studio succeeds through its in-process Linux64 compiler DLL.

## Blocked

## Done

### T-009 [CLI] Bound POSIX child-process execution
Completed: 2026-07-11
Outcome:
- Linux command execution enforces the same 30-second upper bound as Windows without depending on an optional external `timeout` command.
- Timeout cleanup terminates and reaps the child process group without leaving Git processes behind.
- Native Linux regression coverage verifies successful commands and timeout behavior.
Proof:
- PASS: RAD Studio Linux64 Release rebuild.
  Result: zero errors; produced `bin/Linux64/EncodingFixTool` as a native ELF executable.
- PASS: `wsl.exe -e bash tests/Invoke-EncodingFixTool.Linux64.Tests.sh`
  Result: exit=0 in 36.2 seconds; timeout regression passed and verified that both the shell and descendant process were gone.
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 31 passed, 0 failed, 0 ignored.
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- DIAGNOSTIC: DelphiAIKit, raw MSBuild, and a minimal `dcclinux64.exe` probe all fail in the installed command-line toolchain with F2063 for `Posix.Unistd`; tracked separately as T-010.
Touches: src/EncodingFixToolCore.pas, tests/Invoke-EncodingFixTool.Linux64.Tests.sh, agent-skill/encodingfix-delphi-cleanup/
Verify: integration-test, build-only
Notes: Strict RED/GREEN was completed. RED reached the 35-second emergency harness limit with exit 124. GREEN enforces the production timeout internally, terminates the process group, reaps the child, and emits `process timed out`. The IDE-built Linux64 executable supplied the native runtime proof; T-010 isolates the unrelated standalone compiler installation failure.

### T-008 [CLI] Add native Linux64 support
Completed: 2026-07-10
Outcome:
- `EncodingFixTool.dproj` builds a native Linux64 executable at `bin/Linux64/EncodingFixTool` while preserving Win32 and Win64 builds.
- Platform-sensitive process execution, Git integration, shell quoting, user config lookup, paths, encodings, and line endings behave correctly on Windows and Linux.
- `EncodingFixTool.sh` launches the native Linux64 binary directly without `wslpath` or Windows executable interop.
- Native Linux regression coverage verifies ELF execution, CRLF cleanup, legacy encoding conversion, config lookup, backups, case-sensitive paths, and Git paths including Unicode, rename, and copy records.
- README, agent-skill guidance, and CHANGELOG document the native Linux workflow.
Proof:
- PASS: `& $env:DAK_EXE build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Linux64 --config Release --target Rebuild --ai --show-warnings`
  Result: exit=0, zero warnings, zero errors, ELF artifact exists, and the Windows resource hash is preserved.
- PASS: `wsl.exe -e bash tests/Invoke-EncodingFixTool.Linux64.Tests.sh`
  Result: exit=0, output contains `EncodingFixTool Linux64 tests passed.`
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 31 passed, 0 failed, 0 ignored.
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: Win32 and Win64 Release rebuilds through DelphiAIKit with warnings enabled.
  Result: exit=0, zero warnings, zero errors for both platforms.
- PASS: PowerShell Script Analyzer, ShellCheck, Bash syntax, project XML parsing, and `git diff --check`.
  Result: no findings.
Touches: src/EncodingFixTool.dpr, src/EncodingFixTool.dproj, src/EncodingFixTool.res, src/EncodingFixToolCore.pas, bin/EncodingFixTool.sh, tests/Invoke-EncodingFixTool.Linux64.Tests.sh, README.md, agent-skill/encodingfix-delphi-cleanup/SKILL.md, agent-skill/encodingfix-delphi-cleanup/references/encodingfix-tool.md, CHANGELOG.md
Verify: integration-test, cli-proof, build-only
Notes: The approved design keeps `sLineBreak` for platform-native console help and explicit `#13#10` for `eol=crlf`. Linux user config follows XDG with `$HOME/.config` fallback. RED/GREEN was strict for native artifact, Linux path containment, and Unicode Git paths; supplementary platform and rename/copy coverage was completed under GREEN.

### T-007 [CLI] Support single-file path targets
Completed: 2026-06-16
Outcome:
- `path=<file>` is accepted as a first-class narrow target.
- Single-file mode uses the file's parent directory as the scan root and processes only that file when its extension matches the active options or preset.
- README, agent skill, reference docs, and changelog document the single-file workflow.
Proof:
- PASS: `.\bin\EncodingFixTool.Tests.exe --include:SingleFile`
  Result: exit=0, 1 passed, 0 failed
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 31 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `Invoke-ScriptAnalyzer -Path .\tests\Invoke-EncodingFixTool.Tests.ps1 -Severity Warning,Error`
  Result: exit=0, no findings
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild src\EncodingFixTool.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
- PASS: `.\bin\EncodingFixTool.exe path=. preset=delphi-ai scope=git-changed format=json`
  Result: exit=0, `failed=0`
Touches: src/EncodingFixToolCore.pas, tests/EncodingFixTool.IntegrationTests.pas, tests/Invoke-EncodingFixTool.Tests.ps1, README.md, agent-skill/encodingfix-delphi-cleanup/SKILL.md, agent-skill/encodingfix-delphi-cleanup/references/encodingfix-tool.md, CHANGELOG.md
Verify: integration-test, cli-proof, build
Notes: Strict TDD followed for the DUnitX single-file regression. Initial RED was exit code 2 for `path=<file>`; GREEN passed after normalizing file paths into explicit single-file scan mode.

### T-006 [DOC] Refresh README for workflow and presets
Completed: 2026-06-11
Outcome:
- README describes the current CLI surface, including CRLF normalization, binary DFM behavior, AI workflow usage, scopes, formats, and configurable presets once implemented.
- README includes a short "AI/agent usage" section with the intended command shape for Delphi projects.
- README explains preset config precedence and the recommended locations for repo-local and user-global JSON configuration.
- README examples stay copy-pasteable on Windows PowerShell and preserve conservative defaults.
Proof:
- PASS: `Select-String -Path .\README.md -Pattern 'preset=delphi-ai','scope=git-changed','format=json','.encodingfix.json','%APPDATA%','eol=crlf'`
  Result: all patterns found
- PASS: `python -c "from pathlib import Path; p=Path('README.md'); b=p.read_bytes(); assert b.count(b'\n') == b.count(b'\r\n')"`
  Result: exit=0
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 30 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
Touches: README.md, CHANGELOG.md
Deps: T-001, T-002, T-003, T-004
Verify: cli-proof
Ceremony: reduced
Notes: Documentation-only task; strict RED/GREEN production-code TDD does not apply. Local acceptance audit confirmed the README covers the current CLI surface, AI workflow, scopes, formats, presets, and PowerShell examples.

### T-005 [DOC] Create EncodingFix agent skill
Completed: 2026-06-11
Outcome:
- A repo-local skill exists under `agent-skill/` with a `SKILL.md` that teaches AI agents when and how to use EncodingFixTool for Delphi cleanup.
- The skill instructs agents to prefer the AI workflow command after Delphi edits, especially for CRLF and encoding repair caused by code generation tools.
- The skill documents safe defaults, dry-run/check behavior, changed-file scope, JSON summary output, and when not to run the tool.
- The skill includes a concise install/use note so it can be copied into an agent skill directory without needing repo-specific context.
Proof:
- PASS: `Test-Path .\agent-skill\SKILL.md`
  Result: `True`
- PASS: `Select-String -Path .\agent-skill\SKILL.md -Pattern 'preset=delphi-ai','scope=git-changed','format=json','CRLF','Delphi'`
  Result: all patterns found
- PASS: `python -c "from pathlib import Path; p=Path('agent-skill/SKILL.md'); b=p.read_bytes(); assert b.count(b'\n') == b.count(b'\r\n')"`
  Result: exit=0
- PASS: `cmd /s /c '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && msbuild tests\EncodingFixTool.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32'`
  Result: exit=0, zero warnings, zero errors
- PASS: `.\bin\EncodingFixTool.Tests.exe`
  Result: exit=0, 30 passed, 0 failed
- PASS: `powershell -NoProfile -ExecutionPolicy Bypass -File tests\Invoke-EncodingFixTool.Tests.ps1`
  Result: exit=0, output contains `EncodingFixTool CLI tests passed.`
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project src\EncodingFixTool.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
- PASS: `& 'F:\projects\MaxLogic\DelphiAiKit\bin\DelphiAIKit.exe' build --project tests\EncodingFixTool.Tests.dproj --delphi 23.0 --platform Win32 --config Debug --target Build --ai`
  Result: success
Touches: agent-skill/SKILL.md, CHANGELOG.md
Deps: T-003
Verify: cli-proof
Ceremony: reduced
Notes: Documentation-only task; strict RED/GREEN production-code TDD does not apply. Subagent acceptance audit returned REVISE for dry-run/check and rewrite-safety wording; both findings were fixed before completion.

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
