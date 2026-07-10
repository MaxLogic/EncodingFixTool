---
name: encodingfix-delphi-cleanup
description: Use EncodingFixTool whenever an AI agent edits, generates, patches, or reviews Delphi/Pascal files and needs safe CRLF or UTF-8 cleanup. Trigger this skill after apply_patch, WSL/Linux tools, formatters, bulk rewrites, or pre-commit cleanup involving .pas, .dpr, .dpk, .inc, .dfm, or .dproj files, even when the user only mentions wrong line endings or encoding drift.
---

# EncodingFix Delphi Cleanup

Run EncodingFixTool after AI/editor work may have changed Delphi line endings or encoding. Keep the cleanup narrow, auditable, and separate from build/test validation.

## Use This For

- Delphi files: `.pas`, `.dpr`, `.dpk`, `.inc`, `.dfm`, `.dproj`.
- AI-generated or patched Delphi code that should use Windows CRLF.
- Delphi files that should be UTF-8 with BOM when non-ASCII text is present.
- Pre-build or pre-commit cleanup after code generation, patching, formatters, WSL, or Linux tooling.

Do not use it for unrelated dirty files, vendored code, non-Delphi cleanup, or as a replacement for the repo's build/test gates. Binary `.dfm` files are safe because EncodingFixTool skips them.

## Core Workflow

1. Check the dirty set first:

```powershell
git status --short
```

If unrelated user changes are dirty, avoid broad cleanup. Narrow `path=...`, override `ext=...`, or ask before touching files outside our work.

2. In a Git worktree, prefer the AI cleanup preset.

From Linux or WSL, run the Bash wrapper. It launches the native Linux64 binary directly, so Linux paths are passed through unchanged and WSL Windows interoperability is not involved. If the repo `bin` directory is on `PATH`, prefer:

```bash
EncodingFixTool.sh path=. preset=delphi-ai scope=git-changed format=json
```

Otherwise, use the repo-local wrapper path:

```bash
./bin/EncodingFixTool.sh path=. preset=delphi-ai scope=git-changed format=json
```

From Windows PowerShell, run:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json
```

If the executable is not on `PATH`, use a local Windows build such as:

```powershell
.\bin\EncodingFixTool.exe path=. preset=delphi-ai scope=git-changed format=json
```

For one known file, use single-file mode instead of scanning the directory:

```powershell
.\bin\EncodingFixTool.exe path=.\src\GeneratedUnit.pas preset=delphi-ai format=json
```

3. Outside Git, use an explicit path and dry run first.

From WSL:

```bash
EncodingFixTool.sh path=./src preset=delphi-ai format=json dry
```

Use `./bin/EncodingFixTool.sh ...` instead if the wrapper is not on `PATH`.

From Windows PowerShell:

```powershell
EncodingFixTool path=.\src preset=delphi-ai format=json dry
```

Run the same command without `dry` only after the reported scope is correct.

## Read Results

Treat the run as successful only when the process exits with code `0` and JSON `failed` is `0`.

- `changed > 0`: inspect the relevant diffs before reporting done.
- `skipped > 0`: usually expected for binary `.dfm`; mention it only when relevant.
- `failed > 0` or exit code `1`: stop and report the file-level failure.
- Exit code `2`: fix the missing directory or file `path` and rerun.

## Verify

After cleanup:

```powershell
git diff --stat
git ls-files --eol *.pas *.dpr *.dpk *.inc *.dfm *.dproj
```

Then continue with the repo's normal Delphi build/test gates. EncodingFixTool only fixes encoding and line endings.

## Reporting Pattern

Keep the final note short and evidence-based:

```text
Ran EncodingFixTool with preset=delphi-ai scope=git-changed format=json.
Result: scanned=4, changed=2, skipped=1, failed=0.
Checked git diff --stat and continued with the repo build/test gate.
```

If the tool was not run, state why: for example, no Delphi files were touched, the worktree had unrelated changes, or EncodingFixTool was unavailable.

## More Detail

Read `references/encodingfix-tool.md` only when you need command details, preset precedence, dry-run behavior, install/copy notes, or examples for non-standard scopes.
