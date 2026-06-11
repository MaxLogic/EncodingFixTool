---
name: encodingfix-delphi-cleanup
description: Use EncodingFixTool whenever an AI agent edits, generates, patches, or reviews Delphi/Pascal source and needs to repair CRLF line endings or UTF-8 encoding safely. Trigger this skill for Delphi projects after code generation, apply_patch, WSL/Linux tools, formatters, bulk rewrites, or pre-commit cleanup involving .pas, .dpr, .dpk, .inc, .dfm, or .dproj files, even if the user only says the files may have wrong line endings.
---

# EncodingFix Delphi Cleanup

Use this skill to run EncodingFixTool in a Delphi project without wasting tokens on manual line-ending inspection. The goal is to repair only the Delphi files our work may have affected, then hand back a small, auditable summary.

## Compatibility

Requires an available `EncodingFixTool` executable. Prefer `EncodingFixTool` from `PATH`; if that is not available, use a repo-local build output such as `.\bin\EncodingFixTool.exe`.

## When To Use

Run EncodingFixTool after edits made by AI agents, patch tools, WSL/Linux commands, formatters, or bulk scripts that may have written Delphi files with LF endings or inconsistent encodings.

Prefer it for:

- Delphi source and project files: `.pas`, `.dpr`, `.dpk`, `.inc`, `.dfm`, `.dproj`.
- Generated or patched Delphi code that should use Windows CRLF.
- Repos where Delphi files should be UTF-8 with BOM when non-ASCII text is present.
- Pre-commit cleanup before running Delphi builds or DUnitX tests.
- Quick repair after a tool reports `i/crlf w/lf`, LF-only Delphi files, or mixed encoding symptoms.

Do not use it for:

- Binary assets other than binary `.dfm` files, which the tool skips safely.
- Non-Delphi code unless the user explicitly asks for a broader cleanup.
- Vendored or third-party directories unless the task specifically includes them.
- A dirty Git worktree where unrelated user changes would be touched and `scope=git-changed` is too broad.
- Replacing a required code review or build/test gate. This tool only fixes encoding and line endings.

## Workflow

1. Check scope first:

```powershell
git status --short
```

If the dirty set includes unrelated user changes, avoid broad cleanup. Narrow `path=...`, use explicit `ext=...`, or ask before touching files outside our task.

2. From a Git worktree, prefer the compact AI cleanup command:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json
```

This is the normal repair command and rewrites matching changed files when encoding or line endings need cleanup.

3. If `EncodingFixTool` is not on `PATH`, use the executable path directly:

```powershell
.\bin\EncodingFixTool.exe path=. preset=delphi-ai scope=git-changed format=json
```

4. If the project is not a Git worktree, do not use `scope=git-changed`. Use an explicit path and dry run first:

```powershell
EncodingFixTool path=.\src preset=delphi-ai format=json dry
```

Then run the same command without `dry` only when the scope is correct.

The `delphi-ai` workflow is designed for AI/editor cleanup:

- `preset=delphi-ai` selects Delphi file extensions, recursive scan, UTF-8 BOM policy, CRLF normalization, and binary DFM safety.
- `scope=git-changed` limits work to modified and untracked files in the Git worktree.
- `format=json` returns a compact summary such as `{"scanned":2,"changed":1,"skipped":1,"failed":0}`.

## Dry Run And Check Behavior

Use `dry` when the scope is not obvious, when many files are dirty, or when the user only asked for inspection:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json dry
```

`dry` is the safe check mode for this tool: it reports what would change without rewriting files. Proceed with the non-dry command only when the changed-file set is expected.

## Reading Results

For JSON output, treat the run as successful only when the process exits with code `0` and `failed` is `0`.

- `changed > 0`: inspect the relevant diffs before committing or reporting done.
- `skipped > 0`: usually expected for binary `.dfm` files; mention it if it matters.
- `failed > 0` or exit code `1`: stop and report the file-level failure instead of silently continuing.
- Exit code `2`: the `path` argument does not exist; fix the path and rerun.

## Configured Presets

Projects may define repo presets in `.encodingfix.json`; users may define global presets in:

```text
%APPDATA%\MaxLogic\EncodingFixTool\config.json
```

Preset precedence is:

```text
built-in defaults < user config < repo .encodingfix.json < explicit config=... < CLI arguments
```

Use repo presets when the project has stricter rules, but keep command-line overrides explicit and minimal. CLI arguments win, so prefer a visible override such as `ext=pas,inc` for a one-off narrower run.

Example repo config:

```json
{
  "presets": {
    "agent": {
      "ext": "pas,dpr,dpk,inc,dfm,dproj",
      "eol": "crlf",
      "utf8-bom": "y",
      "recursive": "y"
    }
  }
}
```

Run it with:

```powershell
EncodingFixTool path=. preset=agent scope=git-changed format=json
```

## Verification

After running the tool:

1. Check the JSON summary. `failed` should be `0`.
2. Inspect `git diff --stat` and the relevant Delphi diffs.
3. Confirm line endings where required:

```powershell
git ls-files --eol *.pas *.dpr *.dpk *.inc *.dfm *.dproj
```

4. Continue with the repo's normal Delphi build and test gates.

## Response Pattern

When reporting back, keep it short and evidence-based:

```text
Ran EncodingFixTool with preset=delphi-ai scope=git-changed format=json.
Result: scanned=4, changed=2, skipped=1, failed=0.
Verified with git diff --stat and the repo build/test gate.
```

If the tool was not run, state why: for example, no Delphi files were touched, the worktree had unrelated changes, or the executable was unavailable.

## Install Or Copy

To install this as an agent skill, copy the `agent-skill/` directory into the agent's skills directory. The skill is self-contained; the only runtime dependency is an available `EncodingFixTool` executable.
