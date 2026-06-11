---
name: encodingfix-delphi-cleanup
description: Use EncodingFixTool after editing or generating Delphi source so AI agents can quickly repair CRLF line endings and UTF-8 encoding issues. Trigger this whenever working in Delphi/Pascal projects, especially after code generation, patch application, WSL/Linux tools, or any workflow that may leave .pas, .dpr, .dpk, .inc, .dfm, or .dproj files with LF endings or inconsistent encodings.
---

# EncodingFix Delphi Cleanup

Use this skill when our work may have touched Delphi source or project files and we need a fast, token-efficient cleanup pass for encoding and line endings.

## When To Use

Run EncodingFixTool after edits made by AI agents, patch tools, WSL/Linux commands, formatters, or bulk scripts that may have written Delphi files with LF endings or inconsistent encodings.

Prefer it for:

- Delphi source and project files: `.pas`, `.dpr`, `.dpk`, `.inc`, `.dfm`, `.dproj`.
- Generated or patched Delphi code that should use Windows CRLF.
- Repos where Delphi files should be UTF-8 with BOM when non-ASCII text is present.
- Pre-commit cleanup before running Delphi builds or DUnitX tests.

Do not use it for:

- Binary assets other than binary `.dfm` files, which the tool skips safely.
- Non-Delphi code unless the user explicitly asks for a broader cleanup.
- Vendored or third-party directories unless the task specifically includes them.
- A dirty Git worktree where unrelated user changes would be touched and `scope=git-changed` is too broad.

## Preferred Agent Command

From the Delphi repository root, run:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json
```

This is the normal repair command and rewrites matching changed files when encoding or line endings need cleanup.

This command is designed for AI/editor cleanup:

- `preset=delphi-ai` selects Delphi file extensions, recursive scan, UTF-8 BOM policy, CRLF normalization, and binary DFM safety.
- `scope=git-changed` limits work to modified and untracked files in the Git worktree.
- `format=json` returns a compact summary such as `{"scanned":2,"changed":1,"skipped":1,"failed":0}`.

If the executable is not on `PATH`, use the repo-local or build output path directly, for example:

```powershell
.\bin\EncodingFixTool.exe path=. preset=delphi-ai scope=git-changed format=json
```

## Dry Run And Check Behavior

Use `dry` when the scope is not obvious, when many files are dirty, or when the user only asked for inspection:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json dry
```

`dry` is the safe check mode for this tool: it reports what would change without rewriting files. Proceed with the non-dry command only when the changed-file set is expected.

## Configured Presets

Projects may define repo presets in `.encodingfix.json`; users may define global presets in:

```text
%APPDATA%\MaxLogic\EncodingFixTool\config.json
```

Preset precedence is:

```text
built-in defaults < user config < repo .encodingfix.json < explicit config=... < CLI arguments
```

Use repo presets when the project has stricter rules, but keep command-line overrides explicit and minimal.

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
3. Confirm line endings where required, for example:

```powershell
git ls-files --eol *.pas *.dpr *.dpk *.inc *.dfm *.dproj
```

4. Continue with the repo's normal Delphi build and test gates.

## Install Or Copy

To install this as an agent skill, copy the `agent-skill/` directory into the agent's skills directory. The skill is self-contained; the only runtime dependency is an available `EncodingFixTool` executable.
