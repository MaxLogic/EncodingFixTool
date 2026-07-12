---
name: encodingfix-delphi-cleanup
description: Use EncodingFixTool when Delphi/Pascal edits may have changed line endings, encoding, or BOM state. Trigger for changed .pas, .dpr, .dpk, .inc, .dfm, or .dproj files after AI patches, Linux/WSL tools, formatters, generators, bulk rewrites, or at the task verification gate. Run once per coherent edit batch, not after every patch operation.
---

# EncodingFix Delphi cleanup

Apply source hygiene once after a coherent Delphi edit batch and before the
task's final build/commit gate. Keep scope narrow and inspect any resulting
diff. This is a mechanical source-preparation step, not a substitute for tests,
DFM validation, static analysis, or compilation.

## Why this still matters with `.gitattributes`

`.gitattributes` controls Git normalization and checkout behavior; it does not
guarantee that the current working-tree bytes have the required BOM/encoding,
repair files written by non-Git tools, or normalize an uncommitted file before
the compiler reads it. Use EncodingFixTool for the working-tree contract. Do
not run it redundantly when a DAK/source-hygiene gate already ran the same
preset against the exact changed files.

## Workflow

1. Inspect `git status --short` and identify task-owned Delphi files.
2. Prefer one task-gate run with the AI preset:

Windows:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json
```

Linux/WSL:

```bash
EncodingFixTool.sh path=. preset=delphi-ai scope=git-changed format=json
```

Use a repo-local wrapper/binary when it is not on `PATH`. For one known file,
pass that file as `path` instead of scanning the repository.

3. If unrelated dirty Delphi files exist, do not use broad `scope=git-changed`.
   Run explicit file paths or the tool's supported narrow extension/path scope.
4. Treat success as process exit `0` and JSON `failed=0`.
5. If `changed>0`, inspect the diff. Expected line-ending/BOM changes are fine;
   unexpected text changes block completion.
6. Verify with `git diff --check`, `git diff --stat`, and `git ls-files --eol`
   for the affected files, then run the normal DAK/build/test gate.

Binary DFMs are skipped; validate them through the DFM/build tooling.

## DAK integration

When a Delphi orchestration tool exposes an explicit source-hygiene or encoding
check, prefer that single task-gate command so cleanup and verification are
recorded together. The integration must remain explicit and scoped: ordinary
build/analyze commands should not silently rewrite source files.

Until such a command exists, DAK skills should route here once before the final
task-tier build when Delphi files were edited.

## Outside Git

Use an explicit path and dry-run first, then repeat without `dry` only after the
reported scope is correct.

## Report

```text
EncodingFixTool: scanned=N changed=N skipped=N failed=0
Scope: <explicit files or git-changed>
Diff inspection: pass
```

Read [references/encodingfix-tool.md](references/encodingfix-tool.md) only for
runtime setup, preset precedence, non-standard scopes, and installation.
