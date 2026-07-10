# EncodingFixTool Reference

Use this reference when the standard `preset=delphi-ai scope=git-changed` workflow is not enough.

## Runtime Dependency

Prefer `EncodingFixTool` from `PATH` on Windows or `EncodingFixTool.sh` on Linux/WSL. Repo-local paths are `.\bin\EncodingFixTool.exe` on Windows and `./bin/EncodingFixTool.sh` on Linux/WSL. The shell launcher executes `bin/Linux64/EncodingFixTool` directly and does not use Windows interoperability.

On Linux/WSL, the wrapper and binary are one runtime bundle:

```text
bin/EncodingFixTool.sh
bin/Linux64/EncodingFixTool
```

`PATH` is optional. Add the absolute native `bin` path to `~/.profile` for login shells and to `~/.bashrc` for direct non-login interactive Bash shells. Non-interactive automation should receive an explicit `PATH` or use the absolute/repo-local wrapper path. Verify configured shells with `command -v EncodingFixTool.sh` followed by `EncodingFixTool.sh help`.

## Delphi AI Preset

`preset=delphi-ai` is designed for AI/editor cleanup:

- Delphi extensions: `.pas`, `.dpr`, `.dpk`, `.inc`, `.dfm`, `.dproj`.
- Recursive scan.
- CRLF normalization.
- UTF-8 BOM policy for files with non-ASCII text.
- Binary `.dfm` safety.

`scope=git-changed` limits work to modified and untracked files in the current Git worktree. Do not use it outside Git, and do not use it when unrelated dirty files would be included.

## Dry Run

Use `dry` when the scope is not obvious, many files are dirty, or the user only asked for inspection:

```powershell
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json dry
```

`dry` reports what would change without rewriting files. Proceed without `dry` only when the file set is expected.

## Non-Git Or Narrow Scope

Outside a Git worktree:

```powershell
EncodingFixTool path=.\src preset=delphi-ai format=json dry
EncodingFixTool path=.\src preset=delphi-ai format=json
```

For a deliberately narrow repair, keep overrides visible:

```powershell
EncodingFixTool path=.\src preset=delphi-ai ext=pas,inc format=json dry
```

For exactly one file, pass the file path directly. The tool treats the file's directory as the scan root and processes only that file:

```powershell
EncodingFixTool path=.\src\GeneratedUnit.pas preset=delphi-ai format=json
```

## Configured Presets

Repo presets may live in `.encodingfix.json`; user presets may live in:

```text
%APPDATA%\MaxLogic\EncodingFixTool\config.json
$XDG_CONFIG_HOME/MaxLogic/EncodingFixTool/config.json
$HOME/.config/MaxLogic/EncodingFixTool/config.json
```

Preset precedence:

```text
built-in defaults < user config < repo .encodingfix.json < explicit config=... < CLI arguments
```

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

## Install Or Copy

To install this as an agent skill, copy the `encodingfix-delphi-cleanup/` folder into the agent's skills directory. Keep `SKILL.md`, `references/`, and `evals/` together. The skill files do not install the CLI runtime: install or build the Windows executable separately, and on Linux/WSL keep `EncodingFixTool.sh` together with `Linux64/EncodingFixTool` as shown above.
