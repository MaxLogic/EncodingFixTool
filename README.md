# EncodingFixTool

A fast, parallel, command-line fixer for Delphi source file encodings.
It scans `.pas`, `.dpr`, …; detects UTF-8/ASCII vs. legacy single-byte encodings; repairs mixed lines (Windows-1250/1252/ANSI), and writes clean UTF-8 with an optional BOM — without breaking your line endings or folder structure.

---

## Why?

* Old codebases often mix ANSI and UTF-8 (sometimes within a single file).
* Delphi 12+ works best when sources are in UTF-8 (usually with BOM).
* Manual converting is slow and risky. This tool automates it safely and fast.

---

## Quick start

```
EncodingFixTool [params]
```

Examples:

```bat
:: Dry run, current folder, defaults (.pas,.dpr, recursive, BOM=yes)
EncodingFixTool dry

:: Fix a project tree verbosely and back it up before changes
EncodingFixTool path=C:\Projs\MyApp v bkp-dir=C:\backup\myapp

:: Only scan .pas files in ./src (not recursive), remove BOM if present
EncodingFixTool path=.\src recursive=n ext=pas utf8-bom=n

:: Normalize generated Delphi sources to Windows CRLF while fixing encodings
EncodingFixTool path=.\src ext=pas,dpr eol=crlf

:: AI-friendly Delphi cleanup for changed files with compact JSON output
EncodingFixTool path=. preset=delphi-ai scope=git-changed format=json

:: Multiple ext forms are OK; quoted lists work
EncodingFixTool ext="*.pas,*.dpr, .dfm"
```

> Tip: Parameters accept `key=value` or `key:value`. You can prefix flags with `-` (and `/` on Windows), e.g. `-v`, `-dry`.

---

## Parameters

| Param         | Aliases | Values                     | Default     | Meaning                                                                                        |
| ------------- | ------- | -------------------------- | ----------- | ---------------------------------------------------------------------------------------------- |
| `help`        | `-h`    | —                          | —           | Prints extended help and exits.                                                                |
| `dry`         | —       | —                          | off         | Dry run: analyze and report what **would** change; no writes.                                  |
| `s`/`silent`  | —       | —                          | off         | No console output. (Overrides `verbose`.)                                                      |
| `v`/`verbose` | —       | —                          | off         | More output: “OK” lines etc. (Ignored if `silent`.)                                            |
| `path`        | —       | dir                        | current dir | Directory to scan.                                                                             |
| `recursive`   | —       | `y`/`n`/`yes`/`no`/`1`/`0` | `y`         | Recurse into subfolders.                                                                       |
| `ext`         | —       | CSV list                   | `pas,dpr`   | File extensions to include. Smart parsing: accepts `pas`, `.pas`, `*.pas`. Quoted lists OK.    |
| `preset`      | —       | `delphi-ai`                | —           | Applies Delphi agent cleanup defaults: Delphi extensions, UTF-8 BOM, recursive scan, CRLF.     |
| `config`      | —       | JSON file                  | —           | Loads user-defined presets from an explicit JSON config file. Relative paths resolve under `path`. |
| `scope`       | —       | `all`/`git-changed`        | `all`       | Scans all matching files or only Git modified and untracked files.                              |
| `format`      | —       | `text`/`json`              | `text`      | Prints human-readable output or a compact JSON summary.                                        |
| `utf8-bom`    | —       | `y`/`n`                    | `y`         | Whether to **save with** UTF-8 BOM. **Pure US-ASCII files are always left without a BOM**.     |
| `eol`         | —       | `preserve`/`crlf`          | `preserve`  | Whether to preserve original line endings or normalize solitary `LF`/`CR` to Windows `CRLF`.   |
| `bkp-dir`     | —       | dir                        | empty       | If set, backs up every file **before** overwriting, preserving the relative path below `path`. |

### Backup path example

If `path=C:\tmp\` and `bkp-dir=C:\bkp` and a processed file is
`C:\tmp\src\foo\bar\Main.pas` → backup is written to:

```
C:\bkp\src\foo\bar\Main.pas
```

---

## What it does (algorithm)

1. **Gather files**
   Walk the `path` (recursively by default), matching the configured extensions.

2. **Process in parallel**
   Uses `TParallel.For` to utilize multiple cores. Console output is synchronized, and the “files changed” and failure counters are atomic.

3. **Detect encoding per file** (fast pre-check)

   * If the file is a binary Delphi form (`.dfm` starting with `TPF0`) → skip it before any decoding.
   * Uses `System.WideStrUtils.DetectUTF8Encoding` on the raw bytes.
   * If **US-ASCII** → file is left as-is (never add a BOM).
   * If **UTF-8** → ensure BOM matches `utf8-bom` option; adjust if needed (optionally backing up first).
   * Else (treated as **ANSI / unknown**) → go to step 4.

4. **Repair mixed/legacy encodings per line**

   * Split the original bytes by raw CR/LF/CRLF (no decoding yet).
   * For each line, try:

     * **UTF-8 (strict)** — if it round-trips, use it.
     * **Windows-1250** (Central Europe/PL), **Windows-1252** (Western/DE), and **ANSI** — decode and **score**:

       * +2 for valid Polish/German diacritics,
       * +1 for typical source characters (letters/digits/whitespace/common punctuation),
       * −2 for control chars (except tab),
       * −1 for U+FFFD replacements.
     * Pick the **highest-scoring** decode for that line.
   * Reassemble the file:

     * Preserve the **dominant original EOL style** (CRLF/LF/CR), unless `eol=crlf` is requested.
     * Preserve whether the file ended **with a trailing EOL**.
   * Save as **UTF-8** (BOM per `utf8-bom`), optionally to backup first.

5. **Summary**
   At the end prints elapsed time, number of files changed, and number of failures.

---

## Output examples

Dry run:

```
Would fix: src\Utils\StrTools.pas (mixed bytes; would save UTF-8 (BOM=Y, EOL=CRLF) (dry-run))
OK   : src\Main.dpr (UTF-8 OK)
Done in 00:12.384. Files changed: 0. Failures: 0
```

Actual run:

```
Fixed: src\Utils\StrTools.pas (detected Windows-1250; saved UTF-8 (BOM=Y, EOL=CRLF))
Fixed: src\Forms\About.pas (detected UTF-8; Added UTF-8 BOM)
OK   : src\Forms\Main.dfm (Skipped binary DFM)
Done in 00:08.972. Files changed: 2. Failures: 0
```

> When `silent` is enabled, only the summary is suppressed too. In dry-run mode, change messages are phrased as “Would fix”.

---

## Behavior details

* **BOM policy**

  * Default is **BOM = yes** (Delphi IDE/compiler are happiest this way).
  * **US-ASCII** files are **never** given a BOM, even if `utf8-bom=y`.
  * Existing UTF-8 files are re-saved only if the BOM policy differs.

* **Line endings**
  Dominant EOL (CRLF/LF/CR) is detected from raw bytes and **preserved** by default; trailing newline presence is preserved.
  With `eol=crlf`, solitary `LF` and solitary `CR` separators are normalized to Windows `CRLF`, including ASCII and already-valid UTF-8 files that otherwise would not need encoding repair.

* **DFM files**
  Binary Delphi forms are detected from raw bytes and skipped unchanged. Text DFM files remain eligible for the normal encoding and optional line-ending repair path.

* **AI workflow**
  `preset=delphi-ai` is opt-in and intended for agent/editor cleanup after Delphi code generation. Use `scope=git-changed` to limit work to modified and untracked files, and `format=json` for a summary such as `{"scanned":2,"changed":1,"skipped":1,"failed":0}`.

* **Preset configuration**
  User presets live in JSON under a top-level `presets` object, for example `{"presets":{"agent":{"ext":"pas,inc","eol":"crlf"}}}`. Precedence is: built-in defaults, `%APPDATA%\MaxLogic\EncodingFixTool\config.json`, nearest repo `.encodingfix.json` found from `path` upward, explicit `config=...`, then CLI arguments. Invalid preset names, malformed JSON, and invalid preset option values fail before file rewriting starts.

* **Relative reporting**
  Paths in logs are shown **relative to** the scanned `path`, for readability.

---

## Exit codes

* `0` – completed without parse errors or per-file failures.
* `1` – invalid parameter or one or more files failed to process.
* `2` – the `path` argument didn’t exist.
