#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
tool="$repo_root/bin/Linux64/EncodingFixTool"
wrapper="$repo_root/bin/EncodingFixTool.sh"

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

[[ -x "$tool" ]] || fail "Linux64 executable not found: $tool"
file "$tool" | grep -q 'ELF 64-bit' || fail 'Linux64 executable is not an ELF 64-bit binary.'

temp_root="$(mktemp -d)"
trap 'rm -rf -- "$temp_root"' EXIT

single_file="$temp_root/SingleFile.pas"
printf 'unit SingleFile;\ninterface\nend.\n' > "$single_file"
single_output="$(WSL_INTEROP=/run/WSL/nonexistent_interop "$wrapper" \
  path="$single_file" preset=delphi-ai format=json)"
[[ "$single_output" == *'"scanned":1'* && "$single_output" == *'"changed":1'* && "$single_output" == *'"failed":0'* ]] ||
  fail "Unexpected single-file JSON: $single_output"
python3 - "$single_file" <<'PY'
from pathlib import Path
import sys

data = Path(sys.argv[1]).read_bytes()
assert b'\r\n' in data
assert data.replace(b'\r\n', b'').find(b'\n') == -1
PY

legacy_file="$temp_root/Legacy.pas"
python3 - "$legacy_file" <<'PY'
from pathlib import Path
import sys

Path(sys.argv[1]).write_bytes("unit Legacy;\nconst S = 'zażółć';\nend.\n".encode('cp1250'))
PY
legacy_output="$("$tool" path="$legacy_file" preset=delphi-ai format=json)"
[[ "$legacy_output" == *'"scanned":1'* && "$legacy_output" == *'"changed":1'* && "$legacy_output" == *'"failed":0'* ]] ||
  fail "Unexpected CP1250 JSON: $legacy_output"
python3 - "$legacy_file" <<'PY'
from pathlib import Path
import sys

data = Path(sys.argv[1]).read_bytes()
assert data.startswith(b'\xef\xbb\xbf')
assert "zażółć" in data.decode('utf-8-sig')
assert b'\r\n' in data
PY

backup_root="$temp_root/backup"
backup_source_root="$temp_root/backup-source"
mkdir -p "$backup_source_root/nested"
backup_file="$backup_source_root/nested/Backup.pas"
printf 'unit Backup;\nend.\n' > "$backup_file"
backup_output="$("$tool" path="$backup_source_root" preset=delphi-ai bkp-dir="$backup_root" format=json)"
[[ "$backup_output" == *'"scanned":1'* && "$backup_output" == *'"changed":1'* && "$backup_output" == *'"failed":0'* ]] ||
  fail "Unexpected backup JSON: $backup_output"
[[ -f "$backup_root/nested/Backup.pas" ]] || fail 'Linux backup did not preserve the relative path.'
python3 - "$backup_root/nested/Backup.pas" <<'PY'
from pathlib import Path
import sys

assert Path(sys.argv[1]).read_bytes() == b'unit Backup;\nend.\n'
PY

xdg_root="$temp_root/xdg"
mkdir -p "$xdg_root/MaxLogic/EncodingFixTool"
cat > "$xdg_root/MaxLogic/EncodingFixTool/config.json" <<'JSON'
{
  "presets": {
    "linux-user": {
      "ext": "txt",
      "eol": "crlf",
      "utf8-bom": false
    }
  }
}
JSON
xdg_file="$temp_root/XdgConfig.txt"
printf 'first\nsecond\n' > "$xdg_file"
xdg_output="$(XDG_CONFIG_HOME="$xdg_root" HOME=/nonexistent \
  "$tool" path="$xdg_file" preset=linux-user format=json)"
[[ "$xdg_output" == *'"scanned":1'* && "$xdg_output" == *'"changed":1'* && "$xdg_output" == *'"failed":0'* ]] ||
  fail "Unexpected XDG config JSON: $xdg_output"
python3 - "$xdg_file" <<'PY'
from pathlib import Path
import sys

assert Path(sys.argv[1]).read_bytes() == b'first\r\nsecond\r\n'
PY

home_root="$temp_root/home"
mkdir -p "$home_root/.config/MaxLogic/EncodingFixTool"
cp "$xdg_root/MaxLogic/EncodingFixTool/config.json" \
  "$home_root/.config/MaxLogic/EncodingFixTool/config.json"
home_file="$temp_root/HomeConfig.txt"
printf 'home\nfallback\n' > "$home_file"
home_output="$(env -u XDG_CONFIG_HOME HOME="$home_root" \
  "$tool" path="$home_file" preset=linux-user format=json)"
[[ "$home_output" == *'"scanned":1'* && "$home_output" == *'"changed":1'* && "$home_output" == *'"failed":0'* ]] ||
  fail "Unexpected HOME config JSON: $home_output"
python3 - "$home_file" <<'PY'
from pathlib import Path
import sys

assert Path(sys.argv[1]).read_bytes() == b'home\r\nfallback\r\n'
PY

git_root="$temp_root/git-scope-with-'quote"
mkdir -p "$git_root/src"
git -C "$git_root" init -q
git -C "$git_root" config user.email encodingfix-tests@example.invalid
git -C "$git_root" config user.name EncodingFixTests
printf 'unit Changed;\nend.\n' > "$git_root/src/Changed.pas"
printf 'unit Untouched;\nend.\n' > "$git_root/src/Untouched.pas"
printf 'unit UnicodeName;\nend.\n' > "$git_root/src/Żółć.pas"
git -C "$git_root" add .
git -C "$git_root" commit -qm baseline
printf 'unit Changed;\ninterface\nend.\n' > "$git_root/src/Changed.pas"
printf 'unit NewFile;\nend.\n' > "$git_root/src/NewFile.pas"
printf 'unit UnicodeName;\ninterface\nend.\n' > "$git_root/src/Żółć.pas"
git_output="$("$tool" path="$git_root/src" preset=delphi-ai scope=git-changed format=json)"
[[ "$git_output" == *'"scanned":3'* && "$git_output" == *'"changed":3'* && "$git_output" == *'"failed":0'* ]] ||
  fail "Unexpected git-changed JSON: $git_output"
python3 - "$git_root" <<'PY'
from pathlib import Path
import sys

root = Path(sys.argv[1]) / 'src'
assert b'\r\n' in (root / 'Changed.pas').read_bytes()
assert b'\r\n' in (root / 'NewFile.pas').read_bytes()
assert b'\r\n' in (root / 'Żółć.pas').read_bytes()
assert b'\r\n' not in (root / 'Untouched.pas').read_bytes()
PY

rename_root="$temp_root/git-rename"
mkdir -p "$rename_root/src"
git -C "$rename_root" init -q
git -C "$rename_root" config user.email encodingfix-tests@example.invalid
git -C "$rename_root" config user.name EncodingFixTests
printf 'unit RenameSource;\nend.\n' > "$rename_root/src/RenameSource.pas"
printf 'unit Following;\nend.\n' > "$rename_root/src/Following.pas"
git -C "$rename_root" add .
git -C "$rename_root" commit -qm baseline
git -C "$rename_root" mv src/RenameSource.pas src/RenamedTarget.pas
printf 'unit Following;\ninterface\nend.\n' > "$rename_root/src/Following.pas"
rename_output="$("$tool" path="$rename_root/src" preset=delphi-ai scope=git-changed format=json)"
[[ "$rename_output" == *'"scanned":2'* && "$rename_output" == *'"changed":2'* && "$rename_output" == *'"failed":0'* ]] ||
  fail "Unexpected git rename JSON: $rename_output"
python3 - "$rename_root" <<'PY'
from pathlib import Path
import sys

root = Path(sys.argv[1]) / 'src'
assert not (root / 'RenameSource.pas').exists()
assert b'\r\n' in (root / 'RenamedTarget.pas').read_bytes()
assert b'\r\n' in (root / 'Following.pas').read_bytes()
PY

copy_root="$temp_root/git-copy"
mkdir -p "$copy_root/src"
git -C "$copy_root" init -q
git -C "$copy_root" config user.email encodingfix-tests@example.invalid
git -C "$copy_root" config user.name EncodingFixTests
git -C "$copy_root" config status.renames copies
printf 'unit CopySource;\ninterface\nimplementation\nprocedure KeepOne;\nbegin\nend;\nprocedure KeepTwo;\nbegin\nend;\nend.\n' > "$copy_root/src/CopySource.pas"
git -C "$copy_root" add .
git -C "$copy_root" commit -qm baseline
printf '\n// changed copy source\n' >> "$copy_root/src/CopySource.pas"
cp "$copy_root/src/CopySource.pas" "$copy_root/src/CopiedTarget.pas"
git -C "$copy_root" add src/CopySource.pas src/CopiedTarget.pas
git -C "$copy_root" status --porcelain=v1 -z --untracked-files=all | python3 -c \
  "import sys; assert sys.stdin.buffer.read().startswith(b'C ')"
copy_output="$("$tool" path="$copy_root/src" preset=delphi-ai scope=git-changed format=json)"
[[ "$copy_output" == *'"scanned":2'* && "$copy_output" == *'"changed":2'* && "$copy_output" == *'"failed":0'* ]] ||
  fail "Unexpected git copy JSON: $copy_output"
python3 - "$copy_root" <<'PY'
from pathlib import Path
import sys

root = Path(sys.argv[1]) / 'src'
assert b'\r\n' in (root / 'CopySource.pas').read_bytes()
assert b'\r\n' in (root / 'CopiedTarget.pas').read_bytes()
PY

case_root="$temp_root/case-sensitive-scope"
mkdir -p "$case_root/src" "$case_root/Src"
git -C "$case_root" init -q
git -C "$case_root" config user.email encodingfix-tests@example.invalid
git -C "$case_root" config user.name EncodingFixTests
printf 'unit Outside;\nend.\n' > "$case_root/Src/Outside.pas"
git -C "$case_root" add .
git -C "$case_root" commit -qm baseline
printf 'unit Outside;\ninterface\nend.\n' > "$case_root/Src/Outside.pas"
case_output="$("$tool" path="$case_root/src" preset=delphi-ai scope=git-changed format=json)"
[[ "$case_output" == *'"scanned":0'* && "$case_output" == *'"changed":0'* && "$case_output" == *'"failed":0'* ]] ||
  fail "Linux path containment must be case-sensitive: $case_output"
python3 - "$case_root/Src/Outside.pas" <<'PY'
from pathlib import Path
import sys

assert b'\r\n' not in Path(sys.argv[1]).read_bytes()
PY

help_output="$("$tool" help)"
[[ "$help_output" != *$'\r'* ]] || fail 'Linux help output must use native LF line endings.'

printf 'EncodingFixTool Linux64 tests passed.\n'
