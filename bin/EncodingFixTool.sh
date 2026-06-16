#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
exe_path="$script_dir/EncodingFixTool.exe"

if [[ ! -f "$exe_path" ]]; then
  printf 'EncodingFixTool.exe not found next to %s\n' "$0" >&2
  exit 127
fi

is_windows_path() {
  [[ "$1" =~ ^[A-Za-z]:[\\/].* || "$1" =~ ^\\\\.* ]]
}

to_windows_path() {
  local value="$1"

  if [[ -z "$value" ]] || is_windows_path "$value"; then
    printf '%s' "$value"
    return
  fi

  wslpath -aw -- "$value"
}

convert_arg() {
  local arg="$1"

  if [[ "$arg" =~ ^(-{0,2}|/)(path|config|bkp-dir)([:=])(.*)$ ]]; then
    printf '%s%s%s%s' "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}" "$(to_windows_path "${BASH_REMATCH[4]}")"
  else
    printf '%s' "$arg"
  fi
}

shopt -s nocasematch
converted_args=()
for arg in "$@"; do
  converted_args+=("$(convert_arg "$arg")")
done

exec "$exe_path" "${converted_args[@]}"
