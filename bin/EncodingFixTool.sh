#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
tool_path="$script_dir/Linux64/EncodingFixTool"

if [[ ! -x "$tool_path" ]]; then
  printf 'EncodingFixTool Linux64 executable not found or not executable: %s\n' "$tool_path" >&2
  exit 127
fi

exec "$tool_path" "$@"
