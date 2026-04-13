#!/bin/sh
# PermissionRequest hook: 権限要求イベントをJSONLに記録する
# Stop hookにも登録した場合の二重実行ガード（現在はPermissionRequestのみ登録）
INPUT=$(cat)
if [ "$(printf '%s\n' "$INPUT" | jq -r '.stop_hook_active' 2>/dev/null)" = "true" ]; then
  exit 0
fi

METRICS_DIR="$HOME/.claude/tmp/metrics"
mkdir -p "$METRICS_DIR"
OUTPUT_FILE="$METRICS_DIR/permission-requests.jsonl"

printf '%s\n' "$INPUT" | jq -c '{
  timestamp: (now | strftime("%Y-%m-%dT%H:%M:%SZ")),
  session_id: (.session_id // "unknown"),
  permission_mode: (.permission_mode // "unknown"),
  cwd: (.cwd // "unknown"),
  tool: (.tool_name // "unknown"),
  key_info: (
    if .tool_name == "Bash" then (.tool_input.command // "" | .[0:200])
    elif .tool_name == "Edit" or .tool_name == "Write" then (.tool_input.file_path // "")
    elif .tool_name == "Agent" then (.tool_input.prompt // "" | .[0:200])
    else (.tool_input // {} | tostring | .[0:200])
    end
  ),
  input_preview: (.tool_input // {} | tostring | .[0:200])
}' >> "$OUTPUT_FILE"

exit 0
