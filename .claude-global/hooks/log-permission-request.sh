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

TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
TOOL=$(printf '%s\n' "$INPUT" | jq -r '.tool // "unknown"')
INPUT_PREVIEW=$(printf '%s\n' "$INPUT" | jq -r '.input // "" | .[0:100]')

jq -n \
  --arg timestamp "$TIMESTAMP" \
  --arg tool "$TOOL" \
  --arg input_preview "$INPUT_PREVIEW" \
  '{"timestamp": $timestamp, "tool": $tool, "input_preview": $input_preview}' \
  >> "$OUTPUT_FILE"

exit 0
