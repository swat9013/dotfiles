#!/bin/sh
COMMAND=$(jq -r '.tool_input.command // empty')
LINECOUNT=$(printf '%s' "$COMMAND" | wc -l | tr -d ' ')
if [ "$LINECOUNT" -gt 0 ]; then
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"改行を含むコマンドは禁止。&&チェーンまたは複数のBash呼び出しに分割してください"}}\n'
fi
exit 0
