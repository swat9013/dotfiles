#!/bin/sh
COMMAND=$(jq -r '.tool_input.command // empty')

# bash/sh -n → shellcheck に差し替え
if printf '%s\n' "$COMMAND" | grep -qE '^(bash|sh) -n [^ ]+$'; then
  FILE=$(printf '%s\n' "$COMMAND" | sed -E 's/^(bash|sh) -n //')
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow","updatedInput":{"command":"shellcheck %s"}}}\n' "$FILE"
  exit 0
fi

# それ以外の bash/sh 実行は deny
printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"bash/shの直接実行は禁止。構文チェックにはshellcheckを使用してください"}}\n'
exit 0
