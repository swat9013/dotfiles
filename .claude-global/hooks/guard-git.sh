#!/bin/sh
INPUT=$(cat)
COMMAND=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.command // empty')

deny() {
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"%s"}}\n' "$1"
  exit 0
}

# git add . / -A 禁止
if printf '%s\n' "$COMMAND" | grep -qE '^git add (-A|--all|\.)( |$)'; then
  deny "git add . / -A は禁止。ファイルを個別に指定してください"
fi

# 破壊的 git 操作
if printf '%s\n' "$COMMAND" | grep -qE 'git reset.*--hard|git clean.*(--force|-f)'; then
  deny "破壊的git操作は禁止。git stashで退避してから操作してください"
fi
if printf '%s\n' "$COMMAND" | grep -qE 'git checkout -- '; then
  deny "git checkout -- は禁止。git stashで退避してから操作してください"
fi
if printf '%s\n' "$COMMAND" | grep -qE 'git restore ' && ! printf '%s\n' "$COMMAND" | grep -qE 'git restore --staged'; then
  deny "git restore（ワーキングツリー変更の破棄）は禁止。git stashで退避してください"
fi

# git push 条件付き許可
if printf '%s\n' "$COMMAND" | grep -qE '^git push'; then
  if printf '%s\n' "$COMMAND" | grep -qE -- '--force($| )|-f($| )' && ! printf '%s\n' "$COMMAND" | grep -q -- '--force-with-lease'; then
    deny "force pushは禁止。ユーザーに確認を求めてください"
  fi
  BRANCH=$(git branch --show-current 2>/dev/null || printf '')
  if printf '%s\n' "$BRANCH" | grep -qE '^(main|master)$'; then
    deny "main/masterへの直接pushは禁止。PRを経由してください"
  fi
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}\n'
  exit 0
fi

exit 0
