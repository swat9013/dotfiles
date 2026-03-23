#!/bin/bash
INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

deny() {
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"%s"}}\n' "$1"
  exit 0
}

if [ "$TOOL" = "Bash" ]; then
  # 改行を含むコマンドは禁止
  if printf '%s' "$COMMAND" | grep -qP '\n'; then
    deny "改行を含むコマンドは禁止。&&チェーンまたは複数のBash呼び出しに分割してください"
  fi

  # A: rm 禁止（rmtrash使用を強制）
  if echo "$COMMAND" | grep -qE '^rm( |$)'; then
    deny "rmは使用禁止。rmtrash（ファイル）またはrmtrash -r（ディレクトリ）を使用してください"
  fi

  # D: 破壊的 git 操作 deny
  if echo "$COMMAND" | grep -qE 'git reset.*--hard|git clean.*(--force|-f)'; then
    deny "破壊的git操作は禁止。git stashで退避してから操作してください"
  fi
  if echo "$COMMAND" | grep -qE 'git checkout -- '; then
    deny "git checkout -- は禁止。git stashで退避してから操作してください"
  fi
  if echo "$COMMAND" | grep -qE 'git restore ' && ! echo "$COMMAND" | grep -qE 'git restore --staged'; then
    deny "git restore（ワーキングツリー変更の破棄）は禁止。git stashで退避してください"
  fi

  # C: git push 条件付き許可
  if echo "$COMMAND" | grep -qE '^git push'; then
    # force push は deny
    if echo "$COMMAND" | grep -qE -- '--force($| )|-f($| )' && ! echo "$COMMAND" | grep -q -- '--force-with-lease'; then
      deny "force pushは禁止。ユーザーに確認を求めてください"
    fi
    # main/master ブランチへの push は deny（現在ブランチで判定）
    BRANCH=$(git branch --show-current 2>/dev/null || echo "")
    if echo "$BRANCH" | grep -qE '^(main|master)$'; then
      deny "main/masterへの直接pushは禁止。PRを経由してください"
    fi
    # feature ブランチへの通常 push は allow
    printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}\n'
    exit 0
  fi
fi

# B: 機密ファイルへの Write/Edit 拒否
if [ "$TOOL" = "Write" ] || [ "$TOOL" = "Edit" ]; then
  if echo "$FILE_PATH" | grep -qE '\.(env|key|pem|p12|pfx|crt)$|id_rsa|\.netrc|/credentials$|/secrets/|/private/'; then
    deny "機密ファイルへの書き込みは禁止"
  fi
fi

exit 0
