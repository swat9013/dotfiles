#!/bin/bash
INPUT=$(cat)
TOOL=$(printf '%s\n' "$INPUT" | jq -r '.tool_name')
COMMAND=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.command // empty')
FILE_PATH=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.file_path // empty')

deny() {
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"%s"}}\n' "$1"
  exit 0
}

if [ "$TOOL" = "Bash" ]; then
  # 改行を含むコマンドは禁止
  LINECOUNT=$(printf '%s' "$COMMAND" | wc -l | tr -d ' ')
  if [ "$LINECOUNT" -gt 0 ]; then
    deny "改行を含むコマンドは禁止。&&チェーンまたは複数のBash呼び出しに分割してください"
  fi

  # A: bash/sh 実行制御（-n は shellcheck に差し替え、それ以外は deny）
  if printf '%s\n' "$COMMAND" | grep -qE '^(bash|sh)( |$)'; then
    if printf '%s\n' "$COMMAND" | grep -qE '^(bash|sh) -n [^ ]+$'; then
      FILE=$(printf '%s\n' "$COMMAND" | sed -E 's/^(bash|sh) -n //')
      printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow","updatedInput":{"command":"shellcheck %s"}}}\n' "$FILE"
      exit 0
    fi
    deny "bash/shの直接実行は禁止。構文チェックにはshellcheckを使用してください"
  fi

  # D: git add . / -A 禁止（明示的なファイル指定を強制）
  if printf '%s\n' "$COMMAND" | grep -qE '^git add (-A|--all|\.)( |$)'; then
    deny "git add . / -A は禁止。ファイルを個別に指定してください"
  fi

  # E: 破壊的 git 操作 deny
  if printf '%s\n' "$COMMAND" | grep -qE 'git reset.*--hard|git clean.*(--force|-f)'; then
    deny "破壊的git操作は禁止。git stashで退避してから操作してください"
  fi
  if printf '%s\n' "$COMMAND" | grep -qE 'git checkout -- '; then
    deny "git checkout -- は禁止。git stashで退避してから操作してください"
  fi
  if printf '%s\n' "$COMMAND" | grep -qE 'git restore ' && ! printf '%s\n' "$COMMAND" | grep -qE 'git restore --staged'; then
    deny "git restore（ワーキングツリー変更の破棄）は禁止。git stashで退避してください"
  fi

  # F: git push 条件付き許可
  if printf '%s\n' "$COMMAND" | grep -qE '^git push'; then
    # force push は deny
    if printf '%s\n' "$COMMAND" | grep -qE -- '--force($| )|-f($| )' && ! printf '%s\n' "$COMMAND" | grep -q -- '--force-with-lease'; then
      deny "force pushは禁止。ユーザーに確認を求めてください"
    fi
    # main/master ブランチへの push は deny（現在ブランチで判定）
    BRANCH=$(git branch --show-current 2>/dev/null || printf '')
    if printf '%s\n' "$BRANCH" | grep -qE '^(main|master)$'; then
      deny "main/masterへの直接pushは禁止。PRを経由してください"
    fi
    # feature ブランチへの通常 push は allow
    printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}\n'
    exit 0
  fi
fi

# G: 機密ファイルへの Write/Edit 拒否
if [ "$TOOL" = "Write" ] || [ "$TOOL" = "Edit" ]; then
  if printf '%s\n' "$FILE_PATH" | grep -qE '\.(env|key|pem|p12|pfx|crt)$|id_rsa|\.netrc|/credentials$|/secrets/|/private/'; then
    deny "機密ファイルへの書き込みは禁止"
  fi
fi

exit 0
