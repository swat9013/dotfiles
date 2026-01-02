#!/bin/bash
# Claude Code file suggestion script
# git管理外のファイルも含めて検索する

query=$(cat | jq -r '.query')
cd "$CLAUDE_PROJECT_DIR" || exit 1

# rgで全ファイルを取得（.gitignore無視、隠しファイル含む）
# .git, node_modules, .next などは除外
rg --files --hidden --no-ignore \
  --glob '!.git/' \
  --glob '!node_modules/' \
  --glob '!.next/' \
  --glob '!*.lock' \
  --glob '!package-lock.json' \
  | grep -i "$query" \
  | head -20
