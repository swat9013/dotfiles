#!/bin/bash
# Claude Code file suggestion script
# git管理外のファイルも含めて検索する

query=$(cat | jq -r '.query')
cd "$CLAUDE_PROJECT_DIR" || exit 1

# rgで全ファイルを取得、fzyでファジーマッチング
# fzy -e: ノンインタラクティブモード（スコア順でソート）
rg --files --hidden --no-ignore \
  --glob '!.git/' \
  --glob '!node_modules/' \
  --glob '!.next/' \
  --glob '!*.lock' \
  --glob '!package-lock.json' \
  --glob '!__pycache__/' \
  --glob '!*.pyc' \
  --glob '!*.pyo' \
  --glob '!.pytest_cache/' \
  --glob '!.mypy_cache/' \
  --glob '!*.egg-info/' \
  --glob '!.venv/' \
  --glob '!venv/' \
  | fzy -e "$query" -l 20
