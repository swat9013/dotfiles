#!/bin/bash
# 現在のセッションファイルを特定
# 引数: プロジェクトパス（デフォルト: カレントディレクトリ）

PROJECT_PATH="${1:-$(pwd)}"
PROJECT_DIR=$(echo "$PROJECT_PATH" | sed 's/[^a-zA-Z0-9]/-/g')
SESSION_DIR="$HOME/.claude/projects/$PROJECT_DIR"

# 最新セッション = 現在実行中のセッション
ls -t "$SESSION_DIR"/*.jsonl 2>/dev/null | grep -v "agent-" | head -1
