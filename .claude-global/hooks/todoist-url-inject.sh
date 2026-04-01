#!/bin/bash
# UserPromptSubmit hook: Todoist URL検出→タスク情報自動注入
# set -e 不使用（hookアンチパターン準拠: 途中失敗でもexit 0で無害終了）

INPUT=$(cat)

# プロンプト抽出（失敗時はサイレント終了）
PROMPT=$(printf '%s\n' "$INPUT" | jq -r '.prompt // empty' 2>/dev/null)
[ -z "$PROMPT" ] && exit 0

# Todoist タスクURL検出（最初の1件のみ）
URL=$(printf '%s\n' "$PROMPT" | grep -oE 'https://app\.todoist\.com/app/task/[^ )"'"'"'<>]*' | head -1)
[ -z "$URL" ] && exit 0

# 未認証チェック（config未存在→スキップ）
[ ! -f "$HOME/.config/todoist/config.json" ] && exit 0

# スクリプトパス確認
SCRIPT="$HOME/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py"
[ ! -f "$SCRIPT" ] && exit 0

# タスク情報取得（15秒タイムアウト: uv初回起動コスト考慮）
if ! RESULT=$(timeout 15 /opt/homebrew/bin/uv run "$SCRIPT" get "$URL" 2>/dev/null); then
  exit 0
fi
[ -z "$RESULT" ] && exit 0

# stdout出力（Claude への注入）
printf '## Todoist タスク情報（自動取得）\n%s\n' "$RESULT"
