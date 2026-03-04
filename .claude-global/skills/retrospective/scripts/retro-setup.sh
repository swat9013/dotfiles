#!/usr/bin/env bash
# 3つの初期化スクリプトを合成するwrapper
# Usage: retro-setup.sh [--since=Nd] [--limit=N]
# Output: SESSIONS / EXISTING_CONTEXT / STATS の3セクション

set -euo pipefail

SCRIPT_DIR="$(dirname "$0")"
ARGS=("$@")

# 1. find-sessions.sh でセッション一覧取得
sessions=$("$SCRIPT_DIR/find-sessions.sh" "${ARGS[@]}")

if [ -z "$sessions" ]; then
    echo "=== SESSIONS ==="
    echo "=== EXISTING_CONTEXT ==="
    echo "=== STATS ==="
    echo "total=0 new=0 cached=0 skipped=0"
    exit 0
fi

# 30件超なら自動で --limit=30 付与して再取得
count=$(echo "$sessions" | wc -l | tr -d ' ')
if [ "$count" -gt 30 ]; then
    sessions=$("$SCRIPT_DIR/find-sessions.sh" "${ARGS[@]}" --limit=30)
    count=30
fi

# 2. prepare-sessions.sh でキャッシュチェック・メッセージ抽出
# shellcheck disable=SC2086
prepared=$("$SCRIPT_DIR/prepare-sessions.sh" $sessions)

# 3. collect-existing-context.sh で既存コンテキスト収集
context=$("$SCRIPT_DIR/collect-existing-context.sh")

# 統計計算
total=0
new_count=0
cached_count=0
skipped=0

if [ -n "$prepared" ]; then
    total=$(echo "$prepared" | wc -l | tr -d ' ')
    new_count=$(echo "$prepared" | grep -c '^NEW ' || true)
    cached_count=$(echo "$prepared" | grep -c '^CACHED ' || true)
fi
skipped=$((count - total))

# 出力
echo "=== SESSIONS ==="
if [ -n "$prepared" ]; then
    while IFS= read -r line; do
        status=$(echo "$line" | awk '{print $1}')
        bytes=$(echo "$line" | awk '{print $2}')
        path=$(echo "$line" | awk '{print $3}')

        if [ "$status" = "NEW" ]; then
            uuid=$(basename "$path" .txt | sed 's/^retro_//')
        else
            uuid=$(basename "$path" .txt)
        fi

        echo "$status $bytes $path $uuid"
    done <<< "$prepared"
fi
echo "=== EXISTING_CONTEXT ==="
echo "$context"
echo "=== STATS ==="
echo "total=$total new=$new_count cached=$cached_count skipped=$skipped"
