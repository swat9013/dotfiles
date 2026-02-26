#!/usr/bin/env bash
# セッションファイルのキャッシュチェック・メッセージ抽出・ステータス判定
# Usage: prepare-sessions.sh SESSION_FILE...
# Output (1行/セッション):
#   CACHED <bytes> <cache_path>
#   NEW <bytes> <tmp_path>
#   (2000バイト未満のセッションは出力なし)

set -euo pipefail

CACHE_DIR=".claude/retrospective-cache"
EXTRACT_SCRIPT="$(dirname "$0")/extract-messages.sh"

mkdir -p "$CACHE_DIR"

for f in "$@"; do
    uuid=$(basename "$f" .jsonl)
    cache_file="$CACHE_DIR/$uuid.txt"

    if [ -f "$cache_file" ]; then
        size=$(wc -c < "$cache_file" | tr -d ' ')
        echo "CACHED $size $cache_file"
    else
        outfile="/tmp/retro_${uuid}.txt"
        "$EXTRACT_SCRIPT" "$f" --max-chars=100000 > "$outfile"
        size=$(wc -c < "$outfile" | tr -d ' ')
        if [ "$size" -le 2000 ]; then
            rm -f "$outfile"
        else
            echo "NEW $size $outfile"
        fi
    fi
done
