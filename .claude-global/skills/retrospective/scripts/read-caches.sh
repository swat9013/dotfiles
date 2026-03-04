#!/usr/bin/env bash
# CACHEDセッションのStage 1結果を一括読み取り
# Usage: read-caches.sh <cache_file>...
# Output: 各ファイル内容をデリミタ付きで連結

set -euo pipefail

for f in "$@"; do
    uuid=$(basename "$f" .txt)
    echo "=== $uuid ==="
    cat "$f"
    echo ""
done
