#!/bin/sh
set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <filepath>" >&2
    exit 1
fi

# 相対パスを絶対パスに変換
case "$1" in
    /*) FILEPATH="$1" ;;
    *)  FILEPATH="$PWD/$1" ;;
esac

# gitリポジトリルートを取得し、そのプロジェクトのZedウィンドウで開く
REPO_ROOT=$(git -C "$(dirname "$FILEPATH")" rev-parse --show-toplevel 2>/dev/null || true)

if [ -n "$REPO_ROOT" ]; then
    /opt/homebrew/bin/zed "$REPO_ROOT" "$FILEPATH"
else
    /opt/homebrew/bin/zed "$FILEPATH"
fi
