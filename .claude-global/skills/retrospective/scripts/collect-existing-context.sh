#!/usr/bin/env bash
# CLAUDE.md見出しとrules/ファイル一覧を収集（Stage 1 重複回避用コンテキスト）
# Usage: collect-existing-context.sh

set -euo pipefail

echo "## CLAUDE.md セクション見出し"
if [ -f "./CLAUDE.md" ]; then
    grep '^##' ./CLAUDE.md || echo "(なし)"
else
    echo "(CLAUDE.mdなし)"
fi

echo ""
echo "## rules/ ファイル一覧"
if [ -d ".claude/rules" ]; then
    ls .claude/rules/ 2>/dev/null || echo "(なし)"
else
    echo "(rules/なし)"
fi
