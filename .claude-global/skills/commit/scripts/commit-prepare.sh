#!/usr/bin/env bash
# commit-prepare.sh - コミット前の情報収集と状態判定
#
# Usage: commit-prepare.sh
#
# 出力:
#   RESULT: STAGED    ステージ済み変更あり（diff stat, recent log, full diff を含む）
#   RESULT: UNSTAGED  未ステージ/未追跡のみ（ファイル一覧を含む）
#   RESULT: NO_CHANGES 変更なし

set -uo pipefail

if ! git rev-parse --is-inside-work-tree &>/dev/null; then
  echo "ERROR: Not a git repository" >&2
  exit 1
fi

STAGED=$(git diff --cached --name-only 2>/dev/null)

if [[ -n "$STAGED" ]]; then
  echo "=== Commit Prepare ==="
  echo ""
  echo "--- Staged Files ---"
  git diff --cached --stat
  echo ""
  echo "--- Recent Commits ---"
  git log --oneline -5
  echo ""
  echo "--- Diff ---"
  git diff --cached
  echo ""
  echo "RESULT: STAGED"
else
  UNSTAGED=$(git diff --name-only 2>/dev/null)
  UNTRACKED=$(git ls-files --others --exclude-standard 2>/dev/null)

  if [[ -n "$UNSTAGED" ]] || [[ -n "$UNTRACKED" ]]; then
    echo "=== Commit Prepare ==="
    echo ""
    echo "--- Unstaged/Untracked Files ---"
    git status --short
    echo ""
    echo "RESULT: UNSTAGED"
  else
    echo "=== Commit Prepare ==="
    echo ""
    echo "RESULT: NO_CHANGES"
  fi
fi
