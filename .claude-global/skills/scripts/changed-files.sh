#!/usr/bin/env bash
# changed-files.sh - レビュー対象ファイルの特定と規模・種別の報告
#
# Usage: changed-files.sh [--base=REF]
#
# --base: 比較基準のgit ref（デフォルト: HEAD）
#         未コミット変更 → HEAD
#         ブランチ比較 → main, origin/main 等

# set -e は意図的に省略: git diff/ls-files が空結果で非ゼロ終了する場合の || true 処理と両立させるため
set -uo pipefail

# --- オプション解析 ---
BASE_REF="HEAD"

for arg in "$@"; do
  case "$arg" in
    --base=*) BASE_REF="${arg#--base=}" ;;
    --help|-h)
      echo "Usage: changed-files.sh [--base=REF]"
      exit 0
      ;;
    -*) echo "Unknown option: $arg" >&2; exit 1 ;;
  esac
done

# --- git リポジトリ確認 ---
if ! git rev-parse --is-inside-work-tree &>/dev/null; then
  echo "ERROR: Not a git repository" >&2
  exit 1
fi

# --- 変更ファイル取得 ---
# HEAD比較の場合: ステージ済み + 未ステージ + 未追跡
# ブランチ比較の場合: REF...HEAD の差分
if [[ "$BASE_REF" == "HEAD" ]]; then
  CHANGED_FILES=$(
    {
      git diff --name-only HEAD 2>/dev/null || true
      git diff --name-only --cached 2>/dev/null || true
      git ls-files --others --exclude-standard 2>/dev/null || true
    } | sort -u
  )
else
  CHANGED_FILES=$(
    {
      git diff --name-only "${BASE_REF}...HEAD" 2>/dev/null || true
      git ls-files --others --exclude-standard 2>/dev/null || true
    } | sort -u
  )
fi

# --- 変更なしチェック ---
if [[ -z "$CHANGED_FILES" ]]; then
  echo "=== Changed Files ==="
  echo "Files: 0"
  echo ""
  echo "RESULT: NO_CHANGES"
  exit 0
fi

# --- 統計計算 ---
# 行数は tracked ファイルのみ。untracked ファイルは FILE_COUNT に含まれるが行数には含まれない
FILE_COUNT=$(echo "$CHANGED_FILES" | wc -l | tr -d ' ')

# 変更行数カウント
if [[ "$BASE_REF" == "HEAD" ]]; then
  # git diff HEAD = staged + unstaged の合算。--cached との併用は二重計上になるため単独使用
  LINES_CHANGED=$(
    git diff --shortstat HEAD 2>/dev/null | awk '{
      for(i=1;i<=NF;i++) {
        if($(i+1) ~ /insertion/) ins += $i
        if($(i+1) ~ /deletion/) del += $i
      }
    } END { print ins + del + 0 }'
  )
else
  LINES_CHANGED=$(
    git diff --shortstat "${BASE_REF}...HEAD" 2>/dev/null | awk '{
      for(i=1;i<=NF;i++) {
        if($(i+1) ~ /insertion/) ins += $i
        if($(i+1) ~ /deletion/) del += $i
      }
    } END { print ins + del + 0 }'
  )
fi

# --- ファイル分類 ---
CODE_FILES=0
CONFIG_FILES=0
TEST_FILES=0
DOC_FILES=0

while IFS= read -r file; do
  [[ -z "$file" ]] && continue

  case "$file" in
    *.test.*|*.spec.*|*_test.*|*_spec.*|test_*|tests/*)
      TEST_FILES=$((TEST_FILES + 1))
      ;;
    *.md|*.txt|*.rst|docs/*)
      DOC_FILES=$((DOC_FILES + 1))
      ;;
    *.json|*.yaml|*.yml|*.toml|*.ini|*.cfg|*.conf|.*rc|.gitignore|Makefile|Dockerfile)
      CONFIG_FILES=$((CONFIG_FILES + 1))
      ;;
    *.ts|*.tsx|*.js|*.jsx|*.py|*.rb|*.go|*.rs|*.java|*.kt|*.swift|*.c|*.cpp|*.h|*.sh|*.zsh|*.bash)
      CODE_FILES=$((CODE_FILES + 1))
      ;;
    *)
      CODE_FILES=$((CODE_FILES + 1))
      ;;
  esac
done <<< "$CHANGED_FILES"

# --- RESULT判定 ---
RESULT="PROCEED"

if [[ $FILE_COUNT -gt 50 ]]; then
  RESULT="TOO_LARGE"
elif [[ $CODE_FILES -eq 0 ]] && [[ $CONFIG_FILES -gt 0 ]]; then
  RESULT="CONFIG_ONLY"
fi

# --- 出力 ---
echo "=== Changed Files ==="
echo "Base: $BASE_REF"
echo "Files: $FILE_COUNT"
echo "Lines changed: $LINES_CHANGED"
echo "Code files: $CODE_FILES"
echo "Test files: $TEST_FILES"
echo "Config files: $CONFIG_FILES"
echo "Doc files: $DOC_FILES"
echo ""
echo "--- File List ---"
echo "$CHANGED_FILES"
echo ""
echo "RESULT: $RESULT"
