#!/usr/bin/env bash
set -euo pipefail

# scan-config.sh
# Claude Code 設定を決定論的にスキャンし、構造化テキストで出力する

PROJECT_ROOT="${PWD}"
CLAUDE_DIR="${PROJECT_ROOT}/.claude"

# ──────────────────────────────────────────────────────────
# セクション: .claude/ ディレクトリ構成（1階層）
# ──────────────────────────────────────────────────────────
echo "=== .claude/ directory structure (1 level) ==="
if [ -d "${CLAUDE_DIR}" ]; then
  ls "${CLAUDE_DIR}" 2>/dev/null | sort | while IFS= read -r entry; do
    epath="${CLAUDE_DIR}/${entry}"
    if [ -d "${epath}" ]; then
      echo "  ${entry}/"
    else
      echo "  ${entry}"
    fi
  done
else
  echo "(directory not found: ${CLAUDE_DIR})"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション: project-context
# ──────────────────────────────────────────────────────────
echo "=== project-context ==="
for fname in package.json pyproject.toml Cargo.toml go.mod Makefile; do
  if [ -f "${PROJECT_ROOT}/${fname}" ]; then
    echo "${fname}: exists: true"
  else
    echo "${fname}: exists: false"
  fi
done
for dname in ".github/workflows" ".gitlab-ci.yml"; do
  if [ -e "${PROJECT_ROOT}/${dname}" ]; then
    echo "${dname}: exists: true"
  else
    echo "${dname}: exists: false"
  fi
done

echo ""
