#!/usr/bin/env bash
set -euo pipefail

# scan-claude-md.sh
# CLAUDE.md と rules/ を検証する

PROJECT_ROOT="${PWD}"
CLAUDE_DIR="${PROJECT_ROOT}/.claude"
WARN_COUNT=0

increment_warn() { WARN_COUNT=$((WARN_COUNT + 1)); }

# ──────────────────────────────────────────────────────────
# セクション1: CLAUDE.md スキャン + PASS/WARN
# ──────────────────────────────────────────────────────────
echo "=== CLAUDE.md ==="
CLAUDE_MD="${PROJECT_ROOT}/CLAUDE.md"
if [ -f "${CLAUDE_MD}" ]; then
  echo "exists: true"

  LINE_COUNT=""
  LINE_COUNT="$(wc -l < "${CLAUDE_MD}" | tr -d ' ' || echo "0")"
  echo "lines: ${LINE_COUNT}"

  echo "sections:"
  grep -E '^## ' "${CLAUDE_MD}" | while IFS= read -r line; do
    echo "  - ${line}"
  done || true

  if [ "${LINE_COUNT}" -gt 150 ]; then
    echo "status: WARN (lines ${LINE_COUNT} > 150)"
    increment_warn
  else
    echo "status: PASS"
  fi
else
  echo "exists: false"
  echo "status: WARN (CLAUDE.md not found)"
  increment_warn
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション2: rules/ スキャン + PASS/WARN
# ──────────────────────────────────────────────────────────
echo "=== rules/ ==="
RULES_DIR="${CLAUDE_DIR}/rules"
if [ -d "${RULES_DIR}" ]; then
  # ファイル一覧を配列に格納してサブシェルを回避
  while IFS= read -r fname; do
    fpath="${RULES_DIR}/${fname}"
    if [ ! -f "${fpath}" ]; then
      continue
    fi
    flines=""
    flines="$(wc -l < "${fpath}" | tr -d ' ' || echo "0")"

    # frontmatter の paths 抽出
    paths_value=""
    paths_value="$(grep -m1 '^paths:' "${fpath}" | sed 's/^paths:[[:space:]]*//' || true)"

    if [ -n "${paths_value}" ]; then
      echo "file: ${fname} (${flines} lines) [paths: ${paths_value}]"
    else
      echo "file: ${fname} (${flines} lines) [paths: none]"
    fi

    if [ "${flines}" -gt 200 ]; then
      echo "  status: WARN (lines ${flines} > 200)"
      increment_warn
    else
      echo "  status: PASS"
    fi
  done < <(ls "${RULES_DIR}" 2>/dev/null | sort)
else
  echo "(directory not found: ${RULES_DIR})"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション3: rules-paths-check + PASS/WARN
# ──────────────────────────────────────────────────────────
echo "=== rules-paths-check ==="
RULES_DIR_CHECK="${CLAUDE_DIR}/rules"
if [ -d "${RULES_DIR_CHECK}" ]; then
  while IFS= read -r fname; do
    fpath="${RULES_DIR_CHECK}/${fname}"
    if [ ! -f "${fpath}" ]; then
      continue
    fi
    paths_value=""
    paths_value="$(grep -m1 '^paths:' "${fpath}" | sed 's/^paths:[[:space:]]*//' || true)"
    if [ -z "${paths_value}" ]; then
      continue
    fi
    echo "rule: ${fname} [paths: ${paths_value}]"
    # カンマ区切りのglobを順に処理（process substitution でサブシェル回避）
    while IFS= read -r raw_glob; do
      # 前後の空白を除去
      trimmed_glob=""
      trimmed_glob="$(printf '%s' "${raw_glob}" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')"
      if [ -z "${trimmed_glob}" ]; then
        continue
      fi
      match_count=0
      # globにパスセパレータが含まれる場合はfind -pathを使用
      case "${trimmed_glob}" in
        */*)
          # findは**を理解しないため*に置換してから検索
          find_glob="$(printf '%s' "${trimmed_glob}" | sed 's/\*\*\/*/\*/g; s/\*\*/\*/g')"
          match_count="$(find "${PROJECT_ROOT}" -path "*/${find_glob}" 2>/dev/null | wc -l | tr -d ' ')"
          ;;
        *)
          match_count="$(find "${PROJECT_ROOT}" -name "${trimmed_glob}" 2>/dev/null | wc -l | tr -d ' ')"
          ;;
      esac
      if [ "${match_count}" -eq 0 ]; then
        echo "  glob: ${trimmed_glob} -> ${match_count} file(s) [WARN: no matches]"
        increment_warn
      else
        echo "  glob: ${trimmed_glob} -> ${match_count} file(s) [PASS]"
      fi
    done < <(printf '%s\n' "${paths_value}" | tr ',' '\n')
  done < <(ls "${RULES_DIR_CHECK}" 2>/dev/null | sort)
else
  echo "(rules directory not found)"
fi

echo ""
echo "SUMMARY: ${WARN_COUNT} WARN(s) found"
