#!/usr/bin/env bash
set -euo pipefail

# scan-hooks.sh
# settings.json と hooks スクリプトを検証する

PROJECT_ROOT="${PWD}"
CLAUDE_DIR="${PROJECT_ROOT}/.claude"
WARN_COUNT=0

increment_warn() { WARN_COUNT=$((WARN_COUNT + 1)); }

# ──────────────────────────────────────────────────────────
# セクション1: settings.json スキャン + PASS/WARN
# ──────────────────────────────────────────────────────────
echo "=== settings.json ==="

scan_settings_file() {
  local fpath="${1}"
  local label="${2}"

  if [ ! -f "${fpath}" ]; then
    echo "${label}: not found"
    return
  fi

  echo "${label}: exists"

  if ! command -v jq > /dev/null 2>&1; then
    echo "  (jq not available)"
    return
  fi

  # $schema の有無
  schema_val=""
  schema_val="$(jq -r '.["$schema"] // empty' "${fpath}" 2>/dev/null || true)"
  if [ -n "${schema_val}" ]; then
    echo "  \$schema: ${schema_val}"
    echo "  [PASS] \$schema is set"
  else
    echo "  \$schema: absent"
    echo "  [WARN] \$schema is not set"
    increment_warn
  fi

  # permissions 構造
  has_permissions=""
  has_permissions="$(jq -r 'if has("permissions") then "present" else "absent" end' "${fpath}" 2>/dev/null || true)"
  echo "  permissions: ${has_permissions}"
  if [ "${has_permissions}" = "present" ]; then
    perm_keys=""
    perm_keys="$(jq -r '.permissions | keys[]' "${fpath}" 2>/dev/null | tr '\n' ', ' | sed 's/,$//' || true)"
    echo "  permissions keys: ${perm_keys}"

    # allow と deny の矛盾検出
    has_allow=""
    has_allow="$(jq -r 'if .permissions | has("allow") then "yes" else "no" end' "${fpath}" 2>/dev/null || true)"
    has_deny=""
    has_deny="$(jq -r 'if .permissions | has("deny") then "yes" else "no" end' "${fpath}" 2>/dev/null || true)"

    if [ "${has_allow}" = "yes" ] && [ "${has_deny}" = "yes" ]; then
      # allow と deny に同一パターンが存在するか検出
      conflict_count=0
      conflict_count="$(jq -r '
        (.permissions.allow // []) as $allow |
        (.permissions.deny // []) as $deny |
        [ $allow[] | select(. as $a | $deny[] | select(. == $a)) ] | length
      ' "${fpath}" 2>/dev/null || echo "0")"

      if [ "${conflict_count}" -gt 0 ]; then
        echo "  [WARN] permissions.allow と deny に ${conflict_count} 個の矛盾するパターンが存在する"
        increment_warn
      else
        echo "  [PASS] permissions.allow と deny に矛盾なし"
      fi
    fi
  fi

  # hooks の有無と種別
  has_hooks=""
  has_hooks="$(jq -r 'if has("hooks") then "present" else "absent" end' "${fpath}" 2>/dev/null || true)"
  echo "  hooks: ${has_hooks}"
  if [ "${has_hooks}" = "present" ]; then
    hook_types=""
    hook_types="$(jq -r '.hooks | keys[]' "${fpath}" 2>/dev/null | tr '\n' ', ' | sed 's/,$//' || true)"
    echo "  hooks types: ${hook_types}"
  fi
}

scan_settings_file "${CLAUDE_DIR}/settings.json" ".claude/settings.json"
scan_settings_file "${CLAUDE_DIR}/settings.local.json" ".claude/settings.local.json"

echo ""

# ──────────────────────────────────────────────────────────
# セクション2: hooks-scripts スキャン + PASS/WARN
# ──────────────────────────────────────────────────────────
echo "=== hooks-scripts ==="
if command -v jq > /dev/null 2>&1; then
  hooks_found=0
  for settings_path in "${CLAUDE_DIR}/settings.json" "${CLAUDE_DIR}/settings.local.json"; do
    if [ ! -f "${settings_path}" ]; then
      continue
    fi
    has_hooks=""
    has_hooks="$(jq -r 'if has("hooks") then "present" else "absent" end' "${settings_path}" 2>/dev/null || true)"
    if [ "${has_hooks}" != "present" ]; then
      continue
    fi
    hooks_found=1
    echo "from: ${settings_path}"
    while IFS= read -r cmd; do
      # コマンドの先頭トークンをスクリプトパスとして抽出
      script_path=""
      script_path="$(printf '%s' "${cmd}" | sed 's/[[:space:]].*//')"
      if [ -z "${script_path}" ]; then
        continue
      fi
      # チルダ展開（先頭の ~ を $HOME に置換）
      script_path="$(printf '%s' "${script_path}" | sed "s|^~|${HOME}|")"
      if [ -e "${script_path}" ]; then
        if [ -x "${script_path}" ]; then
          echo "  script: ${script_path} [exist: true, executable: true]"
          echo "  [PASS] exist: true"
          echo "  [PASS] executable: true"
        else
          echo "  script: ${script_path} [exist: true, executable: false]"
          echo "  [PASS] exist: true"
          echo "  [WARN] executable: false"
          increment_warn
        fi
      else
        echo "  script: ${script_path} [exist: false, executable: false]"
        echo "  [WARN] exist: false"
        echo "  [WARN] executable: false"
        increment_warn
        increment_warn
      fi
    done < <(jq -r '
      .hooks
      | to_entries[]
      | .value[]
      | .hooks[]?
      | .command // empty
    ' "${settings_path}" 2>/dev/null || true)
  done
  if [ "${hooks_found}" -eq 0 ]; then
    echo "(no hooks configured)"
  fi
else
  echo "(jq not available)"
fi

echo ""
echo "SUMMARY: ${WARN_COUNT} WARN(s) found"
