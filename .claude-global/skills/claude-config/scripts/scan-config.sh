#!/usr/bin/env bash
set -euo pipefail

# scan-config.sh
# Claude Code 設定を決定論的にスキャンし、構造化テキストで出力する

PROJECT_ROOT="${PWD}"
CLAUDE_DIR="${PROJECT_ROOT}/.claude"
GLOBAL_CLAUDE_DIR="${HOME}/.claude"

# .claude-global のシンボリックリンク元を特定（dotfiles 構成を考慮）
GLOBAL_SKILLS_DIR=""
if [ -d "${HOME}/.dotfiles/.claude-global/skills" ]; then
  GLOBAL_SKILLS_DIR="${HOME}/.dotfiles/.claude-global/skills"
elif [ -d "${HOME}/.claude/skills" ]; then
  GLOBAL_SKILLS_DIR="${HOME}/.claude/skills"
fi

# ──────────────────────────────────────────────────────────
# セクション: CLAUDE.md
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
else
  echo "exists: false"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション: rules/
# ──────────────────────────────────────────────────────────
echo "=== rules/ ==="
RULES_DIR="${CLAUDE_DIR}/rules"
if [ -d "${RULES_DIR}" ]; then
  # ファイルを決定論的にソート
  ls "${RULES_DIR}" 2>/dev/null | sort | while IFS= read -r fname; do
    fpath="${RULES_DIR}/${fname}"
    if [ -f "${fpath}" ]; then
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
    fi
  done
else
  echo "(directory not found: ${RULES_DIR})"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション: skills/
# ──────────────────────────────────────────────────────────
echo "=== skills/ ==="

# スキルディレクトリのスキャン関数
scan_skills_dir() {
  local base_dir="${1}"
  local label="${2}"

  if [ ! -d "${base_dir}" ]; then
    return
  fi

  ls "${base_dir}" 2>/dev/null | sort | while IFS= read -r dname; do
    dpath="${base_dir}/${dname}"
    if [ ! -d "${dpath}" ]; then
      continue
    fi

    skill_md="${dpath}/SKILL.md"
    if [ -f "${skill_md}" ]; then
      slines=""
      slines="$(wc -l < "${skill_md}" | tr -d ' ' || echo "0")"

      # name: の有無
      name_val=""
      name_val="$(grep -m1 '^name:' "${skill_md}" | sed 's/^name:[[:space:]]*//' || true)"

      # description: の有無（値があるか）
      desc_present="absent"
      desc_val=""
      desc_val="$(grep -m1 '^description:' "${skill_md}" | sed 's/^description:[[:space:]]*//' || true)"
      if [ -n "${desc_val}" ]; then
        desc_present="present"
      fi

      if [ -n "${name_val}" ]; then
        echo "dir: ${label}/${dname}/ [SKILL.md: ${slines} lines, name: ${name_val}, description: ${desc_present}]"
      else
        echo "dir: ${label}/${dname}/ [SKILL.md: ${slines} lines, name: (none), description: ${desc_present}]"
      fi
    else
      echo "dir: ${label}/${dname}/ [SKILL.md: not found]"
    fi
  done
}

# .claude/skills/
LOCAL_SKILLS="${CLAUDE_DIR}/skills"
if [ -d "${LOCAL_SKILLS}" ]; then
  scan_skills_dir "${LOCAL_SKILLS}" ".claude/skills"
else
  echo "(.claude/skills/ not found)"
fi

# .claude-global/skills/ (dotfiles 経由)
if [ -n "${GLOBAL_SKILLS_DIR}" ]; then
  scan_skills_dir "${GLOBAL_SKILLS_DIR}" ".claude-global/skills"
else
  echo "(.claude-global/skills/ not found)"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション: settings.json
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

  if command -v jq > /dev/null 2>&1; then
    # $schema の有無
    schema_val=""
    schema_val="$(jq -r '.["$schema"] // empty' "${fpath}" 2>/dev/null || true)"
    if [ -n "${schema_val}" ]; then
      echo "  \$schema: ${schema_val}"
    else
      echo "  \$schema: absent"
    fi

    # permissions 構造
    has_permissions=""
    has_permissions="$(jq -r 'if has("permissions") then "present" else "absent" end' "${fpath}" 2>/dev/null || true)"
    echo "  permissions: ${has_permissions}"
    if [ "${has_permissions}" = "present" ]; then
      perm_keys=""
      perm_keys="$(jq -r '.permissions | keys[]' "${fpath}" 2>/dev/null | tr '\n' ', ' | sed 's/,$//' || true)"
      echo "  permissions keys: ${perm_keys}"
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
  else
    echo "  (jq not available)"
  fi
}

scan_settings_file "${CLAUDE_DIR}/settings.json" ".claude/settings.json"
scan_settings_file "${CLAUDE_DIR}/settings.local.json" ".claude/settings.local.json"

echo ""

# ──────────────────────────────────────────────────────────
# セクション: hooks（.claude-global/settings.json も確認）
# ──────────────────────────────────────────────────────────
echo "=== hooks (global settings) ==="

GLOBAL_SETTINGS="${HOME}/.claude/settings.json"
# dotfiles 経由の場合も考慮
if [ -L "${GLOBAL_SETTINGS}" ]; then
  # シンボリックリンクなら実体を解決
  REAL_GLOBAL=""
  REAL_GLOBAL="$(readlink "${GLOBAL_SETTINGS}")"
  case "${REAL_GLOBAL}" in
    /*) GLOBAL_SETTINGS="${REAL_GLOBAL}" ;;
    *)  GLOBAL_SETTINGS="$(dirname "${GLOBAL_SETTINGS}")/${REAL_GLOBAL}" ;;
  esac
fi

if [ -f "${GLOBAL_SETTINGS}" ]; then
  if command -v jq > /dev/null 2>&1; then
    has_hooks=""
    has_hooks="$(jq -r 'if has("hooks") then "present" else "absent" end' "${GLOBAL_SETTINGS}" 2>/dev/null || true)"
    echo "global settings.json hooks: ${has_hooks}"
    if [ "${has_hooks}" = "present" ]; then
      hook_types=""
      hook_types="$(jq -r '.hooks | keys[]' "${GLOBAL_SETTINGS}" 2>/dev/null | tr '\n' ', ' | sed 's/,$//' || true)"
      echo "hooks types: ${hook_types}"
      # 各 hook type の matcher 数
      jq -r '.hooks | to_entries[] | "\(.key): \(.value | length) matcher(s)"' "${GLOBAL_SETTINGS}" 2>/dev/null || true
    fi
  else
    echo "(jq not available)"
  fi
else
  echo "global settings.json: not found"
fi

echo ""

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
