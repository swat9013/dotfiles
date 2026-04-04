#!/usr/bin/env bash
set -euo pipefail

# scan-config.sh
# Claude Code 設定を決定論的にスキャンし、構造化テキストで出力する

PROJECT_ROOT="${PWD}"
CLAUDE_DIR="${PROJECT_ROOT}/.claude"

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

# ──────────────────────────────────────────────────────────
# セクション: hooks-scripts
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
    jq -r '
      .hooks
      | to_entries[]
      | .value[]
      | .hooks[]?
      | .command // empty
    ' "${settings_path}" 2>/dev/null | while IFS= read -r cmd; do
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
        else
          echo "  script: ${script_path} [exist: true, executable: false]"
        fi
      else
        echo "  script: ${script_path} [exist: false, executable: false]"
      fi
    done || true
  done
  if [ "${hooks_found}" -eq 0 ]; then
    echo "(no hooks configured)"
  fi
else
  echo "(jq not available)"
fi

echo ""

# ──────────────────────────────────────────────────────────
# セクション: rules-paths-check
# ──────────────────────────────────────────────────────────
echo "=== rules-paths-check ==="
RULES_DIR_CHECK="${CLAUDE_DIR}/rules"
if [ -d "${RULES_DIR_CHECK}" ]; then
  ls "${RULES_DIR_CHECK}" 2>/dev/null | sort | while IFS= read -r fname; do
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
    # カンマ区切りのglobを順に処理
    printf '%s\n' "${paths_value}" | tr ',' '\n' | while IFS= read -r raw_glob; do
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
      echo "  glob: ${trimmed_glob} -> ${match_count} file(s)"
    done || true
  done
else
  echo "(rules directory not found)"
fi
