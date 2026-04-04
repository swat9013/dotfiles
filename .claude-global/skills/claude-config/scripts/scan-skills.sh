#!/usr/bin/env bash
set -euo pipefail

# scan-skills.sh
# スキルディレクトリを自動スキャンし品質検証する

# WARN_COUNT はサブシェル(while内)で更新されるため、ファイル経由で集計する
TMPFILE="/tmp/scan-skills-warn-$$.txt"
printf '0' > "${TMPFILE}"

# increment_warn: 一時ファイルのカウンタをインクリメント
increment_warn() {
  local prev
  prev="$(cat "${TMPFILE}")"
  printf '%d' "$((prev + 1))" > "${TMPFILE}"
}

# scan_dir_and_count: 指定ディレクトリ配下のスキルを検証する
# $1: base_dir  スキャン対象のベースディレクトリ
# $2: label     出力に使うラベル文字列
scan_dir_and_count() {
  local base_dir="${1}"
  local label="${2}"

  echo "=== Scanning ${label} ==="

  if [ ! -d "${base_dir}" ]; then
    echo "(directory not found: ${base_dir})"
    echo ""
    return
  fi

  find "${base_dir}" -mindepth 1 -maxdepth 1 -type d | sort | while IFS= read -r dpath; do
    dname="${dpath##*/}"

    # _shared と scripts は除外
    if [ "${dname}" = "_shared" ] || [ "${dname}" = "scripts" ]; then
      continue
    fi

    skill_md="${dpath}/SKILL.md"

    if [ ! -f "${skill_md}" ]; then
      echo "[${dname}] SKIP: no SKILL.md"
      echo ""
      continue
    fi

    # 検証1: 行数チェック (≤ 500)
    line_count=""
    line_count="$(wc -l < "${skill_md}" | tr -d ' ')"
    if [ "${line_count}" -le 500 ]; then
      echo "[${dname}] SKILL.md lines: ${line_count} ... PASS"
    else
      echo "[${dname}] SKILL.md lines: ${line_count} ... WARN (> 500)"
      increment_warn
    fi

    # 検証2: description 文字数チェック (≤ 130)
    desc_val=""
    desc_val="$(grep -m1 '^description:' "${skill_md}" | sed 's/^description:[[:space:]]*//' || true)"
    desc_len="${#desc_val}"
    if [ "${desc_len}" -le 130 ]; then
      echo "[${dname}] description length: ${desc_len} ... PASS"
    else
      echo "[${dname}] description length: ${desc_len} ... WARN (> 130)"
      increment_warn
    fi

    # 検証3: Use when チェック
    case "${desc_val}" in
      *"Use when"*)
        echo "[${dname}] description Use when: PASS"
        ;;
      *)
        echo "[${dname}] description Use when: WARN (not found)"
        increment_warn
        ;;
    esac

    # 検証4: .claude/commands/ ディレクトリチェック
    commands_dir="${dpath}/.claude/commands"
    if [ -d "${commands_dir}" ]; then
      echo "[${dname}] .claude/commands/: found ... ALERT (未移行スキルの可能性)"
      increment_warn
    else
      echo "[${dname}] .claude/commands/: not found ... PASS"
    fi

    echo ""
  done
}

# スキャン実行
scan_dir_and_count "${HOME}/.dotfiles/.claude-global/skills" ".claude-global/skills"
scan_dir_and_count "${HOME}/.dotfiles/.claude/skills" ".claude/skills"

# 集計結果出力
TOTAL_WARNS=""
TOTAL_WARNS="$(cat "${TMPFILE}")"
rm -f "${TMPFILE}"

echo "SUMMARY: ${TOTAL_WARNS} WARN(s) found"

exit 0
