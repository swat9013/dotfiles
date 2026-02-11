#!/usr/bin/env bash
# skills配下の各SKILL.mdをclaude -pで個別レビュー・修正する
#
# Usage: review-skills.sh [-n ITERATIONS] [-p PARALLEL] [-m MODEL] [SKILL_NAME]
#   -n  レビュー回数（デフォルト: 3）
#   -p  並列数（デフォルト: 3）
#   -m  モデル（デフォルト: opus）
#   SKILL_NAME  特定スキルのみ対象（省略で全スキル）

set -uo pipefail

# --- オプション解析 ---
ITERATIONS=3
PARALLEL=3
MODEL=opus
while getopts "n:p:m:" opt; do
  case "$opt" in
    n) ITERATIONS="$OPTARG" ;;
    p) PARALLEL="$OPTARG" ;;
    m) MODEL="$OPTARG" ;;
    *) echo "Usage: $0 [-n iterations] [-p parallel] [-m model] [skill-name]" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))
TARGET_SKILL="${1:-}"

# --- 子プロセス管理 ---
CHILD_PIDS=()

cleanup() {
  echo ""
  echo "中断: 子プロセスを停止中..."
  for pid in "${CHILD_PIDS[@]}"; do
    kill "$pid" 2>/dev/null
  done
  wait 2>/dev/null
  echo "停止完了"
  exit 130
}
trap cleanup INT TERM

# --- パス設定 ---
SKILLS_DIR="$HOME/.dotfiles/.claude-global/skills"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
LOG_DIR="$PROJECT_ROOT/tmp/review-skills-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$LOG_DIR"

# --- レビュー用プロンプト ---
REVIEW_PROMPT='あなたはClaude Codeスキルの品質レビュアーです。

## レビュー対象
以下のSKILL.mdファイルをレビューし、問題があれば直接修正してください。

対象ファイル: {{SKILL_PATH}}

## レビュー基準

managing-skillsスキルを呼び出し、そのガイドライン・チェックリスト・設計パターンをレビュー基準として使用してください。

## 修正ルール
- 問題があればEditツールで直接修正する
- 修正不要なら「修正不要」とだけ出力する
- references/やtemplates/配下のファイルも必要に応じて読み、整合性を確認する
- 既存の構造や設計意図を尊重し、意味を変えない範囲で改善する
- descriptionのトリガーキーワードは、ユーザーが自然に使う言葉を網羅する

## 出力形式
修正した場合:
```
[修正] 修正内容の1行要約
```
修正不要の場合:
```
[OK] 修正不要
```'

# --- スキルディレクトリ一覧 ---
if [[ -n "$TARGET_SKILL" ]]; then
  SKILL_DIRS=("$SKILLS_DIR/$TARGET_SKILL")
  if [[ ! -d "${SKILL_DIRS[0]}" ]]; then
    echo "エラー: スキル '$TARGET_SKILL' が見つかりません" >&2
    exit 1
  fi
else
  SKILL_DIRS=()
  for d in "$SKILLS_DIR"/*/; do
    [[ "$(basename "$d")" == "managing-skills" ]] && continue
    [[ -f "$d/SKILL.md" ]] && SKILL_DIRS+=("${d%/}")
  done
fi

TOTAL=${#SKILL_DIRS[@]}
echo "=== Skills Review ==="
echo "対象: ${TOTAL}スキル × ${ITERATIONS}回（並列: ${PARALLEL}, モデル: ${MODEL}）"
echo "ログ: $LOG_DIR"
echo ""

# --- レビュー実行 ---
review_skill() {
  local skill_dir="$1"
  local iter="$2"
  local skill_name
  skill_name=$(basename "$skill_dir")
  local skill_path="$skill_dir/SKILL.md"
  local log_file="$LOG_DIR/${skill_name}_iter${iter}.log"

  local prompt="${REVIEW_PROMPT//\{\{SKILL_PATH\}\}/$skill_path}"

  if claude -p "$prompt" \
      --allowedTools 'Read,Edit,Glob,Grep,Skill' \
      --model "$MODEL" \
      --add-dir "$SKILLS_DIR" \
      > "$log_file" 2>&1; then
    local result
    result=$(grep -E '^\[(修正|OK)\]' "$log_file" | head -1) || true
    # 結果ファイルに書き出し（並列時のターミナル混在防止）
    echo "${result:-[?] 結果不明}" > "$LOG_DIR/${skill_name}_iter${iter}.result"
  else
    echo "claude実行失敗" > "$LOG_DIR/${skill_name}_iter${iter}.result"
  fi
}

for ((iter=1; iter<=ITERATIONS; iter++)); do
  echo "--- Iteration $iter/$ITERATIONS ---"

  for ((i=0; i<TOTAL; i+=PARALLEL)); do
    CHILD_PIDS=()
    for ((j=i; j<i+PARALLEL && j<TOTAL; j++)); do
      skill_name=$(basename "${SKILL_DIRS[$j]}")
      echo "[$((j+1))/$TOTAL] $skill_name (iter $iter) 開始..."
      review_skill "${SKILL_DIRS[$j]}" "$iter" &
      CHILD_PIDS+=($!)
    done

    for pid in "${CHILD_PIDS[@]}"; do
      wait "$pid" || true
    done

    # バッチ完了後にまとめて結果表示（出力混在防止）
    for ((k=i; k<i+PARALLEL && k<TOTAL; k++)); do
      local_name=$(basename "${SKILL_DIRS[$k]}")
      result_file="$LOG_DIR/${local_name}_iter${iter}.result"
      if [[ -f "$result_file" ]]; then
        echo "  [完了] $local_name: $(cat "$result_file")"
      fi
    done
  done

  echo ""
done

echo "=== 完了 ==="
echo "ログディレクトリ: $LOG_DIR"
