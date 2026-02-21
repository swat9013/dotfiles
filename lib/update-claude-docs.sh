#!/bin/bash
# update-claude-docs.sh - docs/claude-code配下のドキュメントを最新仕様に更新
#
# /researcher スキルを使い、Claude Code ヘッドレスモードで
# 各ドキュメントの最新仕様とベストプラクティスを調査・更新する。
#
# 使用方法:
#   ./lib/update-claude-docs.sh                        # 全ファイル更新
#   ./lib/update-claude-docs.sh hooks.md               # 特定ファイルのみ
#   ./lib/update-claude-docs.sh hooks.md settings.md   # 複数指定

set -euo pipefail

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
DOCS_DIR="$DOTFILES_DIR/docs/claude-code"
LOG_DIR="$DOTFILES_DIR/.work/doc-update-logs"
MODEL="sonnet"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# 前提チェック
command -v claude >/dev/null 2>&1 || {
  echo "Error: claude コマンドが見つかりません" >&2
  exit 1
}

[ -d "$DOCS_DIR" ] || {
  echo "Error: $DOCS_DIR が見つかりません" >&2
  exit 1
}

mkdir -p "$LOG_DIR"

# 対象ファイル決定
if [ $# -gt 0 ]; then
  files=()
  for arg in "$@"; do
    target="$DOCS_DIR/$arg"
    [ -f "$target" ] || {
      echo "Error: $target が見つかりません" >&2
      exit 1
    }
    files+=("$target")
  done
else
  files=("$DOCS_DIR"/*.md)
fi

total=${#files[@]}

echo "=== Claude Code ドキュメント更新 ==="
echo "モデル: $MODEL | 対象: $total ファイル"
echo "ログ: $LOG_DIR"
echo ""

success=0
fail=0
current=0

for file in "${files[@]}"; do
  ((current++))
  filename=$(basename "$file")
  topic="${filename%.md}"
  logfile="$LOG_DIR/${topic}_${TIMESTAMP}.log"

  echo -n "[$current/$total] $filename ... "

  prompt=$(cat <<PROMPT
/researcher を使って「Claude Code の ${topic}」について最新仕様とベストプラクティスを調査して。

調査対象ファイル: ${file}

## 出力先の変更

今回は report.md を作成しない。
調査結果は対象ファイルに直接反映する:
1. Read ツールで ${file} の現在の内容を把握
2. 調査結果と比較して差分を特定
3. Edit ツールでファイルを直接更新:
   - 既存の構造・見出しを維持
   - 古い情報を最新に更新
   - 新機能・変更点を適切な位置に追加
   - 非推奨・削除された機能があれば注記
4. 変更サマリーを出力（変更した箇所と理由）

変更がない場合は「変更なし: [理由]」と報告。
PROMPT
  )

  if claude -p \
    --model "$MODEL" \
    --allowed-tools "Skill,Read,Edit,WebSearch,WebFetch" \
    --no-session-persistence \
    "$prompt" \
    > "$logfile" 2>&1; then
    echo "完了"
    ((success++))
  else
    echo "失敗 (詳細: $logfile)"
    ((fail++))
  fi

  # レート制限対策
  sleep 2
done

echo ""
echo "=== 結果 ==="
echo "成功: $success / 失敗: $fail / 合計: $total"
echo "ログ: $LOG_DIR"
