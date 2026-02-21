#!/bin/bash
# 期間フィルタ付きで複数セッションファイルパスを返す
# 引数: [PROJECT_PATH] [--since=Nd | --since=YYYY-MM-DD] [--limit=N]
# 出力: セッションファイルパス（改行区切り、新しい順）

# デフォルト値
SINCE_DATE=$(date +%Y-%m-%d)
LIMIT=0  # 0=制限なし
PROJECT_PATH=""

# 引数解析（位置引数とオプションを分離）
for arg in "$@"; do
  case "$arg" in
    --since=*d)
      DAYS="${arg#--since=}"
      DAYS="${DAYS%d}"
      SINCE_DATE=$(date -v-"${DAYS}"d +%Y-%m-%d 2>/dev/null || date -d "-${DAYS} days" +%Y-%m-%d)
      ;;
    --since=*)
      SINCE_DATE="${arg#--since=}"
      ;;
    --limit=*)
      LIMIT="${arg#--limit=}"
      ;;
    --*)
      # 未知のオプションは無視
      ;;
    *)
      # オプションでない最初の引数をPROJECT_PATHとして取得
      if [ -z "$PROJECT_PATH" ]; then
        PROJECT_PATH="$arg"
      fi
      ;;
  esac
done

PROJECT_PATH="${PROJECT_PATH:-$(pwd)}"
PROJECT_DIR=$(echo "$PROJECT_PATH" | sed 's/[^a-zA-Z0-9]/-/g')
SESSION_DIR="$HOME/.claude/projects/$PROJECT_DIR"

# セッションディレクトリが存在しない場合は空出力で正常終了
[ -d "$SESSION_DIR" ] || exit 0

# mtimeベースでフィルタ（find -newermt）
files=$(find "$SESSION_DIR" -maxdepth 1 -name "*.jsonl" -not -name "agent-*" \
  -newermt "$SINCE_DATE" 2>/dev/null)

[ -z "$files" ] && exit 0

# mtime の新しい順にソート（ls -t）
echo "$files" | xargs ls -1t | \
  if [ "$LIMIT" -gt 0 ]; then head -"$LIMIT"; else cat; fi
