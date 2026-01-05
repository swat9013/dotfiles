#!/bin/bash
# Claude Code 作業完了通知スクリプト
# プロジェクト名と最初のタスクを表示して、どのセッションか識別しやすくする

# stdinからJSONデータを読み取る
INPUT=$(cat)

# 各フィールドを取得
TRANSCRIPT_PATH=$(echo "$INPUT" | jq -r '.transcript_path // empty')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')

# プロジェクト名を取得（cwdの最後のディレクトリ名）
if [ -n "$CWD" ]; then
    PROJECT_NAME=$(basename "$CWD")
else
    PROJECT_NAME="Unknown"
fi

# トランスクリプトがない場合はシンプルな通知
if [ -z "$TRANSCRIPT_PATH" ] || [ ! -f "$TRANSCRIPT_PATH" ]; then
    osascript -e "display notification \"作業が完了しました\" with title \"Claude Code - $PROJECT_NAME\" sound name \"Glass\""
    exit 0
fi

# 最初のユーザーメッセージを取得（セッションの最初のタスク）
# トランスクリプト形式: {"type":"user","message":{"role":"user","content":"..."}}
# isMeta: true のメッセージはシステムメッセージなので除外
FIRST_TASK=$(head -100 "$TRANSCRIPT_PATH" | \
    jq -r 'select(.type == "user" and (.isMeta | not)) | .message.content | select(type == "string")' 2>/dev/null | \
    head -1 | \
    head -c 150 | \
    tr '\n' ' ' | \
    sed 's/"/\\"/g')

# タスクが取得できなかった場合のフォールバック
if [ -z "$FIRST_TASK" ]; then
    FIRST_TASK="作業が完了しました"
fi

# 長すぎる場合は省略
if [ ${#FIRST_TASK} -gt 100 ]; then
    FIRST_TASK="${FIRST_TASK:0:97}..."
fi

# macOS通知を送信（タイトルにプロジェクト名、本文に最初のタスク）
osascript -e "display notification \"$FIRST_TASK\" with title \"Claude Code - $PROJECT_NAME\" sound name \"Glass\""
