#!/bin/sh
# PostToolUse: 対象ファイルのパスを一時ファイルに蓄積（実際のopenはStop hookが担当）
INPUT=$(cat)
FILE_PATH=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.file_path // empty' 2>/dev/null) || exit 0
[ -z "$FILE_PATH" ] && exit 0

QUEUE="/tmp/claude-zed-queue-${PPID}"

case "$FILE_PATH" in
  */.claude/plans/*|*/.claude/research/*|*/.claude/implement/*|*/.claude/discovery/*|*/.claude/dialogues/*)
    printf '%s\n' "$FILE_PATH" >> "$QUEUE"
    ;;
esac

exit 0
