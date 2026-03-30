#!/bin/sh
set -e

INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')

case "$TOOL" in
  Write|Edit) ;;
  *) exit 0 ;;
esac

FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
[ -z "$FILE_PATH" ] && exit 0

case "$FILE_PATH" in
  */.claude/plans/*|*/.claude/research/*|*/.claude/implement/*|*/.claude/discovery/*|*/.claude/review/*|*/.claude/dialogues/*)
    ~/.dotfiles/.claude-global/skills/scripts/open-in-zed.sh "$FILE_PATH" || true
    ;;
esac

exit 0
