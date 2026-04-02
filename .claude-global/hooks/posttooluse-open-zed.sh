#!/bin/sh
INPUT=$(cat)
FILE_PATH=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.file_path // empty' 2>/dev/null) || exit 0
[ -z "$FILE_PATH" ] && exit 0

case "$FILE_PATH" in
  */.claude/plans/*|*/.claude/research/*|*/.claude/implement/*|*/.claude/discovery/*|*/.claude/dialogues/*)
    ~/.dotfiles/.claude-global/skills/scripts/open-in-zed.sh "$FILE_PATH" || true
    ;;
esac

exit 0
