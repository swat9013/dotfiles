#!/bin/sh
INPUT=$(cat)
FILE_PATH=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.file_path // .tool_response.filePath // empty' 2>/dev/null)
[ -z "$FILE_PATH" ] && exit 0
command -v uvx >/dev/null 2>&1 || exit 0

RESULT=$(uvx ruff check --select E9 "$FILE_PATH" 2>/dev/null) || true

if [ -n "$RESULT" ]; then
  printf '%s\n' "$RESULT" | jq -Rs '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":("ruff構文チェック:\n" + .)}}'
fi

exit 0
