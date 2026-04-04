#!/bin/sh
# Stop/PermissionRequest hook: 蓄積されたファイルをまとめてZedで開く
# Claude停止時・権限確認待ち時にキューをフラッシュする
INPUT=$(cat)
if [ "$(printf '%s\n' "$INPUT" | jq -r '.stop_hook_active' 2>/dev/null)" = "true" ]; then
  exit 0
fi
QUEUE="/tmp/claude-zed-queue-${PPID}"
[ -f "$QUEUE" ] || exit 0

# 重複除去して開く
sort -u "$QUEUE" | while IFS= read -r filepath; do
  [ -n "$filepath" ] && ~/.dotfiles/.claude-global/skills/scripts/open-in-zed.sh "$filepath" || true
done

rm -f "$QUEUE"
exit 0
