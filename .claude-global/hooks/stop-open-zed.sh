#!/bin/sh
# Stop hook: 蓄積されたファイルをまとめてZedで開く
QUEUE="/tmp/claude-zed-queue-${PPID}"
[ -f "$QUEUE" ] || exit 0

# 重複除去して開く
sort -u "$QUEUE" | while IFS= read -r filepath; do
  [ -n "$filepath" ] && ~/.dotfiles/.claude-global/skills/scripts/open-in-zed.sh "$filepath" || true
done

rm -f "$QUEUE"
exit 0
