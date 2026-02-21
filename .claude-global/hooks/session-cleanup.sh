#!/bin/bash
# セッション終了時にサブエージェントをクリーンアップ
# 背景: Task toolで起動したサブエージェントはdetachedプロセスとして起動され、
#   親セッション終了時にSIGTERMが伝播しないためorphan化する (Issue #20369)
#
# 2段階で処理:
#   1. 現セッションの子サブエージェントを停止（orphan化を予防）
#   2. 既存のorphanサブエージェントを停止（過去の取りこぼしを回収）

# hookの実行コンテキスト: Claude Codeが sh -c でスクリプトを実行
# $PPID = Claude Code本体のPID（sh が exec で置換されるため）
LOG="/tmp/session-cleanup.log"
PARENT_PID=$PPID

echo "$(date '+%Y-%m-%d %H:%M:%S') SessionEnd hook fired (PPID=$PARENT_PID)" >> "$LOG"

# 1. 現セッションの子サブエージェントを停止
if [[ -n "$PARENT_PID" ]] && ps -p "$PARENT_PID" -o comm= 2>/dev/null | grep -q claude; then
  pgrep -P "$PARENT_PID" -x claude 2>/dev/null | while read pid; do
    kill -9 "$pid" 2>/dev/null && echo "  killed child subagent: $pid" >> "$LOG"
  done
fi

# 2. 既存orphanの回収（PPID=1 かつ stream-json = サブエージェント）
pgrep -P 1 -x claude 2>/dev/null | while read pid; do
  args=$(ps -p "$pid" -o args= 2>/dev/null)
  if [[ "$args" == *"stream-json"* ]]; then
    kill -9 "$pid" 2>/dev/null && echo "  killed orphan subagent: $pid" >> "$LOG"
  fi
done

echo "$(date '+%Y-%m-%d %H:%M:%S') SessionEnd hook completed" >> "$LOG"
