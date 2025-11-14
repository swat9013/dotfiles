#!/bin/bash
#==============================================================================
# vibe-kanban.sh - Vibe Kanban サーバー管理スクリプト
#==============================================================================
#
# 【概要】
#   Vibe Kanban (npx vibe-kanban) をバックグラウンドで起動・停止・再起動・
#   ステータス確認するためのラッパースクリプト
#
# 【使い方】
#   vibe-kanban.sh [command]
#
#   commands:
#     start   - Vibe Kanban をバックグラウンドで起動（デフォルト）
#     stop    - Vibe Kanban を停止
#     restart - Vibe Kanban を再起動
#     status  - Vibe Kanban の稼働状態を確認
#
# 【設定】
#   PORT: 33333
#   ログファイル: work/logs/vibe_kanban.log
#
# 【動作】
#   - プロセス検出: pgrep で vibe-kanban プロセスを検出
#   - 起動: nohup で npx vibe-kanban を起動し、ログをファイルに記録
#   - 停止: SIGTERM で終了を試み、失敗時は SIGKILL で強制終了
#   - 冪等性: すでに起動中の場合は再起動しない（restart コマンド除く）
#
# 【前提条件】
#   - npx コマンドが利用可能であること
#   - work/logs ディレクトリが作成可能であること
#
# 【注意事項】
#   - このスクリプトは dotfiles リポジトリのルートから相対パスで動作
#   - 複数のプロセスが起動している場合、すべて停止される
#
#==============================================================================
set -euo pipefail

# 前提条件チェック
if ! command -v npx >/dev/null 2>&1; then
  echo "Error: npx command not found" >&2
  echo "Please install Node.js and npm first" >&2
  exit 1
fi

# プロジェクトルートに移動
cd "$(dirname "$0")/../../" || {
  echo "Error: Failed to change directory to project root" >&2
  exit 1
}

# 設定
PORT=33333
LOG_FILE="work/logs/vibe_kanban.log"

# ディレクトリ作成（冪等性）
mkdir -p work/logs

# プロセス名でチェック（シンプル版、スクリプト自身は除外）
is_running() {
  pgrep -f "npx.*vibe-kanban|vibe-kanban.*--mcp|vibe-kanban/dist" > /dev/null 2>&1
}

# ログローテーション（10MBを超えたら.oldにリネーム）
rotate_log_if_needed() {
  if [ -f "$LOG_FILE" ]; then
    # macOS と Linux 両対応
    local log_size
    if [[ "$OSTYPE" == "darwin"* ]]; then
      log_size=$(stat -f%z "$LOG_FILE" 2>/dev/null || echo 0)
    else
      log_size=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
    fi

    # 10MB (10485760 bytes) を超えたらローテーション
    if [ "$log_size" -gt 10485760 ]; then
      echo "Log file size: $(($log_size / 1024 / 1024))MB - rotating..."
      mv "$LOG_FILE" "$LOG_FILE.old"
    fi
  fi
}

# 起動処理
start_vibe_kanban() {
  if is_running; then
    echo "Vibe Kanban is already running"
    echo "Use 'stop' command to stop it first, or 'restart' to restart."
    status_vibe_kanban
    return 0
  fi

  # ログローテーション実行
  rotate_log_if_needed

  echo "Starting Vibe Kanban on port $PORT..."
  env PORT="$PORT" nohup npx vibe-kanban >> "$LOG_FILE" 2>&1 &

  # 起動確認（2秒待機）
  sleep 2
  if is_running; then
    echo "Vibe Kanban started successfully"
    echo "Log file: $LOG_FILE"
    status_vibe_kanban
    return 0
  else
    echo "Failed to start Vibe Kanban"
    echo "Check log file: $LOG_FILE"
    return 1
  fi
}

# 停止処理
stop_vibe_kanban() {
  if ! is_running; then
    echo "Vibe Kanban is not running"
    return 0
  fi

  echo "Stopping all vibe-kanban processes..."
  local pids=$(pgrep -f "npx.*vibe-kanban|vibe-kanban.*--mcp|vibe-kanban/dist")

  # 通常終了を試行
  while read -r pid; do
    echo "Stopping process (PID: $pid)..."
    kill "$pid" 2>/dev/null || true
  done <<< "$pids"

  # 終了確認（最大10秒待機）
  for i in {1..10}; do
    if ! is_running; then
      echo "Vibe Kanban stopped successfully"
      return 0
    fi
    sleep 1
  done

  # 強制終了
  if is_running; then
    echo "Force killing remaining processes..."
    local force_pids=$(pgrep -f "npx.*vibe-kanban|vibe-kanban.*--mcp|vibe-kanban/dist")
    while read -r pid; do
      kill -9 "$pid" 2>/dev/null || true
    done <<< "$force_pids"
    sleep 1
  fi

  if is_running; then
    echo "Warning: Some processes may still be running"
    status_vibe_kanban
    return 1
  else
    echo "Vibe Kanban stopped successfully"
    return 0
  fi
}

# 状態確認
status_vibe_kanban() {
  if is_running; then
    echo ""
    echo "Active vibe-kanban processes:"
    ps aux | grep -E "npx.*vibe-kanban|vibe-kanban.*--mcp|vibe-kanban/dist" | grep -v grep | awk '{printf "  PID: %-7s CMD: %s\n", $2, substr($0, index($0,$11))}'
  else
    echo "Vibe Kanban is not running"
  fi
}

# メイン処理
case "${1:-start}" in
  start)
    start_vibe_kanban
    ;;
  stop)
    stop_vibe_kanban
    ;;
  restart)
    if stop_vibe_kanban; then
      sleep 1
      start_vibe_kanban
    else
      echo "Failed to stop, aborting restart" >&2
      exit 1
    fi
    ;;
  status)
    if is_running; then
      echo "Vibe Kanban is running on port $PORT"
    else
      echo "Vibe Kanban is not running"
    fi
    status_vibe_kanban
    ;;
  *)
    echo "Usage: $0 {start|stop|restart|status}"
    echo "  start   - Start Vibe Kanban in background (default)"
    echo "  stop    - Stop Vibe Kanban"
    echo "  restart - Restart Vibe Kanban"
    echo "  status  - Check Vibe Kanban status"
    exit 1
    ;;
esac
