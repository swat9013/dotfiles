#!/bin/bash
#==============================================================================
# claude-statusline.sh - Claude Code Statusline Script
#==============================================================================
# Display: dir git:branch* | Ctx:XX% | "session title..."
#
# Color scheme:
#   - Directory: Green
#   - Git branch: Yellow
#   - Context %: Green (0-50), Yellow (50-80), Red (80+)
#   - Session title: Cyan
#==============================================================================

set -euo pipefail

# ANSI color codes
GREEN='\033[32m'
YELLOW='\033[33m'
RED='\033[31m'
CYAN='\033[36m'
RESET='\033[0m'

# Check jq dependency
if ! command -v jq &> /dev/null; then
    echo "[Claude]"
    exit 0
fi

# Read JSON from stdin
input=$(cat)

# Extract values from JSON
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path // ""')

# Expand transcript path once (used for both context and session title)
EXPANDED_TRANSCRIPT=""
if [ -n "$TRANSCRIPT_PATH" ]; then
    EXPANDED_TRANSCRIPT="${TRANSCRIPT_PATH/#\~/$HOME}"
    [ ! -f "$EXPANDED_TRANSCRIPT" ] && EXPANDED_TRANSCRIPT=""
fi

# Shorten directory name
if [ "$CURRENT_DIR" = "$HOME" ]; then
    DIR_NAME="~"
elif [ "$CURRENT_DIR" = "/" ]; then
    DIR_NAME="/"
else
    DIR_NAME="${CURRENT_DIR##*/}"
fi

# Get git branch and dirty status
GIT_INFO=""
if git -C "$CURRENT_DIR" rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git -C "$CURRENT_DIR" --no-optional-locks branch --show-current 2>/dev/null || echo "")
    if [ -n "$BRANCH" ]; then
        DIRTY=""
        if ! git -C "$CURRENT_DIR" --no-optional-locks diff --quiet 2>/dev/null || \
           ! git -C "$CURRENT_DIR" --no-optional-locks diff --cached --quiet 2>/dev/null; then
            DIRTY="*"
        fi
        GIT_INFO=" ${YELLOW}git:${BRANCH}${DIRTY}${RESET}"
    fi
fi

#------------------------------------------------------------------------------
# Context Usage Calculation
#------------------------------------------------------------------------------
# 調査結果 (2026-01):
#
# 1. /context コマンドの表示値には Autocompact buffer (~45k tokens) が含まれる
#    - 実使用量: System prompt + Tools + Memory + Messages ≈ 63k
#    - /context表示: 上記 + Autocompact buffer ≈ 108k (54%)
#
# 2. context_window.current_usage はタイミングによって null になる
#    - 信頼できないため、プライマリソースとして使用不可
#
# 3. トランスクリプトファイル (JSONL) の message.usage が最も正確
#    - input_tokens + cache_creation_input_tokens + cache_read_input_tokens
#    - これが実際のコンテキストウィンドウ使用量に近い
#
# 参考: ccstatusline, claude-code-statusline も同様の手法を採用
#------------------------------------------------------------------------------
CURRENT_TOKENS=0

# 優先: トランスクリプトファイルから最新のusage情報を取得
if [ -n "$EXPANDED_TRANSCRIPT" ]; then
    # 最新のassistantメッセージからusage情報を取得 (1回のjq呼び出しで計算)
    CURRENT_TOKENS=$(tail -20 "$EXPANDED_TRANSCRIPT" 2>/dev/null \
        | grep '"type":"assistant"' \
        | tail -1 \
        | jq -r '.message.usage | ((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0))' 2>/dev/null) || CURRENT_TOKENS=0
    # 数値でない場合は0にリセット
    [[ ! "$CURRENT_TOKENS" =~ ^[0-9]+$ ]] && CURRENT_TOKENS=0
fi

# フォールバック: JSONのcurrent_usageのみ使用
# 注意: total_input_tokens/total_output_tokensは累積値のため使用しない
#       /new直後など取得できない場合は0%表示が正しい動作
if [ "$CURRENT_TOKENS" -eq 0 ]; then
    USAGE=$(echo "$input" | jq '.context_window.current_usage')
    if [ "$USAGE" != "null" ] && [ -n "$USAGE" ]; then
        CURRENT_TOKENS=$(echo "$USAGE" | jq -r '((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0))') || CURRENT_TOKENS=0
    fi
    [[ ! "$CURRENT_TOKENS" =~ ^[0-9]+$ ]] && CURRENT_TOKENS=0
fi

if [ "$CONTEXT_SIZE" -gt 0 ] && [ "$CURRENT_TOKENS" -gt 0 ]; then
    PERCENT=$((CURRENT_TOKENS * 100 / CONTEXT_SIZE))
else
    PERCENT=0
fi

# Color based on usage
if [ "$PERCENT" -lt 50 ]; then
    CTX_COLOR="${GREEN}"
elif [ "$PERCENT" -lt 80 ]; then
    CTX_COLOR="${YELLOW}"
else
    CTX_COLOR="${RED}"
fi

# Get session title from transcript file (first user message)
SESSION_TITLE=""
if [ -n "$EXPANDED_TRANSCRIPT" ]; then
    FIRST_MSG=$(grep -m1 '"userType":"external"' "$EXPANDED_TRANSCRIPT" 2>/dev/null \
        | jq -r '.message.content // empty' 2>/dev/null \
        | head -1)
    if [ -n "$FIRST_MSG" ]; then
        # Truncate to 25 characters
        if [ ${#FIRST_MSG} -gt 25 ]; then
            SESSION_TITLE="${FIRST_MSG:0:22}..."
        else
            SESSION_TITLE="$FIRST_MSG"
        fi
    fi
fi

# Build output
OUTPUT="${GREEN}${DIR_NAME}${RESET}${GIT_INFO} | ${CTX_COLOR}Ctx:${PERCENT}%${RESET}"

if [ -n "$SESSION_TITLE" ]; then
    OUTPUT="${OUTPUT} | ${CYAN}\"${SESSION_TITLE}\"${RESET}"
fi

printf "%b\n" "$OUTPUT"
