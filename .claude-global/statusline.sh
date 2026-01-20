#!/bin/bash
#==============================================================================
# claude-statusline.sh - Claude Code Statusline Script
#==============================================================================
# Display: dir git:branch* | Model | Ctx:XX% | "session title..."
#
# Color scheme:
#   - Directory: Green
#   - Git branch: Yellow
#   - Context %: Green (0-50), Yellow (50-70), Red (70+)
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
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path // ""')
MODEL=$(echo "$input" | jq -r '.model.display_name // "Claude"')

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

# Context usage (v2.1.6+)
PERCENT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
[[ ! "$PERCENT" =~ ^[0-9]+$ ]] && PERCENT=0

# Color based on usage (50%: green, 50-70%: yellow, 70%+: red)
if [ "$PERCENT" -lt 50 ]; then
    CTX_COLOR="${GREEN}"
elif [ "$PERCENT" -lt 70 ]; then
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
OUTPUT="${GREEN}${DIR_NAME}${RESET}${GIT_INFO} | ${MODEL} | ${CTX_COLOR}Ctx:${PERCENT}%${RESET}"

if [ -n "$SESSION_TITLE" ]; then
    OUTPUT="${OUTPUT} | ${CYAN}\"${SESSION_TITLE}\"${RESET}"
fi

printf "%b\n" "$OUTPUT"
