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
MODEL=$(echo "$input" | jq -r '.model.display_name // "Claude"')
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path // ""')

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

# Calculate context usage percentage from current_usage (not cumulative totals)
USAGE=$(echo "$input" | jq '.context_window.current_usage')
if [ "$USAGE" != "null" ]; then
    # current_usage contains the actual context window state
    CURRENT_TOKENS=$(echo "$USAGE" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
    if [ "$CONTEXT_SIZE" -gt 0 ]; then
        PERCENT=$((CURRENT_TOKENS * 100 / CONTEXT_SIZE))
    else
        PERCENT=0
    fi
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
if [ -n "$TRANSCRIPT_PATH" ]; then
    # Expand ~ to $HOME
    EXPANDED_PATH="${TRANSCRIPT_PATH/#\~/$HOME}"
    if [ -f "$EXPANDED_PATH" ]; then
        # Get first user message from the session file
        FIRST_MSG=$(grep -m1 '"userType":"external"' "$EXPANDED_PATH" 2>/dev/null | jq -r '.message.content // empty' 2>/dev/null | head -1)
        if [ -n "$FIRST_MSG" ]; then
            # Truncate to 25 characters
            if [ ${#FIRST_MSG} -gt 25 ]; then
                SESSION_TITLE="${FIRST_MSG:0:22}..."
            else
                SESSION_TITLE="$FIRST_MSG"
            fi
        fi
    fi
fi

# Build output
OUTPUT="${GREEN}${DIR_NAME}${RESET}${GIT_INFO} | ${CTX_COLOR}Ctx:${PERCENT}%${RESET}"

if [ -n "$SESSION_TITLE" ]; then
    OUTPUT="${OUTPUT} | ${CYAN}\"${SESSION_TITLE}\"${RESET}"
fi

printf "%b\n" "$OUTPUT"
