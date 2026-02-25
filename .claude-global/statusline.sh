#!/bin/bash
#==============================================================================
# claude-statusline.sh - Claude Code Statusline Script
#==============================================================================
# Two-line layout:
#   Line 1: dir git:branch*              (location context)
#   Line 2: Ctx:XX% | Model | "title..." (session state)
#
# Dir/branch names are capped to prevent Line 2 from being pushed off-screen.
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

# Max lengths for dir/branch (fixed caps to keep lines short)
MAX_DIR=20
MAX_BRANCH=20

# Check jq dependency
if ! command -v jq &> /dev/null; then
    echo "[Claude]"
    exit 0
fi

# Truncate string with ellipsis
truncate() {
    local str="$1" max="$2"
    if [ "${#str}" -gt "$max" ]; then
        printf '%s…' "${str:0:$((max-1))}"
    else
        printf '%s' "$str"
    fi
}

# Read JSON from stdin
input=$(cat)

# Extract values from JSON
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path // ""')
MODEL=$(echo "$input" | jq -r '.model.display_name // "Claude"')

# Expand transcript path once (used for session title)
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
GIT_BRANCH=""
GIT_DIRTY=""
if git -C "$CURRENT_DIR" rev-parse --git-dir > /dev/null 2>&1; then
    GIT_BRANCH=$(git -C "$CURRENT_DIR" --no-optional-locks branch --show-current 2>/dev/null || echo "")
    if [ -n "$GIT_BRANCH" ]; then
        if ! git -C "$CURRENT_DIR" --no-optional-locks diff --quiet 2>/dev/null || \
           ! git -C "$CURRENT_DIR" --no-optional-locks diff --cached --quiet 2>/dev/null; then
            GIT_DIRTY="*"
        fi
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
# || true prevents pipefail exit when grep finds no match
SESSION_TITLE=""
if [ -n "$EXPANDED_TRANSCRIPT" ]; then
    FIRST_MSG=$(grep -m1 '"userType":"external"' "$EXPANDED_TRANSCRIPT" 2>/dev/null \
        | jq -r '.message.content // empty' 2>/dev/null \
        | head -1 || true)
    [ -n "$FIRST_MSG" ] && SESSION_TITLE="$FIRST_MSG"
fi

# Adaptive title truncation based on terminal width
COLS=$(tput cols 2>/dev/null || echo 80)

if [ "$COLS" -lt 60 ]; then
    MAX_TITLE=15
elif [ "$COLS" -lt 80 ]; then
    MAX_TITLE=20
else
    MAX_TITLE=30
fi

# Line 1: location context (dir + git branch)
DIR_DISPLAY=$(truncate "$DIR_NAME" "$MAX_DIR")
LINE1="${GREEN}${DIR_DISPLAY}${RESET}"

if [ -n "$GIT_BRANCH" ]; then
    BRANCH_DISPLAY=$(truncate "$GIT_BRANCH" "$MAX_BRANCH")
    LINE1="${LINE1} ${YELLOW}git:${BRANCH_DISPLAY}${GIT_DIRTY}${RESET}"
fi

# Line 2: session state (ctx% + model + title)
LINE2="${CTX_COLOR}Ctx:${PERCENT}%${RESET} | ${MODEL}"

if [ -n "$SESSION_TITLE" ]; then
    TITLE_DISPLAY=$(truncate "$SESSION_TITLE" "$MAX_TITLE")
    LINE2="${LINE2} | ${CYAN}\"${TITLE_DISPLAY}\"${RESET}"
fi

printf "%b\n" "$LINE1"
printf "%b\n" "$LINE2"
