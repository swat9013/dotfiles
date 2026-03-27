#!/bin/bash
#==============================================================================
# claude-statusline.sh - Claude Code Statusline Script
#==============================================================================
# Four-line layout:
#   Line 1: dir git:branch*                          (location context)
#   Line 2: cx ⣿⣿⣤     030% | Model | "title..."   (session state)
#   Line 3: 5h ⣿⣿⣿⣿⣿⣀   050%  Reset 4pm            (5-hour usage)
#   Line 4: 7d ⣿⣿⣿⣿⣿⣿⣷  080%  Reset Mar 6 1pm      (7-day usage)
#
# Lines 3-4 appear only when rate_limits data is available in stdin JSON.
#
# Braille dots (density 8 levels): ' ⣀⣄⣤⣦⣶⣷⣿'
#
# Color scheme:
#   - Directory: Green
#   - Git branch: Yellow
#   - Context/Usage %: Green (0-49), Yellow (50-79), Red (80+)
#   - Session title: Cyan
#==============================================================================

set -euo pipefail

# ANSI color codes
GREEN='\033[32m'
YELLOW='\033[33m'
RED='\033[31m'
CYAN='\033[36m'
DIM='\033[2m'
RESET='\033[0m'

# Braille dots characters (density: empty → full)
BRAILLE=(' ' '⣀' '⣄' '⣤' '⣦' '⣶' '⣷' '⣿')

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

# Color for usage percentage: green (0-49), yellow (50-79), red (80+)
usage_color() {
    local pct="$1"
    if [ "$pct" -lt 50 ]; then
        printf '%s' "$GREEN"
    elif [ "$pct" -lt 80 ]; then
        printf '%s' "$YELLOW"
    else
        printf '%s' "$RED"
    fi
}

# Render braille dot bar from percentage
render_braille_bar() {
    local pct="$1"
    local width=8
    pct=$(( pct > 100 ? 100 : pct ))
    pct=$(( pct < 0 ? 0 : pct ))
    local bar=""
    for ((i=0; i<width; i++)); do
        local seg_start=$(( i * 100 / width ))
        local seg_end=$(( (i + 1) * 100 / width ))
        if [ "$pct" -ge "$seg_end" ]; then
            bar+="${BRAILLE[7]}"
        elif [ "$pct" -le "$seg_start" ]; then
            bar+="${BRAILLE[0]}"
        else
            local frac=$(( (pct - seg_start) * 7 / (seg_end - seg_start) ))
            [ "$frac" -gt 7 ] && frac=7
            bar+="${BRAILLE[$frac]}"
        fi
    done
    printf '%s' "$bar"
}

# Format reset time from Unix epoch
# Output: "4pm" (same day) or "Mar 6 1pm" (different day)
format_reset_time() {
    local epoch="$1"
    [ -z "$epoch" ] || [ "$epoch" = "null" ] && return

    local today_date reset_date
    today_date=$(date "+%Y-%m-%d")
    reset_date=$(date -r "$epoch" "+%Y-%m-%d" 2>/dev/null) || return

    if [ "$today_date" = "$reset_date" ]; then
        LC_ALL=C date -r "$epoch" "+%-l%p" 2>/dev/null | sed 's/AM/am/;s/PM/pm/'
    else
        LC_ALL=C date -r "$epoch" "+%b %-d %-l%p" 2>/dev/null | sed 's/AM/am/;s/PM/pm/'
    fi
}

# Format a single usage line with braille bar
# Args: label, percentage, reset_epoch
format_usage_line() {
    local label="$1" pct="$2" reset_epoch="$3"
    local color bar reset_str

    # Round to integer
    pct=$(printf '%.0f' "$pct" 2>/dev/null) || pct=0
    [[ ! "$pct" =~ ^[0-9]+$ ]] && pct=0

    color=$(usage_color "$pct")
    bar=$(render_braille_bar "$pct")
    reset_str=$(format_reset_time "$reset_epoch")

    local line="${DIM}${label}${RESET} ${color}${bar}${RESET} $(printf '%2d' "$pct")%"
    [ -n "$reset_str" ] && line="${line}  ${reset_str}"
    printf '%s' "$line"
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

CTX_COLOR=$(usage_color "$PERCENT")

# Get session title from transcript file (first user message)
# || true prevents pipefail exit when grep finds no match
SESSION_TITLE=""
if [ -n "$EXPANDED_TRANSCRIPT" ]; then
    FIRST_MSG=$(grep -m1 '"userType":"external"' "$EXPANDED_TRANSCRIPT" 2>/dev/null \
        | jq -r '.message.content // empty' 2>/dev/null \
        | sed 's/<command-message>[^<]*<\/command-message>//g' \
        | sed 's/^[[:space:]]*//' \
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

# Line 2: session state (ctx braille bar + model + title)
CTX_BAR=$(render_braille_bar "$PERCENT")
LINE2="${DIM}cx${RESET} ${CTX_COLOR}${CTX_BAR}${RESET} $(printf '%2d' "$PERCENT")%  ${MODEL}"

if [ -n "$SESSION_TITLE" ]; then
    TITLE_DISPLAY=$(truncate "$SESSION_TITLE" "$MAX_TITLE")
    LINE2="${LINE2} | ${CYAN}\"${TITLE_DISPLAY}\"${RESET}"
fi

printf "%b\n" "$LINE1"
printf "%b\n" "$LINE2"

# Lines 3-4: Rate limits from stdin JSON (v2.1.80+)
FIVE_HOUR_PCT=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty' 2>/dev/null) || true
FIVE_HOUR_RESET=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty' 2>/dev/null) || true
SEVEN_DAY_PCT=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty' 2>/dev/null) || true
SEVEN_DAY_RESET=$(echo "$input" | jq -r '.rate_limits.seven_day.resets_at // empty' 2>/dev/null) || true

if [ -n "$FIVE_HOUR_PCT" ]; then
    LINE3=$(format_usage_line "5h" "$FIVE_HOUR_PCT" "$FIVE_HOUR_RESET")
    printf "%b\n" "$LINE3"
fi
if [ -n "$SEVEN_DAY_PCT" ]; then
    LINE4=$(format_usage_line "7d" "$SEVEN_DAY_PCT" "$SEVEN_DAY_RESET")
    printf "%b\n" "$LINE4"
fi
