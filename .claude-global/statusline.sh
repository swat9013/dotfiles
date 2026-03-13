#!/bin/bash
#==============================================================================
# claude-statusline.sh - Claude Code Statusline Script
#==============================================================================
# Four-line layout:
#   Line 1: dir git:branch*              (location context)
#   Line 2: Ctx:XX% | Model | "title..." (session state)
#   Line 3: 5h  [=====-----]  50%  Reset 4pm        (5-hour usage)
#   Line 4: 7d  [========--]  80%  Reset Mar 6 1pm   (7-day usage)
#
# Lines 3-4 appear only when usage data is available (graceful degradation).
#
# Color scheme:
#   - Directory: Green
#   - Git branch: Yellow
#   - Context %: Green (0-50), Yellow (50-70), Red (70+)
#   - Session title: Cyan
#   - Usage bar: Green (0-49%), Yellow (50-79%), Red (80-100%)
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

# Usage cache settings
USAGE_CACHE="/tmp/claude-usage-cache.json"
CACHE_TTL=360  # 6 minutes

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

# Render progress bar: [=====-----] from percentage
render_progress_bar() {
    local pct="$1"
    local width=10
    local filled=$(( pct * width / 100 ))
    [ "$filled" -gt "$width" ] && filled=$width
    local empty=$(( width - filled ))
    local bar=""
    for ((i=0; i<filled; i++)); do bar+="="; done
    for ((i=0; i<empty; i++)); do bar+="-"; done
    printf '[%s]' "$bar"
}

# Format reset time to local short format
# Input: ISO 8601 timestamp (e.g. 2025-11-04T04:59:59.943648+00:00)
# Output: "4pm" (same day) or "Mar 6 1pm" (different day)
format_reset_time() {
    local iso_time="$1"
    [ -z "$iso_time" ] || [ "$iso_time" = "null" ] && return

    # macOS date: convert ISO 8601 to epoch, then format
    local reset_epoch today_date reset_date
    # Strip sub-second precision, remove colon from TZ offset only (BSD date requires +0000 not +00:00)
    local clean_time="${iso_time%%.*}$(echo "$iso_time" | grep -oE '[+-][0-9]{2}:[0-9]{2}$' || true)"
    clean_time=$(echo "$clean_time" | sed 's/\([+-][0-9][0-9]\):\([0-9][0-9]\)$/\1\2/')
    reset_epoch=$(date -jf "%Y-%m-%dT%H:%M:%S%z" "$clean_time" "+%s" 2>/dev/null) || return
    today_date=$(date "+%Y-%m-%d")
    reset_date=$(date -r "$reset_epoch" "+%Y-%m-%d" 2>/dev/null) || return

    if [ "$today_date" = "$reset_date" ]; then
        # Same day: show time only (e.g. "4pm")
        LC_ALL=C date -r "$reset_epoch" "+%-l%p" 2>/dev/null | sed 's/AM/am/;s/PM/pm/'
    else
        # Different day: show date + time (e.g. "Mar 6 1pm")
        LC_ALL=C date -r "$reset_epoch" "+%b %-d %-l%p" 2>/dev/null | sed 's/AM/am/;s/PM/pm/'
    fi
}

# Fetch usage data with caching
get_usage_data() {
    # macOS only
    command -v security &>/dev/null || return

    # Check cache freshness
    if [ -f "$USAGE_CACHE" ]; then
        local cache_mtime now age
        cache_mtime=$(stat -f "%m" "$USAGE_CACHE" 2>/dev/null) || return
        now=$(date "+%s")
        age=$(( now - cache_mtime ))
        if [ "$age" -lt "$CACHE_TTL" ]; then
            cat "$USAGE_CACHE"
            return
        fi
    fi

    # Get OAuth token from keychain
    local creds token
    creds=$(security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null) || return
    token=$(echo "$creds" | jq -r '.claudeAiOauth.accessToken // empty' 2>/dev/null)
    [ -z "$token" ] && return

    # Call API with timeout
    local response
    response=$(curl -s --max-time 3 \
        -H "Authorization: Bearer $token" \
        -H "anthropic-beta: oauth-2025-04-20" \
        -H "Content-Type: application/json" \
        "https://api.anthropic.com/api/oauth/usage" 2>/dev/null) || return

    # Validate response (must have five_hour field, no error)
    if echo "$response" | jq -e '.five_hour.utilization' &>/dev/null; then
        echo "$response" > "$USAGE_CACHE"
        echo "$response"
    elif [ -f "$USAGE_CACHE" ]; then
        # API error: fall back to stale cache
        cat "$USAGE_CACHE"
    fi
}

# Format a single usage line
# Args: label, utilization%, reset_time_iso
format_usage_line() {
    local label="$1" pct="$2" reset_iso="$3"
    local color bar reset_str

    # Round to integer
    pct=$(printf '%.0f' "$pct" 2>/dev/null) || pct=0
    [[ ! "$pct" =~ ^[0-9]+$ ]] && pct=0

    color=$(usage_color "$pct")
    bar=$(render_progress_bar "$pct")
    reset_str=$(format_reset_time "$reset_iso")

    local line="${color}${label}${RESET} ${color}${bar}${RESET} ${color}${pct}%${RESET}"
    [ -n "$reset_str" ] && line="${line}  Reset ${reset_str}"
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

# Lines 3-4: API usage (graceful degradation — skip on failure)
USAGE_DATA=$(get_usage_data 2>/dev/null) || true
if [ -n "$USAGE_DATA" ]; then
    FIVE_HOUR_PCT=$(echo "$USAGE_DATA" | jq -r '.five_hour.utilization // empty' 2>/dev/null) || true
    FIVE_HOUR_RESET=$(echo "$USAGE_DATA" | jq -r '.five_hour.resets_at // empty' 2>/dev/null) || true
    SEVEN_DAY_PCT=$(echo "$USAGE_DATA" | jq -r '.seven_day.utilization // empty' 2>/dev/null) || true
    SEVEN_DAY_RESET=$(echo "$USAGE_DATA" | jq -r '.seven_day.resets_at // empty' 2>/dev/null) || true

    if [ -n "$FIVE_HOUR_PCT" ]; then
        LINE3=$(format_usage_line "5h" "$FIVE_HOUR_PCT" "$FIVE_HOUR_RESET")
        printf "%b\n" "$LINE3"
    fi
    if [ -n "$SEVEN_DAY_PCT" ]; then
        LINE4=$(format_usage_line "7d" "$SEVEN_DAY_PCT" "$SEVEN_DAY_RESET")
        printf "%b\n" "$LINE4"
    fi
fi
