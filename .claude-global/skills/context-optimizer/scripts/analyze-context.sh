#!/bin/bash
# analyze-context.sh - CLAUDE.md と .claude/ の構造を分析
#
# 使用方法: ./analyze-context.sh [target_dir]
# target_dir: 分析対象ディレクトリ（デフォルト: カレント）

set -euo pipefail

TARGET_DIR="${1:-.}"

# 色定義
RED='\033[0;31m'
YELLOW='\033[0;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# 行数カウント関数
count_lines() {
    local file="$1"
    if [[ -f "$file" ]]; then
        wc -l < "$file" | tr -d ' '
    else
        echo "0"
    fi
}

# ディレクトリ内の総行数
count_dir_lines() {
    local dir="$1"
    if [[ -d "$dir" ]]; then
        find "$dir" -name "*.md" -type f -exec cat {} + 2>/dev/null | wc -l | tr -d ' '
    else
        echo "0"
    fi
}

# ファイル数カウント
count_files() {
    local dir="$1"
    if [[ -d "$dir" ]]; then
        find "$dir" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' '
    else
        echo "0"
    fi
}

# ステータス表示
show_status() {
    local lines="$1"
    local threshold_warn="$2"
    local threshold_ok="$3"

    if [[ "$lines" -gt "$threshold_warn" ]]; then
        echo -e "${RED}[!]${NC}"
    elif [[ "$lines" -gt "$threshold_ok" ]]; then
        echo -e "${YELLOW}[*]${NC}"
    else
        echo -e "${GREEN}[OK]${NC}"
    fi
}

echo "=== Context Analysis ==="
echo "Target: $TARGET_DIR"
echo ""

# CLAUDE.md 分析
CLAUDE_MD="$TARGET_DIR/CLAUDE.md"
CLAUDE_LINES=$(count_lines "$CLAUDE_MD")
CLAUDE_STATUS=$(show_status "$CLAUDE_LINES" 150 50)

echo "## CLAUDE.md"
echo "  Lines: $CLAUDE_LINES $CLAUDE_STATUS"
echo "  Target: < 50 lines"
echo ""

# .claude/ ディレクトリ分析
CLAUDE_DIR="$TARGET_DIR/.claude"

echo "## .claude/ Structure"

# Rules
RULES_DIR="$CLAUDE_DIR/rules"
RULES_FILES=$(count_files "$RULES_DIR")
RULES_LINES=$(count_dir_lines "$RULES_DIR")
echo "  rules/:    $RULES_FILES files, $RULES_LINES lines"

# Skills
SKILLS_DIR="$CLAUDE_DIR/skills"
SKILLS_FILES=$(count_files "$SKILLS_DIR")
SKILLS_LINES=$(count_dir_lines "$SKILLS_DIR")
echo "  skills/:   $SKILLS_FILES files, $SKILLS_LINES lines"

# Commands
COMMANDS_DIR="$CLAUDE_DIR/commands"
COMMANDS_FILES=$(count_files "$COMMANDS_DIR")
COMMANDS_LINES=$(count_dir_lines "$COMMANDS_DIR")
echo "  commands/: $COMMANDS_FILES files, $COMMANDS_LINES lines"

# Agents
AGENTS_DIR="$CLAUDE_DIR/agents"
AGENTS_FILES=$(count_files "$AGENTS_DIR")
AGENTS_LINES=$(count_dir_lines "$AGENTS_DIR")
echo "  agents/:   $AGENTS_FILES files, $AGENTS_LINES lines"

echo ""

# 総計
TOTAL_LINES=$((CLAUDE_LINES + RULES_LINES + SKILLS_LINES + COMMANDS_LINES + AGENTS_LINES))
echo "## Summary"
echo "  Total lines: $TOTAL_LINES"
echo "  CLAUDE.md ratio: $(( CLAUDE_LINES * 100 / (TOTAL_LINES + 1) ))%"
echo ""

# 推奨事項
echo "## Recommendations"
if [[ "$CLAUDE_LINES" -gt 150 ]]; then
    echo "  - CLAUDE.md exceeds 150 lines. Consider extraction."
elif [[ "$CLAUDE_LINES" -gt 50 ]]; then
    echo "  - CLAUDE.md exceeds 50 lines. Optimization recommended."
else
    echo "  - CLAUDE.md is within target range."
fi
