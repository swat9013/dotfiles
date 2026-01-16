#!/bin/bash
# Claude Code設定の機械的チェック（行数、構造、frontmatter）
# Usage: ./check-config.sh

echo "=== 行数チェック ==="
echo ""

# CLAUDE.md チェック（グローバルとプロジェクト）
check_claude_md() {
    local path="$1"
    local limit=150
    local expanded_path="${path/#\~/$HOME}"

    if [[ -f "$expanded_path" ]]; then
        local line_count=$(wc -l < "$expanded_path" | tr -d ' ')
        if [[ $line_count -le $limit ]]; then
            echo "  ✓ ${path/#$HOME/\~}: ${line_count}行 (${limit}行以下)"
        else
            echo "  ✗ ${path/#$HOME/\~}: ${line_count}行 (${limit}行超過)"
        fi
    else
        echo "  - ${path/#$HOME/\~}: 存在しない"
    fi
}

# グローバルCLAUDE.md
check_claude_md ~/.dotfiles/.claude-global/CLAUDE.md
check_claude_md ~/.claude/CLAUDE.md

# プロジェクトCLAUDE.md
check_claude_md .claude/CLAUDE.md
check_claude_md CLAUDE.md

echo ""

# Rules チェック
echo "[Rules]"
RULES_DIR_GLOBAL="$HOME/.dotfiles/.claude-global/rules"
RULES_DIR_LOCAL=".claude/rules"
RULES_LIMIT=200

check_rules_dir() {
    local rules_dir="$1"
    if [[ -d "$rules_dir" ]]; then
        while IFS= read -r -d '' rule_file; do
            local rule_name=$(basename "$rule_file" .md)
            local line_count=$(wc -l < "$rule_file" | tr -d ' ')

            if [[ $line_count -le $RULES_LIMIT ]]; then
                echo "  ✓ ${rule_file/#$HOME/\~}: ${line_count}行 (${RULES_LIMIT}行以下)"
            else
                echo "  ✗ ${rule_file/#$HOME/\~}: ${line_count}行 (${RULES_LIMIT}行超過)"
            fi
        done < <(find -L "$rules_dir" -name "*.md" -type f -print0 2>/dev/null | sort -z)
    fi
}

check_rules_dir "$RULES_DIR_GLOBAL"
check_rules_dir "$RULES_DIR_LOCAL"

if [[ ! -d "$RULES_DIR_GLOBAL" && ! -d "$RULES_DIR_LOCAL" ]]; then
    echo "  - Rules ディレクトリ: 存在しない"
fi

echo ""

# Skills チェック
echo "[Skills]"
SKILLS_LIMIT=500

check_skills_dir() {
    local skills_dir="$1"
    if [[ -d "$skills_dir" ]]; then
        while IFS= read -r -d '' skill_file; do
            local skill_dir=$(dirname "$skill_file")
            local skill_name=$(basename "$skill_dir")

            # _sharedディレクトリはスキップ
            if [[ "$skill_name" == "_shared" ]]; then
                continue
            fi

            local line_count=$(wc -l < "$skill_file" | tr -d ' ')

            if [[ $line_count -le $SKILLS_LIMIT ]]; then
                echo "  ✓ ${skill_name}: ${line_count}行 (${SKILLS_LIMIT}行以下)"
            else
                echo "  ✗ ${skill_name}: ${line_count}行 (${SKILLS_LIMIT}行超過)"
            fi
        done < <(find -L "$skills_dir" -name "SKILL.md" -type f -print0 2>/dev/null | sort -z)
    fi
}

check_skills_dir "$HOME/.dotfiles/.claude-global/skills"
check_skills_dir "$HOME/.claude/skills"
check_skills_dir ".claude/skills"

echo ""
echo "=== 構造チェック ==="
echo ""

# CLAUDE.md存在確認
echo "[CLAUDE.md]"
for path in ~/.dotfiles/.claude-global/CLAUDE.md ~/.claude/CLAUDE.md .claude/CLAUDE.md CLAUDE.md; do
    expanded_path="${path/#\~/$HOME}"
    if [[ -f "$expanded_path" ]]; then
        echo "  ✓ ${path/#$HOME/\~}: 存在"
    fi
done

echo ""

# Rules frontmatter globs チェック
echo "[Rules frontmatter]"

check_rules_frontmatter() {
    local rules_dir="$1"
    if [[ -d "$rules_dir" ]]; then
        while IFS= read -r -d '' rule_file; do
            local rule_name=$(basename "$rule_file" .md)

            # frontmatterを抽出（1行目が---の場合、2行目から次の---まで）
            if head -n 1 "$rule_file" | grep -q '^---$'; then
                frontmatter=$(sed -n '2,/^---$/p' "$rule_file" | grep -v '^---$')

                # globs チェック
                if echo "$frontmatter" | grep -q '^globs:'; then
                    globs=$(echo "$frontmatter" | grep '^globs:' | sed 's/^globs: *//')
                    echo "  ✓ ${rule_file/#$HOME/\~}: globs設定あり ($globs)"
                else
                    echo "  ✗ ${rule_file/#$HOME/\~}: globs未設定"
                fi
            else
                echo "  ✗ ${rule_file/#$HOME/\~}: frontmatter なし"
            fi
        done < <(find -L "$rules_dir" -name "*.md" -type f -print0 2>/dev/null | sort -z)
    fi
}

check_rules_frontmatter "$HOME/.dotfiles/.claude-global/rules"
check_rules_frontmatter ".claude/rules"

if [[ ! -d "$HOME/.dotfiles/.claude-global/rules" && ! -d ".claude/rules" ]]; then
    echo "  - Rules ディレクトリ: 存在しない"
fi

echo ""
echo "=== チェック完了 ==="
