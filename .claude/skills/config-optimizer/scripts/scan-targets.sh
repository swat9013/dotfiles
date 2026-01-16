#!/bin/bash
# Claude Code設定ファイルの対象をスキャンするスクリプト

# スキャン関数
scan_target() {
    local path="$1"
    local expanded_path="${path/#\~/$HOME}"

    if [[ -L "$expanded_path" ]]; then
        local target=$(readlink "$expanded_path")
        echo "link:$expanded_path->$target"
    elif [[ -f "$expanded_path" ]]; then
        echo "file:$expanded_path"
    elif [[ -d "$expanded_path" ]]; then
        local count=$(find "$expanded_path" -type f -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
        echo "dir:$expanded_path:$count"
    else
        echo "none:$expanded_path"
    fi
}

# 出力
echo "=== Claude Code 設定スキャン ==="
echo ""
echo "[グローバル]"

for path in ~/.dotfiles/.claude-global/CLAUDE.md ~/.dotfiles/.claude-global/settings.json ~/.dotfiles/.claude-global/skills ~/.dotfiles/.claude-global/commands ~/.dotfiles/.claude-global/agents; do
    result=$(scan_target "$path")
    case "$result" in
        none:*)
            echo "  ✗ ${path/#$HOME/\~} (存在しない)"
            ;;
        file:*)
            echo "  ✓ ${path/#$HOME/\~}"
            ;;
        link:*-\>*)
            target="${result#link:*->}"
            echo "  ✓ ${path/#$HOME/\~} → ${target/#$HOME/\~}"
            ;;
        dir:*:*)
            count="${result##*:}"
            echo "  ✓ ${path/#$HOME/\~} (*.md: $count 個)"
            ;;
    esac
done

echo ""
echo "[プロジェクト]"

for path in .claude/CLAUDE.md CLAUDE.md .claude/skills .claude/commands .claude/agents; do
    result=$(scan_target "$path")
    case "$result" in
        none:*)
            echo "  ✗ $path (存在しない)"
            ;;
        file:*)
            echo "  ✓ $path"
            ;;
        link:*-\>*)
            target="${result#link:*->}"
            echo "  ✓ $path → ${target/#$HOME/\~}"
            ;;
        dir:*:*)
            count="${result##*:}"
            echo "  ✓ $path (*.md: $count 個)"
            ;;
    esac
done
