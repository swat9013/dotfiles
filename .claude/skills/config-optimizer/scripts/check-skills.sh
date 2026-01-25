#!/bin/bash
# Claude Code Skillsのfrontmatterと行数をチェックするスクリプト
# Usage: ./check-skills.sh [skills-directory]

SKILLS_DIR="${1:-$HOME/.claude/skills}"

if [[ ! -d "$SKILLS_DIR" ]]; then
    echo "エラー: $SKILLS_DIR が存在しません"
    exit 1
fi

echo "=== Skills チェック ==="
echo ""

# SKILL.mdファイルを検索
while IFS= read -r -d '' skill_file; do
    skill_dir=$(dirname "$skill_file")
    skill_name=$(basename "$skill_dir")

    # _sharedディレクトリはスキップ
    if [[ "$skill_name" == "_shared" ]]; then
        continue
    fi

    # 行数カウント
    line_count=$(wc -l < "$skill_file" | tr -d ' ')

    echo "$skill_name ($line_count 行)"

    # frontmatterを抽出（2行目から---までの間）
    frontmatter=$(sed -n '2,/^---$/p' "$skill_file" | grep -v '^---$')

    # name チェック
    name=$(echo "$frontmatter" | grep '^name:' | sed 's/^name: *//')
    if [[ -n "$name" ]]; then
        echo "  ✓ name: $name"
    else
        echo "  ✗ name: なし"
    fi

    # description チェック
    description=$(echo "$frontmatter" | grep '^description:' | sed 's/^description: *//')

    if [[ -n "$description" ]]; then
        # トリガーキーワードの確認
        if echo "$description" | grep -q '「.*」.*依頼.*時.*使用'; then
            triggers=$(echo "$description" | grep -o '「[^」]*」' | tr '\n' ' ')
            echo "  ✓ description: あり"
            echo "  ✓ トリガー: $triggers"
        else
            echo "  ✓ description: あり"
            echo "  ✗ トリガー: 「〜と依頼された時に使用」形式なし"
        fi
    else
        echo "  ✗ description: なし"
    fi

    # 行数チェック
    if [[ $line_count -le 500 ]]; then
        echo "  ✓ 行数: OK (500行以下)"
    else
        echo "  ✗ 行数: $line_count 行 (500行超)"
    fi

    echo ""

done < <(find -L "$SKILLS_DIR" -name "SKILL.md" -type f -print0 2>/dev/null | sort -z)

echo "=== チェック完了 ==="
