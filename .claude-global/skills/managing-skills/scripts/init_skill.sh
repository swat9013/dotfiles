#!/bin/bash
# スキル雛形を生成するスクリプト
# Usage: init_skill.sh <skill-name> [output-path]

set -e

SKILL_NAME="$1"
OUTPUT_PATH="${2:-.}"

# 引数チェック
if [ -z "$SKILL_NAME" ]; then
    echo "Usage: $0 <skill-name> [output-path]"
    echo "Example: $0 my-awesome-skill ~/.claude/skills"
    exit 1
fi

# スキル名の検証（ハイフンケース、64文字以内）
if ! echo "$SKILL_NAME" | grep -qE '^[a-z0-9]([a-z0-9-]*[a-z0-9])?$'; then
    echo "Error: skill-name must be lowercase alphanumeric with hyphens"
    echo "Example: my-skill, pdf-processor, code-review"
    exit 1
fi

if [ ${#SKILL_NAME} -gt 64 ]; then
    echo "Error: skill-name must be 64 characters or less"
    exit 1
fi

SKILL_DIR="$OUTPUT_PATH/$SKILL_NAME"

# 既存チェック
if [ -d "$SKILL_DIR" ]; then
    echo "Error: Directory already exists: $SKILL_DIR"
    exit 1
fi

# ディレクトリ作成
mkdir -p "$SKILL_DIR"/{references,scripts,assets}

# SKILL.md生成
cat > "$SKILL_DIR/SKILL.md" << EOF
---
name: $SKILL_NAME
description: |-
  [TODO: 三人称で記述。例：「〇〇を担当する支援エージェント。」]
  [TODO: トリガーキーワード。例：「〜して」「〜したい」と依頼された時に使用。]
---

# ${SKILL_NAME//-/ }

## 概要

[TODO: 1-2文でスキルの目的]

## 手順

### 1. [ステップ名]

[TODO: 具体的な手順]

### 2. [ステップ名]

[TODO: 具体的な手順]

## 入出力例

**入力:**
\`\`\`
[TODO: 入力例]
\`\`\`

**出力:**
\`\`\`
[TODO: 出力例]
\`\`\`

## 注意事項

- [TODO: 重要な制約や注意点]
EOF

# サンプルreference生成
cat > "$SKILL_DIR/references/README.md" << EOF
# References

詳細なドキュメントをここに配置。

- APIリファレンス
- スキーマ定義
- 詳細な例
EOF

echo "Created skill at: $SKILL_DIR"
echo ""
echo "Next steps:"
echo "  1. Edit $SKILL_DIR/SKILL.md"
echo "  2. Update frontmatter description"
echo "  3. Fill in [TODO] sections"
echo "  4. Add references if needed"
