# config-optimizer スクリプト

config-optimizerスキルで使用するレビュー支援スクリプト。

## スクリプト一覧

### scan-targets.sh

Claude Code設定ファイル・ディレクトリの存在確認。

**使用方法**:
```bash
~/.dotfiles/.claude/skills/config-optimizer/scripts/scan-targets.sh
```

**出力例**:
```
=== Claude Code 設定スキャン ===

[グローバル]
  ✓ ~/.claude/CLAUDE.md → ~/.dotfiles/.claude-global/CLAUDE.md
  ✓ ~/.claude/settings.json → ~/.dotfiles/.claude-global/settings.json
  ✓ ~/.claude/skills → ~/.dotfiles/.claude-global/skills
  ✗ ~/.claude/commands (存在しない)
  ✓ ~/.claude/agents (*.md: 0 個)

[プロジェクト]
  ✓ CLAUDE.md
  ✗ .claude/skills (存在しない)
```

**チェック項目**:
- シンボリックリンクとリンク先
- ファイルの存在
- ディレクトリ内の.mdファイル数

---

### check-skills.sh

Skillsのfrontmatter検証と行数チェック。

**使用方法**:
```bash
# デフォルト（~/.claude/skills/）
~/.dotfiles/.claude/skills/config-optimizer/scripts/check-skills.sh

# ディレクトリ指定
~/.dotfiles/.claude/skills/config-optimizer/scripts/check-skills.sh /path/to/skills
```

**出力例**:
```
=== Skills チェック ===

code-review (194 行)
  ✓ name: code-review
  ✓ description: あり
  ✓ トリガー: 「コードレビュー」 「レビューして」
  ✓ 行数: OK (500行以下)

design-workflow (220 行)
  ✓ name: design-workflow
  ✓ description: あり
  ✗ トリガー: 「〜と依頼された時に使用」形式なし
  ✓ 行数: OK (500行以下)
```

**チェック項目**:
- `name` フィールドの存在
- `description` フィールドの存在
- トリガーキーワード（「〜と依頼された時に使用」形式）
- 行数（500行以下）

---

## 開発メモ

### 設計方針

- **シンプル**: テキスト出力のみ（JSON不要）
- **読みやすさ**: Claudeが解釈しやすい形式
- **軽量**: 各スクリプト70行程度

### 実行許可

`settings.json` に以下が設定済み:
```json
"allow": [
  "Bash(~/.dotfiles/.claude/skills/config-optimizer/scripts/scan-targets.sh:*)",
  "Bash(~/.dotfiles/.claude/skills/config-optimizer/scripts/check-skills.sh:*)"
]
```
