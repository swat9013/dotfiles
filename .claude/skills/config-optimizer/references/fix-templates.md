# Config Optimizer 修正テンプレート

このファイルは各カテゴリの問題に対する具体的な修正テンプレートを提供します。

---

## Skills - frontmatter

### name修正

**問題**: name が64文字を超えている、またはハイフンケースでない

**修正テンプレート**:
```yaml
name: {kebab-case-name}  # 64文字以内
```

**例**:
```yaml
# 修正前
name: ConfigOptimizer

# 修正後
name: config-optimizer
```

---

### description追加

**問題**: description が欠落している、または形式が不適切

**修正テンプレート**:
```yaml
description: {動詞} {何を}。「{トリガー1}」「{トリガー2}」と依頼された時に使用。{追加説明}
```

**例**:
```yaml
# 修正前
description: This skill optimizes Claude Code configuration.

# 修正後
description: Claude Code設定の最適化と改善提案。「設定を最適化」「config-optimizer」「Claude設定チェック」と依頼された時に使用。skills/commands/agents/settings.json/CLAUDE.mdをベストプラクティスで分析・最適化。
```

**descriptionの原則**:
- 日本語で記述
- 動詞で始める（例：「実行」「分析」「最適化」）
- トリガーフレーズを「」で明記
- 簡潔かつ具体的に（1-2文）

---

## settings.json

### $schema追加

**問題**: `$schema` フィールドが欠落している

**修正テンプレート**:
```json
{
  "$schema": "https://cdn.jsdelivr.net/npm/@anthropic-ai/claude-code@latest/config-schema.json",
  // 既存の設定...
}
```

**配置**: ファイルの最初のフィールドとして追加

---

### deny追加（機密ファイル）

**問題**: `permissions.deny` に機密ファイルパターンが含まれていない

**修正テンプレート**:
```json
"permissions": {
  "deny": [
    "**/.env*",
    "**/credentials*",
    "**/*secret*",
    "**/*password*",
    "**/token*",
    "**/*.key",
    "**/*.pem"
  ]
}
```

**カスタマイズ**:
- プロジェクト固有の機密ファイルパターンを追加
- 既存のdenyパターンとマージ（重複削除）

---

### statusLine追加

**問題**: `statusLine` が未設定で、デフォルト表示のまま

**修正テンプレート**:
```json
"statusLine": {
  "path": "/path/to/statusline.sh",
  "timeout": 2000
}
```

**推奨スクリプト例**:
```bash
#!/bin/bash
# Git branch, context usage, session title
git_branch=$(git branch --show-current 2>/dev/null)
echo "${git_branch:+[$git_branch]} | Context: $CLAUDE_CONTEXT_USED/$CLAUDE_CONTEXT_MAX"
```

---

### fileSuggestion追加

**問題**: `fileSuggestion` が未設定で、ファイル提案が遅い

**修正テンプレート**:
```json
"fileSuggestion": {
  "path": "/path/to/file-suggestion.sh"
}
```

**推奨実装**: ripgrepを使った高速検索

---

## Commands

### frontmatter形式

**問題**: frontmatter が欠落している、または形式が不適切

**修正テンプレート**:
```yaml
---
name: command-name
description: {動詞} {何を}。
---
```

**例**:
```yaml
---
name: review-pr
description: Pull Requestをレビューし、改善提案を生成。
---
```

---

## Skills - 構造テンプレート

### 基本構造

**問題**: Skills の構造が不適切、または必要なディレクトリが欠落

**修正テンプレート**:
```
skills/
└── skill-name/
    ├── SKILL.md              # メインスキル定義 (150-200行)
    ├── references/           # 詳細ドキュメント
    │   ├── details.md       # 詳細ガイド
    │   ├── examples.md      # 使用例
    │   └── templates.md     # テンプレート集
    └── scripts/             # ヘルパースクリプト（必要に応じて）
        └── helper.sh
```

**SKILL.md 基本構造**:
```markdown
---
name: skill-name
description: {動詞} {何を}。「{トリガー}」と依頼された時に使用。{追加説明}
---

# Skill Name

[1-2文で概要説明]

## 実行手順

### 1. ステップ1
[簡潔な説明]

### 2. ステップ2
[簡潔な説明]

## 成功基準

1. [基準1]
2. [基準2]

---

## 原則

- [原則1]
- [原則2]

詳細: [references/details.md](references/details.md)
```

**分割の目安**:
- SKILL.md が300行を超える場合
- 複数の独立したセクションがある場合
- 詳細な例やテンプレートが多い場合

---

## Rules

### frontmatter形式

**問題**: frontmatter が欠落している、または形式が不適切

**修正テンプレート**:
```yaml
---
globs: ["pattern/**/*.ext"]
---
```

**例**:
```yaml
---
globs: ["src/**/*.test.ts", "tests/**/*.ts"]
---

# テストファイルのルール

このプロジェクトのテストファイルでは以下のルールに従ってください。

## テスト構造
[ルールの詳細]
```

**globs パターンの書き方**:
- 配列形式で記述: `["pattern1", "pattern2"]`
- ワイルドカード使用可: `**/*.ts`, `src/**/test-*.js`
- 否定パターン: `!node_modules/**`

### Rules vs CLAUDE.md 判断

**問題**: 内容が適切なレイヤーに配置されていない

| 内容 | 推奨レイヤー | 理由 |
|------|------------|------|
| 特定ファイルパターンへの制約 | Rules | globsで対象を限定できる |
| テストファイルの命名規則 | Rules | `**/*.test.ts`で限定 |
| 認証方式の説明 | CLAUDE.md | プロジェクト全体に影響 |
| ディレクトリ構造の説明 | CLAUDE.md | 特定パターンに限定できない |
| 環境変数の取り扱い | Rules | `.env*`で限定 |

**修正例**:
```yaml
# Before (CLAUDE.md)
## テストファイル
- テストファイルは describe/it 構造を使用
- モックは jest.mock() で行う

# After (Rules: tests.md)
---
globs: ["**/*.test.ts", "**/*.spec.ts"]
---
# テストファイル規約
- describe/it 構造を使用
- モックは jest.mock() で行う
```

---

## Agents

### frontmatter形式

**問題**: frontmatter が欠落している、または形式が不適切

**修正テンプレート**:
```yaml
---
name: agent-name
description: {役割}エージェント。{何をするか}。
tools: [Tool1, Tool2, Tool3]
---
```

**例**:
```yaml
---
name: architect
description: システム設計エージェント。要件から技術選定とアーキテクチャ設計を行う。
tools: [Read, Glob, Grep, WebSearch, WebFetch]
---
```

---

## CLAUDE.md

### 構造テンプレート

**問題**: セクション構成が不適切、または情報が不足

**修正テンプレート**:
```markdown
# CLAUDE.md

## リポジトリの概要
[1-2文で説明]

## アーキテクチャ
[システム構造、主要コンポーネント]

## コマンド
\`\`\`bash
# 主要なコマンド
\`\`\`

## ディレクトリ構造
[重要なディレクトリの説明]

## 技術スタック
[使用している主要技術]

## 注意事項
[開発時の制約、既知の問題]
```

---

## ファイル分割

### SKILL.md行数超過

**問題**: SKILL.md が300行を超えている

**修正テンプレート**:

**主要SKILL.md**:
```markdown
---
name: skill-name
description: ...
---

# Skill Name

[概要]

## 実行手順
[簡潔な手順、詳細はreferencesへ]

## 成功基準
[チェックリスト]

詳細: [references/details.md](references/details.md)
```

**references/details.md**:
```markdown
# Skill Name 詳細ガイド

[詳細な実装ガイド、ベストプラクティス、テンプレート等]
```

**分割基準**:
- メインSKILL.md: 150-200行以内
- references/: カテゴリ別詳細（各100-300行）

---

## 適用順序

1. **Critical**: $schema、deny、name形式
2. **High**: description、frontmatter完全性
3. **Medium**: ファイル分割、構造改善
4. **Low**: コメント、フォーマット

修正は優先度順に実行し、各修正後に整合性を確認。
