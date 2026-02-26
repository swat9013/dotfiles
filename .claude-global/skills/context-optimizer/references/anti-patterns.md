# アンチパターン

コンテキスト最適化で避けるべきパターン集。

---

## CLAUDE.md のアンチパターン

### 1. 巨大なCLAUDE.md

```markdown
# CLAUDE.md（500行以上）

## プロジェクト概要
...（50行）

## 技術スタック
...（30行）

## コーディング規約
...（200行）  ← Rulesへ抽出すべき

## ワークフロー
...（150行）  ← Skillsへ抽出すべき

## コマンドテンプレート
...（70行）   ← Commandsへ抽出すべき
```

**問題**: 毎回全文が読み込まれ、コンテキストを圧迫。

### 2. パス固有ルールの埋め込み

```markdown
## TypeScript規約
- 厳格な型付けを使用
- any禁止
...
```

**問題**: TypeScript以外のファイル編集時も読み込まれる。

**対策**: `rules/typescript.md` に `paths: **/*.ts` で抽出。

### 3. ワークフローの詳細記述

```markdown
## コードレビュー手順
1. まず対象ファイルを特定
2. セキュリティ観点でチェック
3. パフォーマンス観点でチェック
...（30行以上）
```

**問題**: レビュー依頼されるまで不要な情報。

**対策**: `skills/review-fix/SKILL.md` に抽出。

---

## frontmatter のアンチパターン

### 1. paths 漏れ（Rules）

```yaml
---
# paths がない！
---
# TypeScript規約
```

**問題**: 全ファイルで読み込まれ、Rulesの意味がない。

### 2. description 漏れ（Skills）

```yaml
---
name: review-fix
# description がない！
---
```

**問題**: Claudeが自動判断でトリガーできない。

### 3. トリガー説明の欠如

```yaml
---
name: review-fix
description: コードをレビューする
---
```

**問題**: 「いつ使うか」が不明確。

**対策**:

```yaml
description: |
  コードをレビューする。
  「レビューして」「PRチェックして」と依頼された時に使用。
```

---

## 構造のアンチパターン

### 1. フラットな Skills

```
skills/
├── review.md        ← SKILL.md でない
├── commit.md
└── debug.md
```

**問題**: 詳細を分離できない。

**対策**:

```
skills/
├── review/
│   ├── SKILL.md
│   └── references/
├── commit/
│   └── SKILL.md
└── debug/
    └── SKILL.md
```

### 2. 過剰な分割

```
rules/
├── typescript-types.md
├── typescript-naming.md
├── typescript-imports.md
├── typescript-exports.md
└── typescript-functions.md
```

**問題**: 管理が煩雑、関連ルールが分散。

**対策**: `rules/typescript.md` に統合。

---

## 最適化時のアンチパターン

### 1. 過剰な圧縮

```markdown
# CLAUDE.md
Node.js API。詳細は/skills参照。
```

**問題**: 最低限の指針も不明。

### 2. 参照の連鎖

```markdown
# CLAUDE.md
詳細は rules/overview.md 参照。

# rules/overview.md
詳細は skills/guide/SKILL.md 参照。
```

**問題**: 情報到達までに複数ステップ。

### 3. 重複した記述

```markdown
# CLAUDE.md
- TypeScriptで厳格な型付け

# rules/typescript.md
- 厳格な型付けを使用
```

**問題**: 情報の不整合リスク、トークン無駄。

---

## チェックリスト

最適化後、以下を確認:

- [ ] CLAUDE.md が50行未満
- [ ] 各Ruleに`paths:`がある
- [ ] 各Skillに`name:`と`description:`がある
- [ ] descriptionに「〜時に使用」がある
- [ ] 重複記述がない
- [ ] 参照の連鎖がない（最大1段階）
