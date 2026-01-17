---
paths: "**/.claude/rules/**"
---

# プロジェクト Rules ガイド

## 制約

| 項目 | 値 |
|------|-----|
| 推奨サイズ | 200行以下 |
| 命名規則 | kebab-case |
| 形式 | Markdown + frontmatter |

## frontmatter

```yaml
---
paths: src/**/*.ts          # 単一
---
```

```yaml
---
paths:                      # 複数・除外
  - "src/api/**"
  - "!**/__mocks__/**"      # ! で除外
---
```

## Rules vs CLAUDE.md vs skills

| 判断基準 | rules/ | CLAUDE.md | skills/ |
|---------|--------|-----------|---------|
| 適用 | パス固有・自動 | 全体・常時 | 明示的呼び出し |
| 内容 | ファイル種別の制約 | 全体方針 | ワークフロー |

## 良いRulesの特徴

- **パス固有**: globで明確に絞り込める
- **自己完結**: 他ファイル参照なし
- **アクション指向**: 「〜すること」「〜しないこと」

## 構成例

```markdown
---
paths: src/api/**
---

# API層ガイドライン

## 必須事項
- エラーハンドリング必須
- 型定義必須

## 禁止事項
- 直接DB操作
- console.log
```

## アンチパターン

| パターン | 対策 |
|---------|------|
| `paths: "**/*"` | CLAUDE.mdへ |
| 300行超 | skills/へ |
| 複数ルールで重複 | CLAUDE.mdに集約 |
