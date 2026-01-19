---
paths: "**/.claude/skills/**"
---

# Skills 作成ルール

## 必須制約

- **SKILL.md**: 500行以下
- **frontmatter**: name（gerund形式推奨）+ description（三人称・トリガー必須）
- **Progressive Disclosure**: 詳細は `references/` に分離（1階層まで）

## ディレクトリ構成

```
skill-name/
├── SKILL.md           # 必須
├── references/        # 詳細（必要時のみ、1階層まで）
├── scripts/           # 実行スクリプト
└── assets/            # テンプレート等
```

## 命名規則

| 形式 | 推奨度 | 例 |
|------|--------|-----|
| gerund形式 | ◎ | `processing-pdfs`, `analyzing-data` |
| 曖昧な名前 | ✕ | `helper`, `utils` |

## frontmatter必須項目

```yaml
---
name: my-skill
description: |
  三人称で記述。目的を1行で。トリガーキーワード明記。
---
```

オプション: `allowed-tools`, `denied-tools`, `user-invocable-only`

## description設計

- **三人称で記述**（一人称・二人称は発見に問題）
- 具体的なトリガーキーワードを含める

## skills vs rules

| skills | rules |
|--------|-------|
| ワークフロー・手順 | パス固有のルール |
| 対話型・明示的呼び出し | 自動適用 |

## アンチパターン

- 500行超 → references/に分離
- トリガーなし → 自動選択されない
- 汎用description → 誤マッチ
- 深い参照ネスト → 1階層までに制限

## _shared/ 配置

2つ以上のスキルで共有 → `_shared/` に配置
