---
paths: "**/.claude/skills/**"
---

# Skills 作成ルール

## 必須制約

- **SKILL.md**: 500行以下
- **frontmatter**: name（ハイフンケース）+ description（トリガーキーワード必須）
- **Progressive Disclosure**: 詳細は `references/` に分離

## ディレクトリ構成

```
skill-name/
├── SKILL.md           # 必須
├── references/        # 詳細（必要時のみ読み込み）
├── scripts/           # 実行スクリプト
└── assets/            # テンプレート等
```

## frontmatter必須項目

```yaml
---
name: my-skill
description: |
  目的を1行で。「〜と依頼されたら」形式でトリガー明記。
---
```

オプション: `allowed-tools`, `denied-tools`, `user-invocable-only`

## skills vs rules

| skills | rules |
|--------|-------|
| ワークフロー・手順 | パス固有のルール |
| 対話型・明示的呼び出し | 自動適用 |

## アンチパターン

- 500行超 → references/に分離
- トリガーなし → 自動選択されない
- 汎用description → 誤マッチ

## _shared/ 配置

2つ以上のスキルで共有 → `_shared/` に配置
