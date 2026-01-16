---
paths: .claude/rules/**
---

# Rules ガイド

## Rules 制約

- **推奨サイズ**: 1ファイル200行以下
- **frontmatter必須**: `paths` でglob指定
- **命名**: kebab-case（例: `claude-md.md`）

## frontmatter形式

```yaml
---
paths: src/**/*.ts
---
```

## 良いRulesの特徴

- 特定パスに対する明確な制約
- 短く具体的なガイドライン
- 自己完結（他ファイル参照なし推奨）

## 避けるべき内容

- globsで絞り込めない汎用的な内容 → CLAUDE.mdへ
- 長い手順やワークフロー → skillsへ
- 200行を超える詳細 → 分割を検討
