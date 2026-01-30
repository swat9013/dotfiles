---
paths: **/.claude/skills/**
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

オプション: `argument-hint`, `disable-model-invocation`, `user-invocable`, `allowed-tools`, `model`, `context`, `agent`, `hooks`

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
- コマンド存在確認 → 不要（fail-fastで対応）

## サブエージェント活用原則

スキル内でTask toolによるサブエージェント委譲を積極活用:

**推奨パターン**:
- 複数の独立した調査・分析（並列実行）
- 異なる観点からの評価（例: Security/Performance/Design）
- 時間のかかる処理の非同期実行

**モデル選択**:
- 調査・検索: haiku（軽量）
- 分析・設計: sonnet（標準）
- 高精度必須: opus（慎重に）

**実装例**:
```markdown
## 実行手順
### Phase 1: 並列調査（3サブエージェント）
Task toolで以下を同時実行:
- Agent A: セキュリティ観点分析
- Agent B: パフォーマンス観点分析
- Agent C: 設計観点分析
```

## _shared/ 配置

2つ以上のスキルで共有 → `_shared/` に配置
