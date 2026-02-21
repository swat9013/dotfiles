# Rules 設計ガイド

## 概要

Rulesは特定のパスにアクセスした際に自動適用されるコンテキスト固有のガイドライン。frontmatterの `paths` で指定したglob patternにマッチするファイルを開くと、そのルールが自動的に適用される。

## ファイル制約

| 項目 | 値 |
|------|-----|
| 推奨サイズ | 200行以下 |
| 配置場所 | `.claude/rules/` |
| 形式 | Markdown + YAML frontmatter |
| 命名規則 | kebab-case（例: `api-endpoints.md`, `test-files.md`） |

## frontmatter 形式

### 単一パターン

```yaml
---
paths: src/**/*.ts
---
```

### 複数パターン

```yaml
---
paths: src/api/**, src/services/**
---
```

### 除外パターン

```yaml
---
paths: **/*.test.ts, !**/__mocks__/**
---
```

**注意**: YAML配列形式（ハイフン複数行）は適用されない。カンマ区切りワンライナーで記述すること。

## Rules vs CLAUDE.md vs skills

比較表と使い分け判断フローは [context-architecture.md](./context-architecture.md) を参照。

## 良いRulesの特徴

- **パス固有**: globで明確に絞り込める
- **短く具体的**: 200行以下で要点を伝える
- **自己完結**: 他ファイル参照なしで理解可能
- **アクション指向**: 「〜すること」「〜しないこと」が明確

## ルール内容の構成例

```markdown
---
paths: src/api/**
---

# API層のガイドライン

## 必須事項
- すべてのエンドポイントでエラーハンドリング
- リクエスト/レスポンスの型定義必須

## 禁止事項
- 直接のDB操作（サービス層経由）
- コンソールログ（ロガー使用）

## 命名規則
- エンドポイント: RESTful形式
- ファイル名: `{resource}.controller.ts`
```

## アンチパターン

| パターン | 問題点 | 代替案 |
|---------|--------|--------|
| globで絞れない汎用的な内容 | 全ファイルに適用 = CLAUDE.md | CLAUDE.mdに記載 |
| 長い手順（300行超） | ルールではなくワークフロー | skills/に分離 |
| 複数ルールで同じ内容 | DRY違反 | CLAUDE.mdまたは共通rulesに集約 |
| 実装詳細の羅列 | ルールではなくドキュメント | docs/に移動 |

### 悪い例

```yaml
---
paths: "**/*"              # 広すぎる → CLAUDE.mdへ
---
```

```yaml
---
paths: src/**
---
# 以下500行の詳細手順...  # 長すぎる → skills/へ
```

## ルールファイルの分割基準

1ファイルが200行を超える場合:

- **関心事ごとに分割**: `api-validation.md`, `api-error-handling.md`
- **共通部分はCLAUDE.mdに昇格**: 複数ルールで同じ内容は上位層へ
