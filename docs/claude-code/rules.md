# Rules 設計ガイド

## 概要

Rulesはモジュール化されたコンテキスト固有のガイドライン。セッション開始時に `.claude/rules/` 以下（サブディレクトリ含む）が再帰的にロードされ、frontmatterの `paths` で指定したglob patternにマッチするファイルにアクセスした際に適用される。

## ファイル制約

| 項目 | 値 |
|------|-----|
| 推奨サイズ | 200行以下 |
| 配置場所 | `.claude/rules/`（サブディレクトリ可: `rules/frontend/react.md`） |
| 形式 | Markdown + YAML frontmatter |
| 命名規則 | kebab-case（例: `api-endpoints.md`, `test-files.md`） |
| シンボリックリンク | サポート（循環リンク検出あり） |
| ユーザーレベル | `~/.claude/rules/` にも配置可能（ただし `paths:` は無視される） |

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

> **注意**: `!` 接頭辞による除外パターンは実装によって動作しない場合がある。可能であれば肯定的なパターンのみで表現すること。

**YAML記法の注意点**:
- YAML配列形式（ハイフン複数行）は適用されない。カンマ区切りワンライナーで記述すること（[Issue #13905](https://github.com/anthropics/claude-code/issues/13905)）。
- `{src,lib}/**` のような波括弧を含むパターンはYAML予約文字のためクォートが必要: `"src/**", "lib/**"` に分割するか `"{src,lib}/**"` とクォートする。
- クオーテーションなしの記述が基本（シンプルなパターンは不要）。

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
| `~/.claude/rules/` の `paths:` に依存 | ユーザーレベルルールでは `paths:` が無視される ([Issue #21858](https://github.com/anthropics/claude-code/issues/21858)) | プロジェクトレベル (`.claude/rules/`) に配置 or symlink |

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
- **サブディレクトリで整理**: `rules/frontend/react.md`, `rules/backend/database.md`
- **共通部分はCLAUDE.mdに昇格**: 複数ルールで同じ内容は上位層へ

## 既知の問題（2026年2月時点）

| Issue | 概要 | 回避策 |
|-------|------|--------|
| [#16299](https://github.com/anthropics/claude-code/issues/16299) | path-scoped rules がセッション開始時に無条件ロードされる（paths 条件が実質機能せずcontext bloat の原因） | ルール数・サイズを最小限に保つ |
| [#21858](https://github.com/anthropics/claude-code/issues/21858) | `~/.claude/rules/` の `paths:` frontmatter が無視される | プロジェクトレベル (`./.claude/rules/`) に配置、またはシンボリックリンクを使用 |
| [#13905](https://github.com/anthropics/claude-code/issues/13905) | YAML配列形式（`paths: - "..."` 複数行）が機能しない | カンマ区切りワンライナーで記述 |
