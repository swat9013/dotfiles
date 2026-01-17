# Frontmatter テンプレート集

各設定ファイルで使用する frontmatter の例。

---

## Rules（パス固有ルール）

### 必須フィールド

- `paths`: 適用するファイルパス（glob形式）

### 基本テンプレート

```yaml
---
paths:
  - "src/**/*.ts"
---
```

### 除外パターン付き

```yaml
---
paths:
  - "src/**/*.ts"
  - "!src/**/*.test.ts"
  - "!src/**/*.spec.ts"
---
```

### 複数言語対応

```yaml
---
paths:
  - "**/*.ts"
  - "**/*.tsx"
  - "**/*.js"
  - "**/*.jsx"
---
```

### ディレクトリ指定

```yaml
---
paths:
  - "src/api/**"
  - "src/routes/**"
---
```

---

## Skills（ワークフロー）

### 必須フィールド

- `name`: スキル名（kebab-case、64文字以内）
- `description`: 説明（トリガーキーワードを含める）

### オプションフィールド

- `allowed-tools`: 使用可能ツールの制限
- `model`: 使用モデル（claude-sonnet-4など）
- `user-invocable`: `/skill`で実行可能か（デフォルト: true）

### 基本テンプレート

```yaml
---
name: code-review
description: |
  5観点でコードレビューを実行。
  「コードレビュー」「レビューして」と依頼された時に使用。
---
```

### ツール制限付き

```yaml
---
name: security-check
description: |
  セキュリティ観点でコードを検査。
  「セキュリティチェック」「脆弱性確認」と依頼された時に使用。
allowed-tools:
  - Read
  - Grep
  - Glob
---
```

### 非公開スキル

```yaml
---
name: internal-helper
description: |
  内部処理用のヘルパースキル。
  他のスキルから呼び出される。
user-invocable: false
---
```

---

## description の書き方

### 良い例

```yaml
description: |
  5観点でコードレビューを実行。
  「コードレビュー」「PRレビュー」「レビューして」と依頼された時に使用。
```

- 何をするか（1行目）
- いつ使うか（2行目以降、トリガーキーワード）

### 悪い例

```yaml
description: コードをレビューする
```

- 「いつ使うか」が不明確
- トリガーキーワードがない

---

## チェックリスト

抽出ファイル作成時:

- [ ] Rules: `paths:` が存在するか
- [ ] Skills: `name:` と `description:` が存在するか
- [ ] description に「〜時に使用」相当の説明があるか
