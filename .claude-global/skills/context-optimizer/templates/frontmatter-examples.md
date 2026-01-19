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

- `name`: スキル名（gerund形式推奨、64文字以内）
- `description`: 説明（**三人称で記述**、トリガーキーワードを含める）

### オプションフィールド

- `allowed-tools`: 使用可能ツールの制限
- `model`: 使用モデル（claude-sonnet-4など）
- `user-invocable`: `/skill`で実行可能か（デフォルト: true）

### 基本テンプレート

```yaml
---
name: code-review
description: |
  5観点でコードレビューを実行する。
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

### 記述ルール

- **三人称で記述する**（一人称・二人称は発見に問題を起こす）
- 何をするか（1行目）
- いつ使うか（2行目以降、トリガーキーワード）

### 良い例

```yaml
description: |
  5観点でコードレビューを実行する。
  「コードレビュー」「PRレビュー」「レビューして」と依頼された時に使用。
```

### 悪い例

```yaml
# 曖昧・トリガーなし
description: コードをレビューする

# 二人称（発見に問題）
description: ユーザーのコードレビューを手伝う
```

---

## チェックリスト

抽出ファイル作成時:

- [ ] Rules: `paths:` が存在するか
- [ ] Skills: `name:` と `description:` が存在するか
- [ ] description が**三人称で記述**されているか
- [ ] description に「〜時に使用」相当の説明があるか
- [ ] 参照ファイルが**1階層まで**に制限されているか
