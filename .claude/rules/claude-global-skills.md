---
paths: .claude-global/skills/**
---

# グローバル Skills ルール

## 制約

- **SKILL.md**: 500行以下
- **Progressive Disclosure**: 詳細は `references/` に分離（1階層まで）
- **description**: 三人称で記述、トリガーキーワード必須
- **name**: gerund形式推奨（`processing-pdfs`等）

## description設計

- **三人称で記述**（一人称・二人称は発見に問題を起こす）
- 具体的なトリガーキーワードを含める

## skills vs agents

| skills | agents |
|--------|--------|
| コンテキスト共有 | コンテキスト分離 |
| 手順定義、ガイド | 並列実行、専門役割 |

→ サブエージェントはskill内でTask toolを呼び出す

## スキル一覧

| スキル | トリガー |
|--------|---------|
| architect | 「設計して」「plan.md作成」 |
| breakdown | 「/breakdown」「タスク分解」 |
| implement | 「/implement」「タスク実行」 |
| implement-review | 「/implement-review」「実装してレビューまで」 |
| code-review | 「コードレビュー」「レビューして」 |
| refactor | 「リファクタリング」「コード改善」「スメル検出」 |
| researcher | 「調査して」「リサーチして」 |
| debug | 「デバッグ」「バグ調査」 |
| critical-think | 「批判的に見て」「自己レビュー」 |
| domain-context | 「学びを保存」「ドメイン知識更新」 |
| context-optimizer | 「CLAUDE.md圧縮」「コンテキスト最適化」 |
| config-optimizer | 「設定を最適化」 |
| skill-creator | 「スキルを作って」 |
| task | 「/task」「タスク追加」 |
| ghq | 「ghq」「リポジトリ管理」「リポジトリをクローン」 |
| setup-linter-hooks | 「linter hook作成」「formatter hook設定」「編集後linter実行」 |
