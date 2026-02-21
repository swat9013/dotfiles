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

## 優先度

Enterprise > Personal (`~/.claude/skills/`) > Project (`.claude/skills/`) > Plugin

**skills と commands が同名の場合、skillsが優先**

## スキル一覧

### ワークフロー系スキル

| スキル | トリガー |
|--------|---------|
| architect | 「設計して」「plan.md作成」 |
| breakdown | 「/breakdown」「タスク分解」 |
| implement | 「/implement」「タスク実行」 |
| implement-review | 「/implement-review」「実装してレビューまで」 |
| code-review | 「コードレビュー」「レビューして」 |
| codex-code-review | 「codexレビュー」「アーキテクチャレビュー」「設計レビュー」 |
| refactor | 「リファクタリング」「コード改善」「スメル検出」 |
| requirements | 「要件整理」「requirements」「何を作るべきか明確にして」 |
| researcher | 「調査して」「リサーチして」 |
| debug | 「デバッグ」「バグ調査」 |
| critical-think | 「批判的に見て」「自己レビュー」 |
| domain-context | 「学びを保存」「ドメイン知識更新」 |
| context-optimizer | 「CLAUDE.md圧縮」「コンテキスト最適化」 |
| config-optimizer | 「設定を最適化」 |
| task | 「/task」「タスク追加」 |
| setup-linter-hooks | 「linter hook作成」「formatter hook設定」「編集後linter実行」 |

### 知識系スキル（user-invocable: false）

| スキル | トリガーキーワード | 内容 |
|--------|------------------|------|
| managing-skills | 「スキルを作って」「スキルを変更」「スキルを更新」「新しいスキルを追加」 | スキル作成・変更・更新の知識ガイド |
| ghq | 「ghq」「リポジトリ管理」「リポジトリをクローン」 | リポジトリ管理ツールガイド |
| agent-browser | 「ブラウザ操作」「Webテスト」「スクリーンショット取得」「フォーム入力」 | ブラウザ自動化ツールガイド |
| frontend-design | 「UI作成」「フロントエンド実装」「コンポーネント作成」「Webページ作成」 | UI/フロントエンド実装ガイド |
| claude-mem | 「claude-mem」「メモリ検索」「セッション履歴」「コンテキスト検索」「過去のセッション」 | claude-memプラグインの使用ガイド（MCPツール・3層ワークフロー・Progressive Disclosure） |
| playwright-cli | 「Playwright」「playwright test」「E2Eテスト実行」「codegen」「トレース」 | Playwright CLIコマンドリファレンス |
| marimo | 「marimo」「マリモ」「リアクティブノートブック」「marimo edit」 | リアクティブPythonノートブック操作ガイド |
| slidev | 「Slidev」「スライド作成」「プレゼン作成」「sli.dev」 | Slidevスライド内容生成ガイド |
| log-designing | 「ログ設計」「構造化ログ」「ログ基盤」「verbosity」 | ログ設計7原則・CLIパターン・段階的導入ガイド |
