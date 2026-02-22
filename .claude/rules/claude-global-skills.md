---
paths: .claude-global/skills/**
---

# グローバル Skills ルール

## 制約

| 項目 | 値 |
|------|-----|
| SKILL.md | 500行以下 |
| コンテンツ予算 | 15,000文字（スキル全体） |
| Progressive Disclosure | 詳細は `references/` に分離（1階層まで） |
| 100行超のreferences | 先頭にTOC必須 |

## frontmatter

| フィールド | 必須 | 説明 |
|-----------|-----|------|
| `name` | ○ | gerund形式推奨（`processing-pdfs`等） |
| `description` | ○ | 三人称、トリガーキーワード必須 |
| `argument-hint` | - | 引数のヒント表示 |
| `disable-model-invocation` | - | `true`: Claude自動呼び出し無効 |
| `user-invocable` | - | `false`: ユーザー手動呼び出し無効 |
| `allowed-tools` | - | 使用可能ツールの制限 |
| `model` | - | 使用モデル指定 |
| `context` | - | `fork`: コンテキスト分離 |
| `agent` | - | `true`: サブエージェントとして実行 |
| `hooks` | - | スキル固有のhooks設定 |

## description設計

**フォーマット**: `Verb+what. Use when+trigger1, trigger2, or trigger3.`

- **三人称で記述**（一人称・二人称は発見に問題を起こす）
- 具体的なトリガーキーワードを含める
- 曖昧な表現（「いろいろな」「便利な」）は避ける

## 呼び出し制御

| 設定 | ユーザー | Claude自動 | 用途 |
|------|---------|-----------|------|
| デフォルト | ○ | ○ | 汎用 |
| `disable-model-invocation: true` | ○ | ✕ | deploy, commit等の副作用あり |
| `user-invocable: false` | ✕ | ○ | 背景知識・参照ガイド |

## skills vs agents

| skills | agents |
|--------|--------|
| コンテキスト共有 | コンテキスト分離 |
| 手順定義、ガイド | 並列実行、専門役割 |

> サブエージェントはskill内でTask toolを呼び出す

## 優先度

Enterprise > Personal (`~/.claude/skills/`) > Project (`.claude/skills/`) > Plugin

**skills と commands が同名の場合、skillsが優先**

## 高度な機能

### 文字列置換

| 変数 | 内容 |
|------|------|
| `$ARGUMENTS` | 呼び出し時の引数全体 |
| `$ARGUMENTS[0]`, `[1]` | 位置指定アクセス |
| `${CLAUDE_SESSION_ID}` | セッションID |

### 動的コンテキスト注入

`` !`git branch --show-current` `` — スキルロード時にコマンド出力を展開

### Extended Thinking

SKILL.md内に `ultrathink` を含めると拡張思考モード有効化

### コンテキスト分離

```yaml
context: fork
agent: true
```

サブエージェントとして隔離実行する場合に設定

## Progressive Disclosure

- SKILL.md本体にワークフロー/概要のみ
- 詳細は `references/` に分離（**1階層まで**）
- 100行超のreferencesは**先頭にTOC必須**
- 2つ以上のスキルで共有するreferences → `_shared/` に配置

## 設計注意事項

- **スキルリネーム時の同時更新箇所**: ディレクトリ名・frontmatter name・タイトル・使用例コマンド名・スキルカタログ（claude-global-skills.md）の5箇所
- **`[NEEDS CLARIFICATION]` マーカー**: 非対話的成果物（レポート等）向け。対話型スキルでは直接質問する
- **知識系スキルの判断基準**: 利用場面が特定コンテキストのみ → 既存スキルの `references/` に内包。複数コンテキスト → 独立スキル
- **references に書かない内容**: Claude の一般知識（ライブラリ名・バージョン等）の列挙は価値低。プロジェクト固有の判断基準・フローチャートのみ
- **references のハードコード日付**: 例に具体的な日付を入れると腐る。相対表現を使う
- **調査→スキル化後**: 元の report.md 等を削除するか参照に更新。放置すると二重管理
- **`$ARGUMENTS` 展開の注意**: 長い引き継ぎ文をそのまま渡すと TaskCreate の subject が肥大化。要約して渡す

## アンチパターン

| パターン | 対策 |
|---------|------|
| 500行超 | references/に分離 |
| 15,000文字超 | 圧縮またはreferences/に分離 |
| 100行超referencesにTOCなし | 先頭にTOC追加 |
| トリガーなしdescription | 自動選択されない→トリガー追加 |
| 汎用description | 誤マッチ→具体的に |
| 深い参照ネスト | 1階層までに制限 |

## スキル一覧

### ワークフロー系スキル

| スキル | トリガー |
|--------|---------|
| architect | 「設計して」「plan.md作成」 |
| breakdown | 「/breakdown」「タスク分解」 |
| implement | 「/implement」「タスク実行」 |
| implement-review | 「/implement-review」「実装してレビューまで」 |
| review-fix | 「/review-fix」「レビュー修正」「実装後レビュー」 |
| code-review | 「コードレビュー」「レビューして」 |
| codex-code-review | 「codexレビュー」「アーキテクチャレビュー」 |
| refactor | 「リファクタリング」「コード改善」 |
| requirements | 「要件整理」「requirements」 |
| researcher | 「調査して」「リサーチして」 |
| troubleshooting | 「デバッグ」「バグ調査」 |
| critical-think | 「批判的に見て」「自己レビュー」 |
| discovery | 「壁打ちしたい」「問題を整理したい」 |
| retrospective | 「学びを保存」「ドメイン知識更新」「振り返り」「retrospective」 |
| context-optimizer | 「CLAUDE.md圧縮」「コンテキスト最適化」 |
| doc-architect | 「ドキュメント設計」「docs作成」 |
| todoist | 「Todoist」「タスク追加」「タスクリファイン」「タスク整理」 |
| setup-linter-hooks | 「linter hook作成」「formatter hook設定」 |

### 知識系スキル（user-invocable: false）

| スキル | トリガーキーワード | 内容 |
|--------|------------------|------|
| managing-skills | 「スキルを作って」「スキル更新」 | スキル作成・変更ガイド |
| ghq | 「ghq」「リポジトリ管理」 | リポジトリ管理ツールガイド |
| agent-browser | 「ブラウザ操作」「Webテスト」 | ブラウザ自動化ツールガイド |
| frontend-design | 「UI作成」「フロントエンド実装」 | UI/フロントエンド実装ガイド |
| claude-mem | 「claude-mem」「メモリ検索」 | claude-memプラグイン使用ガイド |
| playwright-cli | 「Playwright」「E2Eテスト」 | Playwright CLIリファレンス |
| marimo | 「marimo」「リアクティブノートブック」 | リアクティブPython操作ガイド |
| slidev | 「Slidev」「スライド作成」 | Slidevスライド内容生成ガイド |
| log-designing | 「ログ設計」「構造化ログ」 | ログ設計7原則・CLIパターンガイド |
| serena | 「Serena」「シンボル検索」 | セマンティックコード検索MCPガイド |
