# Skill内サブエージェントオーケストレーション

skill/command内でTask toolを使ってサブエージェントを動的に起動するパターン集。

Task tool（サブエージェント起動）と Tasks API（タスク管理）は別物。Tasks APIについては [task-management.md](./task-management.md) を参照。

## 概要

### なぜskill内でadhocに呼び出すか

- **柔軟性**: タスクに応じて指示を調整可能
- **コンテキスト最適化**: 呼び出し時に必要な情報だけを渡す
- **管理コスト削減**: 別ファイルでの定義が不要

### Skills vs Subagents

コンテキスト設計の全体像は [context-architecture.md](./context-architecture.md) を参照。

```
「作業中に継続的な対話が必要？」
├─ YES → Skills（コンテキスト共有）
│   例: コードレビューでフィードバックを受けながら修正
│
└─ NO → 「試行錯誤や並列実行が必要？」
    ├─ YES → Subagents（コンテキスト分離）
    │   例: 5観点を並列でレビュー、調査タスク
    │
    └─ NO → Skills
```

---

## Task tool パラメータ一覧

| パラメータ | 必須 | 型 | 説明 |
|-----------|------|-----|------|
| `subagent_type` | ✓ | string | 使用するエージェントタイプ |
| `prompt` | ✓ | string | タスク内容 |
| `description` | 推奨 | string | 3-5語のタスク説明（ログ・進捗表示用） |
| `model` | - | `sonnet` / `opus` / `haiku` / `inherit` | 使用モデル（デフォルト: `inherit`） |
| `run_in_background` | - | boolean | バックグラウンド実行（デフォルト: `false`） |
| `isolation` | - | `"worktree"` | git worktreeで分離実行 |
| `resume` | - | string | 既存セッションを再開（session ID） |
| `max_turns` | - | number | 最大ターン数 |

### 利用可能な subagent_type

| タイプ | ツール | 用途 |
|--------|--------|------|
| `general-purpose` | 全ツール | 複雑な多段階タスク、探索+修正 |
| `Explore` | 読み取り専用 | コードベース探索・検索（高速・低コスト） |
| `Plan` | 読み取り専用 | プランモードでの調査 |
| `Bash` | Bash のみ | コマンド実行（独立コンテキスト） |
| カスタム名 | 設定次第 | `.claude/agents/` で定義したエージェント |

---

## モデル選択ガイド

| タスク種別 | モデル | 特徴 | 例 |
|-----------|--------|------|-----|
| 軽量チェック | Haiku | 高速・低コスト | 前提条件チェック、ファイル検索 |
| 中程度の分析 | Sonnet | バランス型 | サマリー生成、コンプライアンスチェック |
| 複雑な判断 | Opus | 高精度 | バグ検出、セキュリティ分析、複雑な推論 |

---

## Skill内でのサブエージェント呼び出し

### 基本パターン

```markdown
## Step 2: 並列レビュー

以下のサブエージェントを**単一メッセージで並列起動**:

### Agent 1: セキュリティチェック（Opus）
Task tool:
- model: opus
- prompt: |
    あなたはセキュリティ専門のレビュアーです。
    diffのみに集中。フラグすべき: SQLインジェクション、XSS

### Agent 2: ロジックチェック（Sonnet）
Task tool:
- model: sonnet
- prompt: |
    あなたはロジック専門のレビュアーです。
    ...
```

### 並列実行のガイドライン

- **単一メッセージ**で複数Task tool呼び出し
- 同時実行数は5-7程度を目安
- 依存関係がなければ並列、あれば順次

---

## 段階的パイプラインパターン

```
┌─────────────┐
│ 1. 前処理   │ ← Haiku（条件チェック、対象特定）
└──────┬──────┘
       ↓
┌─────────────┐
│ 2. メイン処理│ ← Sonnet/Opus 並列実行
└──────┬──────┘
       ↓
┌─────────────┐
│ 3. 検証     │ ← サブエージェントで再検証
└──────┬──────┘
       ↓
┌─────────────┐
│ 4. フィルタ │ ← 検証されなかったものを破棄
└──────┬──────┘
       ↓
┌─────────────┐
│ 5. 出力     │ ← 高信号のみ報告
└─────────────┘
```

---

## 並列実行パターン

### 同じ観点を分割

```markdown
## Step 2: CLAUDE.mdコンプライアンスチェック

以下を**単一メッセージで並列起動**:

### Agent 1（Sonnet）
ファイル1-5のCLAUDE.mdコンプライアンスをチェック

### Agent 2（Sonnet）
ファイル6-10のCLAUDE.mdコンプライアンスをチェック
```

### 異なる観点を同時実行

```markdown
## Step 2: 並列レビュー

以下を**単一メッセージで並列起動**:

### Agent 1: CLAUDE.mdコンプライアンス（Sonnet x 2）
### Agent 2: バグ検出（Opus x 2）
```

重要: 「単一メッセージで」を明記することで、Claudeが複数のTask toolを1回のAPIコールで呼び出す。

---

## バックグラウンド実行（run_in_background）

`run_in_background: true` でサブエージェントを非同期実行し、`TaskOutput` で結果を取得:

```markdown
## Step 1: バックグラウンドで並列起動

以下を**単一メッセージで**起動:

### Agent 1（セキュリティ分析）
Task tool:
- subagent_type: general-purpose
- model: sonnet
- run_in_background: true
- prompt: セキュリティ脆弱性を分析...

### Agent 2（パフォーマンス分析）
Task tool:
- subagent_type: general-purpose
- model: sonnet
- run_in_background: true
- prompt: パフォーマンスボトルネックを分析...

## Step 2: 結果を収集

TaskOutput で各エージェントの完了を待機（block: true）。
```

### フォアグラウンド vs バックグラウンドの使い分け

| | フォアグラウンド | バックグラウンド |
|--|--|--|
| 権限プロンプト | 実行中に表示 | 事前承認が必要（なければ失敗） |
| MCP ツール | 利用可 | 利用不可 |
| 推奨用途 | 短時間・インタラクティブ | 長時間・CPU集約型・完全自律 |
| 同時実行目安 | 5-7個 | 3-5個 |

---

## Worktree分離（isolation: "worktree"）

並列エージェントが同一リポジトリで独立して作業:

```markdown
Task tool:
- isolation: "worktree"
- run_in_background: true
- prompt: authentication モジュールをリファクタリング...
```

- `.claude/worktrees/` 以下に一時的な git worktree を作成（HEAD ベース）
- 変更がない場合は実行後に自動削除
- 複数エージェントが同一ファイルを修正しても競合しない
- 変更があった場合はブランチ名とworktreeパスが返される

---

## サブエージェント指示設計

### 必須要素

```markdown
Task tool:
- model: opus
- prompt: |
    ## ロール
    あなたはセキュリティ専門のレビュアーです。

    ## フォーカス範囲
    diffのみに集中。外部コンテキストは不要。

    ## フラグすべき
    - SQLインジェクション、XSS
    - 認証・認可の欠陥

    ## フラグしない
    - コードスタイル
    - 潜在的な問題
    - 確実でないもの

    ## 出力形式
    | ファイル | 行 | 種別 | 説明 |
```

### フォーカス範囲の明示

- 「diffのみに集中」
- 「外部コンテキストは不要」
- 「変更コード内のみを対象」

### ツール使用原則

```markdown
## 全エージェント共通ルール
- 全ツールは機能すると仮定（テスト呼び出し禁止）
- 探索的呼び出しは禁止（必要な場合のみ呼び出す）
- 各サブエージェントにも同じルールを周知
```

---

## 高信号フィルタリング

### フラグすべき

- コンパイル/パースエラー
- 型エラー、missing imports
- 明白なロジックエラー
- 明確なルール違反（引用可能）

### フラグしない（偽陽性回避）

- コードスタイルの懸念
- 入力依存の潜在的問題
- 主観的な改善提案
- 確実でない問題
- 既存の問題（新規導入ではない）
- ベテランエンジニアがフラグしない細部
- Linterがcatchする問題

---

## 検証サブエージェント

検出されたissueを別エージェントで再検証:

```markdown
## Step 3: 検証

Step 2で検出された各issueに対して:
- 並列でサブエージェントを起動
- バグ: Opus、CLAUDE.md違反: Sonnet
- issueの妥当性を検証
- 検証されなかったissueは破棄
```

---

## サブエージェントへのルール継承

```markdown
## サブエージェント呼び出し時
親エージェントのルールをサブエージェントにも周知:
- 共通原則をプロンプトに含める
- 出力形式を統一
- フラグ基準を継承
```

---

## 出力形式の標準化

### リンク形式

```
形式: https://github.com/owner/repo/blob/[FULL_SHA]/path#L[start]-L[end]

要件:
- フルgit SHA必須（短縮形NG）
- 行範囲: L[start]-L[end]
- コンテキスト: 対象行の前後1行以上を含める
```

### コメント形式

- 小規模（6行以下）: 修正提案を含める
- 大規模（6行以上）: 説明のみ
- 1 issue = 1 comment（重複禁止）

---

## 実例: code-review.mdパターン

Claude Code公式のcode-review.mdの構造:

```markdown
## Step 1: 前処理チェック（Haiku）
PRがクローズ済み/ドラフト/レビュー不要かをチェック

## Step 2: CLAUDE.md検索（Haiku）
関連するCLAUDE.mdファイルのパスを取得

## Step 3: PRサマリー（Sonnet）
PR全体の変更サマリーを取得

## Step 4: 並列レビュー
- Agent 1 & 2: CLAUDE.mdコンプライアンス（Sonnet x 2）
- Agent 3 & 4: バグ検出（Opus x 2）

## Step 5: 検証
各issueをサブエージェントで再検証

## Step 6: フィルタリング
検証されなかったissueを削除

## Step 7: 出力
高信号issueのみをインラインコメントで投稿
```

---

## カスタムエージェントテンプレート

カスタムエージェントは `.claude/agents/` または `~/.claude/agents/` に Markdown ファイルとして定義する。

### frontmatter 完全仕様

| フィールド | 必須 | 説明 |
|-----------|------|------|
| `name` | ✓ | 小文字とハイフンのみ。エージェント識別子 |
| `description` | ✓ | Claudeによる自動選択の判断材料 |
| `tools` | - | 許可ツールリスト（省略時: 全ツール継承） |
| `disallowedTools` | - | 拒否ツールリスト |
| `model` | - | `sonnet` / `opus` / `haiku` / `inherit`（デフォルト: `inherit`） |
| `permissionMode` | - | `default` / `acceptEdits` / `dontAsk` / `bypassPermissions` / `plan` |
| `maxTurns` | - | 最大ターン数 |
| `skills` | - | 起動時にコンテキストへ注入するスキル |
| `mcpServers` | - | 使用可能なMCPサーバー |
| `hooks` | - | ライフサイクルフック（PreToolUse, PostToolUse, Stop） |
| `memory` | - | 永続メモリスコープ: `user` / `project` / `local` |
| `background` | - | `true` で常にバックグラウンド実行 |
| `isolation` | - | `worktree` で一時的なgit worktree内で実行 |

### 定義例

```yaml
# .claude/agents/security-reviewer.md
---
name: security-reviewer
description: |
  セキュリティ観点でコードをレビュー。OWASP Top 10に基づいて脆弱性を検出。
  Use PROACTIVELY after code changes.
tools: Read, Grep, Glob
model: sonnet
permissionMode: dontAsk
skills:
  - security-patterns
---

# セキュリティレビュアー

## 役割
OWASP Top 10 に基づいてセキュリティ脆弱性を検出。

## チェック項目
- インジェクション攻撃
- 認証・認可の不備
- 機密データの露出
- XSS（クロスサイトスクリプティング）
```

### エージェントスコープ

| ロケーション | スコープ | 優先度 |
|-------------|--------|--------|
| `--agents` CLI引数 | 現在セッション限定 | 最高 |
| `.claude/agents/` | プロジェクト共有 | 高 |
| `~/.claude/agents/` | 全プロジェクト共通 | 低 |

- プロジェクトエージェント (`.claude/agents/`) は git に含めてチーム共有を推奨
- グローバルエージェント (`~/.claude/agents/`) は全プロジェクトで再利用

### description 設計

Claudeによる**自動選択**を制御するキーワード:

```yaml
# 積極的な自動選択
description: Use PROACTIVELY when code is written or modified.

# 必須呼び出し
description: MUST BE USED for all security-related code reviews.

# 即座に呼び出し
description: Invoke IMMEDIATELY after writing authentication code.
```

### Skills preloading

`skills` フィールドでスキルのフル内容を起動時に注入:

```yaml
---
name: api-developer
description: API エンドポイントの実装
skills:
  - api-conventions
  - error-handling-patterns
---
```

- スキルは起動時に完全ロードされる（遅延ロードではない）
- 親セッションのスキルは継承されない。明示的にリストすること
- **組み込みエージェント（Explore、Plan、general-purpose）は `skills` フィールドを持たない**

### サブエージェント生成制限

`tools` フィールドで起動可能なサブエージェントを制限:

```yaml
---
name: coordinator
description: 専門エージェントへのタスク振り分け
tools: Task(worker, researcher), Read, Bash
---
```

- `Task(worker, researcher)`: `worker` と `researcher` のみ生成可能
- `Task` のみ: 全サブエージェントを許可
- `Task` を省略: サブエージェント生成禁止

### permissionMode

| モード | 動作 |
|--------|------|
| `default` | 標準的な権限チェック |
| `acceptEdits` | ファイル編集を自動承認 |
| `dontAsk` | 権限プロンプトを自動拒否（読み取り専用分析向け） |
| `bypassPermissions` | 全権限チェックをスキップ（⚠️ 信頼できる環境のみ） |
| `plan` | 読み取り専用（プランモード） |

### 永続メモリ（memory フィールド）

```yaml
---
name: code-reviewer
memory: project
---

レビューを通じて発見したパターンや慣習をメモリに記録すること。
次回のレビュー前にメモリを参照して過去の知識を活用すること。
```

| スコープ | 保存先 | 用途 |
|---------|---------|------|
| `user` | `~/.claude/agent-memory/<name>/` | 全プロジェクト共通の知識 |
| `project` | `.claude/agent-memory/<name>/` | プロジェクト固有・チーム共有 |
| `local` | `.claude/agent-memory-local/<name>/` | プロジェクト固有・個人用 |
