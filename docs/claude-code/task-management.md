# タスク管理システム（Tasks API）

## 概要

Claude Code v2.1.16（2026-01-22）で導入されたタスク管理システム。旧 `TodoWrite/TodoRead` を置き換え、依存関係追跡・ファイルシステム永続化・クロスセッション協調を提供する。

4つのツールで構成:
- **TaskCreate** — タスク作成
- **TaskUpdate** — ステータス・依存関係・メタデータの更新
- **TaskGet** — タスク詳細の取得
- **TaskList** — 全タスク一覧の取得

## ツール仕様

### TaskCreate

| パラメータ | 必須 | 型 | 説明 |
|-----------|------|-----|------|
| `subject` | 必須 | string | 命令形のタイトル（例: "Fix authentication bug"） |
| `description` | 任意 | string | 詳細な要件・受入条件 |
| `activeForm` | 推奨 | string | `in_progress`時のスピナー表示（例: "Fixing authentication bug"） |
| `metadata` | 任意 | object | 任意のキー・バリュー（priority, filesなど） |

- `subject`: 命令形（"Run tests"）
- `activeForm`: 現在進行形（"Running tests"）
- 全タスクは `pending` で作成される

### TaskUpdate

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `taskId` | string（必須） | 対象タスクID |
| `status` | enum | `pending` / `in_progress` / `completed` / `deleted` |
| `subject` | string | タイトル変更 |
| `description` | string | 説明変更 |
| `activeForm` | string | スピナーテキスト変更 |
| `owner` | string | 所有者（エージェント名） |
| `metadata` | object | メタデータのマージ（keyをnullで削除） |
| `addBlocks` | array | このタスク完了まで開始不可なタスクID |
| `addBlockedBy` | array | 先に完了が必要なタスクID |

### TaskGet

`taskId` を指定して完全な詳細（description, metadata, activeForm, タイムスタンプ含む）を取得。

### TaskList

パラメータなし。返却フィールドは `id`, `subject`, `status`, `owner`, `blockedBy` のみ。

**重要**: `description`, `metadata`, `activeForm` は TaskList では取得できない。

## ステータスワークフロー

```
pending → in_progress → completed
                      ↘ deleted
```

| ルール | 説明 |
|--------|------|
| 作業開始前に `in_progress` | スピナー表示が開始される |
| 実際の完了確認後に `completed` | 部分完了でのマーク禁止 |
| エラー・中断時はステータス維持 | 報告して次のアクションを判断 |
| `TaskUpdate`前に`TaskGet` | 最新状態を確認してから更新 |

## 依存関係管理

DAG（有向非循環グラフ）構造で依存関係を表現。

```
# Task 3はTask 1, 2の完了が前提
TaskUpdate({ taskId: "3", addBlockedBy: ["1", "2"] })

# Task 1が完了するまでTask 3は開始不可（逆方向の宣言）
TaskUpdate({ taskId: "1", addBlocks: ["3"] })
```

**注意事項**:
- `addBlockedBy` / `addBlocks` は配列への追加（置換ではない）
- システムは依存関係を物理的に強制しない（宣言のみ）
- 循環依存の自動検出なし（設計時に確認が必要）
- ブロック解除は依存タスクの `completed` で自動

## 使用判断

### タスク管理を使うべき場面

- 3ステップ以上の複合タスク
- Plan Modeでの作業
- 複数セッション・エージェント間の協調
- ユーザーが明示的にタスクリストを要求

### 使うべきでない場面

- 単一ファイルの変更
- 3ステップ未満の直線作業
- 会話・情報提供のみ

## タスク分解のベストプラクティス

### 粒度の目安

| 粒度 | 例 | 評価 |
|------|-----|------|
| 粗すぎる | 「認証システム全体を作れ」 | NG — 進捗確認不能 |
| 適切 | 「JWTトークン検証ミドルウェアを実装」 | OK — 独立した成果物 |
| 細かすぎる | 「importステートメントを追加」 | NG — 管理コスト過大 |

推奨タスク数: 8〜15タスク（大規模機能の場合）

### subject設計

TaskListでは `description` が見えないため、`subject` に重要情報を集約する。

```
# NG
subject: "データベース作業"

# OK
subject: "JWT認証用のusersテーブルにemail/password_hashカラムを追加"
```

### Wave分解パターン

```
Wave 1（並列）: DBスキーマ設計 | 認証設定
Wave 2（依存）: API実装（Wave 1完了後）
Wave 3（依存）: フロントエンド（Wave 2完了後）
Wave 4（独立）: テスト | ドキュメント
```

### 1エージェント・1ファイルオーナーシップ

複数エージェントが同じファイルを編集するとコンフリクトが発生する。タスク分割時にファイルの所有権を明確にする。

## Task tool（サブエージェント）との統合

### 使い分け

| 概念 | 目的 |
|------|------|
| `Task` tool | サブエージェント（別コンテキストウィンドウ）の起動 |
| `TaskCreate/Update/Get/List` | 共有タスクリストの管理 |

連携パターン: TaskCreateでタスク定義 → Task toolでサブエージェント起動 → サブエージェントがTaskUpdateでステータス更新

### 並列実行

- Task toolの並列上限: **最大7〜10エージェント**
- `run_in_background: true` でオーケストレーターのコンテキストを温存
- 小タスクはサブエージェントより直接実行が約10倍安い

## クロスセッション協調

### 環境変数

```bash
# 複数セッションでタスクリストを共有
CLAUDE_CODE_TASK_LIST_ID="my-project" claude

# タスク機能を無効化（旧TodoWriteに戻す）
CLAUDE_CODE_ENABLE_TASKS=false claude

# ヘッドレスモードでタスク有効化
CLAUDE_CODE_ENABLE_TASKS=true claude -p "..."
```

### Agent Teams（実験的、v2.1.32〜）

```bash
# 有効化
CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1 claude
```

- **Team Lead**: タスク作成・割り当て・調整
- **Teammates**: 独立コンテキストでタスク実行
- **共有タスクリスト**: 全メンバーがクレーム・更新可能

### 関連フックイベント

| イベント | 発火タイミング | 用途 |
|---------|---------------|------|
| `TaskCompleted` | タスク完了遷移時 | 品質ゲート（exit 2で完了ブロック） |
| `TeammateIdle` | チームメイトのアイドル直前 | 追加作業の指示 |
| `SubagentStop` | サブエージェント停止時 | 出力品質検証 |

## UIショートカット

| キー | 動作 |
|------|------|
| `Ctrl+T` | タスクリスト表示/非表示 |
| `Shift+Down` | チームメート間サイクル（Agent Teams） |

## ストレージ

```
~/.claude/tasks/{task-list-id}/tasks.json   # タスクデータ
~/.claude/teams/{team-name}/config.json      # チーム設定
```

プロジェクトディレクトリではなくホームディレクトリに保存される。

## 既知の制限・注意事項

### TaskListのフィールド制限

description, metadata, activeFormを取得するにはTaskGetが必要。N件のタスク詳細取得 = 1（TaskList）+ N（TaskGet）= N+1 APIコール。

**対策**: subjectに重要情報を集約し、TaskGetの呼び出しを最小化。

### フックバイパス問題

TaskCreate/TaskUpdate/TaskGet/TaskListは `PreToolUse`/`PostToolUse` フックをバイパスする。（[Issue #20243](https://github.com/anthropics/claude-code/issues/20243)）

### ヘッドレスモードでのデフォルト無効

`claude -p` ではデフォルトでTask*ツールが使えない。`CLAUDE_CODE_ENABLE_TASKS=true` で明示的に有効化が必要。（[Issue #20463](https://github.com/anthropics/claude-code/issues/20463)）

### 並行アクセスの競合

複数セッションが同じtasks.jsonを同時更新した場合はLast-Write-Wins（後勝ち）。ロック機構はローカルファイルシステムの.lockファイルのみ。

### Agent Teamsの制限

- セッション再開（`/resume`, `/rewind`）でチームメイトは復元されない
- ネストしたチーム不可（リードのみがチームを作成可能）
- 1セッション・1チーム

## コスト最適化

| 戦略 | 効果 |
|------|------|
| サブエージェントのモデル選択（haiku/sonnet） | コスト削減（Opusの1/10〜1/3） |
| 大量出力タスクをサブエージェントに委譲 | メインコンテキスト節約 |
| 小タスクはメインスレッドで直接実行 | サブエージェント起動コスト回避 |
| `run_in_background: true` | コンテキスト汚染防止 |

## TodoWrite からの移行比較

| 観点 | TodoWrite（旧） | Tasks API（現行） |
|------|-----------------|------------------|
| スコープ | 単一セッション | クロスセッション |
| 依存関係 | なし | blockedBy/blocks |
| ストレージ | メモリ内 | ファイルシステム永続化 |
| 協調 | 単一エージェント | 複数セッション同時 |
| ステータス | pending/in_progress/completed | + deleted |
| フック | PreToolUse対応 | バイパス（未修正） |

## バージョン履歴

| バージョン | 日付 | 変更 |
|-----------|------|------|
| v2.1.16 | 2026-01-22 | Tasks API導入（TaskCreate/Update/Get/List） |
| v2.1.19 | 2026-01-23 | デフォルト有効化、`CLAUDE_CODE_ENABLE_TASKS=false`で旧システム復帰 |
| v2.1.20 | 2026-01-27 | `status: "deleted"` によるタスク削除 |
| v2.1.32 | 2026-02頃 | Agent Teams リサーチプレビュー |
| v2.1.33 | 2026-02-06 | TeammateIdle / TaskCompleted フックイベント追加 |
| v2.1.47 | 2026-02-19 | バックグラウンドエージェントのメッセージ履歴トリム |

## 参考資料

- [Claude Code Agent Teams 公式ドキュメント](https://code.claude.com/docs/en/agent-teams) (2026-02)
- [Claude Code タスク管理ガイド — claudefa.st](https://claudefa.st/blog/guide/development/task-management) (2026-02)
- [Tasks API vs TodoWrite — DeepWiki](https://deepwiki.com/FlorianBruniaux/claude-code-ultimate-guide/7.1-tasks-api-vs-todowrite) (2026-02)
- [Task Management 詳細解説 — DeepWiki](https://deepwiki.com/FlorianBruniaux/claude-code-ultimate-guide/8-task-management) (2026-02)
- [TaskCreate システムプロンプト仕様 — Piebald-AI](https://github.com/Piebald-AI/claude-code-system-prompts/blob/main/system-prompts/tool-description-taskcreate.md) (2026-02)
- [Claude Code CHANGELOG — GitHub](https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md)
- [Hook bypass issue #20243 — GitHub](https://github.com/anthropics/claude-code/issues/20243) (2026-01)
- [Headless mode issue #20463 — GitHub](https://github.com/anthropics/claude-code/issues/20463) (2026-01)
