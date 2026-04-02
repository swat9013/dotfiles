# Tasks API リファレンス

## TOC
1. [4ツールのパラメータ](#4ツールのパラメータ)
2. [ステータスワークフロー](#ステータスワークフロー)
3. [依存関係管理（DAG）](#依存関係管理dag)
4. [クロスセッション協調](#クロスセッション協調)
5. [Agent Teams](#agent-teams)
6. [既知バグ・制限](#既知バグ制限)
7. [TodoWrite との比較](#todowrite-との比較)
8. [使用判断基準](#使用判断基準)

導入バージョン: v2.1.16（2026-01-22）。`claude -p` ではデフォルト無効。

---

## 4ツールのパラメータ

### TaskCreate

| パラメータ | 必須 | 説明 |
|-----------|------|------|
| `subject` | ✓ | 命令形タイトル（"Fix authentication bug"）。TaskList で見えるため重要情報を集約 |
| `description` | - | 詳細な要件・受入条件（TaskList では取得できない） |
| `activeForm` | 推奨 | `in_progress` 時のスピナー表示（現在進行形: "Fixing authentication bug"） |
| `metadata` | - | 任意の key-value（priority, files 等） |

全タスクは `pending` で作成される。

### TaskUpdate

| パラメータ | 必須 | 型 | 説明 |
|-----------|------|-----|------|
| `taskId` | ✓ | string | 対象タスク ID |
| `status` | - | enum | `pending` / `in_progress` / `completed` / `deleted` |
| `subject` | - | string | タイトル変更 |
| `description` | - | string | 説明変更 |
| `activeForm` | - | string | スピナーテキスト変更 |
| `owner` | - | string | 所有者（エージェント名） |
| `metadata` | - | object | key-value マージ（key を null で削除） |
| `addBlocks` | - | array | このタスク完了まで開始不可なタスク ID |
| `addBlockedBy` | - | array | 先に完了が必要なタスク ID |

**TaskUpdate 前に TaskGet**: 最新状態を確認してから更新（並行アクセス対策）。

### TaskGet / TaskList

- **TaskGet**: `taskId` 指定。返却: `id`, `subject`, `status`, `owner`, `blockedBy`, `description`, `metadata`, `activeForm`, タイムスタンプ
- **TaskList**: パラメータなし。返却: `id`, `subject`, `status`, `owner`, `blockedBy` のみ

`description`, `metadata`, `activeForm` は TaskList で取得不可 → TaskGet が必要。
N件詳細取得 = 1（TaskList）+ N（TaskGet）= N+1 API コール。対策: `subject` に重要情報を集約して TaskGet を最小化。

---

## ステータスワークフロー

```
pending → in_progress → completed
                      ↘ deleted
```

| ルール | 理由 |
|--------|------|
| 作業開始前に `in_progress` へ | スピナー表示が開始される |
| 実際の完了確認後に `completed` | 部分完了でのマーク禁止 |
| エラー時はステータス維持 | 状態を保持して再試行可能にする |
| `deleted` で論理削除 | 物理削除は不可 |

---

## 依存関係管理（DAG）

DAG（有向非循環グラフ）構造。依存関係の宣言のみで物理的強制はない。

```javascript
TaskUpdate({ taskId: "3", addBlockedBy: ["1", "2"] })  // Task 3 は Task 1, 2 の完了が前提
TaskUpdate({ taskId: "1", addBlocks: ["3"] })           // 逆方向の宣言も可能
```

- `addBlockedBy` / `addBlocks` は配列への**追加**（既存依存関係は維持）
- 依存タスクが `completed` になると自動的にブロック解除
- 循環依存の自動検出なし（設計時に確認必要）

**Wave分解パターン**:
```
Wave 1（並列）: DBスキーマ設計 | 認証設定
Wave 2（Wave 1依存）: API実装
Wave 3（Wave 2依存）: フロントエンド
Wave 4（独立）: テスト | ドキュメント
```

---

## クロスセッション協調

```bash
CLAUDE_CODE_TASK_LIST_ID="my-project" claude          # 複数セッションでタスクリストを共有
CLAUDE_CODE_ENABLE_TASKS=false claude                  # タスク機能を無効化
CLAUDE_CODE_ENABLE_TASKS=true claude -p "..."          # ヘッドレスモードで有効化
CLAUDE_CODE_TEAM_NAME="my-team" claude                 # チーム名指定
CLAUDE_CODE_DISABLE_1M_CONTEXT=true claude             # 1M context ウィンドウを無効化（v2.1.50〜）
CLAUDE_CODE_SIMPLE=true claude                         # スキル・エージェント・フックを除いたシンプルモード（v2.1.50〜）
```

**ストレージパス**（プロジェクトではなくホームディレクトリに保存）:
```
~/.claude/tasks/{task-list-id}/tasks.json
~/.claude/teams/{team-name}/config.json
```

並行アクセスの競合: 複数セッション同時更新は Last-Write-Wins。ロック機構はローカルの `.lock` ファイルのみ。

---

## Agent Teams

有効化: `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1 claude`（実験的、v2.1.32〜）

| 役割 | 説明 |
|------|------|
| Team Lead | タスク作成・割り当て・調整 |
| Teammates | 独立コンテキストでタスク実行。チームメイト同士が直接メッセージ可 |

**使用ケース比較**:

| ケース | Agent Teams | Subagents |
|--------|:-----------:|:---------:|
| 並列レビュー（セキュリティ/パフォーマンス/テスト） | 推奨 | - |
| 複数仮説の並列検証・討論 | 推奨 | - |
| 独立モジュールの並列開発 | 推奨 | - |
| 短期集中タスク（ファイル1〜3個） | - | 推奨 |
| 依存関係の多い順序実行 | NG | 推奨 |
| 同一ファイルの並列編集 | NG（競合） | NG |

**Agent Teams の制限**:
- セッション再開（`/resume`, `/rewind`）でチームメイトは復元されない
- ネストしたチーム不可（リードのみがチームを作成可能）
- 1セッション・1チーム、トークンコストが高い（各メンバーが独立したコンテキスト）

**関連フックイベント**:

| イベント | 用途 |
|---------|------|
| `TaskCompleted` | exit 2 で完了ブロック（品質ゲート） |
| `TeammateIdle` | アイドル化直前に追加作業を指示 |
| `SubagentStop` | 出力品質検証 |

**UI ショートカット**: `Ctrl+T` タスクリスト表示/非表示、`Shift+Down` チームメート間サイクル、`Ctrl+F` バックグラウンドエージェント終了（v2.1.49〜）

---

## 既知バグ・制限

| バグ/制限 | 詳細 |
|----------|------|
| フックバイパス（Issue #20243） | TaskCreate/Update/Get/List は PreToolUse/PostToolUse をバイパス。TaskAPI 操作を監査ログに記録できない |
| ヘッドレスモードでのデフォルト無効（Issue #20463） | `claude -p` では Task* ツールがデフォルト無効。`CLAUDE_CODE_ENABLE_TASKS=true` で明示的に有効化必須 |
| TaskList フィールド制限 | `description`, `metadata`, `activeForm` は TaskList で返却されない。必要な場合は TaskGet を N 回呼ぶ必要がある |

---

## TodoWrite との比較

| 観点 | TodoWrite（旧） | Tasks API（現行） |
|------|:--------------:|:---------------:|
| スコープ | 単一セッション | クロスセッション |
| 依存関係 | なし | blockedBy/blocks |
| ストレージ | メモリ内 | ファイルシステム永続化 |
| 協調 | 単一エージェント | 複数セッション同時 |
| ステータス | pending/in_progress/completed | + deleted |
| フック対応 | PreToolUse 対応 | バイパス（Issue #20243、未修正） |
| TaskOutput | 利用可 | **非推奨**（v2.1.83）→ `Read` で出力ファイル取得 |
| 復旧 | セッション終了で消失 | tasks.json から復旧可 |

---

## 使用判断基準

**使うべき場面**: 3ステップ以上の複合タスク、Plan Mode での作業計画、複数セッション・エージェント間の協調、ユーザーが明示的にタスクリストを要求

**使うべきでない場面**: 単一ファイルの変更、3ステップ未満の直線作業、会話・情報提供のみ、`claude -p` ヘッドレス実行（`CLAUDE_CODE_ENABLE_TASKS=true` が必要）

**粒度の目安**:

| 粒度 | 例 | 評価 |
|------|-----|------|
| 粗すぎる | 「認証システム全体を作れ」 | NG |
| 適切 | 「JWTトークン検証ミドルウェアを実装」 | OK |
| 細かすぎる | 「importステートメントを追加」 | NG |

推奨タスク数: 8〜15タスク（大規模機能）。1エージェント・1ファイルオーナーシップで競合を防ぐ。
