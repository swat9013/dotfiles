---
name: todoist
description: |
  プロジェクトのタスク管理ワークフロー。リポジトリに対応するTodoistプロジェクトでタスクの初期化・一覧・追加・更新・リファインメントを行う。
  「Todoist」「タスク追加」「タスク一覧」「タスク完了」「タスク削除」「todo確認」「やること確認」「次のタスク」「タスク整理」「タスクリファイン」「タスク管理」「todo整理」「残タスク」「タスクURL」「タスクリンク」「todoist.com/app/task」と依頼された時に参照する。
user-invocable: true
disable-model-invocation: true
---

# todoist

`projects/<basename>` でリポジトリ単位のタスク管理を行うワークフロー。Python SDK（REST API v1）を使用。

**重要**: このスキルでの「タスク」は全て **Todoist（外部サービス）** のタスクを指す。Claude内部のTaskCreate/TaskUpdate/TaskListツールは使用しない。タスクの追加・一覧・更新・完了・削除は全て `todoist.py` スクリプト経由で実行する。

## スクリプト

パス: `~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py`

PEP 723 インラインメタデータで依存宣言。`uv run` で自動インストール・実行される（pip install 不要）。

### サブコマンド

| コマンド | 説明 |
|---------|------|
| `init` | `projects/<basename>` サブプロジェクト作成（冪等） |
| `list` | プロジェクトのタスク一覧（優先度→期日ソート） |
| `get TASK_URL` | タスク詳細取得（URLまたはID） |
| `add [opts] TITLE` | タスク追加 |
| `update TASK_ID [opts]` | タスク更新 |
| `close TASK_ID` | タスク完了 |
| `delete TASK_ID` | タスク削除（取り消し不可） |

### add オプション

| オプション | 説明 |
|-----------|------|
| `--priority N` | 優先度（1=最高, 4=最低） |
| `--due DATE` | 期日（`tomorrow`, `next Friday`, `YYYY-MM-DD` 等） |
| `--labels L1,L2` | カンマ区切りラベル |
| `--description DESC` | 説明文 |
| `--parent-id ID` | 親タスクID（サブタスク作成用） |

### update オプション

`--content TEXT`, `--description TEXT`, `--priority N`, `--due DATE`, `--labels L1,L2`

## ワークフロー

### 初回セットアップ

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py init
```

### タスク確認（即実行）

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
```

### 次タスク選定（Claude判断）

1. `list` 結果を取得
2. 優先度(p1→p4) → 期日(近い順) → 期日なし後回し で順位付け
3. ユーザーに提案し、AskUserQuestion で承認を得る

### タスク着手（URLから即実行）

1. ユーザーがTodoistリンクまたはタスクIDを共有
2. タスク詳細を取得:
   ```bash
   ~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py get URL_OR_ID
   ```
3. 出力からタスク内容・description・サブタスクを確認し、作業を開始

### タスク追加（確認後実行）

1. AskUserQuestion で追加内容を確認:
   ```
   以下のタスクを追加します:
   - 名前: "設計書レビュー"
   - 優先度: 1
   - 期日: tomorrow
   実行してよいですか?
   ```
2. 承認後に実行

### タスクリファインメント（対話的）

1. `list` でタスク一覧を取得
2. 各タスクを以下の基準で評価し、不適合項目があればリファイン対象:
   - **content が動詞+目的語か** — 曖昧なら書き換え（例: 「パフォーマンス」→「APIレスポンスを500ms以下に最適化する」）
   - **Small**: 1日以内で完了できる粒度か — 大きければサブタスク分割（`add --parent-id`）
   - **Testable**: 完了を客観的に判定できるか — 曖昧なら description に完了の定義を追記
   - **Valuable**: 完了時に具体的な価値を届けるか — 方針・心がけ（「〜を使う」「〜に気をつける」）は具体的タスクに変換（例: 「TDDを使う」→「CLAUDE.mdにTDDルールを追加する」）
   - 必要に応じて: **Independent**（依存関係の解消）、**Negotiable**（手段→目的ベースに）、**Estimable**（不明点を調査タスクに切り出し）
3. 完了条件が曖昧なタスクは、description に完了の定義を追記:
   ```
   ## 完了の定義
   - [ ] 具体的な完了条件
   ```
   - 「〜を検討」「〜を改善」→ 測定可能な条件に変換（例: 「パフォーマンスを改善」→「レスポンスタイムが500ms以下」）
4. 作業開始に必要な情報が不足しているタスクは、description に追記:
   - 関連ファイルパス・コード箇所 / 参考URL / 前提・制約条件
5. 不適合タスクごとに改善提案を AskUserQuestion で確認
6. 承認後に実行:
   - **具体化**: `update --content/--description` で改善
   - **分割**: `add --parent-id {ID}` でサブタスク化
   - **優先度・期日の再設定**: `update --priority/--due` で調整

### 完了/削除（確認後実行）

完了:
```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py close TASK_ID
```

削除（確認時に「取り消し不可」を明示）:
```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py delete TASK_ID
```

## 書き込み操作ルール

`init` / `add` / `update` / `close` / `delete` は **AskUserQuestion で確認後に実行**する。`list` / `get` は即実行可。

## エラー対応

| エラー | 原因 | 対応 |
|--------|------|------|
| `Authentication failed` | トークン無効 | `~/.config/todoist/config.json` の `token` を確認 |
| `Project not found` | 未初期化 | `todoist.py init` を実行 |
| `Task not found` | ID不正 | `todoist.py list` で正しいIDを確認 |
| `API request failed` | ネットワーク/API障害 | リトライ |
