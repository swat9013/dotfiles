---
name: todoist
description: |
  プロジェクトのタスク管理ワークフロー。リポジトリに対応するTodoistプロジェクトでタスクの初期化・一覧・追加・更新・リファインメントを行う。
  「Todoist」「タスク追加」「タスク一覧」「タスク完了」「タスク削除」「todo確認」「やること確認」「次のタスク」「タスク整理」「タスクリファイン」「タスク管理」「todo整理」「残タスク」と依頼された時に参照する。
user-invocable: true
disable-model-invocation: true
---

# todoist

`projects/<basename>` でリポジトリ単位のタスク管理を行うワークフロー。Python SDK（REST API v1）を使用。

## スクリプト

パス: `~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py`

PEP 723 インラインメタデータで依存宣言。`uv run` で自動インストール・実行される（pip install 不要）。

### サブコマンド

| コマンド | 説明 |
|---------|------|
| `init` | `projects/<basename>` サブプロジェクト作成（冪等） |
| `list` | プロジェクトのタスク一覧（優先度→期日ソート） |
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
2. 以下の基準でリファイン対象を特定:
   - タスク名が**動詞+目的語**の形になっていない（曖昧）
   - 完了条件が不明確（「〜を検討」「〜を改善」など）
   - 1日で完了できない規模（分割が必要）
   - **方針・心がけ**になっている（「〜を使う」「〜に気をつける」→ 具体的な設定タスクに変換。例: 「TDDを使う」→「CLAUDE.mdにTDDルールを追加する」）
3. 対象タスクごとに提案:
   - **分割**: `add --parent-id {ID}` でサブタスク化、または元タスク `close` + 新タスク `add`
   - **具体化**: `update --content/--description` で「動詞+目的語+完了条件」形式に
   - **優先度・期日の再設定**: `update --priority/--due` で調整
4. 各変更を AskUserQuestion で個別に確認後実行

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

`init` / `add` / `update` / `close` / `delete` は **AskUserQuestion で確認後に実行**する。`list` のみ即実行可。

## エラー対応

| エラー | 原因 | 対応 |
|--------|------|------|
| `Authentication failed` | トークン無効 | `~/.config/todoist/config.json` の `token` を確認 |
| `Project not found` | 未初期化 | `todoist.py init` を実行 |
| `Task not found` | ID不正 | `todoist.py list` で正しいIDを確認 |
| `API request failed` | ネットワーク/API障害 | リトライ |
