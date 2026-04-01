---
name: todoist-refine
description: Refines Todoist tasks by clarifying background, intent, and done criteria. Use when「タスクリファイン」「タスク整理」「タスク明確化」。
user-invocable: true
---

# todoist-refine

Todoistタスクに**背景・意図・完了条件**を補完し、着手可能な状態にするワークフロー。

**重要**: このスキルでの「タスク」は全て **Todoist（外部サービス）** のタスクを指す。

## スクリプト

パス: `~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py`

## リファイン対象の判定

以下のいずれかに該当するタスクが対象。明快なタスクはスキップする:

- content が曖昧（動詞+目的語の形になっていない）
- 背景/意図が不明（なぜやるかわからない）
- 完了条件が客観的に判定できない
- 粒度が大きすぎる（1日以内で完了できない）

## ワークフロー

1. `list` でタスク一覧を取得:
   ```bash
   ~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
   ```

2. 各タスクをリファイン対象か判定し、対象タスクごとに以下を整理:

   **content の明確化** — 動詞+目的語に書き換え
   - 例: 「パフォーマンス」→「APIレスポンスを500ms以下に最適化する」
   - 例: 「TDDを使う」→「CLAUDE.mdにTDDルールを追加する」

   **description に3点を補完**:
   ```
   **背景**: なぜ今このタスクが必要か
   **意図**: 達成したい状態・アウトカム
   **完了条件**:
   - [ ] 客観的に判定可能な条件
   ```
   - 「〜を検討」「〜を改善」→ 測定可能な条件に変換
   - 既に description がある場合は既存内容を保持し、不足分のみ追記

   **粒度の分割** — 1日以内で完了できない場合はサブタスク分割を提案（`add --parent-id`）

3. リファイン対象タスクごとに改善提案を AskUserQuestion で確認

4. 承認後に実行:
   - `update --content/--description` で明確化
   - `add --parent-id {ID}` でサブタスク化
   - `update --priority/--due` で優先度・期日の再設定

## 書き込み操作ルール

`update` / `add` は **AskUserQuestion で確認後に実行**する。`list` / `get` は即実行可。
