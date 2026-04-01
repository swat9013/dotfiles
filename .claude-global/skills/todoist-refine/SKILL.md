---
name: todoist-refine
description: Refines Todoist tasks using INVEST criteria (S/T/V focus). Use when「タスクリファイン」「タスク整理」「INVEST」。
user-invocable: true
---

# todoist-refine

Todoistタスクを INVEST 基準（S/T/V 重視）でリファインするワークフロー。

**重要**: このスキルでの「タスク」は全て **Todoist（外部サービス）** のタスクを指す。

## スクリプト

パス: `~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py`

## ワークフロー

1. `list` でタスク一覧を取得:
   ```bash
   ~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
   ```
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

## 書き込み操作ルール

`update` / `add` は **AskUserQuestion で確認後に実行**する。`list` / `get` は即実行可。
