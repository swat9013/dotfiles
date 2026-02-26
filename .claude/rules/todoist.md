---
paths: .claude-global/skills/todoist/**
---

# Todoist スキルルール

## リファインメント基準

- INVEST全6原則は参照用。個人タスク管理の主軸は **S**(mall)・**T**(estable)・**V**(aluable) の3項目
- I(ndependent)・N(egotiable)・E(stimable) は稀なケースのみ適用

## DoD（Definition of Done）

- 全タスクに必須ではない。完了条件が曖昧なタスクのみ限定適用
- 「CLAUDE.mdにルール追加」のような明快なタスクにはDoD不要

## git worktree 対応

- プロジェクト名解決は `git rev-parse --git-common-dir` で親リポジトリ名を取得
- `--show-toplevel` は worktree 名を返すため使用しない
