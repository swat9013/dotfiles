---
paths: .claude-global/skills/todoist/**
---

# Todoist スキルルール

## リファインメント基準

- リファインの軸は**背景・意図・完了条件**の3点補完
- 明快なタスク（「CLAUDE.mdにルール追加」等）はリファイン不要。スキップする
- 粒度が大きいタスク（1日超）はリファイン時に分割提案を併せて行う

## git worktree 対応

- プロジェクト名解決は `git rev-parse --git-common-dir` で親リポジトリ名を取得
- `--show-toplevel` は worktree 名を返すため使用しない
