---
name: todoist-refine
description: Refines Todoist tasks by clarifying background, intent, and done criteria. Use when「タスクリファイン」「タスク整理」「タスク明確化」。
user-invocable: true
model: sonnet
effort: medium
---

# todoist-refine

Todoistタスクに**背景・意図・完了条件**を補完し、着手可能な状態にするワークフロー。

**重要**: このスキルでの「タスク」は全て **Todoist（外部サービス）** のタスクを指す。

## スクリプト

パス: `~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py`

## リファインの原則

- **推測は仮説**: Claudeの推測を「補完案」として提示し、ユーザーに確認する。合っていればそのまま採用、違えば深堀り質問で掘り下げる
- **対話で引き出す**: 推測だけで埋めて終わりにしない。確認プロセスを通じてユーザーの思考を外部化する

## ワークフロー

### Phase 0: タスク取得

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
```

### Phase 1: 曖昧度スクリーニング

各タスクを以下の3段階に分類する。明快なタスクはスキップ:

| 段階 | 条件 | 次のステップ |
|------|------|-------------|
| SKIP | 動詞+具体的目的語、なぜやるかが自明 | リファイン不要 |
| LIGHT | actionは明確だが背景or完了条件が不明 | Phase 2→確認OKならPhase 4 |
| DEEP | contentが曖昧、または粒度が大きすぎる（1日超） | Phase 2→Phase 3→Phase 4 |

### Phase 2: 推測提示+確認

対象タスクごとに推測で補完案を作り、AskUserQuestion で提示する:

**content の明確化** — 動詞+目的語に書き換え
- 例: 「パフォーマンス」→「APIレスポンスを500ms以下に最適化する」

**description の3点補完案**:
```
**背景**: なぜ今このタスクが必要か（推測）
**意図**: 達成したい状態・アウトカム（推測）
**完了条件**:
- [ ] 客観的に判定可能な条件（推測）
```

AskUserQuestion で「この解釈で合っていますか？」と確認:
- 合っている → Phase 4へ
- 違う箇所がある → Phase 3へ（不一致箇所を深堀り）

既に description がある場合は既存内容を保持し、不足分のみ補完案を作る。

### Phase 3: 深堀り質問

Phase 2で不一致だった箇所に対し、質問カタログから適切な質問を選んで AskUserQuestion で掘り下げる。

質問カタログ: [references/question-catalog.md](references/question-catalog.md)

**選択の指針**:
- 背景が違う → WHY系（「なぜ今これが必要？」「やらないとどうなる？」）
- contentが曖昧 → WHAT系（「〇〇とは具体的に何を指す？」）
- 完了条件が不明 → DONE系（「どうなれば完了？」「何を確認すべき？」）
- 粒度が大きい → SIZE系（「1日で終わる？最初の一歩は？」）

### Phase 4: 補完・更新

Phase 2の確認済み推測 + Phase 3の回答を統合し、AskUserQuestion で最終確認後に実行:
- `update --content` で content 明確化
- `update --description` で description 補完
- `add --parent-id {ID}` でサブタスク化（粒度分割あり）
- `update --priority/--due` で優先度・期日の再設定

## 書き込み操作ルール

`update` / `add` は **AskUserQuestion で確認後に実行**する。`list` / `get` は即実行可。
