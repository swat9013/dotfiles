---
name: commit
description: >-
  Commits staged changes and updates corresponding Todoist tasks.
  Use when 「/commit」「コミットして」「コミット作成」「変更をコミット」.
user-invocable: true
disable-model-invocation: true
---
# commit

ステージ済み変更のコミットとTodoistタスク連携を行うワークフロー。

## ワークフロー

### Step 1: 変更確認

```bash
git status
git diff --cached --stat
```

- ステージ済み変更がある → Step 2 へ
- ステージ済み変更がない → `git diff --stat` で未ステージ変更を確認
  - 未ステージ変更あり → AskUserQuestion でステージ対象を提案（ファイル一覧を提示）
  - 変更なし → 「コミット対象の変更がありません」と伝えて終了

### Step 2: コミットメッセージ生成

```bash
git log --oneline -5
git diff --cached
```

1. 直近のコミット履歴からプロジェクトの命名慣習を把握
2. diff内容を分析し、慣習に合わせたコミットメッセージを生成
3. AskUserQuestion で提案（編集可能にする）

**メッセージ原則**:

- Why（変更の動機と背景）を表現する
- 1行目は簡潔に（50文字目安）
- 必要に応じて本文で補足

### Step 3: コミット実行

```bash
git commit -m "$(cat <<'EOF'
{承認されたメッセージ}

EOF
)"
```

コミット後、`git status` で結果を確認。

### Step 4: ドキュメント更新チェック

コミット内容を分析し、以下のドキュメントに更新が必要か判定する:

| 対象 | 更新が必要なケース |
|------|------------------|
| `CLAUDE.md` | アーキテクチャ変更、新ツール/コマンド追加、Gotchas追加 |
| `README.md` | インストール手順変更、セットアップ手順変更 |
| `docs/` 配下 | 既存ドキュメントの対象領域に影響する変更 |

1. コミットのdiff内容から更新候補を特定
2. 更新が必要な場合 → AskUserQuestion で対象と変更内容を提案
3. 承認された更新を適用し、追加コミットを作成（`git add <files> && git commit`）
4. 更新不要と判定した場合 → スキップして次のステップへ

### Step 5: Todoist連携

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
```

- `Project not found` エラー → 「Todoistプロジェクト未初期化のため連携をスキップします」と通知して終了
- タスク一覧取得成功 → 以下を実行:

**完了タスク検出**:

1. コミット内容とタスク一覧を照合し、完了候補を特定
2. AskUserQuestion で完了対象を確認（複数選択可）
3. 承認されたタスクを close:

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py close TASK_ID
```

**残タスク追加（任意）**:

1. AskUserQuestion で「新たに追加するタスクはありますか？」と確認
2. 追加ありの場合、内容を確認後に実行:

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py add [opts] TITLE
```

## エラー対応

| エラー               | 対応                                                                           |
| -------------------- | ------------------------------------------------------------------------------ |
| pre-commit hook 失敗 | 問題を修正し、再ステージしてから**新しいコミットを作成**（--amend 禁止） |
| Todoist認証エラー    | `~/.config/todoist/config.json` の確認を案内                                 |
| コミット対象なし     | 終了（空コミット禁止）                                                         |
