---
name: wtp
description: >-
  タスク内容からブランチ名を生成し、wtp add -b でワークツリーを作成する。
  「/wtp」「wtp add」「ワークツリー作成」「worktree作成」「worktree追加」と依頼された時に使用。
user-invocable: true
disable-model-invocation: true
argument-hint: "[タスク説明（省略時はTodoistから取得）]"
---

# wtp

タスク内容から `type/slug` 形式のブランチ名を生成し、`wtp add -b` でワークツリーを作成する。

## ワークフロー

### Step 1: タスク内容の取得

**引数あり** (`$ARGUMENTS` が空でない場合):
- 引数をタスク内容として使用

**引数なし**:
- Todoistからタスク一覧を取得:

```bash
~/.dotfiles/.claude-global/skills/todoist/scripts/todoist.py list
```

- `Project not found` → AskUserQuestion でタスク内容を直接入力してもらう
- タスク一覧表示 → AskUserQuestion で対象タスクを選択してもらう

### Step 2: ブランチ名の生成

タスク内容から `type/slug` 形式のブランチ名を生成する。

**type の判定基準**:

| type | 内容 |
|------|------|
| feat | 新機能追加 |
| fix | バグ修正 |
| refactor | リファクタリング |
| docs | ドキュメント変更 |
| chore | 設定・依存関係・CI等 |
| test | テスト追加・修正 |

**slug の規則**:
- 英語 kebab-case、3〜5語程度
- 具体的かつ簡潔に（例: `add-user-auth`, `fix-login-redirect`）

### Step 3: 確認と実行

1. 生成したブランチ名を AskUserQuestion で提示（編集可能にする）
2. 承認後に実行:

```bash
wtp add -b {ブランチ名}
```

3. 実行結果を表示して完了
