---
name: task
description: プロジェクトタスクの永続管理。「/task」「タスク追加」「タスク一覧」「タスク管理」「タスク確認」「タスク完了」「タスク削除」「タスク更新」「残タスク」「やること確認」と依頼された時に使用。.claude/tasks.jsonにタスクを保存し、セッションをまたいで継続管理。
disable-model-invocation: true
---

# Task Manager

プロジェクト単位でタスクを永続管理するスキル。

## TodoWriteとの違い

| 項目 | TodoWrite | /task |
|------|-----------|-------|
| スコープ | セッション内 | プロジェクト |
| 永続化 | なし | .claude/tasks.json |
| 用途 | 作業中の進捗追跡 | 中長期タスク管理 |

**使い分け**:
- 今すぐ完了する作業 → TodoWrite
- 後で着手/複数セッションにまたがる → /task

## サブコマンド

### add - タスク追加

```
/task add "認証機能を実装する"
/task add "バグ修正" "詳細な説明をここに記載"
/task add "APIのバグ修正" "認証エラーの原因調査" bug
/task add "新機能実装" "" feature
```

**種別（type）**: bug, feature, refactor, review, docs, test

**重要**:
- type は任意。未指定でもタスク追加可能
- タスク内容から type を推測して入力するか、AskUserQuestion で確認すること
- タスク内容が不明確な場合は、ユーザーに質問して詳細を確認し、description として補足すること

### list - 一覧表示

```
/task list                  # 未完了タスク一覧
/task list pending          # 未着手のみ
/task list in_progress      # 進行中のみ
/task list bug              # bug のみ
/task list pending bug      # pending かつ bug
```

### done - 完了

```
/task done a1b2c3d4
```

### update - 更新

```
/task update a1b2c3d4 status in_progress
/task update a1b2c3d4 description "詳細な説明を追加"
```

### delete - 削除

```
/task delete a1b2c3d4
```

### clear - クリア

```
/task clear             # 完了タスクを削除
/task clear all         # 全タスクを削除
```

### show - 詳細表示

```
/task show a1b2c3d4
```

## ワークフロー

### 1. タスク登録時

Claudeがタスクを追加し、IDを記録します。

```bash
~/.dotfiles/.claude-global/skills/task/scripts/task.sh add "タスクタイトル"
~/.dotfiles/.claude-global/skills/task/scripts/task.sh add "タスクタイトル" "詳細な説明"
~/.dotfiles/.claude-global/skills/task/scripts/task.sh add "タスクタイトル" "詳細な説明" bug
```

### 2. 作業開始時

現在のタスク一覧を確認し、着手するタスクをin_progressに変更します。

```bash
~/.dotfiles/.claude-global/skills/task/scripts/task.sh list pending
~/.dotfiles/.claude-global/skills/task/scripts/task.sh update <id> status in_progress
```

### 3. 完了時

```bash
~/.dotfiles/.claude-global/skills/task/scripts/task.sh done <id>
```

## 出力形式（list時）

```
a1b2c3d4 [pending] [bug] 認証機能のバグ修正
b2c3d4e5 [in_progress] [feature] API設計
c3d4e5f6 [completed] [docs] ドキュメント更新
d4e5f6g7 [pending] [none] 種別なしタスク
```

## 注意事項

- IDは8桁16進数（例: a1b2c3d4）
- スクリプトパスは絶対パス: `~/.dotfiles/.claude-global/skills/task/scripts/task.sh`
- タスク内容が不明な場合は、AskUserQuestion ツールを使用して詳細を確認すること
- doneで完了したタスクは`.claude/tasks.json`から`.claude/completed.json`に移動される
- clearは`.claude/completed.json`を空にする。`clear all`は両ファイルを空にする

## 成功基準

1. add後に`.claude/tasks.json`が更新されている
2. listで追加/更新したタスクが表示される（completedフィルタ時は`.claude/completed.json`から読み込み）
3. update/done/deleteの操作が反映される

## 完了チェックリスト

- [ ] タスク操作が`.claude/tasks.json`に永続化された
- [ ] listコマンドで期待する結果が表示される
- [ ] フィルタ（status, type）が正常に機能する
- [ ] 不明なタスクはAskUserQuestionで詳細を確認した
