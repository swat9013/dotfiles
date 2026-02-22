---
name: todoist
description: Todoist CLIによるタスク管理ガイド。タスク一覧表示、追加、完了、削除の操作方法を含む。「Todoist」「タスク追加」「タスク一覧」「タスク完了」「タスク削除」「todo確認」「やること確認」と依頼された時に参照する。
user-invocable: false
---

# todoist

Todoist CLIを使ってタスクを管理するための操作スキル。タスクの閲覧・追加・完了・削除を扱う。

## 前提条件

```bash
which todoist || brew install todoist-cli
```

APIトークンの設定が必要: Todoist Web の Settings > Integrations > Developer からトークンを取得し、初回 `todoist sync` 実行時に入力する。

## 基本コマンド

### タスク一覧

```bash
todoist list                             # タスク一覧（デフォルトフィルター）
todoist l                                # エイリアス
todoist --csv list                       # CSV形式出力（パース向け）
# CSV列: ID,Priority,DueDate,Project,Labels,Content
# DueDate形式: YY/MM/DD(曜日) HH:MM（未設定は空）
todoist list -f "today"                  # 今日のタスク
todoist list -f "p1"                     # 優先度1（最高）のみ
todoist list -f "#ProjectName"           # プロジェクト絞り込み
todoist list -f "@label"                 # ラベル絞り込み
```

### タスク追加

```bash
todoist add "タスク名"                                      # 最小構成
todoist a "タスク名"                                        # エイリアス
todoist add --priority 1 "タスク名"                         # 優先度指定（1=最高, 4=最低）
todoist add --date "tomorrow" "タスク名"                    # 期日指定
todoist add --project-name "ProjectName" "タスク名"         # プロジェクト指定
```

> **注意**: オプションはタスク名（コンテンツ）より**前**に置く。タスク名の後にオプションを置くと引数エラーになる。

オプション組み合わせ例:

```bash
todoist add --priority 1 --project-name "ProjectName" --date "tomorrow" "タスク名"
```

### タスク完了

```bash
todoist close <ID>                       # タスクを完了にする
todoist c <ID>                           # エイリアス
```

### タスク削除

```bash
todoist delete <ID>                      # タスクを削除する
todoist d <ID>                           # エイリアス
```

### クイック追加

```bash
todoist quick "タスク名 tomorrow p1"    # 自然言語で日付・優先度を自動パース
todoist q "タスク名 tomorrow p1"        # エイリアス
```

### 同期

```bash
todoist sync                             # 手動同期（Todoistサーバーと同期）
todoist s                                # エイリアス
```

> **注意**: `add` / `close` / `delete` の実行後、`todoist list` への反映には `todoist sync` が必要。

IDの取得: `todoist list` の出力先頭列がタスクID。

## プロジェクトマッピング

カレントディレクトリの `basename` をプロジェクト名として使う規約:

```bash
PROJECT=$(basename "$PWD")
todoist add --project-name "$PROJECT" "タスク名"
```

プロジェクト名が一致しない場合は `--project-name` で明示指定する。存在しないプロジェクト名を指定するとエラー（`Error: Did not find a project named 'xxx'`、終了コード1）になるため、事前に確認する:

```bash
todoist projects                         # プロジェクト一覧を確認
todoist add-project "ProjectName"        # 存在しない場合は新規作成
```

## ワークフロー

操作種別によって実行の判断を変える。

### 読み取り操作（即実行）

`list` は確認不要で実行する:

```bash
todoist list
todoist list -f "today"
todoist --csv list
```

### 書き込み操作（確認後実行）

`add` / `close` / `delete` は AskUserQuestion で実行内容を提示し、ユーザー確認後に実行する。

**例: タスク追加**

1. 追加内容を提示:
   ```
   以下のタスクを追加します:
   - 名前: "設計書レビュー"
   - プロジェクト: ProjectName
   - 優先度: 1
   - 期日: tomorrow
   実行してよいですか?
   ```
2. ユーザーが承認したら実行:
   ```bash
   todoist add --priority 1 --project-name "ProjectName" --date "tomorrow" "設計書レビュー"
   ```

**例: タスク完了**

1. 対象タスクを提示（IDと名前）:
   ```
   タスク ID=12345678「設計書レビュー」を完了にします。よいですか?
   ```
2. 承認後:
   ```bash
   todoist close 12345678
   ```

**例: タスク削除**

完了との区別を明示する（削除は取り消し不可）:
```
タスク ID=12345678「設計書レビュー」を削除します（取り消し不可）。よいですか?
```

## よくある操作

| 目的 | コマンド |
|------|----------|
| 今日のタスク一覧 | `todoist list -f "today"` |
| 優先度の高いタスク一覧 | `todoist list -f "p1"` |
| プロジェクトのタスク一覧 | `todoist list -f "#ProjectName"` |
| CSVで取得してパース | `todoist --csv list` |
| クイック追加（自然言語） | `todoist quick "タスク名 tomorrow p1"` |
| タスクを完了にする | `todoist close <ID>` |
| タスクを削除する | `todoist delete <ID>` |
| Todoistと同期 | `todoist sync` |

詳細なフィルター構文・フラグは `references/commands.md` を参照。

## 成功基準

1. `todoist list` が正常終了してタスク一覧が表示される
2. 書き込み操作はユーザー確認後に実行されている
3. 追加・完了・削除後に `todoist list` で結果が反映されている
4. エラー時は原因とリカバリー手順を提示している

## 完了チェックリスト

- [ ] 操作が正常終了した（エラーなし）
- [ ] 書き込み操作はユーザーに確認を取った
- [ ] 実行結果を `todoist list` で確認した
- [ ] 必要に応じて `todoist sync` で同期した
