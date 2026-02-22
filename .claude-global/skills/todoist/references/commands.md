# Todoist CLI コマンドリファレンス

## TOC

1. [グローバルオプション](#グローバルオプション)
2. [list / l — タスク一覧](#list--l--タスク一覧)
3. [add / a — タスク追加](#add--a--タスク追加)
4. [modify / m — タスク編集](#modify--m--タスク編集)
5. [その他コマンド](#その他コマンド)
6. [フィルター構文](#フィルター構文)
7. [ID の取得方法](#id-の取得方法)

バージョン: **v0.23.0** (sachaos/todoist)

---

## グローバルオプション

コマンドの**前**に置く: `todoist [global options] command`

| オプション | 説明 |
|------------|------|
| `--header` | ヘッダー付き出力 |
| `--color` | カラー出力 |
| `--csv` | CSV形式出力 |
| `--debug` | デバッグログ出力 |
| `--namespace` | 親タスクをnamespace形式で表示 |
| `--indent` | 子タスクをインデント表示 |
| `--project-namespace` | 親プロジェクトをnamespace形式で表示 |

```bash
todoist --csv list
todoist --header --csv list -f "today"
```

---

## list / l — タスク一覧

```bash
todoist list [options]
todoist l [options]
```

| オプション | 説明 |
|------------|------|
| `--filter value, -f value` | フィルター式 |
| `--priority, -p` | 優先度でソート |

出力形式: `ID  priority  date  #project  @labels  content`

---

## add / a — タスク追加

```bash
todoist add [options] <content>
```

| オプション | 説明 |
|------------|------|
| `--priority value, -p value` | 優先度 1〜4（1=最高, 4=最低, デフォルト: 4） |
| `--label-names value, -L value` | ラベル名（カンマ区切りで複数指定可） |
| `--project-name value, -N value` | プロジェクト名 |
| `--project-id value, -P value` | プロジェクトID |
| `--date value, -d value` | 期日（`today`, `tomorrow`, `next monday`, `in 3 days`, `next month 18:00`） |
| `--reminder, -r` | リマインダー設定（プレミアムユーザーのみ） |

```bash
todoist add -p 1 -d tomorrow -N "仕事" "レポート提出"
todoist add -L "errands,home" -d "next monday" "買い物"
```

> **注意**: オプションはコンテンツ（タスク名）より**前**に置く。タスク名の後にオプションを置くと引数エラーになる。

---

## modify / m — タスク編集

```bash
todoist modify [options] <ID>
```

| オプション | 説明 |
|------------|------|
| `--content value, -c value` | 内容 |
| `--priority value, -p value` | 優先度 1〜4 |
| `--label-names value, -L value` | ラベル名（カンマ区切り） |
| `--project-name value, -N value` | プロジェクト名 |
| `--project-id value, -P value` | プロジェクトID |
| `--date value, -d value` | 期日 |

---

## その他コマンド

| コマンド | エイリアス | 説明 |
|----------|-----------|------|
| `show <ID>` | — | タスク詳細表示（`-o` でURL自動オープン） |
| `close <ID>` | `c` | タスク完了 |
| `delete <ID>` | `d` | タスク削除 |
| `quick "text"` | `q` | 自然言語でクイック追加 |
| `sync` | `s` | キャッシュ同期 |
| `projects` | — | プロジェクト一覧 |
| `labels` | — | ラベル一覧 |
| `add-project "name"` | `ap` | プロジェクト追加 |
| `completed-list` | `c-l`, `cl` | 完了済みタスク一覧（プレミアムのみ） |
| `karma` | — | カルマ表示 |

---

## フィルター構文

`todoist list -f "FILTER"` または `todoist l -f "FILTER"`

### 日付

| フィルター | 意味 |
|------------|------|
| `today` | 今日のタスク |
| `overdue` | 期限切れ |
| `tomorrow` | 明日 |
| `7 days` | 7日以内 |
| `no date` | 日付なし |

### 優先度・プロジェクト・ラベル

| フィルター | 意味 |
|------------|------|
| `p1` / `p2` / `p3` / `p4` | 優先度指定 |
| `#project_name` | プロジェクト指定 |
| `@label_name` | ラベル指定 |

### 論理演算子

| 演算子 | 意味 |
|--------|------|
| `&` | AND |
| `\|` | OR |
| `( )` | グルーピング |

```bash
todoist list -f "today"
todoist list -f "overdue | today"
todoist list -f "p1 & #仕事"
todoist list -f "7 days & @重要"
todoist list -f "(today | overdue) & p1"
```

---

## ID の取得方法

```bash
# 1列目がID
todoist list

# CSV形式で取得（1列目がID）
todoist --csv list

# フィルター適用時
todoist --csv list -f "today"
```

> **注意**: `--json` オプションは v0.23.0 では非対応。構造化出力は `--csv` を使用する。
