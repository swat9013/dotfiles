# サービスコマンドリファレンス

## TOC

1. [Gmail](#1-gmail)
2. [Calendar](#2-calendar)
3. [Drive・Docs・Sheets・Slides](#3-drivedocssheetsslides)
4. [Tasks](#4-tasks)
5. [Contacts・People](#5-contactspeople)
6. [Chat](#6-chat)
7. [その他サービス](#7-その他サービス)
8. [共通フラグ](#8-共通フラグ)

## 1. Gmail

### サブコマンド

| コマンド | 説明 |
|---------|------|
| `gmail search <query>` | スレッド検索 |
| `gmail messages search <query>` | メッセージ単位検索 |
| `gmail send` | メール送信 |
| `gmail thread get <id>` | スレッド詳細取得 |
| `gmail drafts create` | 下書き作成 |
| `gmail drafts send <id>` | 下書き送信 |
| `gmail labels list` | ラベル一覧 |
| `gmail labels create <name>` | ラベル作成 |
| `gmail labels modify <threadIds>` | ラベル変更（バッチ） |
| `gmail filters list` | フィルタ一覧 |
| `gmail settings vacation enable` | 不在応答設定 |
| `gmail watch` | Pub/Sub プッシュ通知 |

### 主要フラグ

| フラグ | 説明 |
|--------|------|
| `--max <n>` | 最大件数 |
| `--to <email>` | 宛先 |
| `--cc <email>` | CC |
| `--subject <text>` | 件名 |
| `--body <text>` | 本文 |
| `--body-html <html>` | HTML本文 |
| `--body-file <path>` | ファイルから本文 |
| `--attach <path>` | 添付ファイル |

### 使用例

```bash
# 未読メール検索
gog gmail search "is:unread newer_than:7d" --max 10

# メール送信（添付付き）
gog gmail send --to user@example.com --subject "Report" --body-file report.txt --attach data.csv

# 古いメールをアーカイブ（JSON + jq）
gog --json gmail search 'older_than:1y' --max 200 \
  | jq -r '.threads[].id' \
  | xargs -n 50 gog gmail labels modify --remove INBOX
```

## 2. Calendar

### サブコマンド

| コマンド | 説明 |
|---------|------|
| `calendar events <calId>` | イベント一覧 |
| `calendar create <calId>` | イベント作成 |
| `calendar respond <eventId>` | 出欠回答 |
| `calendar freebusy` | 空き時間確認 |
| `calendar conflicts` | 予定の重複検出 |
| `calendar team <groupEmail>` | チームカレンダー表示 |

### 主要フラグ

| フラグ | 説明 |
|--------|------|
| `--today` | 今日のイベント |
| `--from <date>` | 開始日 |
| `--to <date>` | 終了日 |
| `--summary <text>` | イベント名 |
| `--attendees <emails>` | 参加者（カンマ区切り） |
| `--event-type <type>` | `default`, `focus-time`, `out-of-office` |
| `--all` | 全カレンダー対象 |

### 使用例

```bash
# 今日のイベント
gog calendar events primary --today

# イベント作成（参加者付き）
gog calendar create primary --summary "Meeting" --attendees "a@b.com,c@d.com" \
  --from 2026-04-01T10:00:00+09:00 --to 2026-04-01T11:00:00+09:00

# 空き時間確認
gog calendar freebusy --calendars "primary,work@example.com" \
  --from 2026-04-01T00:00:00Z --to 2026-04-02T00:00:00Z
```

## 3. Drive・Docs・Sheets・Slides

### Drive

| コマンド | 説明 |
|---------|------|
| `drive ls` | ファイル一覧 |
| `drive search <query>` | ファイル検索 |
| `drive upload <path>` | アップロード |
| `drive upload <path> --replace <id>` | ファイル内容更新（共有リンク維持） |
| `drive download <id>` | ダウンロード |
| `drive share <id>` | 共有設定 |
| `drive mkdir <name>` | フォルダ作成 |

### Docs

| コマンド | 説明 |
|---------|------|
| `docs create <title>` | ドキュメント作成 |
| `docs export <id> --format <fmt>` | エクスポート（pdf, docx等） |
| `docs copy <id>` | コピー |
| `docs sed <id> 's/pattern/replacement/g'` | パターン置換（Markdown書式対応） |

### Sheets

| コマンド | 説明 |
|---------|------|
| `sheets get <id> <range>` | セル値取得 |
| `sheets update <id> <range> <values>` | セル値更新 |
| `sheets append <id> <range> <values>` | 行追加 |
| `sheets format <id> <range>` | セル書式設定 |
| `sheets insert <id>` | シート追加 |
| `sheets named-ranges <id>` | 名前付き範囲管理 |

### 使用例

```bash
# ファイル一覧（最大20件）
gog drive ls --max 20

# PDF エクスポート
gog docs export <docId> --format pdf

# スプレッドシート読み取り
gog sheets get <sheetId> 'Sheet1!A1:B10'

# セル更新（パイプ区切り=列、カンマ区切り=行）
gog sheets update <sheetId> 'A1' 'val1|val2,val3|val4'
```

## 4. Tasks

| コマンド | 説明 |
|---------|------|
| `tasks lists` | タスクリスト一覧 |
| `tasks lists create <name>` | タスクリスト作成 |
| `tasks list <listId>` | タスク一覧 |
| `tasks add <listId> --title <text>` | タスク追加 |
| `tasks done <listId> <taskId>` | タスク完了 |
| `tasks delete <listId> <taskId>` | タスク削除 |
| `tasks clear <listId>` | 完了タスク一括削除 |

### 繰り返しタスク

```bash
gog tasks add <listId> --title "Weekly sync" --repeat weekly --repeat-count 4
gog tasks add <listId> --title "Custom" --rrule "FREQ=MONTHLY;BYDAY=1MO"
```

## 5. Contacts・People

| コマンド | 説明 |
|---------|------|
| `contacts list` | 連絡先一覧 |
| `contacts search <query>` | 連絡先検索 |
| `contacts create` | 連絡先作成 |
| `contacts update <resourceName>` | 連絡先更新 |
| `contacts delete <resourceName>` | 連絡先削除 |
| `people me` / `people whoami` | 自分の情報 |
| `people directory search <query>` | 組織ディレクトリ検索（Workspace限定） |

### 使用例

```bash
# 連絡先作成
gog contacts create --given "John" --family "Doe" --email "john@example.com"

# 連絡先更新
gog contacts update people/<resourceName> --address "123 Main St"
```

## 6. Chat

Workspace 限定サービス。

| コマンド | 説明 |
|---------|------|
| `chat spaces` | スペース一覧 |
| `chat spaces create <name>` | スペース作成 |
| `chat messages send <spaceId>` | メッセージ送信 |
| `chat dm <email>` | ダイレクトメッセージ |

## 7. その他サービス

### Keep（Workspace限定）

| コマンド | 説明 |
|---------|------|
| `keep create --title <text>` | ノート作成 |
| `keep search <query>` | ノート検索 |

```bash
gog keep create --title "Todo" --item "Milk" --item "Eggs"
```

### Forms

| コマンド | 説明 |
|---------|------|
| `forms create --title <text>` | フォーム作成 |
| `forms add-question <formId>` | 質問追加 |
| `forms responses <formId>` | 回答一覧 |

### Apps Script

| コマンド | 説明 |
|---------|------|
| `appscript run <scriptId> <function>` | 関数実行 |
| `appscript content <scriptId>` | スクリプト内容表示 |

```bash
gog appscript run <scriptId> myFunction --params '["arg1", 123]'
```

### Classroom（Workspace Education限定）

コース管理、課題作成、提出・採点、告知、Guardian 通知、トピック管理。

### Admin（Workspace限定、Service Account必須）

```bash
gog admin users list --domain example.com
gog admin groups members add engineering@example.com user@example.com
```

### Groups

```bash
gog groups list
gog groups members <groupEmail>
```

### Time（ユーティリティ）

```bash
gog time now --timezone America/New_York
```

## 8. 共通フラグ

| フラグ | 説明 | 例 |
|--------|------|-----|
| `--account <email>` | アカウント指定 | `--account work@co.com` |
| `--json` | JSON出力 | スクリプト向け |
| `--plain` | TSV出力 | パイプ向け |
| `--max <n>` | 最大件数 | `--max 50` |
| `--force` | 確認スキップ | 破壊的操作時 |
| `--no-input` | 非対話モード | CI向け |
| `--verbose` | 詳細ログ | デバッグ時 |
| `--color <mode>` | 色制御 | `auto`, `always`, `never` |
| `--readonly` | 読み取り専用 | 安全な操作のみ |
