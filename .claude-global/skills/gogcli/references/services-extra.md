# サービスコマンドリファレンス（補足）

Drive / Docs / Sheets / Contacts / Tasks の詳細コマンド。

## Drive・Docs・Sheets・Slides

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

## Tasks

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

## Contacts・People

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
