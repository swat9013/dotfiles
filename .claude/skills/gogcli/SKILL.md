---
name: gogcli
description: |-
  Google Workspace CLI（gogcli）の認証設定・サービス操作・トラブルシューティングを支援する知識ベーススキル。
  Use when「gogcli」「gog」「Gmail CLI」「Google Calendar CLI」「Google Drive CLI」「Google Workspace」。
user-invocable: false
---

# gogcli 知識ベース

## バージョン情報

現在のバージョン: `!`gog version 2>/dev/null || echo "(未インストール)"``

記録バージョン: `0.12.0`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

## コマンド早見表

```bash
gog auth credentials <path>           # OAuth2クレデンシャル登録
gog auth add <email>                   # アカウント認証
gog auth list --check                  # トークン有効性検証
gog gmail search "is:unread"           # Gmail検索
gog gmail send --to <email> --body "." # メール送信
gog calendar events primary --today    # 今日のイベント一覧
gog drive ls                           # Driveファイル一覧
gog drive upload ./file.pdf            # ファイルアップロード
gog tasks add <listId> --title "task"  # タスク追加
gog contacts search "name"             # 連絡先検索
gog config list                        # 設定一覧
gog --json <command>                   # JSON出力
gog --account <email> <command>        # アカウント指定
```

## 主要カテゴリ概要

| カテゴリ | 代表コマンド | 詳細 |
|---------|-------------|------|
| 認証・アカウント | `auth add`, `auth list`, `auth credentials` | references/auth-and-config.md §1-§3 |
| スコープ管理 | `--services`, `--readonly`, `--drive-scope` | references/auth-and-config.md §4 |
| 出力制御・設定 | `--json`, `--plain`, `config list` | references/auth-and-config.md §5-§7 |
| Gmail | `gmail search`, `gmail send`, `gmail labels` | references/services.md §1 |
| Calendar | `calendar events`, `calendar create`, `calendar freebusy` | references/services.md §2 |
| Drive・Docs・Sheets | `drive ls`, `docs export`, `sheets get` | references/services.md §3 |
| Tasks・Contacts・Chat | `tasks add`, `contacts list`, `chat spaces` | references/services.md §4-§6 |
| その他サービス | `keep`, `forms`, `appscript`, `classroom`, `admin` | references/services.md §7 |

## トラブルシューティング要点

| 症状 | 原因・対処 |
|------|-----------|
| `403 insufficient scopes` | `gog auth add <email> --force-consent --services <svc>` でスコープ追加再認証 |
| トークン無効・期限切れ | `gog auth list --check` で確認 → 再認証 |
| Headless で keyring エラー | `GOG_KEYRING_BACKEND=file` + `GOG_KEYRING_PASSWORD` を設定 |
| コマンド未検出 | `brew install gogcli` でインストール確認 |
| 複数アカウント混乱 | `gog auth list` で確認、`--account <email>` で明示指定 |
| Docker/CI 認証失敗 | `GOG_ACCESS_TOKEN` で直接トークン指定、または file keyring 使用 |

## references 案内

| ファイル | 内容 |
|---------|------|
| `references/auth-and-config.md` | 認証フロー・マルチアカウント・スコープ・出力制御・設定・環境変数 |
| `references/services.md` | 全サービスのサブコマンド・フラグ・使用例 |
