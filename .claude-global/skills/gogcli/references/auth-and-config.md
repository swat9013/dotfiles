# 認証・設定リファレンス

## TOC

1. [OAuth2 認証フロー](#1-oauth2-認証フロー)
2. [Service Account](#2-service-account)
3. [マルチアカウント管理](#3-マルチアカウント管理)
4. [スコープ最小化](#4-スコープ最小化)
5. [出力制御](#5-出力制御)
6. [設定管理](#6-設定管理)
7. [環境変数一覧](#7-環境変数一覧)

## 1. OAuth2 認証フロー

### 初回セットアップ

Google Cloud Console で OAuth2 Desktop client を作成し、JSON をダウンロード。

```bash
# 1. クレデンシャル登録
gog auth credentials ~/Downloads/client_secret_....json

# 2. アカウント認証（ブラウザが自動で開く）
gog auth add you@gmail.com

# 3. スコープ指定付き認証
gog auth add you@gmail.com --services drive,gmail,calendar
```

### Headless/リモート認証

ブラウザが使えない環境向けの代替フロー:

```bash
# 手動フロー: URLを別マシンで開いてリダイレクトURLを貼り付け
gog auth add you@gmail.com --manual

# リモート分割フロー: 2段階キャッシュ方式
gog auth add you@gmail.com --remote
```

### Keyring バックエンド

| バックエンド | 環境 | 設定方法 |
|-------------|------|---------|
| auto（デフォルト） | デスクトップ | 自動選択（macOS Keychain等） |
| file | Headless/CI/Docker | `GOG_KEYRING_BACKEND=file` |

File keyring 使用時は `GOG_KEYRING_PASSWORD` でパスフレーズを指定。

### トークン管理

```bash
gog auth list                # 認証済みアカウント一覧
gog auth list --check        # トークン有効性検証
gog auth tokens delete <email>  # トークン削除（再認証が必要）
```

## 2. Service Account

Workspace 環境で domain-wide delegation を使用する場合:

```bash
gog auth service-account set you@yourdomain.com --key service-account.json
```

前提条件:
- Google Cloud Console で Service Account を作成
- Workspace 管理コンソールで OAuth スコープの委譲を設定
- Admin, Classroom, Keep 等の Workspace 専用サービスに必須

## 3. マルチアカウント管理

### アカウント切り替え

```bash
# コマンドごとに指定
gog --account work@company.com gmail search "is:unread"

# デフォルトアカウント設定
export GOG_ACCOUNT=you@gmail.com
```

### Named Clients

プロジェクト別に OAuth クライアントを分離:

```bash
# 別クライアントでクレデンシャル登録
gog --client work auth credentials ~/work-client.json
gog --client work auth add you@company.com

# ドメイン→クライアント自動ルーティング
gog config set client-routes.company.com work
```

### エイリアス

```bash
gog config set alias.work work@company.com
gog --account work gmail search "subject:report"
```

## 4. スコープ最小化

### サービス単位の制限

```bash
# 必要なサービスのみ認証
gog auth add you@gmail.com --services drive,calendar

# 読み取り専用
gog auth add you@gmail.com --services drive --drive-scope readonly
```

### Drive スコープレベル

| スコープ | 説明 |
|---------|------|
| `full` | 全ファイルアクセス |
| `readonly` | 読み取りのみ |
| `file` | アプリ作成ファイルのみ |

### スコープ追加・再認証

```bash
# 既存アカウントにスコープ追加
gog auth add you@gmail.com --force-consent --services drive,gmail,sheets

# カスタム OAuth スコープ追加
gog auth add you@gmail.com --extra-scopes "https://www.googleapis.com/auth/..."
```

## 5. 出力制御

| フラグ | 形式 | 用途 |
|--------|------|------|
| （なし） | 色付きテーブル | 対話利用 |
| `--json` | JSON | スクリプト・jq連携 |
| `--plain` | TSV | パイプ・awk連携 |

- エラー・進捗は stderr に出力（stdout は常にクリーン）
- `--color auto|always|never` で色制御
- `--force` で確認プロンプトスキップ
- `--no-input` で非対話モード（プロンプト時にエラー終了）

### スクリプト統合パターン

```bash
# JSON + jq でバッチ処理
gog --json gmail search 'from:noreply@example.com' --max 200 \
  | jq -r '.threads[].id' \
  | xargs -n 50 gog gmail labels modify --remove UNREAD

# 完全非対話実行（CI向け）
gog --json --force --no-input gmail search "newer_than:1d"
```

## 6. 設定管理

設定ファイルの場所:

| OS | パス |
|----|------|
| macOS | `~/Library/Application Support/gogcli/config.json` |
| Linux | `~/.config/gogcli/config.json` |

```bash
gog config list          # 全設定表示
gog config get <key>     # 個別取得
gog config set <key> <value>  # 設定変更
gog config unset <key>   # 設定削除
gog config keys          # 利用可能なキー一覧
gog config path          # 設定ファイルパス表示
```

## 7. 環境変数一覧

| 変数 | 説明 |
|------|------|
| `GOG_ACCOUNT` | デフォルトメール/エイリアス |
| `GOG_ACCESS_TOKEN` | 直接アクセストークン（リフレッシュなし、CI向け） |
| `GOG_CLIENT` | OAuth クライアント名 |
| `GOG_JSON` | デフォルト JSON 出力（`true` で常時JSON） |
| `GOG_KEYRING_BACKEND` | Keyring バックエンド（`auto`, `file`） |
| `GOG_KEYRING_PASSWORD` | File keyring のパスフレーズ |
| `GOG_ENABLE_COMMANDS` | コマンドホワイトリスト |
| `GOG_TIMEZONE` | Calendar/Gmail 出力タイムゾーン |
