# .envrc → mise.toml [env] 移行ガイド

## 目次

1. [基本変換パターン](#基本変換パターン)
2. [高度なパターン](#高度なパターン)
3. [mise.toml の env 構文リファレンス](#misetoml-の-env-構文リファレンス)
4. [トラブルシューティング](#トラブルシューティング)

---

## 基本変換パターン

### 環境変数の直接定義

```bash
# .envrc
export DATABASE_URL="postgres://localhost/mydb"
export RAILS_ENV="development"
```

```toml
# mise.toml
[env]
DATABASE_URL = "postgres://localhost/mydb"
RAILS_ENV = "development"
```

### .env ファイル読み込み

```bash
# .envrc
dotenv
# または
source_env .env
```

```toml
# mise.toml
[env]
_.file = ".env"
```

複数ファイル:

```toml
[env]
_.file = [".env", ".env.local"]
```

### PATH追加

```bash
# .envrc
PATH_add ./bin
PATH_add ./node_modules/.bin
```

```toml
# mise.toml
[env]
_.path = ["./bin", "./node_modules/.bin"]
```

---

## 高度なパターン

### layout python（venv自動作成）

```bash
# .envrc
layout python3
```

```toml
# mise.toml
[tools]
python = "3.12"

[env]
VIRTUAL_ENV = "{{config_root}}/.venv"
_.path = [".venv/bin"]
```

`mise install` 後、`python -m venv .venv` は手動実行が必要。miseはvenvの自動作成を行わない。

### use node / use ruby

```bash
# .envrc
use node
use ruby
```

```toml
# mise.toml
[tools]
node = "22"
ruby = "3.3"
```

miseが自動でPATHを管理するため、`use` ディレクティブは不要。

### 条件付き環境変数

```bash
# .envrc
if [ -f .env.local ]; then
  source_env .env.local
fi
```

mise.tomlでは条件分岐不可。代替:

```toml
# mise.toml
[env]
# .env.local が存在しなくてもエラーにならない
_.file = ".env.local"
```

### 他ディレクトリの.envrc参照

```bash
# .envrc
source_env ../shared/.envrc
```

mise.tomlでは直接対応不可。代替:

```toml
# mise.toml
[env]
_.file = "../shared/.env"
```

共有環境変数は `.env` ファイル経由で読み込む形に変更する。

---

## mise.toml の env 構文リファレンス

### 特殊キー（`_` プレフィックス）

| キー | 用途 |
|------|------|
| `_.file` | ファイルから環境変数を読み込み |
| `_.path` | PATHに追加 |
| `_.source` | シェルスクリプトをsourceして環境変数を取り込み |

### テンプレート変数

```toml
[env]
PROJECT_ROOT = "{{config_root}}"
LOG_DIR = "{{config_root}}/logs"
```

| 変数 | 展開先 |
|------|--------|
| `{{config_root}}` | mise.toml があるディレクトリの絶対パス |
| `{{cwd}}` | カレントディレクトリ |

### _.source（シェルスクリプト実行）

複雑な `.envrc` ロジックをそのまま移行する最終手段:

```toml
[env]
_.source = "./env.sh"
```

`env.sh` 内で `export` された変数がmise環境に取り込まれる。ただし、direnvの `layout`/`use` 等の組み込み関数は使えない。

---

## トラブルシューティング

| 症状 | 原因・対処 |
|------|---------|
| 環境変数が反映されない | `mise trust` を実行。新ディレクトリの mise.toml は信頼が必要 |
| 変更が反映されない | `cd .` で再評価、またはシェル再起動 |
| _.file で指定したファイルがない | エラーにならず無視される（安全） |
| direnvと競合 | direnvは削除済み。`~/.cache/direnv/` が残っていれば削除 |
