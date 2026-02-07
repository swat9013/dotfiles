---
name: ghq
description: ghqによるリポジトリ管理のガイド。コマンド仕様、ローカルリポジトリ取り込み手順を含む。「ghq」「リポジトリ管理」「リポジトリをクローン」と依頼された時に参照する。
user-invocable: false
---

# ghq

リポジトリをローカルで一元管理するためのツール操作スキル。

## 基本コマンド

### リポジトリ取得

```bash
ghq get <repo>                    # クローン
ghq get -p <repo>                 # SSH でクローン
ghq get --shallow <repo>          # shallow clone
ghq get -u <repo>                 # 既存なら pull
```

**<repo> の形式**:
- `https://github.com/owner/repo`
- `git@github.com:owner/repo.git`
- `owner/repo`（GitHub の場合）

### リポジトリ一覧

```bash
ghq list                          # 相対パス一覧
ghq list -p                       # フルパス一覧
ghq list <query>                  # フィルタリング
```

### パス操作

```bash
ghq root                          # ルートディレクトリ表示
ghq root -all                     # 全ルート表示
```

### リポジトリ作成・削除

```bash
ghq create <repo>                 # 新規作成
ghq rm <repo>                     # 削除（確認あり）
```

## ローカルリポジトリの取り込み

ghq 管理外のローカルリポジトリを管理下に移動するワークフロー。

### 前提条件

- 対象ディレクトリが git リポジトリであること
- リモート origin が設定されていること（推奨）

### 手順

#### 1. 情報収集

```bash
# ghq ルートを確認
GHQ_ROOT=$(ghq root)

# リモート URL を取得
cd /path/to/local/repo
REMOTE_URL=$(git remote get-url origin 2>/dev/null)
```

#### 2. 移動先パスの決定

**リモートがある場合**:
```bash
# URL からホスト/オーナー/リポジトリを抽出
# 例: git@github.com:owner/repo.git → github.com/owner/repo
# 例: https://gitlab.com/owner/repo → gitlab.com/owner/repo
```

**リモートがない場合**:
```bash
# ローカル専用として配置
# $GHQ_ROOT/local/<repo-name>
```

#### 3. 移動実行

```bash
# 移動先ディレクトリを作成
mkdir -p "$(dirname "$GHQ_ROOT/<host>/<owner>/<repo>")"

# リポジトリを移動
mv /path/to/local/repo "$GHQ_ROOT/<host>/<owner>/<repo>"
```

#### 4. 確認

```bash
# ghq list に表示されることを確認
ghq list | grep <repo>
```

### 移動スクリプト例

```bash
#!/bin/bash
set -euo pipefail

LOCAL_REPO="$1"
GHQ_ROOT=$(ghq root)

cd "$LOCAL_REPO"
REMOTE_URL=$(git remote get-url origin 2>/dev/null || echo "")

if [[ -z "$REMOTE_URL" ]]; then
  DEST="$GHQ_ROOT/local/$(basename "$LOCAL_REPO")"
else
  # URL を解析してパスを生成
  if [[ "$REMOTE_URL" =~ ^git@([^:]+):(.+)\.git$ ]]; then
    HOST="${BASH_REMATCH[1]}"
    PATH_PART="${BASH_REMATCH[2]}"
  elif [[ "$REMOTE_URL" =~ ^https?://([^/]+)/(.+?)(\.git)?$ ]]; then
    HOST="${BASH_REMATCH[1]}"
    PATH_PART="${BASH_REMATCH[2]}"
  else
    echo "Unknown URL format: $REMOTE_URL" >&2
    exit 1
  fi
  DEST="$GHQ_ROOT/$HOST/$PATH_PART"
fi

if [[ -e "$DEST" ]]; then
  echo "Destination already exists: $DEST" >&2
  exit 1
fi

mkdir -p "$(dirname "$DEST")"
mv "$LOCAL_REPO" "$DEST"
echo "Moved to: $DEST"
```

## fzf 連携

```bash
# リポジトリ選択して cd
cd "$(ghq list -p | fzf)"

# リポジトリ選択して VSCode で開く
code "$(ghq list -p | fzf)"
```

## 設定（~/.gitconfig）

```ini
[ghq]
  root = ~/ghq
  # root = ~/src  # 複数ルート可
```

## よくある操作

| 目的 | コマンド |
|------|----------|
| GitHub リポジトリをクローン | `ghq get owner/repo` |
| 全リポジトリを更新 | `ghq list | xargs -I{} ghq get -u {}` |
| リポジトリを検索して移動 | `cd "$(ghq list -p \| fzf)"` |
| ローカルリポを管理下へ | 上記ワークフロー参照 |

## 成功基準

1. `ghq list`に対象リポジトリが表示される
2. 目的のパスにリポジトリディレクトリが存在する
3. リモート設定が期待通り（該当する場合）

## 完了チェックリスト

- [ ] 操作が正常終了した（エラーなし）
- [ ] ghq listで対象が確認できる
- [ ] 目的のパスに配置されている
- [ ] 必要に応じて.gitconfigのghq.rootを確認した
